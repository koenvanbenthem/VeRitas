#
# You can learn more about package authoring with RStudio at:
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#
# Run roxygen through roxygen2::roxygenise()



# boe <- function(am,b,d=3,...){
#   if(!missing(...)){
#     append(as.list(environment()),list(...))
#   }else{
#     as.list(environment())
#   }
#
# }
# boe(1,a=7)

#' Create a new node in the decision network
#' @export
node <- function(cond,goT="END",goF="END",doT="",doF=""){
  if(missing(doT)){
    doT <- ""
  }

  if(missing(doF)){
    doF <- ""
  }

  if(missing(goT)){
    goT <- "END"
  }

  if(missing(goF)){
    goF <- "END"
  }

  if(missing(cond)){
    cond <- ""
  }
  data.frame(condition=cond,goT=goT,goF=goF,doT=doT,doF=doF)
}


#' Draw a decision network
#' @export
#' @import igraph
#' @import tidyverse
drawNet <- function(net){
  df <- do.call(rbind,net)
  df$node <- rownames(df)
  if(!all(df$goT %in% c(df$node,'END'))){
    warning("Missing nodes in goT!")
  }


  if(!all(df$goF %in% c(df$node,'END'))){
    warning("Missing nodes in goF!")
  }
  allnodes <- c(df$node,"END")
  df.long <- df %>% pivot_longer(cols=goT:goF)
  df.long$name <- paste(ifelse(df.long$name=="goT",df.long$doT,df.long$doF),ifelse(df.long$name=="goT","Yes","No"))
  relations <- data.frame(from=df.long$node,to=df.long$value,label=df.long$name)

  g <- graph_from_data_frame(relations,vertices=data.frame(id=allnodes,label=paste(allnodes,df$cond[match(allnodes,df$node)],sep="\n")))
  tkplot(g)
  # make graph
  is_acyclic(g)

}

find_vec_recursively <- function(value,obj,ord=TRUE,tol=sqrt(.Machine$double.eps),absval=FALSE){

  any(unlist(lapply(obj, FUN = function(x){
    if(!is.list(x)){
      if(is.numeric(x) & length(x)==length(value)){
        if(!any(is.na(x))){
          if(absval){ x <- abs(x)}
          if(!ord){
            x <- sort(x)
            value <- sort(value)
          }

          all(abs(x-value)<tol,na.rm = TRUE)
        }else{
          FALSE
        }
      }else{
        FALSE
      }
    }else{
      find_vec_recursively(value,x,ord,tol=tol,absval=absval)
    }
  })))
}

has_vec <- function(vec,obj,ord=TRUE,tol= sqrt(.Machine$double.eps),absval=FALSE){

  meh <- (obj$fullRun$output[!obj$fullRun$outputType %in% c('source','recordedplot')])
  c2 <- find_vec_recursively(vec,meh,ord,tol,absval)
  # the output on the screen -> is it on the screen?

  # finally, a list of the defined variables
  c3 <- find_vec_recursively(vec,obj$vars,ord,tol,absval)#any(unlist(lapply(obj$vars, FUN = function(x) isTRUE(all.equal(x,vec,tolerance=tol)))))
  c2 | c3
}

find_val_recursively <- function(value,obj,tol=sqrt(.Machine$double.eps),absval=FALSE){
  any(unlist(lapply(obj, FUN = function(x){
    if(!is.list(x)){
      if(is.numeric(x)){
        if(absval){ x <- abs(x)}
        any(abs(x-value)<tol,na.rm = TRUE)
      }else if(typeof(x)==typeof(value)){
        any(x==value)
      }else{
        FALSE
      }
    }else{
      find_val_recursively(value,x,tol=tol,absval=absval)
    }
  })))
}

has_each_val <- function(value,obj,text=FALSE,tol= sqrt(.Machine$double.eps),absval=FALSE){

  all(sapply(value, FUN = function(x){ has_val(x,obj,text=text,tol=tol,absval=absval)}))
}

has_val <- function(value,obj,text=FALSE,tol= sqrt(.Machine$double.eps),absval=FALSE){

  # 3 sources: 1. code and comments, 2. output, 3. constructed variables
  # obj$code # the code -> is it in the text?
  if(text){
    c1 <- any(grepl(value,obj$code,fixed=TRUE))
  }else{
    c1 <- FALSE
  }
  meh <- (obj$fullRun$output[!obj$fullRun$outputType %in% c('source','recordedplot')])
  # print(value)
  # print(meh)
  c2 <- any(unlist(lapply(meh, FUN = function(x){
    if(is.numeric(x)){
      if(absval){ x <- abs(x)}
      any(abs(x-value)<tol,na.rm = TRUE)
    }else if(typeof(x)==typeof(value)){
      any(x==value)
    }else if(is.list(x)){
      find_val_recursively(value,x,tol=tol,absval=absval)
    }else{
      FALSE
    }
  })))
  # the output on the screen -> is it on the screen?

  # finally, a list of the defined variables
  c3 <- any(unlist(lapply(obj$vars, FUN = function(x){
    if(is.numeric(x)){
      if(absval){ x <- abs(x)}
      any(abs(x-value)<tol,na.rm = TRUE)
    }else if(is.list(x)){
      find_val_recursively(value,x,tol=tol,absval=absval)
    }else{
      FALSE
    }
  })))
  c1 | c2 | c3
}

did_call <- function(fun,pkg,obj,args=NULL,tol= sqrt(.Machine$double.eps)){

  allCalls <- lapply(obj$funCalls, FUN = function(x){
    if(x[[1]] == fun){
      if(!is.null(args) & length(x)>1){
        bla <- lapply(args,FUN = function(y) find_val_recursively(y,x[-1],tol=tol))
        all(unlist(bla))
      }else{
        TRUE
      }
    }else{
      FALSE
    }
  })

  if(any(unlist(allCalls)) > 0){
    TRUE
  }else{
    FALSE
  }
}
## Local Checkr
checkRstart <- function(filename,funList=NULL,csv,killFuns){

  obj <- list(filename=filename)
  # 1. Find and load file
  obj <- fileImport(obj)

  # 2. For each code file, run the file
  obj <- codeRunner(obj,funList=funList,csv=csv,killFuns=killFuns)
  obj
}

#' Function for running the code
#' @import evaluate
codeRunner <- function(obj,varsAsList=TRUE,funList=NULL,csv,killFuns){
  vars <- new.env()

  # remove illegal operations from the code -> currently doesn't do anything, but maybe useful for later
  obj$code <- cleanCode(obj$origCode)

  obj$fullRun <- list()
  obj$funCalls <- list()
  if(!is.null(funList) & length(funList)>0){
    for(i in 1:length(funList)){
      catchFun(funList[[i]][1],funList[[i]][2])
    }
  }

  if(csv != ''){
    csvFun(csv)
  }

  if(length(killFuns)>0){
    for(i in 1:length(killFuns)){
      killFun(killFuns[i])
    }
  }


  # Remove unexpected symbol mentions and try again
  unexpSymbol <- TRUE
  iter <- 1
  while(unexpSymbol & iter < 100){
    obj$fullRun$output <- evaluate(obj$code,envir = vars,output_handler = new_output_handler(value=identity))
    obj$fullRun$outputType <- unlist(sapply(obj$fullRun$output,function(x) paste(class(x),collapse="")))
    errs <- which(obj$fullRun$outputType == "simpleErrorerrorcondition")
    unexpSymbol <- FALSE
    if(length(errs)>0){
      for(i in 1:length(errs)){
        x <- obj$fullRun$output[errs][[1]]
        if(grepl("unexpected symbol",as.character(x))){
          unexpSymbol <- TRUE
          print(strsplit(as.character(x),split=":")[[1]])
          lineNo <- as.numeric(strsplit(as.character(x),split=":")[[1]][3])
          obj$code[lineNo] <- paste("#",obj$code[lineNo])
        }
      }
    }
    iter <- iter + 1
  }
  if(varsAsList){
    obj$vars <- as.list(vars) # store all variables
  }else{
    obj$vars <- vars
  }
  #
  return(obj)
}

fileImport <- function(obj){
  obj$origCode <- readLines(obj$filename,warn = FALSE)
  return(obj)
}

killFun <- function(fun){
  eval(parse(text=paste(fun," <- function(...){}",sep="")),envir=parent.frame())
}

csvFun <- function(csv){
  for(csvfun in c('read.table','read.csv','read.csv2','read.delim','read.delim2')){
    eval(parse(text=paste0(csvfun," <- function(file,...){ utils::",csvfun,"(file='",csv,"',...)}",sep="")),envir=parent.frame())
  }

  for(readrfun in c('read_delim','read_csv','read_csv2','read_tsv')){
    eval(parse(text=paste0(readrfun," <- function(file,...){ readr::",readrfun,"(file='",csv,"',...)}",sep="")),envir=parent.frame())
  }
}

catchFun <- function(lib,fun){
  eval(parse(text=paste(fun," <- function(...){\n
  #print(lapply(as.list(match.call()),eval))\n
  #obj$funCalls <<- append(obj$funCalls,list(as.list(match.call())))\n

  obj$funCalls <<- append(obj$funCalls,list(list('",fun ,"',...)))\n
  #obj$funCalls <<- eval(parse(text=as.character(bla[[1]][3])),envir=parent.frame())\n#lapply(obj$funCalls[[1]],FUN = function(y) c(7,eval(y)))\n

  ",lib,"::",fun,"(...)\n
   }",sep="")),envir=parent.frame())
}

cleanCode <- function(code){
  inds <- grep("residualPlot",code)
  if(length(inds)>0){
    for(i in 1:length(inds)){
      code[inds[i]] <- paste("residualPlot <- residualPlots<-function(...){};",code[inds[i]])
    }
  }
  return(code)
}

# function for making an environment that redefines all test functions,
# so did_call can be evaluated
makeTrackEnv <- function(){
  trackenv <- new.env()
  eval(parse(text="has_val <- function(...){}"),envir=trackenv)
  eval(parse(text="has_each_val <- function(...){}"),envir=trackenv)
  eval(parse(text="has_vec <- function(...){}"),envir=trackenv)
  eval(parse(text="did_call <- function(...){
             tmp <- match.call()
             c(as.character(tmp[[3]]),as.character(tmp[[2]]))
             }"),envir=trackenv)
  eval(parse(text="has_var <- function(...){}"),envir=trackenv)
  trackenv
}
### END local checkR

######### Run student evalation
runEval <- function(Rfile,totrack,ref,net,csv='',killFuns=c("`?`","setwd","system","file.choose","install.packages","View"),verbose=FALSE){
  # Run code
  ref <- as.list(ref)
  obj <- checkRstart(Rfile,funList = totrack,csv=csv,killFuns=killFuns)

  ######### Run student evaluation through decision net
  currNode <- "start"
  iter <- 0
  while(currNode!="END" & iter < 200){

    thisnode <- net[rownames(net)==currNode,]
    outp <- eval(parse(text=thisnode$condition))

    if(outp){
      if(!thisnode$doT==""){
        out.text <- thisnode$doT
        Encoding(out.text) <- "unknown"
        cat(out.text)
        cat("\n\n")
      }
      currNode <- thisnode$goT
    }else{
      if(!thisnode$doF==""){
        out.text <- thisnode$doF
        Encoding(out.text) <- "unknown"
        cat(out.text)
        cat("\n\n")
      }
      currNode <- thisnode$goF
    }
    iter <- iter + 1
  }

  if(verbose){
    return(obj)
  }
}
