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
# Build website through pkgdown::build_site_github_pages()


# boe <- function(am,b,d=3,...){
#   if(!missing(...)){
#     append(as.list(environment()),list(...))
#   }else{
#     as.list(environment())
#   }
#
# }
# boe(1,a=7)


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

### END local checkR

######### Run student evalation
#' Main function for running a single R script
#' @export
runEval <- function(Rfile,totrack,ref,net,csv='',killFuns=c("`?`","setwd","system","file.choose","install.packages","View"),verbose=FALSE,netcount=FALSE){
  # Run code
  ref <- as.list(ref)
  obj <- checkRstart(Rfile,funList = totrack,csv=csv,killFuns=killFuns)
  net$nT <- 0
  net$nF <- 0

  ######### Run student evaluation through decision net
  currNode <- "start"
  iter <- 0
  while(currNode!="END" & iter < 200){
    thisnodeID <- rownames(net)==currNode
    thisnode <- net[thisnodeID,]
    outp <- eval(parse(text=thisnode$condition))

    if(outp){
      if(!thisnode$doT==""){
        out.text <- thisnode$doT
        Encoding(out.text) <- "unknown"
        cat(out.text)
        cat("\n\n")
      }
      net$nT[thisnodeID] <- net$nT[thisnodeID] + 1
      currNode <- thisnode$goT
    }else{
      if(!thisnode$doF==""){
        out.text <- thisnode$doF
        Encoding(out.text) <- "unknown"
        cat(out.text)
        cat("\n\n")
      }
      net$nF[thisnodeID] <- net$nF[thisnodeID] + 1
      currNode <- thisnode$goF
    }
    iter <- iter + 1
  }
  if (verbose & netcount){
    return(list(net=net,obj=obj))
  }

  if(verbose){
    return(obj)
  }

  if(netcount){
    return(net)
  }
}

#' @export
#'
prepareAssignment <- function(net,referencefile){
  ref <- new.env()
  source(referencefile,local=ref)
  ref <- as.list(ref)

  net <- cleanNet(net)
  trackfuns <- extractFuns(net)

  list(net=net,trackfuns=trackfuns,ref=ref)
}

#' @export
runSingle <- function(studentfile,prepared){
  runEval(studentfile,prepared$trackfuns,prepared$ref,prepared$net)
}
