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
# Build website through pkgdown::build_site()


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

#' Assignment setup
#'
#' Uses an assessment net (a named list of nodes) and a reference file to prepare an assignment.
#' @param net A named list of nodes (see \link{node}) that describes what checks should be performed and what feedback they yield. The first node should be named start, and any final nodes should be named END. No other nodes should start with these words in their name.
#' @param referencefile An R script that contains a reference solution. The checks in the nodes use values from the reference solution.
#' @return Returns an initialized assignment, which is in essence a list that contains the net, the functions that should be tracked, and the output of the reference file.
#' @seealso \link{runSingle} for a simple example.
#' @export
prepareAssignment <- function(net,referencefile){
  ref <- new.env()
  source(referencefile,local=ref)
  ref <- as.list(ref)

  net <- cleanNet(net)
  trackfuns <- extractFuns(net)

  list(net=net,trackfuns=trackfuns,ref=ref)
}

#' Evaluate a single script.
#'
#' This function evaluates a single .R file based on a prepared assignment.
#' @param studentfile The name (and location) of the file that is to be evaluated.
#' @param prepared A prepared assignment using \link{prepareAssignment}
#' @returns The feedback is shown on the screen.
#' @examples
#' # example code
#' library(VeRitas)
#' net <- list(start=node(has_val(ref$sol),
#'                        goT=S2,goF=S1,
#'                        doT="[✓] Sum is found!",
#'                        doF="[x] Sum not found"),
#'
#'             S1=node(has_val(ref$wrongsol),
#'                     goT=END,goF=END,
#'                     doT="[?] Did you square?"),
#'
#'              S2=node(did_call("sum","base"),
#'                     goT=END,goF=END,
#'                     doF="[?] Could the sum function help?")
#' )
#' MyAssignment <- prepareAssignment(net,"../../examples/01-simplest/ref1.R")
#' runSingle("../../examples/01-simplest/code2.R",MyAssignment)
#' @export
runSingle <- function(studentfile,prepared){
  runEval(studentfile,prepared$trackfuns,prepared$ref,prepared$net)
}
