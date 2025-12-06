#' Function for running a set of scripts
#' Use with caution!
#' If you run this script, all submitted R scripts will be executed automatically.
#' Be sure that you trust these scripts - they could severly harm your computer.
#' @import crayon
#' @export
runBatch <- function(dirname,assignment,pattern=".*\\.R$",rec=FALSE,showfiles=TRUE){

  allFiles <- list.files(dirname,pattern=pattern,recursive = rec,full.names = FALSE)
  cat("The following files have been found:\n")
  cat(allFiles,sep="\n")
  cat(red$bold("\n\nIf you proceed - all these R files will be executed, with potentially harmful consequences to your computer. Are you certain you want to proceed? Proceeding is entirely at your own risk!\n\n"))
  inp <- "waiting"
  iter <- 1
  while(!inp %in% c('yes','no') & iter < 5){
  if(iter>1){
    cat(red("Inut not recognized. Please type either yes or no."))
  }
  inp <- readline("Do you wish to run all these R files? (yes/no)")
  iter <- iter + 1
  }
  if(!inp %in% c('yes','no')){
    stop("Function arborted -- no valid yes or no answer detected.")
  }

  if(inp =='no'){
    stop("Function aborted by user.")
    return(1)
  }
  cat(inp)
  lapply(allFiles, FUN = function(x) runEval(file.path(dirname,x),assignment$trackfuns,assignment$ref,assignment$net,verbose = TRUE,netcount = TRUE))
}

# myout <- runBatch("C:/Documents and Settings/Koen/Downloads/Assignment exercise Lecture 6 Download 04 December 2025 447 PM",assignment=MyAssignment,rec=TRUE)
