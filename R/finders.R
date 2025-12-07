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

#' Check whether a vector is produced by code
#'
#' Function for checking whether submitted code contains a certain vector.
#' The search is performed recursively, including columns of data frames.
#' @param vec The vector that one is looking for
#' @param obj An object passed on by the evaluator (need not be supplied by the user)
#' @param ord A boolean that indicates whether the vector should be ordered
#' @param tol The tolerance
#' @param absval Whether absolute values should be matched. If true, values in obj and vec are first turned into their absolute values before checking.
#' @returns true or false depending on whether the submitted code produced the vector, either by storing it into a variable, or by putting it on the screen.
#' @export
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

#' Check whether each value in a vector is produced by code
#'
#' Function for checking whether submitted code produces each value of a certain vector.
#' The search does not care where the values are found. Also, it does not account for multiples
#' (i.e. it does not distinguish between a code that produces a certain value once or one that produces it twice)
#' @param value The vector of values that one is looking for
#' @param obj An object passed on by the evaluator (need not be supplied by the user)
#' @param text A boolean that indicates whether also the code itself should be scanned for the values (and not just the output/generated variables)
#' @param tol The tolerance
#' @param absval Whether absolute values should be matched. If true, values in obj and vec are first turned into their absolute values before checking.
#' @returns true or false
#' @export
has_each_val <- function(values,obj,text=FALSE,tol= sqrt(.Machine$double.eps),absval=FALSE){

  all(sapply(values, FUN = function(x){ has_val(x,obj,text=text,tol=tol,absval=absval)}))
}

#' Check whether a value is produced by code
#'
#' Function for checking whether submitted code produces a single value.
#' The search does not care where the values are found. Also, it does not account for multiples
#' (i.e. it does not distinguish between a code that produces a certain value once or one that produces it twice)
#' @param value The value that one is looking for
#' @param obj An object passed on by the evaluator (need not be supplied by the user)
#' @param text A boolean that indicates whether also the code itself should be scanned for the values (and not just the output/generated variables)
#' @param tol The tolerance
#' @param absval Whether absolute values should be matched. If true, values in obj and vec are first turned into their absolute values before checking.
#' @returns true or false
#' @export
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

#' Check whether a function was called by code
#'
#' Function for checking whether submitted code calls a specific function.
#' Only tracks functions that are called with the package attached (i.e. can track plot, but not base::plot).
#' Cannot track functions that load datasets (i.e. read.csv).
#' @param fun The function that one wants to track, provided as a character
#' @param pkg The packages that the function comes from, provided as a character
#' @param obj An object passed on by the evaluator (need not be supplied by the user)
#' @param args A list of single value arguments that need to have been passed to the tracked function, for did_call to return true
#' @param tol The tolerance for the arguments
#' @returns true or false
#' @seealso \link{runSingle} for an example
#' @export
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
