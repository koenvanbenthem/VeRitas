
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
