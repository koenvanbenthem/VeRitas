
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
#' @import tidyr
drawNet <- function(net,draw=c('none','interactive','static'),...){
  df <- do.call(rbind,net)
  df$node <- rownames(df)
  if(!all(df$goT %in% c(df$node,'END'))){
    warning("Missing nodes in goT!")
  }


  if(!all(df$goF %in% c(df$node,'END'))){
    warning("Missing nodes in goF!")
  }
  allnodes <- c(df$node,"END")
  df.long <- df %>% tidyr::pivot_longer(cols=goT:goF)
  df.long$name <- paste(ifelse(df.long$name=="goT","True\n","False\n"),ifelse(df.long$name=="goT",df.long$doT,df.long$doF))
  relations <- data.frame(from=df.long$node,to=df.long$value,label=df.long$name)

  g <- graph_from_data_frame(relations,vertices=data.frame(id=allnodes,label=paste(allnodes,df$cond[match(allnodes,df$node)],sep="\n")))

  if(draw=="interactive"){
    tkplot(g)
  }else if(draw=="static"){
    plot(g,...)
  }
  # make graph

  if(!is_acyclic(g)){
    warning("The graph is not acyclic. There is a risk of infinite loops. Proceed with extreme caution.")
  }

}

#' Function necessary for preparing the net for usage
#' @export
cleanNet <- function(net){
  net <- do.call(rbind,net)
  net$condition <- gsub(")$",",obj=obj)",net$condition)
  net
}

#' Extract the functions that should be tracked
#' @export
extractFuns <- function(net){
  trackenv <- makeTrackEnv()
  totrack <- lapply(net$condition, FUN = function(x) eval(parse(text=x),envir=trackenv))
  totrack <- totrack[!unlist(lapply(totrack,is.null))]
}

#' function for making an environment that redefines all test functions,
#' so did_call can be evaluated
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
