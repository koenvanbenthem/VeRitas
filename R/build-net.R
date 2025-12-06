
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
  }else{

    goTsubs <- substitute(goT)
    if(class(goTsubs)=="name"){
      goT <- deparse(goTsubs)
    }
  }

  if(missing(goF)){
    goF <- "END"
  }else{

    goFsubs <- substitute(goF)
    if(class(goFsubs)=="name"){
      goF <- deparse(goFsubs)
    }
  }

  if(missing(cond)){
    cond <- ""
  }else{

    condsubs <- substitute(cond)
    if(class(condsubs)=="call"){
      cond <- deparse(condsubs)
    }
  }
  data.frame(condition=cond,goT=goT,goF=goF,doT=doT,doF=doF)
}


#' Draw a decision network
#' @export
#' @import igraph
#' @import tidyr
drawNet <- function(net,draw=c('none','interactive','static'),...){
  if(is.data.frame(net)){
    df <- net
    df$condition <- gsub(",obj=obj)$",")",df$condition)
  }else{
    df <- do.call(rbind,net)
  }
  df$node <- rownames(df)
  if(!all(df$goT %in% c(df$node,'END'))){
    warning("Missing nodes in goT!")
  }


  if(!all(df$goF %in% c(df$node,'END'))){
    warning("Missing nodes in goF!")
  }

  if(!any(c(df$goF,df$goT)=="END")){
    warning("No edges ending in 'END' detected")
  }

  if(any(grepl("^END",df$node))){
    warning("It is strongly advised not to have nodes start with END - it may lead to unexpected behaviour!")
  }
  df.long <- df %>% tidyr::pivot_longer(cols=goT:goF)
  df.long$fullname <- paste(ifelse(df.long$name=="goT","True\n","False\n"),ifelse(df.long$name=="goT",df.long$doT,df.long$doF))
  #df.long$name[df.long$value=="END"] <- ""
  df.long$value[df.long$value=="END"] <- paste0("END",1:sum(df.long$value=="END"))
  allnodes <- unique(c(df.long$node,df.long$value))
  relations <- data.frame(from=df.long$node,to=df.long$value,label=df.long$fullname,arrow.mode=2*as.numeric(!grepl("^END",df.long$value)))
  # relations$width <- 1:(nrow(relations))
  vertexlabels <- paste(allnodes,df$cond[match(allnodes,df$node)],sep="\n")
  vertexlabels[grepl(pattern = "^END",allnodes)] <- ""
  g <- graph_from_data_frame(relations,vertices=data.frame(id=allnodes,label=vertexlabels,shape=c("none","circle")[1+as.numeric(grepl("^END",allnodes))],color="grey"))
  E(g)$color <- c('#990000','#009900')[1+as.numeric(df.long$name=="goT")]

  if(draw=="interactive"){
    tkplot(g,edge.arrow.size=0.5,edge.arrow.width=1,edge.label.color=E(g)$color,vertex.label.color="black",...)
  }else if(draw=="static"){
    plot(g,edge.arrow.size=0.5,edge.arrow.width=1,edge.label.color=E(g)$color,vertex.label.color="black",...)
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
