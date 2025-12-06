
net <- list(start=node(has_val(ref$sol),
                       goT=S2,goF=S1,
                       doT="[✓] Sum is found!",
                       doF="[x] Sum not found"),

            S1=node(has_val(ref$wrongsol),
                    goT=END,goF=END,
                    doT="[?] Did you square?"),

            S2=node(did_call("sum","base"),
                    goT=END,goF=END,
                    doF="[?] Could the sum function help?")
)
MyAssignment <- prepareAssignment(net,"examples/01-simplest/ref1.R")
myout <- runBatch("C:/Documents and Settings/Koen/Downloads/Assignment exercise Lecture 6 Download 04 December 2025 447 PM",assignment=MyAssignment,rec=TRUE)
nets <- lapply(myout, FUN = function(x) x$net)
myout[[1]]$obj$filename
nets.df <- do.call(rbind,nets)
library(dplyr)
nets.df %>% group_by(condition,doT,doF,goT,goF) %>% summarize(nT=sum(nT),nF=sum(nF))
which(unlist(lapply(nets,FUN = function(x) x$nT[1]==1)))
myout[[125]]$obj$filename
MyAssignment$ref$sol
