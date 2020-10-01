extract.optline <-
function(matlist){
#  resmat <- matrix(0,length(matlist),ncol(matlist[[1]]))
#  resmat <- as.data.frame(resmat)
#   colnames(resmat) <- colnames(matlist[[1]])
  
  resmat <- subset(matlist[[1]], opt.position==1)
  if(length(matlist)>1){
    for(i in 2:length(matlist)){
      resmat <- rbind(resmat,subset(matlist[[i]], opt.position==1))
    }
  }
  resmat
}

