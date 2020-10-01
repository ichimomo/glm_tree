draw.areaboundary <-
function(lat,lon,areanum,col.var=NULL,add.num=4.5,is.draw=TRUE,add.num2=0,
                              bound.var=NULL,add=FALSE,as.line=FALSE,oldarea=FALSE,...){
  area.vec <- sort(unique(areanum))
  if(add==FALSE && is.draw==TRUE){
    plot(range(lon)+c(0,5),range(lat)+c(0,5),type="n",ylab="Latitude",
         xlab="Longitude")
    if(oldarea==TRUE) write.outearea.Nakano()
  }
  if(is.null(col.var))
    col.var <- gray(seq(from=0.4,to=0.8,length=length(area.vec)))
  if(is.null(bound.var)) 
    bound.var <- rep("darkblue",length(area.vec))

  area.fragment <- rep(0,5)
  
  for(i in 1:length(area.vec)){
    tmp <- area.vec[i]==areanum
    rlon <- range(lon[tmp])
    rlat <- range(lat[tmp])

    area.fragment <- rbind(area.fragment,
                           # $(B2<JU(B (x1,x2,y1,y2)$(B$N=g(B
                           rbind(cbind(seq(from=rlon[1],to=rlon[2],by=5),
                                       seq(from=rlon[1],to=rlon[2],by=5)+5,
                                       rlat[1],rlat[1],area.vec[i]),
                                 # $(B>eJU(B
                                 cbind(seq(from=rlon[1],to=rlon[2],by=5),
                                       seq(from=rlon[1],to=rlon[2],by=5)+5,
                                       rlat[2]+5,rlat[2]+5,area.vec[i]),
                                 # $(B:8JU(B
                                 cbind(rlon[1],rlon[1],
                                       seq(from=rlat[1],to=rlat[2],by=5),
                                       seq(from=rlat[1],to=rlat[2],by=5)+5,area.vec[i]),
                                 # $(B1&JU(B
                                 cbind(rlon[2]+5,rlon[2]+5,
                                       seq(from=rlat[1],to=rlat[2],by=5),
                                       seq(from=rlat[1],to=rlat[2],by=5)+5,area.vec[i])
                                 ))
    if(as.line==FALSE){
      if(is.draw==TRUE){
        polygon(c(rlon[1],rlon[2]+add.num,rlon[2]+add.num,rlon[1],rlon[1]),
                c(rlat[1],rlat[1],rlat[2]+add.num,rlat[2]+add.num,rlat[1]),
                col=col.var[i],border=bound.var[i],...)
      }
    }
    else{
      if(is.draw==TRUE){      
        lines(c(rlon[1]+add.num2,rlon[2]+add.num,rlon[2]+add.num,rlon[1]+add.num2,rlon[1]+add.num2),
              c(rlat[1]+add.num2,rlat[1]+add.num2,rlat[2]+add.num,rlat[2]+add.num,rlat[1]+add.num2),
              col=col.var[i])
      }
    }
  }

  area.fragment <- area.fragment[-1,]
  area.fragment <- as.data.frame(cbind(area.fragment,area.fragment[,1]*1e7+area.fragment[,2]*1e4+
                                            area.fragment[,3]*1e2+area.fragment[,4]))
  colnames(area.fragment) <- c("x1","x2","x3","x4","areanum","all")
  rownames(area.fragment) <- NULL

  # $(B30B&$N6-3&@~$O=|$/!J=EJ#$,$J$$$b$N!K(B
  area.fragment <- area.fragment[-match(names(table(area.fragment$all))[table(area.fragment$all)==1],area.fragment$all),]
  # $(B=EJ#$,$"$k%G!<%?$O=|$/(B
  unique.area <- unique(area.fragment$all)
  # ----------------------------------
  invisible(list(area.fragment=area.fragment,unique.area=unique.area))
}

