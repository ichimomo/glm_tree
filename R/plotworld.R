plotworld <-
function(data,area=NULL,xdiff=0,ydiff=0,oldarea=FALSE,
                      as.boundary=FALSE,as.line=FALSE,show.number=TRUE,
                      col.var=terrain.colors(length(unique(area))),add=FALSE,...){
  if(is.null(area)) area <- data$area
#  if(is.null(col.var)) col.var <- terrain.colors(unique(area))
  
  tmp2 <- tapply(area,list(data$lon,data$lat),mean)
  
  if(as.boundary|as.line){
    draw.areaboundary(data$lat,data$lon,area,add=add,as.line=as.line,
                      col.var=col.var,oldarea=oldarea)
  }
  else{
    image(as.numeric(as.character(rownames(tmp2)))+xdiff,
          as.numeric(as.character(colnames(tmp2)))+ydiff,
          tmp2,col=col.var,
          xlab="Longitude",ylab="Latitude",...)
  }
  x <- sort(unique(area))
  if(show.number){
    for(i in 1:length(x)){
      points(x1 <- mean(range(data$lon[area==x[i]]))+xdiff,
             y1 <- mean(range(data$lat[area==x[i]]))+ydiff,pch=15,cex=2,col="white")
      text(x1,y1,x[i],cex=1)
    }
  }
#  map('world2Hires',xlim=c(100,290),ylim=c(-50,50),lwd=1.5,interior=FALSE,add=T)
  if(!as.boundary){
#    map.axes()
  }
}

