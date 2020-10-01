make.treeglm <-
function(formula,data,debug.mode=FALSE,graph=TRUE,IC="AIC",IC2=NULL,show.deviance=FALSE,
                         max.split=100,safe.mode=TRUE,prearea=NULL#,do.weight=TRUE
                         ){
 #---------------------- Start main procedure -------------------#
#  library(maps)
#  library(mapdata)  
  res.list <- res.list2 <- as.list(numeric())
  area.list <- as.list(numeric())
  dev.list <- as.list(numeric())
  if(graph==TRUE){  
    oldpar <- par(no.readonly=TRUE)
    on.exit(par(oldpar))
    par(mfrow=c(3,2),mar=c(3,3,1,1))
  }
  if(is.null(prearea)){
    data$area <- 1
  }
  else{
    data$area <- prearea
  }

#  cat("digraph \"foodweb.dhp\" {
# size=\"7,7\";
# rankdir=\"LR\";
# node [fontname=\"Helvetica\" fontsize=12 shape=box];
# edge [fontname=\"Helvetica\" fontsize=10];
# center=1;",file="flow.chart.dot")
  
  s <- 0
  ss <- 1  
  sss <- 1 # Total number of calculation of glm
  area.vec.old <- 0
  area.vec <- sort(unique(data$area))
  postAIC <- preAIC <- NULL
  
  while(max(area.vec)!=max(area.vec.old)&max(area.vec)<max.split){
    cat("Current number of area =",max(area.vec),". Current AIC=",postAIC,
        ". Previous AIC=",preAIC,".\n")
    preAIC <- ifelse(ss==1,Inf,postAIC)
#   area.vec <- area.vec[max(area.vec.old)<area.vec]
    area.vec <- sort(unique(data$area))
    n <- length(area.vec)
    s.old <- s
    for(i in 1:n){
      s <- s+1
      nn <- length(res.list)
      tmp <- split.area(formula,data,area.vec[i],s,debug.mode=debug.mode,graph=F,sss=sss,IC=IC,
                        preAIC=preAIC,is.split=FALSE,IC2=IC2,show.deviance=show.deviance)
      
      sss <- tmp$sss
      res.list[[s]] <- tmp$sumres

      if(show.deviance==TRUE){
        dev.list[[s]] <- tmp$deviance
      }

      if(is.data.frame(res.list[[s]])){
#        write.table(res.list[[s]][res.list[[s]]$opt.position==1,])
      }
      else{
        res.list[[s]] <- NULL
        s <- s-1
      }
    }
    #-- after all calculation of area.vec, evaluate optimum boundary
    a <- extract.optline(res.list[(s.old+1):s])
#    write.table(a)    
    x <- 1:nrow(a)
    opt.position <- x[min(a[IC],na.rm=T)==a[IC]]
    opt.position <- opt.position[1]    
    tmp <- data[as.character(a$lat.lon[opt.position])] > a$position[opt.position] & data$area == a$area[opt.position]
    postAIC <- a[IC][opt.position,1]
    a$opt.position[-opt.position] <- 0
    res.list2[[ss]] <- a

    if(graph==T){
      plot(res.list[[s.old+opt.position]]$position,res.list[[s.old+opt.position]][IC][,1],
           ylim=range(c(res.list[[s.old+opt.position]][IC][,1],ifelse(preAIC==Inf,NA,preAIC)),na.rm=T),
           type="p",xlab="Lat or Lon",ylab=IC)
      title(paste("Area",a$area[opt.position]),line=-1)
      abline(v=res.list[[s.old+opt.position]]$position[opt.position],col=2)
      abline(h=a[IC][,1],col="gray")              
      abline(h=postAIC,col=2)        
      abline(h=preAIC,col=3)    
    }

    
    if(postAIC<preAIC){
      data$area[tmp] <- max(data$area)+1
#      b1 <- dim(table(data$lon[tmp],data$lat[tmp]))
      cat(paste(b1 <- a$area[opt.position],"[label=\"(",b1,")A:",round(postAIC),
                ",N:",0,"x",0,"\"];\n",sep=""),
          file="flow.chart.dot",append=TRUE)
#      b2 <- dim(table(data1$lon[!tmp],data1$lat[!tmp]))
      cat(paste(b2 <- max(data$area)+1,"[label=\"(",b2,") A:",round(postAIC),
                ",N:",0,"x",0,"\"];\n",sep=""),
          file="flow.chart.dot",append=TRUE)      
      cat(paste(b1,"->",b1,"[label=",
                "\">",as.character(a$lat.lon[opt.position]),
                a$position[opt.position],"\"];\n"),file="flow.chart.dot",append=TRUE)
      cat(paste(b1,"->",b2,"[label=",
                "\"<",as.character(a$lat.lon[opt.position]),
                  a$position[opt.position],"\"];\n"),file="flow.chart.dot",append=TRUE)            
    }
    #-- finish the replace

    area.list[[ss]] <- data$area
    ss <- ss+1
    area.vec.old <- area.vec
    area.vec <- sort(unique(data$area))
    if(safe.mode==T) {
      tmp <- list(data=data,area=area.list,res=res.list,res2=res.list2,formula=formula,sss=sss)
      save(tmp,file="tmp.R")
      rm(tmp)
      gc()
      gc()
    }
  }

  if(graph==T){
    par(mfrow=c(2,1))
    plotworld(data,col=rainbow(12))
    par(mfrow=c(2,3))
    a <- extract.optline(res.list)
    plot(a$deviance,type="b")
    plot(a$AIC,type="b")
    plot(a$BIC,type="b")
    plot(a$r2,type="b")
    plot(a$DperP,type="b")  
  }
  
  cat("constraint=false}",file="flow.chart.dot",append=TRUE)
  if(.Platform$OS.type=="unix" & graph==TRUE){
    system("dot -Tps flow.chart.dot -o flow.chart.ps")
  }
  sum.stat <- extract.optline(res.list2)
  sum.stat$Num.area <- 2:(nrow(sum.stat)+1)
  return(list(data=data,area=area.list,detailed.stat=res.list,stat=res.list2,
              formula=formula,sss=sss,dev.list=dev.list,summary.stat=sum.stat))
}

