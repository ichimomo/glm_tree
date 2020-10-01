split.area <-
function(formula,data,areanum,snum,debug.mode=F,preAIC=Inf,graph=R,
                       IC="AIC",sss=0,is.split=FALSE,IC2=NULL,do.weight=FALSE,
                       show.deviance=FALSE){

  if(debug.mode==F) library(biglm)

  tmp100 <- as.list(numeric())
  data1 <- data[data$area==areanum,]
  a <- dim(table(data1$lon,data1$lat))
  if(a[1]*a[2]!=1){
	tmp.area <- data$area
#	x1 <- take.medium(seq(from=max(115,min(data1$lon)),to=min(280,max(data1$lon)),by=5))
	x1 <- take.medium(seq(from=min(data1$lon),to=max(data1$lon),by=5))        
#  	x2 <- take.medium(seq(from=max(-5,min(data1$lat)),to=min(40,max(data1$lat)),by=5))
        # $(B$3$3$N?t;z$O!"(Blat,lon$(B$N%9%1!<%k$K9g$o$;$FD4@0$9$k$3$H!*(B
#  	x2 <- take.medium(seq(from=max(-40,min(data1$lat)),to=min(50,max(data1$lat)),by=5))
  	x2 <- take.medium(seq(from=min(data1$lat),to=max(data1$lat),by=5))                
      }
      else{
	x1 <- x2 <- NA
      }

  if(sum(is.na(x1))==1) x1 <- NULL
  if(sum(is.na(x2))==1) x2 <- NULL  
  n <- length(x1)+length(x2)
  
  if(n!=0){
    sum.of.res <- data.frame(lat.lon=c(rep("lon",length(x1)),rep("lat",length(x2))),
                             position=c(x1,x2),area=rep(areanum,n),total.ss=rep(0,n),model.ss=rep(0,n),
                             deviance=rep(0,n),r2=rep(0,n),AIC=rep(0,n),BIC=rep(0,n),CCP=rep(0,n),
			     np=rep(0,n),DperP=rep(0,n),opt.position=rep(0,n),likelihood=rep(0,n),
                             miss.data=rep(0,n),MD=rep(0,n))
    list.res <- as.list(numeric())

    if(debug.mode==F){
      for(i in 1:n){
        if(!is.na(sum.of.res$position[i])){
          # sum.of.res$(B$r;2>H$7$F%(%j%"J,$1$9$k(B
          tmp.area[data$area==areanum] <- areanum+
            as.numeric(data1[as.character(sum.of.res$lat.lon[i])]>sum.of.res$position[i])/2

#---------for NOT using biglm--------------------           
#          res <- glm(lswocpue~as.factor(year)+as.factor(tmp.area)+as.factor(qt)+as.factor(gear)+
#                     as.factor(year)*as.factor(tmp.area)+as.factor(gear)*as.factor(tmp.area)+
#                     as.factor(qt)*as.factor(gear)+as.factor(tmp.area)*as.factor(qt)
#                     ,data=data)
#          list.res[[i]] <- summary(aov(res))
#          sum.of.res$AIC[i] <- AIC(res)
#          sum.of.res$BIC[i] <- AIC(res,k=log(nrow(data)))          
#          sum.of.res$total.ss[i] <- sum(list.res[[i]][[1]][,2])
#          sum.of.res$model.ss[i] <- sum(list.res[[i]][[1]][-nrow(list.res[[i]][[1]]),2])
#          sum.of.res$deviance[i] <- sum(list.res[[i]][[1]][nrow(list.res[[i]][[1]]),2])
#          sum.of.res$r2[i] <- sum.of.res$model.ss[i]/sum.of.res$total.ss[i]

#---------for using biglm--------------------                     
#          res <- biglm(lswocpue~as.factor(year)+as.factor(tmp.area)+as.factor(qt)+as.factor(gear)+
#                     as.factor(year)*as.factor(tmp.area)+as.factor(gear)*as.factor(tmp.area)+
#                     as.factor(qt)*as.factor(gear)+
#                       as.factor(tmp.area)*as.factor(qt)
#                     ,data=data)
#--------------------------------------------

#---------Formula is given by an argument --------------------
          pre.area <- data$area
          data$area <- tmp.area
          
          if(do.weight==FALSE){
            res <- biglm(eval(formula),data=data)
          }
          else{
            res <- biglm(eval(formula),data=data,weights=nop2)            
          }
          if(show.deviance==TRUE){
            tmp100[[i]] <- glm(eval(formula),data=data)$residuals
          }
	  sss <- sss+1
          data$area <- pre.area
          
          list.res[[i]] <- summary(res)
	  np <- sum(!is.na((list.res[[i]]$mat[,1])))
          sum.of.res$AIC[i] <- calAIC(p=np,sse=res$qr$ss,n=res$n,k=2,bias.corrected.AIC=T)
          sum.of.res$BIC[i] <- calAIC(p=np,sse=res$qr$ss,n=res$n,k=log(nrow(data)))
          sum.of.res$deviance[i] <- res$qr$ss          
          sum.of.res$total.ss[i] <- sum((data$lcpue-mean(data$lcpue))^2)
          sum.of.res$model.ss[i] <- sum.of.res$total.ss[i]- sum.of.res$deviance[i]
          sum.of.res$r2[i] <- sum.of.res$model.ss[i]/sum.of.res$total.ss[i]          
          sum.of.res$CCP[i] <- sum.of.res$deviance[i]+1*(length(unique(data$area))+1)
          sum.of.res$np[i] <- np
          sum.of.res$DperP[i] <- res$qr$ss/np
          sum.of.res$likelihood[i] <- exp(-calAIC(p=np,sse=res$qr$ss,n=res$n,k=0)/2)
          sum.of.res$miss.data[i] <- sum(is.na(summary(res)$mat[,1]))
        }}
#      x <- 1:n
#      opt.position <- x[min(sum.of.res[IC],na.rm=T)==sum.of.res[IC]]
#      opt.position <- opt.position[1]
      # $(B$3$3$G!"(Bopt.position$(B$r7hDj!*(B
      if(IC!="MD"){
        opt.position <- which(min(sum.of.res[IC],na.rm=T)==sum.of.res[IC])[1]
      }
      else{
#        browser()
        sum.of.res$MD <- as.numeric(unlist(sum.of.res$miss.data*1e+20 + sum.of.res[IC2]))
        # missing data$(B$K%Z%J%k%F%#$r3]$1$?$b$N$r(BMD$(B$H$9$k(B
        opt.position <- which(sum.of.res$MD==min(sum.of.res$MD,na.rm=T))
      }
    }
    else{  # $(B%G%P%C%0%b!<%I$N$H$-(B
      sss <- sss+1
      opt.position <- floor(runif(1,min=1,max=n))
      sum.of.res[IC][-opt.position,1] <- 30#rnorm(1)
      sum.of.res[IC][opt.position,1] <- round(30/sss)#rnorm(1)
    }

    # 
    sum.of.res$opt.position[opt.position] <- 1
    postAIC <- sum.of.res[IC][opt.position,1]    
    if(graph==T){
	plot(sum.of.res$position,sum.of.res[IC][,1],
             ylim=range(c(sum.of.res[IC][,1],ifelse(preAIC==Inf,NA,preAIC)),na.rm=T),
             type="p",xlab="Lat or Lon",ylab=IC)
	title(paste("Area",areanum),line=-1)
        abline(v=sum.of.res$position[opt.position],col=2)
        abline(h=postAIC,col=2)        
        abline(h=preAIC,col=3)    
       }

    if(is.split==TRUE){
      # set the flag of optimam region, and replace area defintion
      tmp <- data1[as.character(sum.of.res$lat.lon[opt.position])] >
        sum.of.res$position[opt.position]

      if(postAIC<preAIC){
        ma <- max(data$area)
        data1$area[tmp] <- ma+1
        data1$area[!tmp] <- ma+2
        b1 <- dim(table(data1$lon[tmp],data1$lat[tmp]))
        cat(paste(ma+1,"[label=\"(",ma+1,")A:",round(postAIC),
                  ",N:",b1[1],"x",b1[2],"\"];\n",sep=""),
            file="flow.chart.dot",append=TRUE)
        b2 <- dim(table(data1$lon[!tmp],data1$lat[!tmp]))
        cat(paste(ma+2,"[label=\"(",ma+2,") A:",round(postAIC),
                  ",N:",b2[1],"x",b2[2],"\"];\n",sep=""),
            file="flow.chart.dot",append=TRUE)      
        cat(paste(areanum,"->", ma+1,"[label=",
                  "\">",as.character(sum.of.res$lat.lon[opt.position]),
                  sum.of.res$position[opt.position],"\"];\n"),file="flow.chart.dot",append=TRUE)
        cat(paste(areanum,"->", ma+2,"[label=",
                "\"<",as.character(sum.of.res$lat.lon[opt.position]),
                  sum.of.res$position[opt.position],"\"];\n"),file="flow.chart.dot",append=TRUE)      
      }
      else{
        return(list(data=data,sumres=-100,sss=sss,deviance=tmp100))
      }
      data$area[data$area==areanum] <- data1$area
      return(list(data=data,sumres=sum.of.res,sss=sss,deviance=tmp100))
    }
    else{
      # if is.split==FALSE, return only sum.of.res without replacing area definition
      return(list(data=data,sumres=sum.of.res,sss=sss,deviance=tmp100))
    }
  }
  else{  # if n==0
    # This is for the case when the area is sufficiently small
    return(list(data=data,sumres=-100,sss=sss,deviance=tmp100))
  }
}

