\name{ldata}
\alias{ldata}
\docType{data}
\title{
  A data set for testing GLM tree algorithm. 
}
\description{
  A data set for testing GLM tree algorithm. This data set is produced
  from simple simulation as descired in 'source'.  
}
\usage{data(ldata)}
\format{
  A data frame with 16000 observations on the following 7 variables.
  \describe{
    \item{\code{lon}}{a numeric vector}
    \item{\code{lat}}{a numeric vector}
    \item{\code{year}}{a numeric vector}
    \item{\code{N}}{a numeric vector}
    \item{\code{area}}{a numeric vector}
    \item{\code{lonlat}}{a numeric vector}
    \item{\code{lcpue}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ldata)
## maybe str(ldata) ; plot(ldata) ...

## Followings are script to create the simulation data
#-------- R code for creating CPUE simulation data
yearrange <- 1990:2005
# Every year abundance
abundance <- seq(from=10000,to=1000,length=length(yearrange)) 
# Fishing effort (1000 effort per year)
effort <- rep(1000,length(yearrange))
# Determine center of fishing area, which is assumed to shift year by year
catch.position <- data.frame(lat=seq(from=35,to=35,length=length(yearrange)),
                             lon=seq(from=145,to=180,length=length(yearrange)))
catch <- data.frame(lon=0,lat=0,year=0,N=0)
for(y in 1:length(yearrange)){
  # Spatial distribution of fish.  This is not changed by year.  
  fishdist <- data.frame(lon=c(rnorm(abundance[y]/2,sd=8,mean=155),rnorm(abundance[y]/2,sd=10,mean=170)),
                    lat=c(rnorm(abundance[y]/2,sd=5,mean=30),rnorm(abundance[y]/2,sd=5,mean=35)))
  # Determine fishing position randomly 		    
  catch.tmp <- data.frame(lon=rnorm(effort[y],sd=4,mean=catch.position$lon[y]),
                              lat=rnorm(effort[y],sd=4,mean=catch.position$lat[y]),
                              year=yearrange[y],N=0)

  for(i in 1:effort[y]){
    # how many fish school from fishing position
    a <- sum(((fishdist$lon-catch.tmp$lon[i])^2+(fishdist$lat-catch.tmp$lat[i])^2)<0.5)
    # if there are any fish schools, fishing is occured.  
    if(a!=0){
      catch.tmp$N[i] <- floor(sum(exp(rnorm(a,mean=2))))
    }
  }
  catch <- rbind(catch,catch.tmp)
}
catch <- catch[-1,]
ldata <- catch
}
\keyword{datasets}
