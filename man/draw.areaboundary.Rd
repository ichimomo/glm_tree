\name{draw.areaboundary}
\alias{draw.areaboundary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Internal function used in make.treeglm
  %%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw.areaboundary(lat, lon, areanum, col.var = NULL, add.num = 4.5, is.draw = TRUE, add.num2 = 0, bound.var = NULL, add = FALSE, as.line = FALSE, oldarea = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
%%     ~~Describe \code{lat} here~~
}
  \item{lon}{
%%     ~~Describe \code{lon} here~~
}
  \item{areanum}{
%%     ~~Describe \code{areanum} here~~
}
  \item{col.var}{
%%     ~~Describe \code{col.var} here~~
}
  \item{add.num}{
%%     ~~Describe \code{add.num} here~~
}
  \item{is.draw}{
%%     ~~Describe \code{is.draw} here~~
}
  \item{add.num2}{
%%     ~~Describe \code{add.num2} here~~
}
  \item{bound.var}{
%%     ~~Describe \code{bound.var} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{as.line}{
%%     ~~Describe \code{as.line} here~~
}
  \item{oldarea}{
%%     ~~Describe \code{oldarea} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
                           # 下辺 (x1,x2,y1,y2)の順
                           rbind(cbind(seq(from=rlon[1],to=rlon[2],by=5),
                                       seq(from=rlon[1],to=rlon[2],by=5)+5,
                                       rlat[1],rlat[1],area.vec[i]),
                                 # 上辺
                                 cbind(seq(from=rlon[1],to=rlon[2],by=5),
                                       seq(from=rlon[1],to=rlon[2],by=5)+5,
                                       rlat[2]+5,rlat[2]+5,area.vec[i]),
                                 # 左辺
                                 cbind(rlon[1],rlon[1],
                                       seq(from=rlat[1],to=rlat[2],by=5),
                                       seq(from=rlat[1],to=rlat[2],by=5)+5,area.vec[i]),
                                 # 右辺
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

  # 外側の境界線は除く（重複がないもの）
  area.fragment <- area.fragment[-match(names(table(area.fragment$all))[table(area.fragment$all)==1],area.fragment$all),]
  # 重複がある椒如尺タは除く
  unique.area <- unique(area.fragment$all)
  # ----------------------------------
  invisible(list(area.fragment=area.fragment,unique.area=unique.area))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
