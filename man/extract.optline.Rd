\name{extract.optline}
\alias{extract.optline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Internal function used in make.treeglm
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract.optline(matlist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matlist}{
%%     ~~Describe \code{matlist} here~~
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
