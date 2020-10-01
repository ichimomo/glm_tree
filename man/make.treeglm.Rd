\name{make.treeglm}
\alias{make.treeglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This is a function to create adaptive area stratification in conducting
glm.  
}
\description{
  This is a function to create adaptive area stratification in
  conducting
  glm.  This function use a function in library 'biglm'. Before using
  this function, please install the package.  
}
\usage{
make.treeglm(formula, data, debug.mode = FALSE, graph = TRUE, IC = "AIC", IC2 = NULL, show.deviance = FALSE, max.split = 100, safe.mode = TRUE, prearea = NULL, is.win = FALSE, do.weight = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
    Formula for glm.  This formula should be as a object returned from
    'expression' as expression(lcpue~as.factor(year)+as.factor(area)).  
}
  \item{data}{
    Data used in glm. This should be a data frame, containing 'lcpue' as
    an dependent variable, and 'lon' and 'lat' as variables of
    longitude and latitude for dependent vaiables.  
  }
  \item{debug.mode}{
    If debug.mode=TRUE, actual calculation of glm is not conducted, and
    select one bounday randomly.  This option is used only for
    development.  
}
  \item{graph}{
    If graph=TRUE, intermediate steps in calculating AIC or other
    statistics and resultant area stratification are shown by graph.  
}
  \item{IC}{
    Statistics to be minimized.  This can be chosen from 'AIC', 'BIC' or
    'MD'.  Default option is 'AIC.  'MD' option is to avoid creating
    area strata without any data.  In using the option of 'MD', specify
    the argument of 'IC2'.  
  }
  \item{IC2}{
    Statistica to be minimized, when using the option of 'MD'.   
}
  \item{show.deviance}{
    If show.deviance=T, all deviances estimated in all candidates of
    area stratification are shown.  This option shouldn't be used
    because returned object become huge, except for specific usage. 
}
  \item{max.split}{
    To determine maximum number of boundary crated by this glm-tree algorithm.
    If this option is not specified, glm-tree algorith is terminated
    when target statistics (specified by IC or IC2) can't be decreased.  
}
  \item{safe.mode}{
    Currently not for use.  
}
  \item{prearea}{
    If !is.null(prearea), the glm-tree algorith is started from the area
    stratification specified by 'prearea'.  The 'prearea' should be as
    length(prearea)==nrow(data).  
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{data}{Data used in this function}
  \item{area}{List of created area stratification.  Result of n'th
    iteration is restored in n'th list such as area[[n]]. }
  \item{summary.stat}{Summary of statistics produced from each iteration
    such as AIC, BIC, deviance and R-squared value.  }
  \item{detiled.stat, stat}{Detailed statistics calculated in all
    candidates of area stratification in each iteration.  }
  \item{formula}{GLM formula used in this function.}
  \item{sss}{Total number in which biglm procedures are evaluated.}
  \item{dev.list}{Deviance list if show.deviance=TRUE.   }
}
\references{
Ichinokawa and Brodziak (2010).  
}
\author{
  M. Ichinokawa (ichimomo@fra.affrc.go.jp)
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
data(ldata)
ldata$lcpue <- log(ldata$N)
ldata$area <- 0
str(ldata) # Dependent variable should be in 'ldata$lcpue', independent
variable of longitude and latitude should be in 'ldata$lon' and
ldata$lat, respectively
res <- make.treeglm(expression(lcpue~as.factor(year)+as.factor(area)),
                          data=subset(ldata,N>0),debug.mode=F,max.split=10,graph=F)
plotworld(res$data,area=res$area[[3]])
plot(res$summary.stat$Num.area,res$summary.stat$AIC)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
