\name{glm.tree-package}
\alias{glm.tree-package}
\alias{glm.tree}
\docType{package}
\title{
This package is to create adaptive area stratification for glm.  
}
\description{
This package is to create adaptive area stratification for glm.  Details
are in Ichinokawa and Brodziak (2010).  
}
\details{
\tabular{ll}{
Package: \tab glm.tree\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2010-03-31\cr
License: \tab M. Ichinokawa (ichimomo@fra.affrc.go.jp) \cr
LazyLoad: \tab yes\cr
}
%%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Momoko Ichinokawa
Maintainer: M Ichinokawa to complain to <ichimomo@fra.affrc.go.jp>

}
\references{
Ichinokawa, M., and Brodziak,
J. 2010. Using adaptive area stratification to standardize catch rates
with application to North Pacific swordfish (Xiphias gladius). Fish Res
106(3): 249-260.  
}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ %%package
}
\seealso{
  http://cse.fra.affrc.go.jp/ichimomo/indexs.htm
}
\examples{
library(glm.tree)
data(ldata)
# install.packates(biglm) # if "biglm" packages are not installed.  
library(biglm)
ldata$lcpue <- log(ldata$N)
ldata$area <- 0
str(ldata)
# remove zero catch data
ldata <- subset(ldata,N>0)

# create area stratification

# conduct iteration until obtaining minimum AIC
# ==> !! caution: this takes too much time and computer memory
res0 <- make.treeglm(expression(lcpue~as.factor(year)+as.factor(area)),
                          data=ldata,debug.mode=FALSE,max.split=Inf,graph=FALSE,IC="AIC")
# conduct only 10th iterations
res <- make.treeglm(expression(lcpue~as.factor(year)+as.factor(area)),
                          data=ldata,debug.mode=FALSE,max.split=10,graph=FALSE,IC="AIC")
plotworld(res$data,area=res$area[[9]])
plot(res$summary.stat$Num.area,res$summary.stat$AIC)

# compare results
ldata$area2 <- res0$area[[2]]
ldata$area9 <- res0$area[[9]]
ldata$area14 <- res0$area[[14]]
res.glm2 <- glm(lcpue~as.factor(year)+as.factor(area2),data=ldata)
res.glm9 <- glm(lcpue~as.factor(year)+as.factor(area9),data=ldata)
res.glm14 <- glm(lcpue~as.factor(year)+as.factor(area14),data=ldata)

par(mfrow=c(2,2))
plot(exp(c(res.glm2$coef[1],res.glm2$coef[1]+res.glm2$coef[2:16])),
    type="b",main="Iteration=3")
plot(exp(c(res.glm9$coef[1],res.glm9$coef[1]+res.glm9$coef[2:16])),
    type="b",main="Iteration=9")
plot(exp(c(res.glm14$coef[1],res.glm14$coef[1]+res.glm14$coef[2:16])),
    type="b",main="Iteration=14") # Closer to true abundance trends

# Another option to use BIC as an information criteria
res.bic <- make.treeglm(expression(lcpue~as.factor(year)+as.factor(area)),
                          data=ldata,debug.mode=FALSE,max.split=15,graph=FALSE,IC="BIC")
# Another option to create area stratification,
#     for avoiding area strata with missing data
res.md <- make.treeglm(expression(lcpue~as.factor(year)+as.factor(area)),
                          data=ldata,debug.mode=FALSE,max.split=15,graph=FALSE,IC="MD",IC2="AIC")

}
