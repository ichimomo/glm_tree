## Package of 'glm.tree'

* Note that this is only for lognormal distribution as error structure,
currently.

### Change Log:

-   1st Oct 2020. GOTO github
-   3rd August 2012. Define namespace  (ver.1.2)
-   31th March 2011. Debugged because older version can't conduct
    tree-glm only within a specific range of longitude
-   17th Dec 2010. Add binary zip package for Windows XP

### Download and Install

Please use install_github

    install.packages("devtools") 
    devtools::install_github("ichimomo/glm_tree")

### Required additional package
biglm 

### Usage

See help after calling the library of 'glm.tree' such as

     
    library(glm.tree)  
    help(glm.tree)
     

### Examples

same as shown in the help of glm.tree


    library(glm.tree)
    data(ldata)
    # install.packates(biglm) # if 'biglm' packages are not installed.  
    library(biglm)
    ldata$lcpue <- log(ldata$N)
    ldata$area <- 0
    # Dependent variable should be in 'ldata$lcpue', independent
    # variable of longitude and latitude should be in 'ldata$lon' and
    # ldata$lat, respectively
    str(ldata)
    # remove zero catch data
    ldata <- subset(ldata,N>0)

    # create area stratification

    # conduct iteration until obtaining minimum AIC
    # ==> !! caution: this takes too much time and computer memory
    res0 <- make.treeglm(expression(lcpue~as.factor(year)+as.factor(area)),
                              data=ldata,debug.mode=F,max.split=Inf,graph=F,IC="AIC")
    # conduct only 10th iterations
    res <- make.treeglm(expression(lcpue~as.factor(year)+as.factor(area)),
                              data=ldata,debug.mode=F,max.split=10,graph=F,IC="AIC")
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
                              data=ldata,debug.mode=F,max.split=15,graph=F,IC="BIC")
    # Another option to create area stratification,
    #     for avoiding area strata with missing data
    res.md <- make.treeglm(expression(lcpue~as.factor(year)+as.factor(area)),
                              data=ldata,debug.mode=F,max.split=15,graph=F,IC="MD",IC2="AIC")



