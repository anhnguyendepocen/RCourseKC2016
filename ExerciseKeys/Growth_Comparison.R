# AFS Kansas City 20-Aug-16

library(FSAdata)     # for EuroPerchTJ data
library(FSA)         # for vbStarts(), residPlot(), extraSS(), lrt(), vbFuns(), filterD(), col2rbgt()
library(dplyr)       # for mutate()
library(nlstools)

data(EuroPerchTJ)
str(EuroPerchTJ)

## Question 1
clr1 <- c("black","red")
clr2 <- col2rgbt(clr1,1/5)
xlbl <- "Age (yrs)"
ylbl <- "Total Length (mm)"
plot(fl~age,data=EuroPerchTJ,pch=19,col=clr2[sex],xlab=xlbl,ylab=ylbl)

## Question 2a
( svOm <- vbStarts(fl~age,data=EuroPerchTJ,plot=TRUE) )
( svLKt <- Map(rep,svOm,c(2,2,2)) )
vbLKt <- fl~Linf[sex]*(1-exp(-K[sex]*(age-t0[sex])))
fitLKt <- nls(vbLKt,data=EuroPerchTJ,start=svLKt)
residPlot(fitLKt,col=col2rgbt("black",1/3))

## Question 2b
coef(fitLKt)
bootLKt <- nlsBoot(fitLKt,niter=10)

## Question 3a
vbOm <- fl~Linf*(1-exp(-K*(age-t0)))
fitOm <- nls(vbOm,data=EuroPerchTJ,start=svOm)

extraSS(fitOm,com=fitLKt,sim.name="{Omega}",com.name="{Linf,K,t0}")
lrt(fitOm,com=fitLKt,sim.name="{Omega}",com.name="{Linf,K,t0}")

vbLK <- fl~Linf[sex]*(1-exp(-K[sex]*(age-t0)))
( svLK <- Map(rep,svOm,c(2,2,1)) )
fitLK <- nls(vbLK,data=EuroPerchTJ,start=svLK)
vbLt <- fl~Linf[sex]*(1-exp(-K*(age-t0[sex])))
svLt <- Map(rep,svOm,c(2,1,2))
fitLt <- nls(vbLt,data=EuroPerchTJ,start=svLt)
vbKt <- fl~Linf*(1-exp(-K[sex]*(age-t0[sex])))
svKt <- Map(rep,svOm,c(1,2,2))
fitKt <- nls(vbKt,data=EuroPerchTJ,start=svKt)
extraSS(fitLK,fitLt,fitKt,com=fitLKt,com.name="{Linf,K,t0}",
        sim.names=c("{Linf,K}","{Linf,t0}","{K,t0}"))
lrt(fitLK,fitLt,fitKt,com=fitLKt,com.name="{Linf,K,t0}",
    sim.names=c("{Linf,K}","{Linf,t0}","{K,t0}"))

vbL <- fl~Linf[sex]*(1-exp(-K*(age-t0)))
( svL <- Map(rep,svOm,c(2,1,1)) )
fitL <- nls(vbL,data=EuroPerchTJ,start=svL)
vbt <- fl~Linf*(1-exp(-K*(age-t0[sex])))
svt <- Map(rep,svOm,c(1,1,2))
fitt <- nls(vbt,data=EuroPerchTJ,start=svt)
extraSS(fitL,fitt,com=fitLt,com.name="{Linf,t0}",sim.names=c("{Linf}","{t0}"))
lrt(fitL,fitt,com=fitLt,com.name="{Linf,t0}",sim.names=c("{Linf}","{t0}"))

## Question 3b
summary(fitL,correlation=TRUE)
round(cbind(coef(fitL),confint(fitL)),3)


#### Question 4
vb <- vbFuns("typical")
# Females
epF <- filterD(EuroPerchTJ,sex=="female")
svF <- list(Linf=40.5,K=0.37,t0=-0.2)
fitF <- nls(fl~vb(age,Linf,K,t0),data=epF,start=svF)
# Males
epM <- filterD(EuroPerchTJ,sex=="male")
svM <- list(Linf=37.1,K=0.37,t0=-0.2)
fitM <- nls(fl~vb(age,Linf,K,t0),data=epM,start=svM)


## Question 4a
bootF <- nlsBoot(fitF,niter=1000)
cbind(Est=coef(fitF),confint(bootF))
bootM <- nlsBoot(fitM,niter=1000)
cbind(Est=coef(fitM),confint(bootM))


#### Question 5
offset <- 0.04
# Females
plot(fl~I(age-offset),data=epF,pch=19,col=clr2[1],ylim=c(12,43),xlab=xlbl,ylab=ylbl)
curve(vb(x-offset,coef(fitF)),from=1,to=8,col=clr1[1],lwd=2,add=TRUE)
# Males
points(fl~I(age+offset),data=epM,pch=19,col=clr2[2])
curve(vb(x+offset,coef(fitM)),from=2,to=6,col=clr1[2],lwd=2,add=TRUE)
legend("topleft",c("Female","Male"),pch=19,col=clr1,bty="n")




#### Some problems below
vbK <- fl~log(Linf*(1-exp(-K[sex]*(age-t0))))
svK <- Map(rep,svOm,c(1,2,1))
fitK <- nls(vbK,data=EuroPerchTJ,start=svK)

library(AICcmodavg)
ms <- list(fitOm,fitL,fitK,fitt,fitLK,fitLt,fitKt,fitLKt)
mnames <- c("{Omega}","{Linf}","{K}","{t0}","{Linf,K}","{Linf,t0}","{K,t0}","{Linf,K,t0}")
aictab(ms,mnames)


# Script created at 2016-06-23 21:03:39
