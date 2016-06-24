# AFS Kansas City 20-Aug-16

library(FSAdata)     # for Croaker2 data
library(FSA)         # for vbStarts(), residPlot(), extraSS(), lrt(), vbFuns(), filterD(), col2rbgt()
library(dplyr)       # for mutate()

data(Croaker2)
str(Croaker2)
Croaker2 <- mutate(Croaker2,logTL=log(tl))

clr1 <- c("black","red")
clr2 <- col2rgbt(clr1,1/5)
xlbl <- "Age (yrs)"
ylbl <- "Total Length (mm)"
plot(tl~age,data=Croaker2,pch=19,col=clr2[sex],xlab=xlbl,ylab=ylbl)

( svOm <- vbStarts(tl~age,data=Croaker2) )
( svLKt <- Map(rep,svOm,c(2,2,2)) )

vbLKt <- tl~Linf[sex]*(1-exp(-K[sex]*(age-t0[sex])))
fitLKt <- nls(vbLKt,data=Croaker2,start=svLKt)
residPlot(fitLKt,col=col2rgbt("black",1/3))

vbLKt <- logTL~log(Linf[sex]*(1-exp(-K[sex]*(age-t0[sex]))))
fitLKt <- nls(vbLKt,data=Croaker2,start=svLKt)
residPlot(fitLKt,col=col2rgbt("black",1/3))

vbOm <- logTL~log(Linf*(1-exp(-K*(age-t0))))
fitOm <- nls(vbOm,data=Croaker2,start=svOm)

extraSS(fitOm,com=fitLKt,sim.name="{Omega}",com.name="{Linf,K,t0}")
lrt(fitOm,com=fitLKt,sim.name="{Omega}",com.name="{Linf,K,t0}")

vbLK <- logTL~log(Linf[sex]*(1-exp(-K[sex]*(age-t0))))
( svLK <- Map(rep,svOm,c(2,2,1)) )
fitLK <- nls(vbLK,data=Croaker2,start=svLK)
vbLt <- logTL~log(Linf[sex]*(1-exp(-K*(age-t0[sex]))))
svLt <- Map(rep,svOm,c(2,1,2))
fitLt <- nls(vbLt,data=Croaker2,start=svLt)
vbKt <- logTL~log(Linf*(1-exp(-K[sex]*(age-t0[sex]))))
svKt <- Map(rep,svOm,c(1,2,2))
fitKt <- nls(vbKt,data=Croaker2,start=svKt)
extraSS(fitLK,fitLt,fitKt,com=fitLKt,com.name="{Linf,K,t0}",
        sim.names=c("{Linf,K}","{Linf,t0}","{K,t0}"))

vbL <- logTL~log(Linf[sex]*(1-exp(-K*(age-t0))))
( svL <- Map(rep,svOm,c(2,1,1)) )
fitL <- nls(vbL,data=Croaker2,start=svL)
vbK <- logTL~log(Linf*(1-exp(-K[sex]*(age-t0))))
svK <- Map(rep,svOm,c(1,2,1))
fitK <- nls(vbK,data=Croaker2,start=svK)
extraSS(fitL,fitK,com=fitLK,com.name="{Linf,K}",sim.names=c("{Linf}","{K}"))

summary(fitL,correlation=TRUE)
round(cbind(coef(fitL),confint(fitL)),3)

vb <- vbFuns("typical")
# Females
crF <- filterD(Croaker2,sex=="F")
svF <- list(Linf=425,K=0.25,t0=-2)
fitF <- nls(logTL~log(vb(age,Linf,K,t0)),data=crF,start=svF)
# Males
crM <- filterD(Croaker2,sex=="M")
svM <- list(Linf=385,K=0.25,t0=-2)
fitM <- nls(logTL~log(vb(age,Linf,K,t0)),data=crM,start=svM)

offset <- 0.04
# Females
plot(tl~I(age-offset),data=crF,pch=19,col=clr2[1],ylim=c(200,500),xlab=xlbl,ylab=ylbl)
curve(vb(x-offset,coef(fitF)),from=1,to=10,col=clr1[1],lwd=2,add=TRUE)
# Males
points(tl~I(age+offset),data=crM,pch=19,col=clr2[2])
curve(vb(x+offset,coef(fitM)),from=1,to=10,col=clr1[2],lwd=2,add=TRUE)
legend("topleft",c("Female","Male"),pch=19,col=clr1,bty="n")

vbt <- logTL~log(Linf*(1-exp(-K*(age-t0[sex]))))
svt <- Map(rep,svOm,c(1,1,2))
fitt <- nls(vbt,data=Croaker2,start=svt)

library(AICcmodavg)
ms <- list(fitOm,fitL,fitK,fitt,fitLK,fitLt,fitKt,fitLKt)
mnames <- c("{Omega}","{Linf}","{K}","{t0}","{Linf,K}","{Linf,t0}","{K,t0}","{Linf,K,t0}")
aictab(ms,mnames)


# Script created at 2016-06-23 21:03:39
