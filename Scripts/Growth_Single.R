# AFS Kansas City 20-Aug-16

library(FSAdata)           # for TroutBR data
library(FSA)               # for filterD(), headtail(), col2rgbt(), vbFuns(), vbStart()
library(nlstools)          # for nlsBoot()

data(TroutBR)
str(TroutBR)
rbt <- filterD(TroutBR,species=="Rainbow")
headtail(rbt)

xlbl <- "Age (yrs)"
ylbl <- "Total Length (in)"
clr <- col2rgbt("black",0.05)

plot(tl~age,data=rbt,pch=19,col=clr,xlab=xlbl,ylab=ylbl)

vb <- vbFuns("Typical",msg=TRUE)
vb

# Demos manual generation with plot ... LEFT plot
svb <- vbStarts(tl~age,data=rbt,type="Typical",plot=TRUE,
                fixed=list(Linf=30,K=0.3,t0=0))
# Demos automatic generation ... RIGHT plot
svb <- vbStarts(tl~age,data=rbt,type="Typical",plot=TRUE)

fit1 <- nls(tl~vb(age,Linf,K,t0),data=rbt,start=svb)
summary(fit1,correlation=TRUE)
( cf <- coef(fit1) )
confint(fit1)

boot1 <- nlsBoot(fit1,niter=1000)
str(boot1)
headtail(boot1$coefboot)
confint(boot1,plot=TRUE,rows=1,cols=3)

ageX <- 8
predict(fit1,data.frame(age=ageX))
pv <- apply(boot1$coefboot,MARGIN=1,FUN=vb,t=ageX)
quantile(pv,c(0.025,0.975))

plot(tl~age,data=rbt,xlab=xlbl,ylab=ylbl,pch=19,col=clr)
curve(vb(x,cf),from=3,to=10,n=500,lwd=2,col="red",add=TRUE)

residPlot(fit1)


# Script created at 2016-06-23 20:58:20
