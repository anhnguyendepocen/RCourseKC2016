# AFS Kansas City 20-Aug-16

# Appropriately set the working directory before this
setwd("C:/aaaWork/Web/GitHub/RCourseKC2016/ExerciseKeys")
source("ALK_Application.R")
library(nlstools)          # for nlsBoot()

xlbl <- "Age (yrs)"
ylbl <- "Total Length (mm)"
clr <- col2rgbt("black",0.05)

## Questions 1
plot(tl~age,data=rb.comb,pch=19,col=clr,xlab=xlbl,ylab=ylbl)

## Question 2a
vb <- vbFuns("Typical",msg=TRUE)
svb <- vbStarts(tl~age,data=rb.comb,type="Typical",plot=TRUE)
fit1 <- nls(tl~vb(age,Linf,K,t0),data=rb.comb,start=svb)
( cf <- coef(fit1) )
plot(tl~age,data=rb.comb,xlab=xlbl,ylab=ylbl,pch=19,col=clr)
curve(vb(x,cf),from=3,to=11,n=500,lwd=2,col="red",add=TRUE)

## Question 2b
residPlot(fit1)

## Question 2c,d,e
summary(fit1,correlation=TRUE)

## Question 2f
boot1 <- nlsBoot(fit1,niter=1000)
confint(boot1,plot=TRUE,rows=1,cols=3)

## Question 2g
ageX <- 6
predict(fit1,data.frame(age=ageX))
pv <- apply(boot1$coefboot,MARGIN=1,FUN=vb,t=ageX)
quantile(pv,c(0.025,0.975))



###### Question 3
vbgq <- vbFuns("GallucciQuinn",msg=TRUE)
svbgq <- vbStarts(tl~age,data=rb.comb,type="GallucciQuinn",plot=TRUE)
fit2 <- nls(tl~vbgq(age,omega,K,t0),data=rb.comb,start=svbgq)
( cf <- coef(fit1) )
plot(tl~age,data=rb.comb,xlab=xlbl,ylab=ylbl,pch=19,col=clr)
curve(vb(x,cf),from=3,to=11,n=500,lwd=2,col="red",add=TRUE)

## Question 2b
residPlot(fit2)

## Question 2c,d,e
summary(fit2,correlation=TRUE)

## Question 2f
boot2 <- nlsBoot(fit2,niter=1000)
confint(boot2,plot=TRUE,rows=1,cols=3)

## Question 2g
ageX <- 6
predict(fit2,data.frame(age=ageX))
pv2 <- apply(boot1$coefboot,MARGIN=1,FUN=vbgq,t=ageX)
quantile(pv2,c(0.025,0.975))
