# AFS Kansas City 20-Aug-16

# Appropriately set the working directory before this
setwd("C:/aaaWork/Web/GitHub/RCourseKC2016/ExerciseKeys")
source("ALK_Construction.R")

rb.len.mod <- alkIndivAge(ALK.obs,age~tl,data=rb.len)
headtail(rb.len.mod)
rb.comb <- rbind(rb.age,rb.len.mod)
str(rb.comb)

## Question 1a/b
( agefreq <- xtabs(~age,data=rb.comb) )

## Question 1c
hist(~age,data=rb.comb,w=1,xlab="Age (yrs)")

## Question 1d
rb.comb <- mutate(rb.comb,lcat=lencat(tl,w=10))
xtabs(~lcat,data=rb.comb)

## Question 1e
( rb.sum <- Summarize(tl~age,data=rb.comb,digits=2) )

## Question 1f
plot(tl~age,data=rb.comb,ylab=lblTL,xlab="Age (yrs)",pch=19,col=col2rgbt("black",0.05))
lines(mean~fact2num(age),data=rb.sum,col="blue",lwd=2)


###### Question 2
rb.len.mod2 <- alkIndivAge(ALK.sm,age~tl,data=rb.len)
rb.comb2 <- rbind(rb.age,rb.len.mod2)
( agefreq2 <- xtabs(~age,data=rb.comb2) )
hist(~age,data=rb.comb2,w=1,xlab="Age (yrs)")
rb.comb2 <- mutate(rb.comb2,lcat=lencat(tl,w=10))
xtabs(~lcat,data=rb.comb2)
( rb.sum2 <- Summarize(tl~age,data=rb.comb2,digits=2) )
plot(tl~age,data=rb.comb2,ylab=lblTL,xlab="Age (yrs)",pch=19,col=col2rgbt("black",0.05))
lines(mean~fact2num(age),data=rb.sum2,col="blue",lwd=2)
