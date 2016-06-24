# AFS Kansas City 20-Aug-16

library(FSAdata)                          # for WhitefishLC data
library(FSA)                              # for headtail(), ageBias(), agePrecision()

data(WhitefishLC)
str(WhitefishLC)
headtail(WhitefishLC)

## Question 1
ab1 <- ageBias(otolith2~otolith1,data=WhitefishLC)

summary(ab1,what="table",flip.table=TRUE)
summary(ab1,what="symmetry")
plot(ab1,diff=TRUE)

## Question 2
ap1 <- agePrecision(otolith2~otolith1,data=WhitefishLC)
summary(ap1,what="difference",digits=1)
summary(ap1,what="precision")

## Question 3
ab2 <- ageBias(scaleC~otolithC,data=WhitefishLC)

summary(ab2,what="table",flip.table=TRUE)
summary(ab2,what="symmetry")
plot(ab2,diff=TRUE)

## Question 4
ab3 <- ageBias(scale2~scale1,data=WhitefishLC)

summary(ab3,what="table",flip.table=TRUE)
summary(ab3,what="symmetry")
plot(ab3,diff=TRUE)

## Question 5
ap2 <- agePrecision(scale2~scale1,data=WhitefishLC)
summary(ap2,what="difference",digits=1)
summary(ap2,what="precision")

## Question 6
ab4 <- ageBias(scaleC~finrayC,data=WhitefishLC)
summary(ab4,what="table",flip.table=TRUE)
summary(ab4,what="symmetry")
plot(ab4,diff=TRUE)

