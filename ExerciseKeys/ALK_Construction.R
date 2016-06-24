# AFS Kansas City 20-Aug-16

library(FSAdata)        # for RockBassLO2 data
library(FSA)            # for headtail(), alkPlot()
library(dplyr)          # for filter(), mutate()
library(nnet)           # for multinom()

data(RockBassLO2)
str(RockBassLO2)

## Question #1
rb.len <- filter(RockBassLO2,is.na(age))
str(rb.len)
rb.age <- filter(RockBassLO2,!is.na(age))
str(rb.age)

## Question #2
rb.age.mod <- mutate(rb.age,lcat=lencat(tl,w=10))
headtail(rb.age.mod)
( raw <- xtabs(~lcat+age,data=rb.age.mod) )

## Question #3
( ALK.obs <- prop.table(raw,margin=1) )
lblTL <- "Total Length (mm)"
alkPlot(ALK.obs,showLegend=TRUE,xlab=lblTL)

## Question #4
mlr <- multinom(age~lcat,data=rb.age.mod,maxit=500)
lens <- seq(110,270,10)
ALK.sm <- predict(mlr,data.frame(lcat=lens),type="probs")
row.names(ALK.sm) <- lens
round(ALK.sm,3)
alkPlot(ALK.sm,showLegend=TRUE,xlab=lblTL)

