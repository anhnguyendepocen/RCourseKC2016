# AFS Kansas City 20-Aug-16

library(FSAdata)        # for SpotVA2 data
library(FSA)            # for headtail(), alkPlot()
library(dplyr)          # for filter(), mutate()
library(nnet)           # for multinom()

data(SpotVA2)
headtail(SpotVA2)

# ############################################################
# This code demonstrates the use of is.na().  It is not needed
# for the analysis.
tmp <- headtail(SpotVA2) 
cbind(tmp,is.na(tmp$age),!is.na(tmp$age))
# ############################################################

sp.len <- filter(SpotVA2,is.na(age))
headtail(sp.len)
sp.age <- filter(SpotVA2,!is.na(age))
headtail(sp.age)

sp.age.mod <- mutate(sp.age,lcat=lencat(tl,w=1))
headtail(sp.age.mod)

( raw <- xtabs(~lcat+age,data=sp.age.mod) )
( ALK.obs <- prop.table(raw,margin=1) )

mlr <- multinom(age~lcat,data=sp.age.mod,maxit=500)

lens <- 6:13
ALK.sm <- predict(mlr,data.frame(lcat=lens),type="probs")
row.names(ALK.sm) <- lens
round(ALK.sm,3)

lblTL <- "Total Length (cm)"
alkPlot(ALK.obs,xlab=lblTL)
alkPlot(ALK.sm,xlab=lblTL)

alkPlot(ALK.sm,pal="gray",showLegend=TRUE,xlab=lblTL)
alkPlot(ALK.sm,type="area",pal="gray",showLegend=TRUE,xlab=lblTL)

alkPlot(ALK.sm,type="lines",pal="gray",xlab=lblTL)
alkPlot(ALK.sm,type="bubble",xlab=lblTL)


# Script created at 2016-06-23 18:50:47
