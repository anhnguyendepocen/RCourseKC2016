# AFS Kansas City 20-Aug-16

library(FSAdata)                          # for StripedBass4 data
library(FSA)                              # for headtail(), ageBias(), agePrecision()

data(StripedBass4)
SB <- StripedBass4
str(SB)
headtail(SB)

ab <- ageBias(reader2~reader1,data=SB)

summary(ab,what="table",flip.table=TRUE)
summary(ab,what="symmetry")
summary(ab,what="bias")

plot(ab)                                                        # Left
plot(ab,diff=TRUE,show.n=FALSE)                                 # Right

plot(ab,diff=TRUE,show.n=FALSE,show.range=TRUE)                 # Left
plot(ab,diff=TRUE,show.n=FALSE,show.pts=TRUE,transparency=1/25) # Right

plot(ab,what="numbers",xlim=c(2,20),ylim=c(2,20))

ap <- agePrecision(reader2~reader1,data=SB)
summary(ap,what="difference",digits=1)
summary(ap,what="absolute difference",digits=2)
summary(ap,what="precision")

summary(ap,what="detail")  # only some rows shown

# ############################################################
# This is a trick so that it appears that only some rows in
# summary(ap,what="detail") are shown in the handout
headtail(ap$detail,n=5)
# ############################################################


# Script created at 2016-06-23 13:11:44
