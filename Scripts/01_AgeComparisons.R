# AFS Portland 16-Aug-15

library(FSA)                              # for ageBias(), agePrecision()

SB <- read.csv("data/StripedBass4.csv")   # appropriately set the working directory before this
str(SB)

ab <- ageBias(reader2~reader1,data=SB)

summary(ab,what="table",flip.table=TRUE)
summary(ab,what="symmetry")

summary(ab,what="bias")

plot(ab)                                              # Left
plot(ab,diff=TRUE)                                    # Right

plot(ab,diff=TRUE,show.range=TRUE)                    # Left
plot(ab,diff=TRUE,show.pts=TRUE,transparency=1/25)    # Right

plot(ab,what="numbers",xlim=c(2,20),ylim=c(2,20))

ap <- agePrecision(reader2~reader1,data=SB)
summary(ap,what="difference",digits=1)
summary(ap,what="absolute difference",digits=2)
summary(ap,what="precision")

summary(ap,what="detail")  # only some rows shown

# ############################################################
# This is a trick so that it appears that only some rows in
# summary(ap,what="detail") are shown in the handout
headtail(ap$detail)
# ############################################################


# Script created at 2015-07-23 19:38:21
