# AFS Portland 16-Aug-15

library(FSA)                                # for headtail(), alkPlot()
library(dplyr)                              # for filter(), mutate()
library(nnet)                               # for multinom()

sp <- read.csv("data/SpotVA2.csv",header=TRUE)   # appropriately set the working directory before this
headtail(sp)

# ############################################################
# This code demonstrates the use of is.na().  It is not needed
# for the analysis.
tmp <- headtail(sp) 
cbind(tmp,is.na(tmp$age),!is.na(tmp$age))
# ############################################################

sp.len <- filter(sp,is.na(age))
headtail(sp.len)
sp.age <- filter(sp,!is.na(age))
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

alkPlot(ALK.obs,pal="gray",xlab="Total Length (cm)")
alkPlot(ALK.sm,xlab="Total Length (cm)")
alkPlot(ALK.sm,pal="gray",showLegend=TRUE,xlab="Total Length (cm)")
alkPlot(ALK.sm,type="area",pal="gray",showLegend=TRUE,xlab="Total Length (cm)")
alkPlot(ALK.sm,type="lines",pal="gray",xlab="Total Length (cm)")
alkPlot(ALK.sm,type="bubble",xlab="Total Length (cm)")


# Script created at 2015-07-23 19:38:37
