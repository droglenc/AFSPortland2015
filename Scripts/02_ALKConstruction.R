# AFS Portland 16-Aug-15

library(FSA)                                # for headtail(), alkPlot()
library(FSAdata)                            # for SpotVA2 data
library(dplyr)                              # for filter(), mutate()
library(nnet)                               # for multinom()

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

alkPlot(ALK.obs,xlab="Total Length (cm)")
alkPlot(ALK.sm,xlab="Total Length (cm)")

alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",showLegend=TRUE)
alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",showLegend=TRUE,type="area")

alkPlot(ALK.sm,xlab="Total Length (cm)",pal="gray",type="lines")
alkPlot(ALK.sm,xlab="Total Length (cm)",type="bubble")


# Script created at 2015-08-07 10:36:30
