## Age Comparison Slides

library(FSA)
library(FSAdata)
data(AlewifeLH)
str(AlewifeLH)
ab <- ageBias(scales~otoliths,data=AlewifeLH,
                  ref.lab="Otolith Age",nref.lab="Scale Age")
summary(ab,what="table")
summary(ab,what="table",flip.table=TRUE)
summary(ab,what="bias")
summary(ab,what="symmetry")

windows(5,4); par(mar=c(3,3,0.25,0.25),mgp=c(1.75,0.5,0),tcl=-0.2)
plot(ab,ylim=c(0,10))
plot(ab,difference=TRUE)

ap <- agePrecision(scales~otoliths,data=AlewifeLH)
summary(ap,what="difference")
summary(ap,what="detail")
