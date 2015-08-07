# AFS Portland 16-Aug-15

source("scripts/02_ALKConstruction.R")   # appropriately set the working directory before this

ls()
headtail(sp.len)

sp.len.mod <- alkIndivAge(ALK.obs,age~tl,data=sp.len)
headtail(sp.len.mod)

sp.comb <- rbind(sp.age,sp.len.mod)
str(sp.comb)

agefreq <- xtabs(~age,data=sp.comb)
prop.table(agefreq)

hist(~age,data=sp.comb,breaks=0:5,xlab="Age (yrs)")

( sp.sum <- Summarize(tl~age,data=sp.comb,digits=2) )
plot(tl~age,data=sp.comb,ylab="Total Length (mm)",xlab="Age (yrs)",pch=16,col=rgb(0,0,0,0.1))
lines(mean~fact2num(age),data=sp.sum,col="blue",lwd=2)


# Script created at 2015-08-07 10:42:08
