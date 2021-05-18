# Required libraries:

library(PDSCE)
library(biotools)

# Main function:

pi0.mod=function(pi0.est,data,f)
{
S=pdsoft(cov(t(data)),lam=0.3)$sigma
R=cov2cor(S)
change.R=abs(R)-diag(nrow(R))
proc=tocher(as.dist(change.R))
f1=function(index)
{
return(ifelse(length(index)/nrow(data)<f,pi0.est(data),pi0.est(data[as.numeric(unlist(index)),])))
}
f2=function(index)
{
length(index)/nrow(data)
}
pi0.arr=as.numeric(unlist(lapply(proc$clusters,f1)))
wt.arr=as.numeric(unlist(lapply(proc$clusters,f2)))
est=1/sum(wt.arr/pi0.arr)
return(est)
}
