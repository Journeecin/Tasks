setwd("~/Desktop/Evolution/Evolution/Tasks/Task_09")
library("phytools")
Q1-3
trees<-list()
births<-c()
Fractions<-c()
for(i in 1:100){
x<-sample(1:100,1,replace=FALSE)
y<-sample(1:100,1,replace=FALSE)
births[i]=x
Fractions[i]=y
trees[[i]]<-pbtree(b=births[i],d=(births[i]*Fractions[i]),n=100)
}
Q4
The net diversification rate is 0.0391 vs log of -8.32 to -7.12
library(ape)
library(geiger)
plot(trees[[i]],show.tip.label=FALSE)
ltt.plot(trees[[i]],log="y")
bd.ms(phy=trees[[i]]$phy,time=100,n=100)
log(trees[[i]]$edge.length)
Q5
Average branch length is 0.0002497 and lower than speciation rate 
bd.km(trees[[i]],n=100)
for(i in 1:length(trees[[i]])){
branchLength<-mean(trees[[i]]$edge.length)
}
print(branchLength)
plotTree(trees[[i]],fsize=1.5,ftype="i",lwd=1.5,bty=1.5)
Q6
Relationship is 1 
cor(1:2,1:2)
Q7
trees[[i]]
Tree<-trees[[branchLength]]
plot(Tree)
rates<-c()
traits<-list()
for (i in 1:100){
rates[i]<-sample(1:100,1,replace=FALSE)
traits[[i]]<-fastBM(Tree,sig2=rates[i])
print(rates[i])
print(traits[[i]])
Q8
No correlation between rates and mean of traits
traitsMean<-mean(traits[[i]])
traitsMean
ratesMean<-mean(rates[i])
cor(traitsMean,ratesMean)
plot(traitsMean,ratesMean)
Q9
No correlation between variance of traits and rates 
a<-var(traits[[i]])
b<-var(rates[i])
cor(a,b)
Q10
No correlation between 1st trait and 2nd trait elements, its not significant because there is no big difference. 
traitMat<-cbind(traits[[1]], traits[[4]])
cor(traitMat)


