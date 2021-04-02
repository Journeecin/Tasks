setwd("~/Desktop/Evolution/Evolution/Tasks/Task_08")
library("phytools")
tree<-read.tree('https://jonsmitchell.com/data/anolis.tre')
plot(tree,type="fan")
# tree$tip.label
Q1: There are 82 Tips and branch lengths are present. 
data<-read.csv("https://jonsmitchell.com/data/svl.csv",stringsAsFactors=F,row.names=1)
data
data[,1]
Q2: data is a list of each species of lizard and their snout vent length. There are 82 dimensions in this data set. 
svl<-setNames(data$svl,rownames(data))
svl
Ancestors<-fastAnc(tree,svl,vars=TRUE,CI=TRUE)
print(Ancestors,printlen=82)
Q3: The estimated values stored are the Ancesteral character estimates and variances on ancesteral states. The CI95 element is the lower and upper 95% confidence interval between the estimates.
Q4: The unceratnity around ancesteral states are at the same average for all and are small. They could also be considered to have a low correlation. 
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree,type="fan",lwd=2,show.tip.label=F)
tiplabels(pch=16,cex=0.25*svl[tree$tip.label])
nodelabels(pch=16,cex=0.25*Ancestors$ace)
obj<-contMap(tree,svl,plot=F)
plot(obj,type="fan",legend=0.7*max(nodeHeights(tree)),sig=2,fsize=c(0.7,0.9))
fossilData<-data.frame(svl=log(c(25.4,23.2,17.7,19.7,24,31)),tip1=c("Anolis_aliniger","Anolis_aliniger","Anolis_occultus","Anolis_ricordii","Anolis_christatellus","Anolis_occultus"),tip2=c("Anolis_chlorocyanus","Anolis_coelestinus","Anolis_hendersoni","Anolis_cybotes","Anolis_angusticeps","Anolis_angusticeps"))
Q5:
fossilNodes<-c()
nodeN<-c()
{for(i in 1:nrow(fossilData))
i<-if(i==1){print(Ancestors)}}
Node<-fastMRCA(tree,fossilData[i,"tip1"],fossilData[i,"tip2"])
fossilNodes[i]<-fossilData[i,"svl"]
nodeN[i]<-Node
names(fossilNodes)<-nodeN
Ancestors_withFossils<-fastAnc(tree,svl,anc.states=fossilNodes,CI=TRUE,var=TRUE)
Q7: The fossil data plays a significant role in reconstructing small ancesteral; demonstrating the gneral importance of how branch length and  node information affects estimated ancesteral sizes.
Q8: 
install.packages("geiger")
library(geiger)
install.packages("picante")
library(picante)

svlData<-read.csv("https://jonsmitchell.com/data/svl.csv",stringsAsFactors=F,row.names=1)

geo=get(data(geospiza))

tmp=treedata(geo$phy,geo$dat)
phy=tmp$phy
dat=tmp$data

brownFit<-fitContinuous(phy,dat[,"wingL"],SE=NA,control=list(niter=50),ncores=2)

print(names(brownFit))
print(brownFit)

flik=brownFit$lik
print(argn(flik))

fitGeospiza=function(trait=c("wingL","tarsusL","culmenL","beakD","gonysW")){

trait=match.arg(trait,c("wingL","tarsusL","culmenL","beakD","gonysW"))

models=c("BM","OU","EB","white")
summaries=c("diffusion","Ornstein-Uhlenbeck","early burst","white noise")

aic.se=numeric(length(models))
lnl.se=numeric(length(models))

for(m in 1:length(models)){
cat("\n\n\n\n\t*** ", paste(toupper(summaries[m]),": fitting ", sep=""), models[m]," with SE *** \n", sep="")
tmp=fitContinuous(phy,dat[,trait],SE=NA, model=models[m],bounds=list(SE=c(0,0.5)), ncores=2)
print(tmp)
aic.se[m]=tmp$opt$aicc
lnl.se[m]=tmp$opt$lnL}

aic=numeric(length(models))
lnl=numeric(length(models))

for(m in 1:length(models)){
cat("\n\n\n\n\t*** ", paste(toupper(summaries[m]),": fitting ", sep=""), models[m]," *** \n", sep="")
tmp=fitContinuous(phy,dat[,trait],SE=0,model=models[m], ncores=2)
print(tmp)
aic[m]=tmp$opt$aicc
lnl[m]=tmp$opt$lnL
}
	
names(aic.se)<-names(lnl.se)<-names(aic)<-names(lnl)<-models
delta_aic<-function(x)x-x[which(x==min(x))]

daic=delta_aic(aic)
cat("/n/n/n/t/t/t/t***MODEL COMPARISON: ",trait,"***/n",sep="")
cat("/tdelta-AIC values for models assuming no measurement error /t/t/t/t zero indicates the best model /n/n")
print(daic,digits=2)

daic.se=delta_aic(aic.se)
cat("/n/n/n/n/t/t/t/t***MODEL COMPARISON:",trait,"***/n",sep="")
cat("/t/t delta-AIC values for model estimating SE /t/t/t/t zero indicates the best model /n/n/")
print(daic.se,digits=2)
cat("/n/n/n")

res_aicc=rbind(aic,aic.se,daic,daic.se)
rownames(res_aicc)=c("AICc","AICc_SE","dAICc","dAICc_SE")

return(res_aicc)

}

res=fitGeospiza("wingL")
print(res)

lambda<-fitContinuous(phy,dat,model=c("lambda"))
BM<-fitContinuous(phy,dat,model=c("BM"))
nos<-fitContinuous(phy,dat,model=c("white"))
lambda$opt$aicc
BM$opt$aicc
lambda$opt$aicc
nos$opt$aicc
BM
lambda
nos

?fastAnc
?fitContinuous

Q9: The best fit model is White 
Q10: Yes the model is slightly different from what fastAnc assumes. 






 