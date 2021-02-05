Feeds<-which(beren3$event=="bottle")
avgMilk<-mean(beren3$value[Feeds]
avgFeed<-tapply(beren3$value[Feeds],beren3$age[Feeds],mean)
varFeed<-tapply(beren3$value[Feeds],beren3$age[Feeds],var)
totalFeed<-tapply(beren3$value[Feeds],beren3$age[Feeds],sum)
numFeeds<-tapply(beren3$value[Feeds],beren3$age[Feeds],length)
cor(beren3$value[Feeds],beren3$age[Feeds])
cor.test(beren3$value[Feeds],beren3$age[Feeds])
berenCor<-cor.test(beren3$value[Feeds],beren3$age[Feeds])
summary(berenCor)
berenANOVA<-aov(beren3$value[Feeds]~beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds]~beren3$caregiver[Feeds],xlab="who gave the bottle",ylab="amount of milk consumed (oz)")
?par
par(las=1,mar=c(5,5,1,1),mgp=c(2,0.5,0),tck=-0.01)
plot(as.numeric(names(totalFeed)),totalFeed,type="b",pch=16,xlab="age in days",ylab="ounces of milk")
abline(h=mean(totalFeed),lty=2,col='red')
pdf('r02b-totalMilkByDay.pdf',height=4,width=4)
par(las=1,mar=c(5,5,1,1),mgp=c(2,0.5,0),tck=-0.01)
plot(as.numeric(names(totalFeed)),totalFeed,.type="b",pch=16,xlab="age in days",ylab="ounces of milk")
abline(h=mean(totalFeed),lty=2,col='red')
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R")
unique(beren3$event)
Hypothesis 1:  If a baby takes naps during the day, then they will stay up thru the night. 
- If a baby takes a number of naps will it affect the babaies sleeping through the night.
Hypothesis 2: If a baby drinks milk, the baby will weight in mass. 
- If a baby drinks a certain amount of milk, will it affect the babies weight in mass?
Hypothesis 3: If a bottle has high flow nipples, it can affect the number of bowel movements during the day.
- Will high flow nipples affect a baibes bowel movement over a period of time?