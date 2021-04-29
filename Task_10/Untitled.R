setwd("~/Desktop/Evolution/Evolution/Tasks/Task_10")
install.packages("diversitree")
library("diversitree")
transition_0to1<-0.1
transition_1to0<-0.1
speciation_0<-0.2
extinction_0<-0.1
speciation_1<-0.2
extinction_1<-0.1
maxN<-1e3
maxT<-50
Pars<-c(speciation_0,speciation_1,extinction_0,extinction_1,transition_0to1,transition_1to0)
simTree<-tree.bisse(Pars,max.taxa=maxN,max.t=maxT)
str(simTree)
stateTable<-table(simTree$tip.state)
stateTable/sum(stateTable)
plot(simTree)

install.packages("diversitree")
library("diversitree")
transition_0to1<-0.5
transition_1to0<-0.5
speciation_0<-0.2
extinction_0<-0.1
speciation_1<-0.2
extinction_1<-0.1
maxN<-1e3
maxT<-50
Pars<-c(speciation_0,speciation_1,extinction_0,extinction_1,transition_0to1,transition_1to0)
simTree<-tree.bisse(Pars,max.taxa=maxN,max.t=maxT)
str(simTree)
stateTable<-table(simTree$tip.state)
stateTable/sum(stateTable)
plot(simTree)

install.packages("diversitree")
library(rpart)
transition_0to1<-0.4
transition_1to0<-0.4
speciation_0<-0.2
extinction_0<-0.1
speciation_1<-0.3
extinction_1<-0.1
maxN<-1e3
maxT<-50
Pars<-c(speciation_0,speciation_1,extinction_0,extinction_1,transition_0to1,transition_1to0)
iris<-tree.bisse(Pars,max.taxa=maxN,max.t=maxT)
str(iris)
stateTable<-table(iris$tip.state)
stateTable/sum(stateTable)
plot(iris)

install.packages("diversitree")
library(rpart)
transition_0to1<-0.1
transition_1to0<-0.1
speciation_0<-0.2
extinction_0<-0.1
speciation_1<-0.2
extinction_1<-0.1
maxN<-1e3
maxT<-50
Pars<-c(speciation_0,speciation_1,extinction_0,extinction_1,transition_0to1,transition_1to0)
iris<-tree.bisse(Pars,max.taxa=maxN,max.t=maxT)
str(iris)
stateTable<-table(iris$tip.state)
stateTable/sum(stateTable)
plot(iris)


- The points I think are interesting: 
1. When changing to str(iris) with different parameters, the frequency of state 1 has little to know variation. 
2. However, if I keep the parameters the same for str(simTree) and str(iris) .. the frequency of state 1 is higher than state 0. When it is usually state 1 being less than state 0 when the parameter are different. 

