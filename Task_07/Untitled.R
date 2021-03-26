setwd("~/Desktop/Evolution/Evolution/Tasks/Task_07")
install.packages("phytools")
library("phytools")
install.packages("ape")
library("ape")
text.string<-
"(((((((cow,pig),whale),(bat,(lemur,human))),(robin,iguana)),coelacanth
),(gold_fish,trout)),shark);"
vert.tree<-read.tree(text=text.string)
plot(vert.tree,edge.width=2)
nodelabels(frame="circle",bg='white',cex=1)
Q1. The shark because they both come from node 13 and then it branches off to node 14 next, in which the goldfish is most correlated to. There are to many node splits between the human and goldfish. 
vert.tree
Q2. There are no branch lengths. 
str(vert.tree)
tree<-read.tree(text="(((A,B),(C,D)),E);")
plotTree(tree,offset=1)
tiplabels(frame="circle",bg='lightblue',cex=1)
nodelabels(frame="circle",bg='white',cex=1)
tree$tip.label
tree$edge
AnolisTree<-force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length,col='black',border='white',main="",xlab="edge lengths for the Anolis tree",ylim=c(0,50),xlim=c(0,6))
tipEdges<-which(AnolisTree$edge[,2]<=Ntip(AnolisTree))
Lengths<-AnolisTree$edge.length
names(Lengths)<-AnolisTree$tip.label
names(Lengths)[which(Lengths==min(Lengths))]
plot(AnolisTree,cex=0.25)
Labs<-sapply(AnolisTree$edge.length,round,digits=2)
edgelabels(text=Labs,cex=0.25)z
?plot.phylo
Q3. 
plotTree(AnolisTree,offset=1,show.tip.label=FALSE)
Q4.  
plotTree(AnolisTree,alighn.tip.label=TRUE,type="fan")
Q5
plotTree(AnolisTree,offset=1,show.tip.label=TRUE,tip.color="red")
Q6
AnnolisTree<-force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
n<-length(AnolisTree$tip.label)
ee<-setNames(AnolisTree$edge.length[sapply(1:n,function(x,y)which(y==x),y=AnolisTree$edge[,2])],AnolisTree$tip.label)
AnolisTree<-pbtree(n=10)
AnnolisTree$edge.length<-round(AnolisTree$edge.length,0.5)
n<-length(AnolisTree$tip.label)
ee<-setNames(AnolisTree$edge.length[sapply(1:n,function(x,y)which(y==x), y=AnolisTree$edge[,2])], AnolisTree$tip.label)
ee
plot (AnolisTree, cex=1)
edgelabels(AnolisTree$edge.length)
Q7

Q8
anolis.pruned <- collapseTree(AnolisTree)
plotTree(anolis.pruned, type="fan", fsize=0.7, lwd=1, ftype="i")

ltt(AnolisTree)
abline(0,1,lwd=2,col='red',lty=2)
Q10
fit.bd()