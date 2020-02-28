Polylepis.pepei <- read.csv("~/R/pepei/Polylepis pepei.csv", header=TRUE)
Polylepis.pepei
data <- Polylepis.pepei
data
apply(data,2,mean)
apply(data,2,sd)
coefvar <- function(x){100*sd(x)/mean(x)}
coefvar
apply(data,2,coefvar)
boxplot(data,xlab="Especies de vegetación", ylab="Abundancia")
###########
library(vegan)
diversity(data,"shannon")
diversity(data,index="shannon", MARGIN=1,base=2)
diversity(data,index="simpson")
diversity(data,index="invsimpson")
specnumber(data)
###
barplot(diversity(data,index="shannon",base=2),xlab="Bosques de Polylepis",ylab="abundancia", col="black")
log2(specnumber(data))
renyi(data,scale=c(0,1,2),hill=TRUE)
cor(data)
euclidia <- dist(data,method="euclidean", upper=T, diag=T)
euclidia
jaccard <- vegdist(data,method="jaccard", diag=FALSE, upper=FALSE)
jaccard
plot(hclust(vegdist(data,method="jaccard")), hang=-1, main="Analisis de cluster por bosque")
cluster <- t(data)
plot(hclust(vegdist(data,method="jaccard")), hang=-1, main="analisis de cluster", xlab="Muestras", ylab="Disimilaridad de jaccard")
x <- as.matrix(data)
x
heatmap(x,distfun = function(c)vegdist(c,"jaccard"), col=topo.colors(16))
heat

