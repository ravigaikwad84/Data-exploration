
setwd("D:\\H Clustering song dataset")


options(scipen=10)

##--------------------------------------Step1 : Read the data---------------------------------------------
football<-read.csv("football.csv")
names(football)
dim(football)
str(football)
##--------------------------------------Step2 : Scaling data + Finding Distance Measures---------------------------------------------

#Distance calculation : Euclidean
#Method : Average

#Daisy function in R scales the data and computes the distance measures
library(cluster)
#?daisy
dmatrix<-daisy(football[-c(1,9,19)],metric="euclidean",stand=TRUE)
summary(dmatrix)
class(dmatrix)

distmatrix<-dist(dmatrix)
str(distmatrix)

#Writing out the file
d<-as.matrix(distmatrix)
class(d)
write.csv(d,"distmatrix.csv",row.names=F)

##--------------------------------------Step3 : Hierarchcal clusterting algorithm---------------------------------------------

#hclust
?hclust
set.seed(20)
football_clust<-hclust(distmatrix,method="average")
str(football_clust)
#football_clust<-hclust(distmatrix,method="complete")


##--------------------------------------Step4 : Plotting a Dendogram---------------------------------------------

plot(as.dendrogram(football_clust))
#plot(as.dendrogram(football_clust),labels=football$Country)
rect.hclust(football_clust, 5)

##--------------------------------------Step5 : Examining the hclust object---------------------------------------------


#The cluster height used in the dendogram
football_clust$height

#labels for each of the objects being clustered
football_clust$labels<-football$First_Name


#distance measure used
football_clust$dist.method

##----------------------------------Step6 : Slicing the dendogram to get finite number of clusters---------------------------------------------

#To get flat clustering : 
#Divide the data into 20 clusters.This way we ll only have around 20 players in each group.
#It becomes easy for the coach to compare players

k<-cutree(football_clust,k = 20)
head(k)

#Once you have k which is the cluster no, attach it to your dataset
football$cluster<-k

##Looking for the player "Song" in these clusters

index<-which(football$First_Name=="Song")
football$cluster[index]

#This shows that the player "Song" belongs to cluster number 6
data_song_cluster<-football[football$cluster==6,]

write.csv(data_song_cluster,"data_song_cluster.csv",row.names=F)


