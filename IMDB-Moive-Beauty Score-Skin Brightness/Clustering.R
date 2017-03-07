
#read data set
Movie <- read.csv('after1950.csv',header=TRUE,stringsAsFactors=FALSE)
mydata <- data.frame(Movie$ID,Movie$S1,Movie$real_age1,Movie$BS1,Movie$BrS1,
                     Movie$MetaScore1,Movie$S2,Movie$real_age2,Movie$BS2,
                     Movie$BrS2,Movie$MetaScore2)

#set NA value
mydata[mydata=='#N/A']<-NA

#convert to numeric type
mydata$Movie.real_age1<-as.numeric(as.character(mydata$Movie.real_age1))
mydata$Movie.BS1<-as.numeric(as.character(mydata$Movie.BS1))
mydata$Movie.BrS1<-as.numeric(as.character(mydata$Movie.BrS1))
mydata$Movie.MetaScore1 <-as.numeric(as.character(mydata$Movie.MetaScore1))

mydata$Movie.real_age2<-as.numeric(as.character(mydata$Movie.real_age2))
mydata$Movie.BS2<-as.numeric(as.character(mydata$Movie.BS2))
mydata$Movie.BrS2<-as.numeric(as.character(mydata$Movie.BrS2))
mydata$Movie.MetaScore2 <-as.numeric(as.character(mydata$Movie.MetaScore2))

###combine###
actor1 = data.frame('ID' = mydata$Movie.ID,
                    'Name'=mydata$Movie.S1,
                    'Age'=mydata$Movie.real_age1,
                    'BS'=mydata$Movie.BS1,
                    'BRS'=mydata$Movie.BrS1,
                    'MetaScore'=mydata$Movie.MetaScore1)

actor2 = data.frame('ID'=mydata$Movie.ID,
                    'Name'=mydata$Movie.S2,
                    'Age'=mydata$Movie.real_age2,
                    'BS'=mydata$Movie.BS2,
                    'BRS'=mydata$Movie.BrS2,
                    'MetaScore'=mydata$Movie.MetaScore2)

four = rbind(actor1,actor2)

dim(four)

####Mission value###

sum(is.na(four$Age))/length(four$Age)
sum(is.na(four$BS))/length(four$BS)
sum(is.na(four$BRS))/length(four$BRS)
sum(is.na(four$MetaScore))/length(four$MetaScore)

mean(!complete.cases(four))


four_rm <- four[complete.cases(four),]



###historgram###
par(mfrow=c(2,2))
hist(four$Age,col="cadetblue", density=30, angle=50, border="black",
     main="real age", 
     xlab="real age", ylab="frequency")

hist(four$BS,col="orange", density=30, angle=50, border="black",
     main="Beauty Score", 
     xlab="Beauty Score", ylab="frequency")

hist(four$BRS,col="red", density=30, angle=50, border="black",
     main="Skin Brightness", 
     xlab="Skin Brightness", ylab="frequency")

hist(four$MetaScore,col="yellow", density=30, angle=50, border="black",
     main="Meta Score", 
     xlab="Meta Score", ylab="frequency")

###QQ plot###
par(mfrow=c(2,2))
qqnorm(four$Age, main = "Age Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(four$Age,lwd=2)

qqnorm(four$BS, main = "BS Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(four$BS,lwd=2)

qqnorm(four$BRS, main = "BRS Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(four$BRS,lwd=2)

qqnorm(four$MetaScore, main = "MetaScore Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(four$MetaScore,lwd=2)

pairs(four[,c('Age',
                'BS','BRS',
                'MetaScore')])

cor(data.frame(four$Age,
               four$BS,
               four$BRS,
               four$MetaScore),use='na.or.complete')


###clustering###
cl.data.bridge <-data.frame(four_rm$ID,four_rm$Name,four_rm$Age,four_rm$BS,four_rm$BRS,four_rm$MetaScore)

cl.data<-data.frame(cl.data.bridge$four_rm.Age,cl.data.bridge$four_rm.BS,cl.data.bridge$four_rm.BRS,cl.data.bridge$four_rm.MetaScore)

###Elbow method to choose k###

wss <- (nrow(cl.data)-1)*sum(apply(cl.data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cl.data,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters \n with the Elbow Method",
     pch=20, cex=2)

#########
fit <- kmeans(cl.data, 4,iter.max=10,nstart = 100) # 5 cluster solution

aggregate(cl.data,by=list(fit$cluster),FUN=mean)
(fit)

plot(cl.data, col = fit$cluster)
points(fit$centers, col = 1:5, pch = 8)

###pca and 3d###
cl.pca <- prcomp(cl.data,
                 center = TRUE,
                 scale. = TRUE) 
cl.pca
library(scatterplot3d)

cl.pca$x[,1:3]
scatterplot3d(cl.pca$x[,1:3],color = fit$cluster)

cl.pca$x

fit$cluster
####cluster chart###
library(cluster) 

clusplot(cl.data, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
library(fpc)
plotcluster(cl.data, fit$cluster)

cluster.stats(d, fit1$cluster, fit2$cluster)
####
kmeans.results <- kmeans(cluster, 4)

kmeans.results$cluster

plot(cluster[c('four_rm.Age','four_rm.BS')], col = kmeans.results$cluster)

points(kmeans.results$centers[,c("four_rm.Age", "four_rm.BS")], col = 1:3, pch =8,
       cex=2)

###pamk##
pamk.result <- pamk(cl.data)
pamk.result$nc
layout(matrix(c(1,2),1,2))
plot(pamk.result$pamobject)
layout(matrix(1))

###clara###
cla_result <- clara(cl.data,4)
plot(cl.data, col=cla_result$clustering)
points(cla_result$medoids, col = 1:2, pch = 8)

###Hierarchical Clustering###
hc <- hclust(dist(cl.data), method="ave")
plot(hc, hang = -1, labels=hc$labels)
# cut tree into 3 clusters
rect.hclust(hc, k=4)
groups <- cutree(hc, k=4)


###Normalization
normalize<-function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

cl.data.norm <- as.data.frame(lapply(cl.data, normalize))

#check numbers of cluster
wss.norm <- (nrow(cl.data.norm)-1)*sum(apply(cl.data.norm,2,var))
for (i in 2:15) wss.norm[i] <- sum(kmeans(cl.data.norm,
                                     centers=i)$withinss)
plot(1:15, wss.norm, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters \n with the Elbow Method",
     pch=20, cex=2)

#Do the cluster again
fit.norm <- kmeans(cl.data.norm, 4,iter.max=10,nstart = 100) # 5 cluster solution

aggregate(cl.data.norm,by=list(fit.norm$cluster),FUN=mean)
(fit.norm)

plot(cl.data.norm, col = fit.norm$cluster)
points(fit.norm$centers, col = 1:5, pch = 8)

#3d
cl.norm.pca <- prcomp(cl.data.norm,
                 center = TRUE,
                 scale. = TRUE) 
cl.norm.pca
library(scatterplot3d)

cl.norm.pca$x[,1:3]
scatterplot3d(cl.norm.pca$x[,1:3],color = fit.norm$cluster)

cl.norm.pca$x

fit.norm$cluster
#plot cluster
clusplot(cl.data.norm, fit.norm$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
plotcluster(cl.data.norm, fit.norm$cluster)

(fit.norm)
fit.norm$centers

#different clusters

fit8.norm <- kmeans(cl.data.norm, 8,iter.max=10,nstart = 100) # 3 cluster solution

aggregate(cl.data.norm,by=list(fit8.norm$cluster),FUN=mean)
(fit8.norm)

plot(cl.data.norm, col = fit8.norm$cluster)
points(fit8.norm$centers, col = 1:5, pch = 8)

#add clsuter label

label<-fit.norm$cluster

mydata.lr <- data.frame(Movie$ID,Movie$S1,Movie$real_age1,Movie$BS1,Movie$BrS1,
                     Movie$MetaScore1,Movie$S2,Movie$real_age2,Movie$BS2,
                     Movie$BrS2,Movie$MetaScore2,Movie$Revenue)
cl.data['label']<-label

mydata.lr$Movie.real_age1<-as.numeric(as.character(mydata.lr$Movie.real_age1))
mydata.lr$Movie.BS1<-as.numeric(as.character(mydata.lr$Movie.BS1))
mydata.lr$Movie.BrS1<-as.numeric(as.character(mydata.lr$Movie.BrS1))
mydata.lr$Movie.MetaScore1 <-as.numeric(as.character(mydata.lr$Movie.MetaScore1))

mydata.lr$Movie.real_age2<-as.numeric(as.character(mydata.lr$Movie.real_age2))
mydata.lr$Movie.BS2<-as.numeric(as.character(mydata.lr$Movie.BS2))
mydata.lr$Movie.BrS2<-as.numeric(as.character(mydata.lr$Movie.BrS2))
mydata.lr$Movie.MetaScore2 <-as.numeric(as.character(mydata.lr$Movie.MetaScore2))
mydata.lr$Movie.Revenue <-as.numeric(as.character(mydata.lr$Movie.Revenue))

duplicated(mydata.lr$Movie.BrS1)
mydata.lr$Movie.Revenue<-NULL
rm(mydata.lr_rm)
mydata.lr_rm <- mydata.lr[complete.cases(mydata.lr),]


cl.data.bridge['label']<-cl.data$label

actor1_la <- cl.data.bridge[1:6340,]
actor2_la <- cl.data.bridge[6341:11925,]
merge_actor<-merge(actor1_la,actor2_la,by.x = 'four_rm.ID',by.y='four_rm.ID')

mydata.lr_rm['actor1_label']<-merge_actor$label.x
mydata.lr_rm['actor2_label']<-merge_actor$label.y

mydata.rev <- data.frame(Movie$ID,Movie$Revenue)
mydata.rev$Movie.Revenue <-as.numeric(as.character(mydata.rev$Movie.Revenue))

lr.data <- merge(merge_actor,mydata.rev,by.x='four_rm.ID',by.y='Movie.ID')
lr.data.final <- subset(lr.data,Movie.Revenue>0)

