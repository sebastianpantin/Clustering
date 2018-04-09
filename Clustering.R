library(readr)
# Import data, change path to your own data
Lsun <- read_delim("Downloads/FCPS/01FCPSdata/Lsun.lrn", 
                   "\t", escape_double = FALSE, trim_ws = TRUE, 
                   skip = 3)
data<-cbind(Lsun$C1,Lsun$C2)
plot(data)

# NbClust #
nb<-NbClust(data, distance = "euclidean", min.nc=2, max.nc=12,
             method = "kmeans", index = "all")

hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,]))) # 4 seems to be best

# kmeans, decide number of clusters #
wss <- sapply(1:10, function(k){kmeans(data, k)$tot.withinss})
plot(1:10, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
kmeansData <- kmeans(data,3)
plot(data, col = kmeansData$cluster)
points(kmeansData$centers, col = 1:3, pch = 8)

# Pam #
pp<-pam(data,5)
pk <- pamk(data)$nc
S <- sapply(2:10, function(k){pam(dist(data), k)$sil$avg})
plot(2:10, S,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Average ")

mydata<-as.data.frame(data)
mydata$cl<-as.factor(pp$clustering)
ggpairs(data=mydata, # data.frame with variables
        columns=1:2, # columns to plot, default to all.
        title="Sun formation data", # title of the plot
        mapping = ggplot2::aes(color =cl)
)

clusplot(data, pp$cluster, color=TRUE, shade=TRUE, lines=0)

# H clust # 
hh<-hclust(dist(data),"single")
hh<-hclust(dist(data))
hh<-hclust(dist(data), "average")

plot(hh)
cc<-cor(t(data))
hh<-hclust(as.dist(1-cc))
plot(hh)


# M clust, not optimal for lots of parameters - should reduce dimensions #
kk<-Mclust(data)
plot(kk)
# EVI seems to be best
kk<-Mclust(data,model="EVI")
plot(kk)
# 5 seems to be the best number of clusters
kk <-  Mclust(data, G=5)
summary(kk)
plot(kk)

# Dbscan #
kNNdistplot(data, k = 3)
abline(h=.3, col = "red", lty=2)

res = dbscan(data, eps = 0.3, minPts = 5)
plot(data, col=res$cluster)
points(data[res$cluster==0,], pch = 3, col = "grey")
mydata<-as.data.frame(data)
mydata$cl<-as.factor(res$cluster)
ggpairs(data=mydata, # data.frame with variables
        columns=1:2, # columns to plot, default to all.
        title="Sun formation data", # title of the plot
        mapping = ggplot2::aes(color =cl)
        )
# Hdbscan #
myData <- as.data.frame(data)
hdb<-hdbscan(data, minPts = 5)
plot(hdb$hc)
mydata<-as.data.frame(data)
mydata$cl<-as.factor(hdb$cluster)
ggpairs(data=mydata, # data.frame with variables
        columns=1:2, # columns to plot, default to all.
        title="Sun formation data", # title of the plot
        mapping = ggplot2::aes(color =cl)
)
fviz_cluster(res, myData, stand = FALSE, frame = FALSE, geom = "point")

# Apcluster #
apclus <- apcluster(negDistMat(r=2), data)
cat("affinity propogation optimal number of clusters:", length(apclus@clusters), "\n")
heatmap(apclus)
plot(apclus, data)
