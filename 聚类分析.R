#KMEANS
result<-rep(0, 50)
for(k in 1:50){
  set.seed(100)
  model_cluster <- kmeans(model_kmeans_z, k)  #左边不设置K，每次覆盖
  result[k] <- model_cluster$betweenss / model_cluster$totss
}
result
plot(result, xaxt="n", type="o", xlab = "Number of K", ylab = "Between/totalness") #连线图
axis(1, at = seq(0, 50, 2)) 

k <- c(1:117)
cp <- rep(NA, 117)
for (i in 1:117){
  cp[i] = kmeans(gold_scale, i)$betweenss / kmeans(gold_scale, i)$totss
}
cp <- data.frame(cbind(k, cp)) 
ggplot(cp, aes(k, cp)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 120, 5)) + 
  theme_hc()

#fpc包，按照轮廓系数自动选择最优k值
dist_dtm05 <- dist(t(dtm05), method = "euclidean", diag = F)
kmeans_best <- kmeansruns(dist_dtm05, krange = 2:10, criterion = 'asw') 
kmeans_best

#K中心点聚类
pam_best <- pamk(dist_dtm05, krange = 2:10, criterion = "asw")
pfit <- pam(dist_dtm05, 2)
clusplot(pfit, lines = 0, shade = F, labels = 2, color = 1, main = "Clusplot")

#层次聚类
hfit <- hclust(dist_dtm05, method = "complete")

plot(hfit, hang = -1)
rect.hclust(hfit, 2, border = c("red", "blue"))

c2 <- cutree(hfit, 2)  #根据层次聚类的结果取聚类数
c2 <- as.data.frame(c2)
colnames(c2) <- "cluster"
cluster <- rep(NA, 2)
for (i in 1:2){
  cluster[i] <- list(row.names(c2)[c2$cluster == i])
}
cluster #每组的内容列表



#聚类性能
library(clusterCrit) #intCriteria&extCriteria
library(igraph) #compare
unlist(extCriteria(as.integer(group_real$real),as.integer(group_real$`old-group`),
                   crit = c("rand", "folkes_mallows","jaccard","precision","recall"))) #第一版vs真实
compare(group_real$real,group_real$`old-group`,method='adjusted.rand') #第一版vs真实
compare(real3$`old-group`,real3$real,method="nmi") #0.9780145
arandi(real3$`old-group0.15`,real3$real) #mcclust包

#Purity
library(NMF) #Purity
library(IntNMF) #ClusterPurity
ClusterPurity(real3$`old-group`, real3$real) #0.920882

#Jaccard
library(clusteval)
cluster_similarity(real3$`old-group`, real3$real, similarity="jaccard") #0.2443439

