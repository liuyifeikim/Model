library(igraph)

#转换为网络格式
net_graph1 <- graph_from_data_frame(net1[net1$pre == "1", c(2, 4)], directed = F) 


#点线描述
vcount(net)     #点数量
ecount(net)     #边数量
head(V(net),10) #点列表
head(E(net),10) #边列表

#网络格式
get.adjacency(net) #get matrix
get.adjedgelist(net) #get edgelist

#密度
edge_density(net) #density
ecount(net) / (choose(vcount(net), 2) * 2) #density
reciprocity(net) #reciprocity，双向关系数与全部关系数比值，全部为双向关系=1

#距离
diameter(net, directed = T) #最长距离（直径）
get.diameter(net, directed = T) #最长距离路径
eccentricity(net, 'r') #特定点离心率，到各点距离最大值
distances(net) #距离矩阵
shortest.paths(net) #同distance
mean_distance(net) #平均距离
distances(net)['r',] %>% sort() %>% head(10) #特定点与其他点最短距离
distances(net, v = 'r', to = 'python') #特定点与特定点最短距离
shortest.paths(net, v = 'r', to = 'python') #同上
shortest_paths(net, from = 'r', to = 'jquery')$vpath #输出最短路径

#个体中心网
ego(net, 1, 'r') #n步内的连接对象
ego_size(net, 1, 'r') #n步内有连接的对象数量
net_r <- make_ego_graph(net, 1, 'r')[[1]] #局部网络
plot(net_r, vertex.label.cex = 2, edge.arrow.size = 0.4) #局部网络制图


#度中心性（degree centrality）
head(sort(degree(net,mode='in'),decreasing=TRUE)) #in-degree
head(sort(degree(net,mode='out'),decreasing=TRUE)) #out-degree
head(sort(degree(net,mode='all'),decreasing=TRUE)) #all-degree,无向网络以上三个值全部相同
head(sort(degree(net,mode='all',normalized=T),decreasing=TRUE)) #标准化degree，除以n-1

sum(degree(net,mode='in')) 
sum(degree(net,mode='out')) 
sum(degree(net,mode='all')) #无向网络以上三个值全部相同

(sum(max(degree(net)) - degree(net))) / ((vcount(net) - 1) * (vcount(net) - 2)) #群组度中心性
sd(degree(net)) #群组度中心性方差
mean(degree(net)) #度数平均值
mean(degree(net)) / (vcount(net) - 1) #标准化度数平均值=edge_density(net)
edge_density(net) #density，现存关系数与所有可能关系数比值

degree(net) %>% sort(decreasing = T)
degree(net, normalized = TRUE) %>% sort(decreasing = T) 
degree(net, mode = 'in', normalized = T) %>% sort(decreasing = T) 
degree(net, mode = 'in', normalized = T)['r'] 

degree_distribution(net) #度的比例分布
degree_distribution(net, mode = 'in', cumulative = T)

degree(net, mode = 'in') %>% mean() #mean outdegree=mean indegree
degree(net, mode = 'in') %>% sd() #sd indegree


#接近中心性（closeness centrality）
head(sort(closeness(net), decreasing = TRUE), 10) #closeness
head(sort(closeness(net, normalized = T), decreasing = TRUE), 10) #标准化closeness，乘以n-1
(sum(max(closeness(net, normalized = T)) - closeness(net, normalized = T))) /
  ((vcount(net) - 2) * (vcount(net) - 1) / ((2 * vcount(net) - 3))) #群组接近中心性
sd(closeness(net, normalized = T)) #群组接近中心性方差

closeness(net) %>% sort(decreasing = T) #closeness
closeness(net, mode = 'in', normalized = T) %>% sort(decreasing = T) #closeness
closeness(net, mode = 'in', normalized = T)['r']


#中介中心性（betweeness centrality）
head(sort(betweenness(net), decreasing = TRUE), 10) #betweenness
head(sort(betweenness(net, normalized = T), decreasing = TRUE), 10) #标准化betweenness，Bnorm=2*B/(n-1)(n-2)
(2 * sum(max(betweenness(net)) - betweenness(net))) /
  (((vcount(net) - 1) ^ 2) * (vcount(net) - 2)) #群组中介中心性
(sum(max(betweenness(net, normalized = T)) - betweenness(net, normalized = T))) /
  (vcount(net) - 1) #群组中介中心性简化版

betweenness(net) %>% sort(decreasing = T) #betweeness
betweenness(net, normalized = T) %>% sort(decreasing = T)
betweenness(net, normalized = T)['r']
edge.betweenness(net, directed = T) #edge betweeness

#局部网络及可视化
clusters(net) #有联系的结点聚合
components(net) #和cluster一样
component_distribution(net, cumulative = T)
decompose.graph(net) #子图分解

cliques(net) #完全子图,可能为最大完全子图的一部分
max_cliques(net) #最大完全子图,最少2个节点
count_max_cliques(net) #最大完全子图数量
largest_cliques(net) #全局最大完全子图
clique_num(net) #全局最大完全子图的节点数量

max_c1 <- largest_cliques(net)[[1]]
max_c1_plot <- induced_subgraph(graph = net, vids = max_c1) #转换为graph对象
plot(max_c1_plot)

cliques_python <- max_cliques(net, subset = 'python')
cliques_python_plot <- induced_subgraph(net, vids = cliques_python)
plot(cliques_python_plot)

max_cliques <- max_cliques(net)
max_cliques_num <- sapply(as.list(max_cliques), length)
table(max_cliques_num)


#网络聚类系数，联通度
transitivity(net,type = 'globalundirected') #整体,无向图
transitivity(net,type = 'localundirected') #每个点,无向图
transitivity(net,type = 'localaverageundirected') #每个点,无向图,取均值


#每个节点所在最大k核图的核数
coreness(net) 
coreness(net)['python'] 
coreness(net)['r']


#邻近声望
edges_pair_d_matrix <- as.network(edges_pair_d, directed = TRUE)
head(sort(prestige(edges_pair_d_matrix, cmode = 'domain'), decreasing = TRUE), 10)

#BETWEENESS聚类
cluster <- cluster_edge_betweenness(net, directed = T)
dendPlot(cluster, mode = 'hclust')


#######################################################


#网络可视化
plot(net,
     vertex.color = 'blue',                                       #点颜色
     vertex.size = 0,                                             #点大小
     vertex.label.cex = min_max(V(net)$nodesize) + 1,             #点标签大小
     vertex.label.family = 'Times',                               #点标签字体
     edge.color = 'grey', edge.width = E(net)$value / 10,         #边颜色
     edge.arrow.size = 0.1,                                       #箭头大小
     layout = layout_with_fr(net))                                #布局

#最长路径示意图
diam <- get.diameter(net, directed = T) #最长距离路径
vcol <- rep('blue', vcount(net))
vcol[diam] <- 'red'
ecol <- rep('grey', ecount(net))
ecol[E(net, path = diam)] <- 'red'
plot(net, 
     vertex.size = 0,
     vertex.label.color = vcol, vertex.label.cex = min_max(V(net)$nodesize) + 1,
     edge.color = ecol,
     edge.arrow.size = 0.2,
     layout = layout_with_fr(net))

#ggraph包
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)