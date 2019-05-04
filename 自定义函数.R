#异常值处理：+-3个标准差
outlier_sd_3_fun <- function(x){
  mean_x = mean(x)
  sd_x = sd(x)
  x = ifelse(x >= mean_x + 3 * sd_x, mean_x + 3 * sd_x, x)
  x = ifelse(x <= mean_x - 3 * sd_x, mean_x - 3 * sd_x, x)
  return(x)
} 

outlier_fun <- function(x){
   low_out = quantile(x, 0.25) - 1.5 * IQR(x) #下四分位�?-1.5倍四分位�?
  high_out = quantile(x, 0.75) + 1.5 * IQR(x) #上四分位�?+1.5倍四分位�?
   x_clear = ifelse(x < low_out, low_out, x)
   x_clear = ifelse(x_clear > high_out, high_out, x_clear)
   return(x_clear)
}


#极值标准化
min_max <- function(x){
  (x - min(x)) / (max(x) - min(x))
}   

#K近邻肘部�?
knn_acc<-rep(0,20)
for(i in 1:20){
  set.seed(1)
  knn_model <- knn(knn_train[, c(4:36)], knn_test[, c(4:36)], knn_train[, 47], i)
  knn_acc[i] <- confusionMatrix(knn_model, knn_test[, 47])$overall[1]
}

#C4.5调参
#Set confidence threshold for pruning
tree_model_c45_c_acc <- rep(NA, 10)
for(i in 1:20){
  tree_model_c45_c <- J48(formula, tree_model_c45_train, control = Weka_control(C = i * 0.05))
  tree_model_c45_c_pre <- predict(tree_model_c45_c, tree_model_c45_test)
  tree_model_c45_c_acc[i] <- (confusionMatrix(tree_model_c45_c_pre, tree_model_c45_test$TYPE1)$overall[1])
}
tree_model_c45_c_acc

#minimum number of instances per leaf
tree_model_c45_m_acc <- rep(0, 30)
for(i in 1:30){
  tree_model_c45 <- J48(formula, tree_model_c45_train, control = Weka_control(M = i))
  tree_model_c45_pre <- predict(tree_model_c45, tree_model_c45_test)
  tree_model_c45_m_acc[i] <- (confusionMatrix(tree_model_c45_pre, tree_model_c45_test$TYPE1)$overall[1])
}
tree_model_c45_m_acc

#多模型结果比�?
training4model<-function(data){
  
  library(caret)
  library(class)
  library(C50)
  library(e1071)
  library(RWeka)
  library(ggplot2)
  
  set.seed(1)
  ind<-createDataPartition(data$TYPE1,p=0.75,list = F)
  train<-data[ind,]
  test<-data[-ind,]
  formula<-TYPE1~maxfloor+beginfloor+totalfloor+housecount_r+housecount_t+validhouse_t+validhouseproportion_t+
    validhouseproportion_r+bighouse+middlehouse+smallhouse+specialhouse+specialhouseproportion+vaildbighouse+
    vaildbighouseproportion+misshouse_t+misshouseproportion_t+misshouse_c+misshouseproportion_c+misshousecontinues_c+
    misshouse_r+maxvalidhouse_r+differencevaluehouse_t+differencevaluehousecount+differencevaluehouseproportion_r+
    differencevaluehouse_c+differencevaluehouseproportion_c+differencevaluehouscontinues_c+differencevaluespecial+
    differencevaluegroup+notdifferencevaluegroupmax_r+notdifferencevaluehousecontinuesfloors_r+notdifferencevaluehousemax_r
  
  #KNN  
  knn_train<-train
  knn_test<-test
  knn_train[,c(4:36)]<-lapply(knn_train[,c(4:36)],scale)
  knn_test[,c(4:36)]<-lapply(knn_test[,c(4:36)],scale)
  
  knn_acc<-rep(0,20)
  for(i in 1:20){
    set.seed(1)
    knn_model_pre<-knn(knn_train[,c(4:36)],knn_test[,c(4:36)],knn_train[,47],i)
    knn_acc[i]<-confusionMatrix(knn_model_pre,knn_test[,47])$overall[1]
    knn_acc_max<-max(knn_acc[i]) #与单独跑的时候结果不太一�?
  }
  
  #C50
  C50_model_train<-train
  C50_model_test<-test
  
  C50_model<-C5.0(C50_model_train[,c(4:36)],C50_model_train[,47]) 
  C50_model_pre<-predict(C50_model,C50_model_test) 
  C50_acc<-confusionMatrix(C50_model_pre,C50_model_test[,47])$overall[1] 
  
  #NB
  nb_train<-train
  nb_test<-test
  
  nb_model<-naiveBayes(nb_train[,4:36],nb_train[,47],laplace = 1)
  nb_model_pre<-predict(nb_model,nb_test[,c(4:36,47)],type = "class")
  nb_acc<-confusionMatrix(nb_model_pre,nb_test[,47])$overall[1] 
  
  #JRip
  JRIP_model_train<-train
  JRIP_model_test<-test
  
  JRIP_model<-JRip(formula,JRIP_model_train)
  JRIP_model_pre<-predict(JRIP_model,JRIP_model_test)
  JRIP_acc<-confusionMatrix(JRIP_model_pre,JRIP_model_test[,47])$overall[1]
  
  #return the result
  result<-data.frame(c("KNN","C5.0","Naive Bayes","Ripper"),
                     c(knn_acc_max,C50_acc,nb_acc,JRIP_acc))
  colnames(result)<-c("Method","Result")
  result_plot <- ggplot(result,aes(Method,Result))+geom_bar(stat = "identity",fill="black")+
    geom_text(aes(label=round(Result,digits = 4)),vjust=1.5,size=5,colour="white")
  
  return(result_plot)
}

training4model(model)


#K均值聚类肘部法
result<-rep(0, 20)
for(k in 1:20){
  set.seed(100)
  model_cluster <- kmeans(model_kmeans_z, k)                 #不设置k，每次将结果覆盖
  result[k] <- model_cluster$betweenss / model_cluster$totss #设置k，记录每次组间方�?/总方�?
}
result20 <- data.frame(c(1:20), result)
colnames(result20)[1] <- "K"
ggplot(result20, aes(K, result)) + 
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks=seq(0, 20, 1)) + 
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  theme(panel.grid.minor = element_blank())

k <- c(1:117)
cp <- rep(NA, 117)
for (i in 1:117){
  cp[i] = kmeans(gold_scale, i)$betweenss / kmeans(gold_scale, i)$totss
}
cp <- data.frame(cbind(k, cp)) 



#字符清理函数
fun_clean <- function(x){
  x <- str_replace_all(x, "\\�?", "\\(")
  x <- str_replace_all(x, "\\�?", "\\)")
  x <- str_trim(x)
  return(x)
}

#处于末位比例
ep <- rep(NA, nrow(freq))
for (i in 1:nrow(freq)){
  pat1 <- as.vector(freq$Word)
  pat2 <- str_c(pat1,"$")
  ep[i]<-1-prop.table(table(str_detect(lp$修改楼盘,pat1[i]),str_detect(lp$修改楼盘,pat2[i])),1)[2,1]
}  


#方案1precision
r_pre1 <- rep(0, length(two_id))
m_pre1 <- rep(0, length(two_id))
for (i in 1:length(two_id)){
  set.seed(10)
  sample <- sample(two_id, i) #每次抽取不同样本�?
  r<-subset(r_all, residentialId %in% sample) 
  m<-subset(m_all, l_residentialId %in% sample)
  r_pre1[i] <- unlist(extCriteria(as.integer(r$group), as.integer(r$`old-group0.15`), crit = 'precision'))
  m_pre1[i] <- confusionMatrix(m$old_same, m$zhu_same, positive = '1', mode = 'prec_recall')$byClass[5]
  precision1 <- data.frame(c(1:length(two_id)), r_pre1, m_pre1)
  colnames(precision1)[1] <- 'sample_num'  
}
ggplot(precision1) + 
  geom_point(aes(sample_num, r_pre1, color='blue')) + geom_line(aes(sample_num, r_pre1))+
  geom_point(aes(sample_num, m_pre1, color='green')) + geom_line(aes(sample_num, m_pre1))

#F1*100取均�?
set.seed(100)
train_id100 <- createDataPartition(match_a$zhu_same, p = 0.75, times = 100) #随机生成100个训练集
F1 <- rep(NA, 100)
for (i in 1:100){
  train <- match_s[train_id100[[i]],]     #左边不设置i，右边设置，每次覆盖
  test <- match_s[-train_id100[[i]],]
  c50_model <- C5.0(factor(zhu_same) ~ d_floor + d_room + d_hall + tf + ba + tp, train)
  c50_pre <- predict(c50_model, test)
  F1[i] <- confusionMatrix(c50_pre, test$real_same, mode = "prec_recall", positive = "1")$byClass[7]
}
mean(F1) #100次均�?

#循环计算F1
for (i in 1:20){
  pr_b[2+i]<-((i^2+1)*pr_b$p*pr_b$r)/(i^2*pr_b$p+pr_b$r)
  colnames(pr_b)[2+i]<-paste('f1_',i,sep="")
} 

#中位值及均�?
mm <- function(x){
  mean <- mean(x)
  median <- median(x)
  return(c(mean, median))
}
aggregate(d_ba ~ same, pair_new, mm)
aggregate(d_tp ~ same, pair_new, mm)
aggregate(d_tf2 ~ same, pair_new, mm)


#计算各分块像素的函数
mat_fun <- function(x){
  x_long <- melt(x)
  x_wide <- reshape(x_long, timevar = 'Var3', idvar = c('Var1', 'Var2'), direction = 'wide')
  x_wide$mean.value <- apply(x_wide[, 3:5], 1, mean) #计算平均RGB
  xs <- x_wide[, c(1, 2, 6)]
  xs_wide <- reshape(xs, timevar = 'Var2', idvar = 'Var1', direction = 'wide') #转换为类似矩阵的形式
  xs_wide <- xs_wide[, -1]
  xs_mat <- as.matrix(xs_wide) #转换成矩阵进行分块计�?
  x100 <- array(NA, c(10, 10))
  for(i in 1:10){
    for(j in 1:10){
      x100[i, j] <- mean(xs_mat[c((1 + (60 * (i - 1))):(60 * i)), 
                                c((1 + (80 * (j - 1))):(80 * j))]) #只适用�?800*600像素
    }
  }
  x100 <- ifelse(x100 >= mean(x100), 1, 0)
  return(x100) #输出100个分块的均�?
}


#单个文件RGB�?
RGB_FUN <- function(x){
  R = mean(x[, , 1])
  G = mean(x[, , 2])
  B = mean(x[, , 3])
  return(c(R, G, B))
}
RGB_FUN(w1)
RGB_FUN(w2)


#批量输出RGB�?
RGB_FUN2 <- function(x){
  R <- rep(NA, length(x))
  G <- rep(NA, length(x))
  B <- rep(NA, length(x))
  for (i in 1:length(x)){
    d <- readJPEG(x[i])
    R[i] <- mean(d[, , 1])
    G[i] <- mean(d[, , 2])
    B[i] <- mean(d[, , 3])
    mat <- matrix(c(R, G, B), ncol = 3, dimnames = list(c(1:length(x)), c('R', 'G', 'B')))
  }
  return(mat)
} 


#编辑距离
l_health <- matrix(NA, nrow = nrow(poi_yx_health_clear), ncol = nrow(health_yx))
for (i in 1:nrow(poi_yx_health_clear)){
  for (j in 1:nrow(health_yx)){
    l_health[i, j] <- adist(poi_yx_health_clear$na_ad2[i], health_yx$na_ad2[j])
  }
}

#每组的内容列�?
list_group <- rep(NA, length(unique(nodes$group)))
for (i in 1:length(unique(nodes$group))){
  list_group[i] = list(nodes$name[nodes$group == i])
}

list_cluster <- rep(NA, length(unique(v_cluster$cluster)))
for (i in 1:length(unique(v_cluster$cluster))){
  list_cluster[i] = list(v_cluster$vector[v_cluster$cluster == i])
}

#文本清洗函数
corpus_clean_fun <- function(corpus){
  corpus_c <- tm_map(corpus, removeNumbers)                            #删除数字
  corpus_c <- tm_map(corpus_c, removePunctuation)                      #删除标点
  corpus_c <- tm_map(corpus_c, tolower)                                #变成小写
  corpus_c <- tm_map(corpus_c, removeWords, stopwords(kind = "en"))    #删除英文停词
  corpus_c <- tm_map(corpus_c, stripWhitespace)                        #删除空白
  return(corpus_c)
}
qua_corpus_c <- corpus_clean_fun(qua_corpus)
inspect(qua_corpus_c[[3]])

#每组的内容列�?
cluster <- rep(NA, 2)
for (i in 1:2){
  cluster[i] <- list(row.names(c2)[c2$cluster == i])
}

#二元组处理函�?
count_bigrams <- function(dataset){
  dataset %>% 
    unnest_tokens(input = text, output = bigram, token = "ngrams", n = 2) %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>% 
    count(word1, word2, sort = T)
}

#二元组可视化函数
visualize_bigrams <- function(bigrams){
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  bigrams %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow = a, end_cap = circle(.07, "inches")) + 
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

#��tidyverse��������Ҫ��enquo��!!
com_fun <- function(data, var){
  qvar <- enquo(var)
  data %>% tabyl(!!qvar, now_r) %>% adorn_percentages(denominator = "col") %>% select(-no) %>% rename(r = yes) -> group_r
  data %>% tabyl(!!qvar, now_python) %>% adorn_percentages(denominator = "col") %>% select(-no) %>% rename(python = yes) -> group_python
  group_r %>% left_join(group_python) -> result
  return(result)
}
com_fun2 <- function(data, var, r_var, python_var){
  var_en <- enquo(var)
  r_var_en <- enquo(r_var)
  python_var_en <- enquo(python_var)
  data %>% select(!!r_var_en, !!python_var_en, !!var_en) %>% 
    filter(!is.na(!!var_en)) %>% 
    gather(key = tool, value = yn, c(!!r_var_en, !!python_var_en)) %>% 
    filter(yn == "yes") %>% 
    mutate(tool = if_else(tool == "now_r" , "r", "python")) -> long_data
  return(long_data)
}
com_fun2(sf_rp, AssessJob4, now_r, now_python) -> result_4


#��ggplot�йص��Զ��庯��
com_fun_plot2 <- function(data, xvar = "group", yvar = "score"){
  ggplot(data, aes_string(x = xvar, y = yvar)) +
    geom_boxplot() -> result_plot
  return(result_plot)
}
com_fun_plot2(result_4, "tool", "AssessJob4") #Ҫ�����Ų���������