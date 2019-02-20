library(e1071)

model_nb<-naiveBayes(train_nb[,4:36],train_nb[,47], laplace = 1) #拉普拉斯调整
pre_nb<-predict(model_nb,test_nb[,c(4:36,47)],type = "class")    #预测分类
pre_nb_raw<-predict(model_nb,test_nb[,c(4:36,47)],type = "raw") #预测概率


#NB*100
set.seed(100)
part100 <- createDataPartition(pair_new1$same, p = 0.8, times = 100) #随机生成100个训练集
ctrl <- trainControl(method = "LGOCV", p = 0.8, index = part100)
grid_nb <- expand.grid(.fL=1, .usekernel="TRUE", adjust=)
nb_model_100 <- train(pair_new1[, -1], pair_new1$same,
                      method="nb",
                      trControl = ctrl) #保持法

#F1*100
set.seed(100)
train_id100 <- createDataPartition(pair_new1$same, p = 0.8, times = 100) #随机生成100个训练集
F1<-rep(NA, 100)
for (i in 1:100){
  train <- pair_new1[train_id100[[i]],]
  test <- pair_new1[-train_id100[[i]],]
  nb_model <- NaiveBayes(train[,-1], train$same)
  nb_pre <- predict(nb_model, test)
  F1[i] <- confusionMatrix(nb_pre$class, test$same, mode = "prec_recall", positive = "1")$byClass[7]
}
mean(F1) #100次均值


#################################################################################

#文本分析

#生成训练集和测试集
google_df <- as.data.frame(google) #变成数据框
set.seed(100)
train_id <- createDataPartition(google_df$tech_yn, p = 0.75)

#原始数据
google_df_train <- google_df[train_id$Resample1,]
google_df_test <- google_df[-train_id$Resample1,]

#dtm
qua_dtm_train <- qua_dtm[train_id$Resample1,]
qua_dtm_test <- qua_dtm[-train_id$Resample1,]

#语料库
qua_corpus_c_train <- qua_corpus_c[train_id$Resample1]
qua_corpus_c_test <- qua_corpus_c[-train_id$Resample1]

dict_10 <- findFreqTerms(qua_dtm, lowfreq = 10) #选择出现较多的词，至少10次，创建字典，后面创建训练测试集去使用
convert_counts <- function(x){   #将数值转化为因子
  x <- ifelse(x > 0 , 1, 0) %>% factor(levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}
train <- DocumentTermMatrix(qua_corpus_c_train, control = list(dictionary = dict_10)) %>% 
  apply(MARGIN = 2, convert_counts) #变为matrix格式而非dtm格式
test <- DocumentTermMatrix(qua_corpus_c_test, control = list(dictionary = dict_10)) %>% 
  apply(MARGIN = 2, convert_counts)

#e1071包
nb_model <- naiveBayes(train, google_df_train$tech_yn, laplace = 1) #目标变量一定要处理为因子
y_pred <- predict(nb_model, test)
y_pred_p <- predict(nb_model, test, type = "raw")
confusionMatrix(y_pred, google_df_test$tech_yn, mode = "prec_recall", positive = "Yes")

#naivebayes包
nb_model_2 <- naive_bayes(train, google_df_train$tech_yn, laplace = 1)
y_pred_2 <- predict(nb_model_2, test)
confusionMatrix(y_pred_2, google_df_test$tech_yn, mode = "prec_recall", positive = "Yes")

#caret包调参
control <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
grid <- expand.grid(fL = 1, usekernel = FALSE, adjust = FALSE)
set.seed(100)
train <- train(train, google_df_train$tech_yn, 
               method = "nb", 
               trControl = control, 
               tuneGrid = grid)
