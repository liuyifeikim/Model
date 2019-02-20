set.seed(1) #每次结果都会不同
knn_model1<-knn(knn_train[,c(4:36)],knn_test[,c(4:36)],knn_train[,47],3) #knn model class output
confusionMatrix(knn_model1,knn_test[,47])

knn_acc<-rep(0,20)
for(i in 1:20){
  set.seed(1)
  knn_model <- knn(knn_train[, c(4:36)], knn_test[, c(4:36)], knn_train[, 47], i)
  knn_acc[i] <- confusionMatrix(knn_model, knn_test[, 47])$overall[1]
}
knn_acc
knn_acc20 <- as.data.frame(cbind(c(1:20), knn_acc))
colnames(knn_acc20) <- c("K", "Accuracy")
knn_acc20
ggplot(knn_acc20, aes(K, Accuracy)) + geom_point(size = 3) + geom_line(size=1) #不同K值下的模型准确度

#caret包
grid <- expand.grid(.k = c(1:20)) #预设K值
ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "best") #10折检验，选择系数指标最好的模型
set.seed(3)
knn_model_caret3<-train(formula,
                        knn_train,
                        method="knn",
                        trControl = ctrl,
                        tuneGrid = grid)


#knn*100
set.seed(100)
part100 <- createDataPartition(pair_new_k$same, p = 0.8, times = 100) #随机生成100个训练集
ctrl <- trainControl(method = "LGOCV", p = 0.8, index = part100) #保持法
grid_k <- expand.grid(.k = 3)
knn_model_100 <- train(pair_new_k[, -1], pair_new_k$same,
                       method="knn",
                       trControl = ctrl,
                       tuneGrid = grid_k) 
