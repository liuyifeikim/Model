#解决决策树跑不出来的问题
Sys.setlocale(category = "LC_ALL",locale="")
Sys.setlocale(locale="C") 


#CART算法
rpart_model <- rpart(Rating_c ~ Category + Type + Content + Reviews_c + Size_c,
                     data = app_c,
                     method = "anova")

ctrl <- trainControl(method = "cv", number = 10)
set.seed(100)
rpart_model_best <- train(Rating ~ Category + Type + Content + Reviews_bin + Size_bin, 
                          data = app_c,
                          method = "rpart",
                          trControl = ctrl)
rpart_model_best


#C5.0
library(C50)
library(caret)

tree_model_c50 <- C5.0(tree_model_train[, c(4:36)], tree_model_train$TYPE1) 
tree_model_c50_pre <- predict(tree_model_c50, tree_model_test) #分类预测
tree_model_c50_pre_prob <- predict(tree_model_c50, tree_model_test, type = "prob") #概率预测
C5imp(c50_model1) #作图

#混淆矩阵
confusionMatrix(tree_model_c50_pre, tree_model_test$TYPE1, mode = "prec_recall") #Confusionmatrix

#caret调参
ctrl <- trainControl(method = "LGOCV", p = 0.75) #保持法
grid <- expand.grid(.model = "tree", .trials = c(1, 10, 15, 20, 25, 30, 35, 40, 45, 50), .winnow = "FALSE")
ld_c50_model_50 <- train(ld_data_s_plus[, -c(1:3)], ld_data_s_plus$ld_type,
                         method = "C5.0",
                         trControl = ctrl,
                         tuneGrid = grid) 

ctrl<-trainControl(method="LGOCV", p = 0.75) #保持法
grid <- expand.grid(.model = "tree", .trials = c(1, 10, 15, 20, 25, 30, 35, 40, 45, 50), .winnow = "FALSE")
part <- createDataPartition(lp_data_s_plus$lp_type, p = 0.75, list = F, times = 10) #按比例抽取10次
lp_c50_model_50 <- train(lp_data_s_plus[, -c(1:3)], lp_data_s_plus$lp_type, 
                         method = "C5.0",
                         trControl = ctrl,
                         tuneGrid = grid,
                         index = part) #实现10次保持法

set.seed(100)
part100 <- createDataPartition(match_a$zhu_same, p = 0.75, times = 100) #100次抽样
ctrl <- trainControl(method = "LGOCV", index = part100) #将上述参数传入trainControl
grid <- expand.grid(.model="tree", .trials = 1, .winnow = "FALSE")
c50_model_100_a <- train(factor(zhu_same) ~ d_floor + d_room + d_hall + tf + ba + tp, data = match_a,
                         method="C5.0",
                         trControl = ctrl,
                         tuneGrid = grid) #保持法

#C50 ROC
c50_pre2 <- predict(c50_model1, test1, type = "prob")
c50_roc <- roc(test1$same, c50_pre2[, 2])
plot(c50_roc, print.auc=T, auc.polygon = T, max.auc.polygon = T)

#C4.5
library(rpart)
library(rpart.plot)
library(caret)
library(RWeka)
library(RWekajars)
library(rJava)

tree_model_c45<-J48(formula,tree_model_c45_train) #默认设置结果，每次都要跑一次
print(tree_model_c45)
summary(tree_model_c45)
write_to_dot(tree_model_c45)
plot(tree_model_c45)

tree_model_c45_ut <- J48(formula, tree_model_c45_train, control = Weka_control(U = TRUE)) #默认为FALSE
tree_model_c45_rt <- J48(formula, tree_model_c45_train, control = Weka_control(R = TRUE)) #默认为FALSE


#代价矩阵
error_cost_ld<-matrix(c(0,1,2,1,1,2,1,1,2,2,2,1,2,
                        2,0,2,1,2,2,1,2,2,2,2,1,2,
                        1,1,0,1,1,2,1,1,1,1,1,1,2,
                        2,1,2,0,2,2,1,2,2,2,2,2,2,
                        2,1,2,1,0,2,1,2,2,2,2,1,2,
                        1,1,1,1,1,0,1,1,1,1,1,1,2,
                        2,2,2,2,2,2,0,2,2,2,2,2,2,
                        2,1,2,1,1,2,1,0,2,2,2,1,2,
                        1,1,2,1,1,2,1,1,0,1,1,1,2,
                        1,1,2,1,1,2,1,1,2,0,2,1,2,
                        1,1,2,1,1,2,1,1,2,1,0,1,2,
                        2,2,2,2,2,2,1,2,2,2,2,0,2,
                        1,1,1,1,1,1,1,1,1,1,1,1,1),
                      nrow = 13) 
colnames(error_cost_ld) <- levels(ld_train_plus$ld_type)
rownames(error_cost_ld) <- levels(ld_train_plus$ld_type)
ld_c50_model_plus_cost <- C5.0(ld_train_plus[, -c(1:3)], ld_train_plus$ld_type, costs=error_cost_ld) 
ld_c50_pre_plus_cost <- predict(ld_c50_model_plus_cost, ld_test_plus)  #预测结果
confusionMatrix(ld_c50_pre_plus_cost, ld_test_plus$ld_type, mode = "prec_recall") 

error_cost_ld<-matrix(c(0,1,9,1,1,9,1,1,9,9,9,1,9,
                        9,0,9,1,9,9,1,9,9,9,9,1,9,
                        1,1,0,1,1,9,1,1,1,1,1,1,9,
                        9,1,9,0,9,9,1,9,9,9,9,9,9,
                        9,1,9,1,0,9,1,9,9,9,9,1,9,
                        1,1,1,1,1,0,1,1,1,1,1,1,9,
                        9,9,9,9,9,9,0,9,9,9,9,9,9,
                        9,1,9,1,1,9,1,0,9,9,9,1,9,
                        1,1,9,1,1,9,1,1,0,1,1,1,9,
                        1,1,9,1,1,9,1,1,9,0,9,1,9,
                        1,1,9,1,1,9,1,1,9,1,0,1,9,
                        9,9,9,9,9,9,1,9,9,9,9,0,9,
                        1,1,1,1,1,1,1,1,1,1,1,1,1),
                      nrow=13, dimnames = mat_dim_ld)
colnames(error_cost_ld) <- levels(ld_train_plus$ld_type)
rownames(error_cost_ld) <- levels(ld_train_plus$ld_type)
names(mat_dim_ld) <- c("predict", "actual")

ld_c50_model_boost_plus_cost <- C5.0(ld_train_plus[,-c(1:3)], ld_train_plus$ld_type, 
                                     trials = 10, #boosting算法，迭代10次
                                     costs=error_cost_ld) 
ld_c50_pre_boost_plus_cost <- predict(ld_c50_model_boost_plus_cost, ld_test_plus) 
confusionMatrix(ld_c50_pre_boost_plus_cost, ld_test_plus$ld_type) 

#随机森林
#randomforest*100
set.seed(100)
part100 <- createDataPartition(pair_new1$same, p = 0.8, times = 100) #随机生成100个训练集
ctrl <- trainControl(method = "LGOCV",p = 0.8,index = part100) #在part100里面已经设置了p，可能可以不需要再设p
grid_rf <- expand.grid(.mtry = 3)
rf_model_100 <- train(pair_new1[,-1], pair_new1$same,
                    method = "rf",
                    ntree = 500,
                    trControl = ctrl,
                    tuneGrid = grid_rf) #保持法