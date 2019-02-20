set.seed(100)
ts_rf_model <- randomForest(ts_yn ~ ., 
                            data = ts_train, 
                            mtry = 3, 
                            ntree = 500, 
                            importance = TRUE)

ts_rf_model$importance
importance(ts_rf_model, scale = F)
hist(treesize(ts_rf_model))
varImpPlot(ts_rf_model) #变量重要性图

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
grid <- expand.grid(.mtry = c(2, 4, 8))
set.seed(100)
ts_rf_model_best <- train(ts_yn ~ ., 
                          data = ts_train_f,
                          method = "rf",
                          ntree = 500,
                          trControl = ctrl,
                          grid = grid,
                          metric = "ROC"
)
