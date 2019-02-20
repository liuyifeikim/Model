#ACCURACY AND F1
y_pred <- predict(ts_logit_best, newdata = ts_test)
confusionMatrix(y_pred, ts_test$ts_yn, positive = "1", mode = "prec_recall") #Accuracy = 0.8252 

#ROC and AUC
prob <- predict(ts_logit_best, newdata = ts_test, type = "prob")[,2] #取阳性样本的预测概率
pred <- prediction(prob, ts_test$ts_yn) #格式转换
perf <- performance(pred, measure = "tpr", x.measure = "fpr") #ROCR包
plot(perf)
abline(a = 0, b = 1)
auc <- performance(pred, "auc")
auc@y.values[[1]] #AUC = 0.6411953

#LIFT
plotLift(prob, ts_test$ts_yn, cumulative = T, n.buckets = 10, xaxt = "n")  #lift包
axis(1, at = 1:10, las = 1)
