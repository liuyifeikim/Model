library(RWeka)

#单规则
model_1R<-OneR(formula,model_1R_train)
summary(model_1R)
model_1R_pre<-predict(model_1R,model_1R_test)
confusionMatrix(model_1R_pre,model_1R_test$TYPE1)

#多规则
model_JRip<-JRip(formula,model_1R_train)
summary(model_JRip)
model_JRip_pre<-predict(model_JRip,model_1R_test)
confusionMatrix(model_JRip_pre,model_1R_test$TYPE1)
WOW(JRip) #相关参数
