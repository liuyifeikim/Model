#多元线性回归
lm_model <- lm(Rating ~ Category + Type + Content + Reviews_bin + Size_bin, data = app_c)
summary(lm_model)
plot(lm_model)
lm_model_pred <- predict(lm_model, app_c[, -1])
RMSE(lm_model_pred, app_c$Rating)  #caret包,RMSE
cor(lm_model_pred, app_c$Rating) ^ 2 #=R2
lm.beta(reg1) #输出标准化回归系数
rmse1 <- sqrt(mean((reg1$fitted.values - sh_origin_clean_new$bu_num)^2)) #RMSE

#logit回归
logit_model <- glm(real_same ~ ., family = binomial(link = 'logit'), data = match_log)
summary(logit_model)
