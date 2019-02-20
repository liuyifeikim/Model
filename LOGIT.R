ts_logit_best <- train(ts_yn ~ ., 
                       data = ts_train_ba, 
                       method = "glm", 
                       family = "binomial",
                       trControl = trainControl(method = "cv", number = 10)
)

ts_logit1 <- glm(ts_yn ~ ., data = ts_train, family = "binomial")
