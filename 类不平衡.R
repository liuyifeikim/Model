set.seed(10)
ts_train_ba <- SMOTE(ts_yn ~ ., 
                     as.data.frame(ts_train), 
                     perc.over = 1600, 
                     perc.under = 17 / 16 * 100, 
                     k = 5) #不能用tibble，只改变训练数据集比例
