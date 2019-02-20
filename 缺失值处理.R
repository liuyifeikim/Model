#缺失值处理
md.pattern(app, plot = F) #mice包
app_c <- app[complete.cases(app),] #保留完整样本
app <- app %>% group_by(Category, Type, Content) %>% 
  mutate(Rating = ifelse(is.na(Rating), mean(Rating, na.rm = T), Rating)) #分组均值插补