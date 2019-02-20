#计算频数并排序
tidy_books %>% count(word, sort = T) 

#保留小数位
df$X2<-round(as.numeric(df$X2),digits = 2)

#头几条
comic_size50 <- head(comic_size, n = 50L)

#单变量不同取值计数
length(unique(group$`old-group`)) #15253

#交互描述
table(dan$unitcalssify,dan$lyfclass,useNA = "ifany")
prop.table(table(building$ld_yn))
addmargins(table(yun,real))

#分组描述
aggregate(specialhouse ~ lyfclass, dan, mean)

ddply(real, .(residentialId), summarise, len = length(url)) #分组频次统计，plyr包
dply(group, .(residentialId), summarise,
     len = length(unique(url)),
     old = length(unique(`old-group`)),
     dt = length(unique(`dt-group`))) #各小区的分组数量

describeBy(match_s$d_tf, match_s$real_same, mat = T, digits = 2) #psych


#变量摘要
app %>% select(Rating, Reviews, Size) %>% summary()
nrow(new_all_c) #行数
ncol(new_all_c) #列数
dim(new_all_c)  #行列数

#分位值统计
quantile(match_s$tp, p = seq(0.90, 0.99, 0.01))


#相关分析
cor(sh_origin_clean_new[,c("chr_valid_prop","chr_valid_prop2",
                           "copy_chr_prop","copy_valid_chr_prop","copy_valid_prop2",
                           "num_valid_prop","num_valid_prop2",
                           "valid_prop","length","bu_num")])

#方差分析
summary(with(real_new, aov(totalPrice ~ m1)))
TukeyHSD(with(real_new, aov(totalPrice ~ m1)))
oneway.test(ot08 ~ ts_yn, ts_s2, var.equal = F)


#WOE和IV
WOETable(ts_s2$ts_half, ts_s2$ts_yn) #X要是factor，InformationValue包
IV(ts_s2$ts_half, ts_s2$ts_yn)  #预测能力较好
