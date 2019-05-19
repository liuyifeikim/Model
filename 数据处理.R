#剔除无用变量
app <- app_full %>% select(-c(`Last Updated`, `Current Ver`, `Android Ver`, Installs, Price))
value_2013 %>% select(-(a2:a12_4b)) #按列名删除连续列
value_2010 %>% select(-c(1, 3:17)) #按列号删除连续列


app$KB <- NULL

ld_data_s_plus <- ld_data_plus[, -c(2:6, 42, 44, 46)] #删除多余变量


#变量改名
colnames(type2)[1] <- "Type"

app <- app %>% rename(Content = `Content Rating`)
names(app_genres) <- gsub("Genres_", "" , names(app_genres)) 

value_2019_zhong_hun %>% 
  select("编号", q1_1, q37_1:q37_20) %>% 
  mutate(type = "中学???") %>% 
  rename_at(vars(starts_with("q37")), 
            funs(str_replace(., "q37", "q38"))) #将q37改为q38


#将同一列中用分隔符隔开的内容处理成哑变???
app_genres <- app %>% select(Rating, Genres) %>% 
  cSplit_e("Genres", ";", mode = "binary", type = "character", fill = 0, drop = T) #splitstackshape???

model.matrix(~ quant_cat - 1, data = online_retail)

game_df_c %>% 
  select(id, label) %>% 
  mutate(label = str_split(label, ",")) %>% 
  unnest() %>% 
  count(id, label) %>% 
  spread(key = label, value = n, fill = 0, drop = TRUE)


#生成新变???
original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),   #行号
         chapter = cumsum(str_detect(text, regex("^CHAPTER \\d+$", ignore_case = T)))) %>%  #累计计数
  ungroup()

ts_s <- ts_s %>% mutate(mean_arpu = rowMeans(select(., starts_with("arpu")), na.rm = T),
                        mean_dou = rowMeans(select(., starts_with("dou")), na.rm = T),
                        mean_mou = rowMeans(select(., starts_with("mou")), na.rm = T),
                        mean_zt = rowMeans(select(., starts_with("zt")), na.rm = T)) #特定列均???

ts_s <- ts_s %>% mutate(sd_arpu = apply(select(., starts_with("arpu")), 1, sd, na.rm = T),
                        sd_dou = apply(select(., starts_with("dou")), 1, sd, na.rm = T),
                        sd_mou = apply(select(., starts_with("mou")), 1, sd, na.rm = T),
                        sd_zt = apply(select(., starts_with("zt")), 1, sd, na.rm = T)) #特定列标准差


#选择样本
app <- app %>% filter(Category != "1.9")
app <- app %>% filter(!(Content %in% c("Adults only 18+", "Unrated")))
bigram_filtered <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)
kjv_bigrams %>% 
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d"))

r_all <- subset(real, residentialId %in% two_id) 
poi_yx_edu <- subset(poi, adname == '越秀???' & yfLevel_1 == '教育') #poi越秀区教???


#选择变量
ts_s2 %>% select_if(is.numeric)


#排序
type2[order(type2$Freq, decreasing = T),]
app %>% group_by(App) %>% count() %>% arrange(desc(n))

stock_tf_idf <- stock_tokens %>% 
  count(company, word) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  bind_tf_idf(document = company, term = word, n = count) %>% 
  arrange(-tf_idf) #降序排列


#数值变量分???
app_c$Reviews_bin <- discretize(app_c$Reviews, method = "frequency", breaks = 10) #arule???

match_s$tp_group <- cut(match_s$tp, c(seq(0, 0.4, 0.01), Inf),  #分组
                        include.lowest = T,                     #有最低点
                        right = F)                              #有左端点,无右端点


#变量分组
match_s$fpl <- match_s$l_floor / match_s$l_totalFloor
match_s$fpl[which(match_s$fpl > 0.66)] <- "high"
match_s$fpl[which(match_s$fpl > 0.33 & match_s$fpl <= 0.66)] <- "middle"
match_s$fpl[which(match_s$fpl <= 0.33)] <- "low"


#标准???
knn_train[,c(4:36)]<-lapply(knn_train[,c(4:36)],scale)
knn_test[,c(4:36)]<-lapply(knn_test[,c(4:36)],scale)


#随机抽样
set.seed(300)
id40 <- sample(nrow(lpa_data_10w), 40)
lpa_na40 <- lpa_data_10w[id40, ]

set.seed(100)
id100 <- sample(50000, 100)
write.xlsx(na_origin[id100, ], "D:/K/云房/数据/数据自动化清???/分类测试100.xlsx") #随机抽取样本导出

set.seed(100)
random100 <- sample(unique(huhaomysql$hunitid), 100, replace = FALSE) #不放回抽???

A <- round(1 / 10 * sum(dan$lyfclass == "A")) #每一类抽十分之一
B <- round(1 / 10 * sum(dan$lyfclass == "B"))
D <- round(1 / 10 * sum(dan$lyfclass == "D"))
E <- round(1 / 10 * sum(dan$lyfclass == "E"))
F <- round(1 / 10 * sum(dan$lyfclass == "F"))
G <- round(1 / 10 * sum(dan$lyfclass == "G"))
set.seed(100)
sub1 <- strata(dan, stratanames = "lyfclass", 
               size = c(A, B, D, E, F, G), method = "srswor") #sanpling包，分层抽样

#训练集和测试集划???
set.seed(1)
ind <- createDataPartition(model$TYPE1, p = 0.75, list = F) #根据因变量比例划???
train <- model[ind, ]
test <- model[-ind, ]

#长宽数据转换
k10 <- cbind.data.frame(result1, result2, result3, result4, result5, result6, result7, result8, result9, result10)
k10_long <- melt(k10, id.vars = "k", variable.name = "result", value.name = "value") #宽数据→长数???

x_wide <- reshape(x_long, timevar = 'Var3', idvar = c('Var1', 'Var2'), direction = 'wide') #长数据转宽数???
spread(key = sentiment, value = n, fill = 0) #长转宽，将sentiment的取值变为变量，n变为变量取值，???0填充缺失

tidy_books %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0)  #长转???

#在左不在右的数???
setdiff(unique(result$residentialId),unique(real$residentialId))


#对象两两组合并计算相似度
ref <- c('cat', 'dog', 'turtle', 'cow', 'horse', 'pig', 'sheep', 'koala','bear','fish')
words <- c('dog', 'kiwi', 'emu', 'pig', 'sheep', 'cow','cat','horse')
wordlist <- expand.grid(words = words, ref = ref, stringsAsFactors = FALSE)
wordlist %>% 
  group_by(words) %>% 
  mutate(sim_score = stringdist(words, ref, method = "lv")) %>%  #计算相似度得分
  summarise(match_score = sim_score[which.min(sim_score)],       #计算最小相似度得分
            matched_to = ref[which.min(sim_score)])              #最小相似度得分对应的对象

#计算距离
hamming.distance(as.vector(a1mat),as.vector(a2mat))/100 #e1071包，hamming distance

#数据合并
oly_dict <- merge(oly, dict, by.x = 'Country', by.y = 'Code', all = TRUE) #全连???
tidy_books %>% anti_join(stop_words, by = "word") #剔除特定内容
tidy_books %>% 
  filter(book == "Emma") %>% 
  inner_join(nrcjoy)  #内连???
not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(afinn, by = c(word2 = "word")) #连接列不同命

#排列组合
by_comic <- edges_two %>% group_by(comic)
edges_pair <- do(by_comic, data.frame(t(combn(.$hero, 2))))

choose(length(nodes$name), 2) #可能出现的最大连接数
combn(nodes$name, 2) %>% t() %>% as.data.frame() #两两配对

#单列多列转化
bigrams_separated <- austen_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ") #按分隔符分成两列
chapters_gamma <- chapters_gamma %>% 
  separate(col = document, into = c("title", "chapter"), sep = "_", convert = T) #convert将字符转为数???

bigrams_united <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = "") #按分隔符将多列合为一列，separate的相???

#按行堆叠
tweets <- bind_rows(
  tweets_julia %>% mutate(person = "Julia"),
  tweets_dave %>% mutate(person = "David")
)

#清空工作环境
rm(list=ls())

#因子顺序处理
value_2017$age_group <- factor(value_2017$age_group, levels = c("80???", "90???", "95???", "00???"))

#调整字段位置
refcols <- c("year", "id")
value_2010 <- value_2010[, c(refcols, setdiff(names(value_2010), refcols))]

#计算同时有类别和数值变量数据框的距离
thyroid_dist <- daisy(thyroid[,-1], metric = "gower") #cluster包
game_dist <- game_df_model[, c("type", "lang", "size_mb", "time")]
game_dist$type <- as.factor(game_dist$type)
game_dist$lang <- as.factor(game_dist$lang)
game_dist_daisy <- daisy(game_dist, metric = "gower")
game_dist_daisy %>% head()
str(game_dist_daisy)