#直方图+密度曲线
ggplot(app, aes(Rating, ..density..)) + 
  geom_histogram(bins = 30) + 
  geom_density()

ggplot(match_s, aes(tp)) + 
  geom_histogram() +
  scale_x_continuous(breaks = seq(0, 1.5, 0.05)) +
  theme(panel.grid.minor = element_blank()) + 
  facet_grid(real_same ~ .) #按特定变量分面，或.~real_same

ggplot(hn2, aes(housenumber, ..density.., fill = type)) +
  geom_histogram(binwidth = 1, alpha = 0.3) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 2)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_fill_manual(values = c("blue", "red"))#两者分布相似

ggplot(f, aes(length)) + geom_density()
ggplot(f, aes(length)) + geom_histogram()
ggplot(f, aes(length, ..density..)) + geom_histogram() 
ggplot(f, aes(length)) + geom_histogram(stat = "density")
ggplot(f, aes(length)) + geom_histogram() + geom_density()
ggplot(f, aes(length, ..density..)) + geom_histogram() + geom_density()


hist(index5$misshouseproportion_t_abs1, probability = T)
lines(density(index5$misshouseproportion_t_abs1))


###################################################################

#密度图
d_old <- ggplot(old, aes(Freq)) + 
  geom_density() + 
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2))
d_dt <- ggplot(dt,aes(Freq)) + 
  geom_density() + 
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2))
d_zhu <- ggplot(zhu, aes(Freq)) + 
  geom_density() + 
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2))
grid.arrange(d_old, d_dt, d_zhu)

ggplot(sz86_1A1_clean_short) + 
  geom_density(aes(mzm), fill = "blue", alpha = 0.5) +
  geom_density(aes(mhn), fill = "green", alpha = 0.5) +
  scale_x_continuous(name = "", breaks = seq(0, 20, 2))+
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) #叠加密度图

###################################################################

#直方图
ggplot(hn2, aes(housenumber, fill = type)) +
  geom_histogram(binwidth = 1, alpha = 0.4) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 2)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_fill_manual(values = c("blue", "green"))#两者分布相似

ggplot(book_words, aes(n / total, fill = book)) +
  geom_histogram(show.legend = F, bins = 30) +
  xlim(NA, 0.0009) +
  facet_wrap(~ book, ncol = 2, scales = "free_y")

ggplot(tweets, aes(timestamp, fill = person)) +
  geom_histogram(bins = 20, show.legend = F) +
  facet_wrap(~ person, ncol = 1)  #按person分开两个直方图

##################################################################


#柱状图
app_c %>% group_by(Category) %>% summarise(mean_rating = mean(Rating, na.rm = T)) %>% arrange(desc(mean_rating)) %>% 
  ggplot(aes(reorder(Category, mean_rating), mean_rating)) + 
  geom_bar(stat = "identity") + #利用汇总数据的两列数据柱状图
  xlab("Category") + 
  coord_flip() #纵向排列

ggplot(result, aes(Method, Result)) + 
  geom_bar(stat = "identity",fill="black") + #柱状图
  geom_text(aes(label = round(Result, digits = 4)), vjust=1.5, size=5, colour="white") #在柱上增加数据标签

ggplot(model, aes(TYPE1, maxfloor)) + geom_bar(stat = "summary", fun.y="mean") #利用原始数据进行汇总后作柱状图
ggplot(model, aes(TYPE1, maxfloor)) + geom_bar(stat = "summary", fun.y="max")

ggplot(lp_type, aes(reorder(Var1, Freq), Freq)) + #按频数排序
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), size = 5, hjust = -0.1) +                                  #在柱上增加数据标签
  scale_y_continuous(name = "proportion", limits = c(0, 100), breaks = seq(0, 100, 10)) + #设置y轴坐标
  theme(axis.title = element_blank(),             #无轴标题
        panel.grid.minor = element_blank(),       #无网格           
        axis.text.y = element_text(size = 15),    #y轴坐标字体大小
        axis.text.x = element_text(size = 15)) +  #x轴坐标字体大小
  coord_flip()

ggplot(subset(freq[1:30, ]))+   #数据子集
  geom_bar(aes(reorder(Word, -end_prop), end_prop), stat="identity") +     #汇总数据柱状图，按频率排序
  geom_hline(yintercept = 0.5, colour = "red", size = 2, alpha = 0.5) +    #水平线
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +                            #坐标轴设置
  theme(axis.title = element_blank(), panel.grid.minor = element_blank(),  #无标题和网格
        axis.text.x = element_text(size = 20, face="bold"),                #x轴字体
        axis.text.y = element_text(size = 15))                             #y轴字体

ggplot(subset(freq[1:30,]))+
  geom_bar(aes(reorder(Word, -word_prop), word_prop), stat = "identity", width = 0.45, fill = "red", alpha = 0.4) + #柱状设置
  geom_bar(aes(Word, end_prop), stat = "identity", fill = "blue", width = 0.9, alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 15)) #高频词作为结尾占出现该词样本比例+高频词比例

ggplot(pure_sz, aes(reorder(type, -proportion), proportion)) +
  geom_bar(stat = "identity", width=0.5, fill="darkblue") +
  geom_text(aes(label = proportion), vjust = 1.5, size = 6, colour = "white") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  annotate("text", x = 2, y = 1, label = "深圳", colour = "black", size = 8)  #文字注释

ggplot(subset(buchong_after_small_dd_long2, flag == "origin"), aes(index, fill = num_type)) +
  geom_bar() +
  labs(x = "ORIGIN", y = "COUNT", fill = "取值类型") +
  scale_fill_brewer(type = "seq", palette="GnBu", direction = 1) + #调色板
  facet_wrap( ~unitclass, scales = "free_x") +
  coord_flip()

tidy_books %>% 
  count(word, sort = T) %>% 
  filter(n > 600) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() +   #geom_col = geom_bar(stat = "identity")
  xlab(NULL) + 
  coord_flip() 

ggplot(js, aes(index, sentiment, fill = book)) + 
  geom_col(show.legend = F) + 
  facet_wrap(~ book, ncol = 2, scales = "free_x") #按book分面，两列

ggplot(aes(reorder(word2, contribution), contribution, fill = contribution > 0)) +
  geom_col(show.legend = F) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip() #正负分数柱状图

gy_df_c %>%
  filter(!is.na(target)) %>% 
  mutate(target_fz = case_when(target <= 10000 ~ "1万及以下",
                               target > 10000 & target <= 100000 ~ "1-10万",
                               target > 100000 & target <= 1000000 ~ "10-100万",
                               target > 1000000 & target <= 10000000 ~ "100-1000万",
                               target > 10000000 ~ "1000万以上")) %>%
  mutate(target_fz = factor(target_fz, levels = c("1万及以下", "1-10万", "10-100万", "100-1000万", "1000万以上"))) %>%
  ggplot(aes(target_fz, ..prop.., group = 1)) +  #y轴为比例
  geom_bar()
##############################################################

#散点图
ggplot(knn_acc20, aes(K, Accuracy)) + 
  geom_point(size = 3) + 
  geom_line(size = 1) #不同K值下的模型准确度

ggplot(result20, aes(K, result)) + 
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks=seq(0, 20, 1)) + #坐标轴自定义
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  theme(panel.grid.minor = element_blank())

ggplot(freq_kmeans, aes(word_prop, luan_prop, colour = cluster)) +
  geom_point(size = 0) +                                               #将点的大小变为最小    
  geom_text(aes(label = Word), size = 6, fontface="bold") +            #每个点变为文字标签
  scale_colour_discrete(guide = F)                                     #取消图例

ggplot(precision1) + 
  geom_point(aes(sample_num, r_pre1, color='blue')) + geom_line(aes(sample_num, r_pre1)) +  #同一个数据框中同一个x不同y叠加
  geom_point(aes(sample_num, m_pre1, color='green')) + geom_line(aes(sample_num, m_pre1))

ggplot(tp_group, aes(tp_group, sum_1)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = '总价差范围分组') +  #x为定类变量的坐标名称设置
  scale_y_continuous(name = '频数') +
  theme(axis.text.x = element_text(angle=90, vjust=-0.5))  #x轴坐标文字角度和位置

year_term_counts %>% 
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>% 
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") + #y轴在不同分图中范围不同
  scale_y_continuous(labels = scales::percent_format()) + #y轴显示百分比
  ylab("% frequency of word in inaugural address")

ggplot(frequency, aes(Julia, David)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = T, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) + #scales包
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

plot(tree_model_c45_c_acc20, type="o", pch=16, lwd=2, xaxt="n", 
     xlab = "Confidence threshold for pruning", ylab="Accuracy") #设置刻度值
axis(1, at = seq(0.05, 0.95, 0.05)) #c=0.1时最好

plot(result, xaxt="n", type="o",
     xlab = "Number of K", ylab = "Between/totalness")
axis(1, at = seq(0, 50, 2)) 

##########################################################

#折线图

freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = F) +
  scale_x_log10() +
  scale_y_log10()

freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = book)) +
  geom_abline(intercept = -0.6226, slope = -1.1125, color = "gray50", linetype = 2) + #拟合直线
  geom_line(size = 1.1, alpha = 0.8, show.legend = F) +
  scale_x_log10() +
  scale_y_log10()

##########################################################

#盒状图
ggplot(sh_origin_clean_new, aes(cluster, bu_num)) + geom_boxplot()

ggplot(subset(hn2, hn2$housenumber <= 26), aes(type, housenumber, fill = type)) +
  geom_boxplot() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_fill_manual(values = c("blue", "green")) +
  coord_flip()


##########################################################


#相关图
app_c %>% ungroup() %>% select(Rating_c, Reviews_c, Size_c) %>% cor() %>% 
  corrplot(method = "color",         #图类型,有多种类型
           type = "lower",           #保留矩阵下半部分
           tl.col = 'black',         #各变量名称颜色
           tl.cex = 1,               #各变量名称字体大小
           order = "hclust",         #聚合方式
           addCoef.col = "black",    #方格内数字颜色
           number.cex = 1,           #方格内数字大小
           p.mat = p_mat, 
           sig.level = 0.05)           


#correlation
pairs(center_s)
scatterplotMatrix(center_s)   #car包
scatter3d(center_s$degree, center_s$betweenness, center_s$closeness)
corrgram(center_s, order = TRUE, upper.panel = NULL)  #corrgram包


#图片组合
grid.arrange(p1, p2, nrow=2) #gridExtra包

#词云
wordcloud(word = word, freq = n, max.words = 100)

#网络图
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#双类别变量分布点图
ggplot(list, aes(type,organ_type)) + geom_count()