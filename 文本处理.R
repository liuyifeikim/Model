#字符长度
a$length<-str_length(a$content)
a$nchar<-nchar(a$content)

#字符替换
x <- str_replace_all(x, "\\（", "\\(")
class7$楼盘clean<-str_replace_all(class7$楼盘clean,"\\d{1,2}$","") #移除末位数字
class7$楼盘clean<-str_trim(class7$楼盘clean) #移除空白

#合并字符
str_c(pat1,"$")

#变量转化，是否包含特定字符
ld_index_10w$dan <- str_detect(ld_index_10w$lou_dong_hao, "[楼|幢|栋|#].+[单元|门|座|梯]$")
app$Size <- if_else(str_detect(app$Size, "^[Varies]"), "", app$Size) 
app$KB <- if_else(str_detect(app$Size,"[k]$"), 1, 0)

#去除字符
app$Size <- str_replace(app$Size, "[M|k]", "")

#汉字转拼音
ld_data_s_plus$ld_type <- toPinyin(ld_data_s_plus$ld_type) #转换为拼音，否则决策树有问题

#提取内容
zg$报价数量<-str_extract_all(zg$报价,"(?<=有).(?=个)")

#分割字符
fs$split <- str_split(fs$name, "") #分割字符串

#字符串数目
f$num_num <- str_count(f$name, "\\d") #数字数目


###############################################################################

#文本分析

qua_corpus <- Corpus(VectorSource(google$`Preferred Qualifications`)) #处理为语料库

corpus_clean_fun <- function(corpus){
  corpus_c <- tm_map(corpus, removeNumbers)                            #删除数字
  corpus_c <- tm_map(corpus_c, removePunctuation)                      #删除标点
  corpus_c <- tm_map(corpus_c, tolower)                                #变成小写
  corpus_c <- tm_map(corpus_c, removeWords, stopwords(kind = "en"))    #删除英文停词
  corpus_c <- tm_map(corpus_c, stripWhitespace)                        #删除空白
  return(corpus_c)
}
qua_corpus_c <- corpus_clean_fun(qua_corpus)
inspect(qua_corpus_c[[3]])

qua_dtm <- DocumentTermMatrix(qua_corpus_c)    #处理为文档-词矩阵
qua_dtm_matrix <- as.matrix(qua_dtm)           #转为矩阵方便查看
qua_dtm_matrix[1:10, 1:5]                       
findFreqTerms(qua_dtm, lowfreq = 10) #查看词频

term_freq <- colSums(qua_dtm_matrix)             #计算每个词出现的次数
term_freq %>% sort(decreasing = T) %>% head(50)  #出现次数前50的词

qua_dtm_09 <- removeSparseTerms(qua_dtm, 0.9)    #去除出现在少于90%文档的词

wordcloud(names(freq_09), freq_09) #词云

####################################################################################

#中文分析
content_seg <- segmentCN(content, returnType = 'tm') #Rwordseg包的segmentCN,中文分词
docs <- Corpus(VectorSource(content_seg)) #转化为语料库
inspect(docs[[1]])

remove_en <- function(x) gsub('[a-zA-Z]', '', x)           #删除英文字母函数
docs_c <- tm_map(docs, content_transformer(remove_en))     #删除英文字母，可以自定义函数
docs_c <- tm_map(docs_c, removeNumbers)                    #去除数字，可以在DTM部分转
docs_c <- tm_map(docs_c, removePunctuation)                #去除英文标点，可以在DTM部分转
docs_c <- tm_map(docs_c, removeWords, stopwordsCN())       #去除中文停词
docs_c <- tm_map(docs_c, stripWhitespace)                  #去掉空格

dtm <- createDTM(docs_c, 
                 language = 'zh', 
                 removeStopwords = T, 
                 removePunctuation = T, 
                 removeNumbers = T) #用DocumentTermMatrix()会乱码,但可以通过controllist控制词的长度
dtm_mat <- as.matrix(dtm)
dtm_mat[1:5, 1:10]  #转化为矩阵方便描述

word_len <- nchar(dtm$dimnames$Terms) == 1 #词汇长度=1的词，作为停词
stop_word_1 <- dtm$dimnames$Terms[word_len]
docs_c_2 <- tm_map(docs_c, removeWords, stop_word_1) #删除长度=1的词
dtm_2 <- createDTM(docs_c_2, language = 'zh', 
                   removeStopwords = T, removePunctuation = T, removeNumbers = T) #重新制作矩阵


findFreqTerms(dtm_2, lowfreq = 500)  #至少出现500次的词
findMostFreqTerms(dtm_2, 5)[1:5]     #前5个文档中出现最多的5个词

assoc <- findAssocs(dtm_2, c('习近平','环境'), corlimit = 0.6) #相关性分析
assoc$`习近平`
assoc$`环境`

dtm05 <- removeSparseTerms(dtm_2, 0.5)
dtm05_mat_cs <- colSums(as.matrix(dtm05))
wordcloud(names(dtm05_mat_cs), dtm05_mat_cs, min.freq = 200, colors = brewer.pal(3, "Set1")) #词云


#处理成一个单词一行
text_df %>% unnest_tokens(input = text, output = word) 
