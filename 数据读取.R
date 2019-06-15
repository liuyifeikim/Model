#鍥哄畾璺緞璇绘暟鎹?
path <- "D:/K/DATA EXERCISE/Kaggle/" 
app_full <- read_csv(paste0(path, "google-play-store-apps/googleplaystore.csv"))
glimpse(app_c)

game <- read.csv('game.csv', stringsAsFactors = F)

#璇籈XCEL
model <- read.xlsx("D:/K/浜戞埧/鏁版嵁/鍗曞厓绫诲瀷鍒嗙被寤烘ā/sample+model(7.3).xlsx",
                 sheet = "model")  #openxlsx鍖?

#璺緞
nodes <- read.csv('../../../Kaggle/Marvel networks/nodes.csv') #鐩稿璺緞
nodes <- read.csv('./stack-overflow-tag-network/stack_network_nodes.csv', stringsAsFactors = FALSE)  #缁濆璺緞

#data.table璇诲ぇ鏁版嵁
google <- fread("job_skills.csv", encoding = "UTF-8")


#瀵煎嚭鏁版嵁
write.xlsx(lpa_na40,"D:/K/浜戞埧/鏁版嵁/鏁版嵁鑷姩鍖栨竻鐞?/lpa_na40.xlsx")
write.xlsx(ld_index_10w[which(ld_index_10w$dan == TRUE), ], "D:/K/浜戞埧/鏁版嵁/鏁版嵁鑷姩鍖栨竻鐞?/ld(鍗曞厓淇℃伅).xlsx")


#璇诲浘鐗?
a1 <- readJPEG("D:/K/浜戞埧/璇歌憶鎵炬埧椤圭洰/鍥剧墖/heembifc1b72b4e3.JPG")


#鎵归噺璇绘枃浠?
out_name <- list.files("D:/K/浜戞埧/璇歌憶鎵炬埧椤圭洰/鍥剧墖/澶栨櫙/") #鏂囦欢澶逛腑鎵�鏈夋枃浠?
dir_out <- paste("D:/K/浜戞埧/璇歌憶鎵炬埧椤圭洰/鍥剧墖/澶栨櫙/", out_name, sep = "") 
RGB_FUN2(paste("D:/K/浜戞埧/璇歌憶鎵炬埧椤圭洰/鍥剧墖/1/", list.files("D:/K/浜戞埧/璇歌憶鎵炬埧椤圭洰/鍥剧墖/1/"), sep = ""))


#璇籮son
zgjson <- lapply(readLines("D:/K/浜戞埧/璇歌憶鎵炬埧椤圭洰/train_dataset.json"), fromJSON)
metadata <- fromJSON("https://data.nasa.gov/data.json") #jsonlite

#璇籑YSQL
hdproreplenish <- dbConnect(RMySQL::MySQL(),
                            host = "192.168.14.71",         #涓绘満鍚?
                            dbname = "hdproreplenish",      #鏁版嵁搴撳悕
                            user = "root",                  #鐢ㄦ埛鍚?
                            password = "gh001")             #瀵嗙爜  
dbSendQuery(hdproreplenish, 'SET NAMES gbk')                #澶勭悊涔辩爜
head(dbReadTable(hdproreplenish, "tb_hdreplenish"))
huhaomysql <- dbReadTable(hdproreplenish, "tb_hdreplenish") #璇绘暟鎹〃

kaggle_mysql <- dbConnect(MySQL(), 
                          host="localhost", 
                          dbname="kaggle", 
                          user="root", 
                          password="liuyifei")
dbListTables(kaggle_mysql)  #鏁版嵁搴撲笅鎵�鏈夎〃
google_mysql <- dbReadTable(kaggle_mysql, "job_skills_csv") #璇绘暟鎹〃
str(google_mysql)

#判断编码
guess_encoding("D:/K/timesdata/肇庆项目/家宽电视项目/data/宽带电视.csv")

#设置编码
ds2 <- read_csv("D:/K/timesdata/肇庆项目/家宽电视项目/日常字段.csv", locale=locale(encoding="GB2312"))