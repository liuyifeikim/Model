#固定路径读数据
path <- "D:/K/DATA EXERCISE/Kaggle/" 
app_full <- read_csv(paste0(path, "google-play-store-apps/googleplaystore.csv"))
glimpse(app_c)

game <- read.csv('game.csv', stringsAsFactors = F)

#读EXCEL
model <- read.xlsx("D:/K/云房/数据/单元类型分类建模/sample+model(7.3).xlsx",
                 sheet = "model")  #openxlsx包

#路径
nodes <- read.csv('../../../Kaggle/Marvel networks/nodes.csv') #相对路径
nodes <- read.csv('./stack-overflow-tag-network/stack_network_nodes.csv', stringsAsFactors = FALSE)  #绝对路径

#data.table读大数据
google <- fread("job_skills.csv", encoding = "UTF-8")


#导出数据
write.xlsx(lpa_na40,"D:/K/云房/数据/数据自动化清理/lpa_na40.xlsx")
write.xlsx(ld_index_10w[which(ld_index_10w$dan == TRUE), ], "D:/K/云房/数据/数据自动化清理/ld(单元信息).xlsx")


#读图片
a1 <- readJPEG("D:/K/云房/诸葛找房项目/图片/heembifc1b72b4e3.JPG")


#批量读文件
out_name <- list.files("D:/K/云房/诸葛找房项目/图片/外景/") #文件夹中所有文件
dir_out <- paste("D:/K/云房/诸葛找房项目/图片/外景/", out_name, sep = "") 
RGB_FUN2(paste("D:/K/云房/诸葛找房项目/图片/1/", list.files("D:/K/云房/诸葛找房项目/图片/1/"), sep = ""))


#读json
zgjson <- lapply(readLines("D:/K/云房/诸葛找房项目/train_dataset.json"), fromJSON)
metadata <- fromJSON("https://data.nasa.gov/data.json") #jsonlite

#读MYSQL
hdproreplenish <- dbConnect(RMySQL::MySQL(),
                            host = "192.168.14.71",         #主机名
                            dbname = "hdproreplenish",      #数据库名
                            user = "root",                  #用户名
                            password = "gh001")             #密码  
dbSendQuery(hdproreplenish, 'SET NAMES gbk')                #处理乱码
head(dbReadTable(hdproreplenish, "tb_hdreplenish"))
huhaomysql <- dbReadTable(hdproreplenish, "tb_hdreplenish") #读数据表

kaggle_mysql <- dbConnect(MySQL(), 
                          host="localhost", 
                          dbname="kaggle", 
                          user="root", 
                          password="liuyifei")
dbListTables(kaggle_mysql)  #数据库下所有表
google_mysql <- dbReadTable(kaggle_mysql, "job_skills_csv") #读数据表
str(google_mysql)
