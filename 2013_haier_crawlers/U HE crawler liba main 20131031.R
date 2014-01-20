#循环的量大了应注意冗余数据的累积，应该及时rm掉，否则容易报错终止。
#错误未解决:Error in UseMethod("xmlNamespaceDefinitions") : 
#  no applicable method for 'xmlNamespaceDefinitions' applied to an object of class "NULL"


library(XML)
library(RCurl)
#source("D:/cloud/git/U HE crawler liba fun 20131031.R")
#source("D:/百度云/git/U HE crawler liba fun 20131031.R")
source("liba crawler fun.R")

##获取板块列表——只在本地运行!!!待优化
#banlist <- read.csv("D:/cloud/data/20131028crawler19lou/libalist.csv")
banlist <- read.csv("D:/百度云/data/20131028crawler19lou/libalist.csv")

##获取主题iebiao——遍历banlist里面的版块,一个版块抓10条
topicdata <- data.frame()
for (i in 1:37 ) {
  for (j in 1:10) {
    x <- paste( banlist[i,2],"_",j,".htm", sep="")
    data <- getTitleNode(x)
    if ( class(data) == "data.frame" & nrow(data) >2 ) #对函数报错导致data异常的处理
    {data$chan <-  banlist[i,1] 
     topicdata <- rbind(topicdata, data)
    } else { rm(data )} 
  }
}
topicdata$topicid <- seq(1: nrow( topicdata))
#head(topicdata)

library(RODBC) #在家
con <- odbcConnect("mysql", uid="dingc", pwd="dingc" ) 
con <- odbcConnect("mysql", uid="dingc", pwd="dingc" ) # 
sqlSave(con ,topicdata , tablename="liba_title" , addPK=FALSE)
close(con)


##获取帖子详情
#获取主题地址
library(RMySQL)
con=dbConnect(MySQL(),user="dingc",password="dingc",dbname="test")
title <- dbReadTable(con, "liba_title") # 直接抓表
dbDisconnect(con)

#按地址抓内容
data <- data.frame()
dataa <- data.frame()
content <- data.frame()

for (j in 1: nrow(title)) {
  #for (j in 1:10) {
  content <- data.frame()
  dataa <- data.frame()
  for (i in 1:10) {
    url <- sub("1.htm", "", title[j,3], fixed=T) # url拆，处理_1.htm,_2.htm
    url <- paste( url, i, ".htm", sep="") #url拼，处理_1.htm,_2.htm
    rm(data)
    data <- as.data.frame( getTitleDetail( url) ) # 否则是矩阵会出问题
    if ( class(data) == "data.frame" & nrow(data) >=1 )  #异常处理
    {
      data$channel <- as.character( title[j, 7])
      data$topicid <- as.numeric( title[j, 8])
      dataa <- rbind( dataa, data) 
      Sys.sleep( round( runif(1, min=1, max=5)) )  #随机睡眠一段时间，比较稳健
    } else{ rm(data) } 
  }
  content <- rbind( content, dataa) #每个主题一合并
  if ( nrow(content) >=1 ){
    con <- dbConnect(MySQL(),user="dingc",password="dingc",dbname="test")
    dbWriteTable(con,"liba_content_10", content , append=TRUE , row.names=FALSE)
    dbDisconnect(con) } else{ rm(dataa, content) }
}

#j在哪里断掉，就在哪里重新开始！！！
#rm(list=ls(all=TRUE))

warnings()
