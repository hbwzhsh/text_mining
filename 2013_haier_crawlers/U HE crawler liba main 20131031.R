#ѭ����������Ӧע���������ݵ��ۻ���Ӧ�ü�ʱrm�����������ױ�����ֹ��
#����δ���:Error in UseMethod("xmlNamespaceDefinitions") : 
#  no applicable method for 'xmlNamespaceDefinitions' applied to an object of class "NULL"


library(XML)
library(RCurl)
#source("D:/cloud/git/U HE crawler liba fun 20131031.R")
#source("D:/�ٶ���/git/U HE crawler liba fun 20131031.R")
source("liba crawler fun.R")

##��ȡ����б�����ֻ�ڱ�������!!!���Ż�
#banlist <- read.csv("D:/cloud/data/20131028crawler19lou/libalist.csv")
banlist <- read.csv("D:/�ٶ���/data/20131028crawler19lou/libalist.csv")

##��ȡ����iebiao��������banlist����İ��,һ�����ץ10��
topicdata <- data.frame()
for (i in 1:37 ) {
  for (j in 1:10) {
    x <- paste( banlist[i,2],"_",j,".htm", sep="")
    data <- getTitleNode(x)
    if ( class(data) == "data.frame" & nrow(data) >2 ) #�Ժ�����������data�쳣�Ĵ���
    {data$chan <-  banlist[i,1] 
     topicdata <- rbind(topicdata, data)
    } else { rm(data )} 
  }
}
topicdata$topicid <- seq(1: nrow( topicdata))
#head(topicdata)

library(RODBC) #�ڼ�
con <- odbcConnect("mysql", uid="dingc", pwd="dingc" ) 
con <- odbcConnect("mysql", uid="dingc", pwd="dingc" ) # 
sqlSave(con ,topicdata , tablename="liba_title" , addPK=FALSE)
close(con)


##��ȡ��������
#��ȡ�����ַ
library(RMySQL)
con=dbConnect(MySQL(),user="dingc",password="dingc",dbname="test")
title <- dbReadTable(con, "liba_title") # ֱ��ץ��
dbDisconnect(con)

#����ַץ����
data <- data.frame()
dataa <- data.frame()
content <- data.frame()

for (j in 1: nrow(title)) {
  #for (j in 1:10) {
  content <- data.frame()
  dataa <- data.frame()
  for (i in 1:10) {
    url <- sub("1.htm", "", title[j,3], fixed=T) # url�𣬴���_1.htm,_2.htm
    url <- paste( url, i, ".htm", sep="") #urlƴ������_1.htm,_2.htm
    rm(data)
    data <- as.data.frame( getTitleDetail( url) ) # �����Ǿ���������
    if ( class(data) == "data.frame" & nrow(data) >=1 )  #�쳣����
    {
      data$channel <- as.character( title[j, 7])
      data$topicid <- as.numeric( title[j, 8])
      dataa <- rbind( dataa, data) 
      Sys.sleep( round( runif(1, min=1, max=5)) )  #���˯��һ��ʱ�䣬�Ƚ��Ƚ�
    } else{ rm(data) } 
  }
  content <- rbind( content, dataa) #ÿ������һ�ϲ�
  if ( nrow(content) >=1 ){
    con <- dbConnect(MySQL(),user="dingc",password="dingc",dbname="test")
    dbWriteTable(con,"liba_content_10", content , append=TRUE , row.names=FALSE)
    dbDisconnect(con) } else{ rm(dataa, content) }
}

#j������ϵ��������������¿�ʼ������
#rm(list=ls(all=TRUE))

warnings()