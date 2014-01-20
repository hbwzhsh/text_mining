
#爬取55bbs论坛主题（名称、作者、时间等）及主题下所有帖子的内容和发表时间，并建立主题和帖子的对应关系
require(XML)
require(RCurl)
source('crawler_55bbs_fun.r')
options(error=recover)


#-----设置全局变量
#-----------------55bbs
listUrl ="http://www.55bbs.com/"

#-----------------设置抓取话题的页数
pages<-1:10

#------------------设置抓取的每个话题的最大回帖页数
pages.comm<-1:10




#主函数

#------------获取版块链接


chanLink<-getChanList(listUrl)
write.csv(chanLink,paste('55bbs_chanlist_',Sys.Date(),'.csv',sep=''),row.names = F)

if (!is.null(chanLink)){
info.title<-data.frame()
nr<-nrow(chanLink)
#------------获取版块（多页）标题相关信息
for (i in 1:nr){#循环获取板块
chanurl<-chanLink[i,'版块链接']
for (pid in pages){#取板块内多页

#--------------------拼页面链接
id<-gregexpr('-',chanurl)[[1]]
chanurl<-paste(substring(chanurl,1,id[2]),pid,'.html',sep='')
nodes.title<-getTitleNode(chanurl)#获取板块主题结点
if (is.null(nodes.title)|length(nodes.title)==0) next
titles<-sapply(nodes.title,getTitleDetail)#获取主题相关信息
titles<-as.data.frame(t(titles))
titles$'ChanName'<-rep(chanLink[i,'版块名称'],nrow(titles))
info.title<-rbind(info.title,titles)
Sys.sleep(3)
}#end for 页面循环
}#end for 板块循环
ntitle<-nrow(info.title)
info.title$'TopicID'<-1:ntitle
}#end for if !is.null(chanLink)
write.csv(info.title,paste('55bbs_titles_',Sys.Date(),'.csv',sep=''),row.names = F)


#从csv文件中读主题
#info.title<-read.table('55bbs_titles_2013-10-18.csv',header=T,sep=',')

#------------数据分块:读取主题相关回帖
nr<-nrow(info.title)

partition<-100
nump<-ceiling(nr/partition)
for (pid in 1:nump){
info.rp<-data.frame()
for (i in ((pid-1)*partition+1):(pid*partition)){#-----主题循环
if (i>nr) break
pagenum<-as.numeric(info.title[i,'RpPageNum'])
for (j in 1:pagenum){#----回帖页循环
#-------------------------------拼页面链接
urlstr<-as.character(info.title[i,'Link'])
id<-gregexpr('-',urlstr)[[1]]
urlstr<-paste(substring(urlstr,1,id[2]),j,substring(urlstr,id[3],nchar(urlstr)),sep='')

conts<-getRpCont(urlstr,info.title[i,'TopicID'])
if (is.null(conts)) next
info.rp<-rbind(info.rp,conts)
rm(conts)
Sys.sleep(1)
}#end for 回帖页
}#end for 主题循环
write.csv(info.rp,paste('55bbs_com_',pid,'_',Sys.Date(),'.csv',sep=''),row.names = F)

rm(info.rp)
}#end for pid



#从数据库中读主题
library(RMySQL)
con=dbConnect(MySQL(),user="hez",password="hez",dbname="test") #建立与库的连接
info.title<-dbReadTable(con, "55bbs_title") # 直接抓表


#------------数据分块:读取主题相关回帖
nr<-nrow(info.title)

partition<-5
nump<-ceiling(nr/partition)
for (pid in 1:nump){
info.rp<-data.frame()
for (i in ((pid-1)*partition+1):(pid*partition)){#-----主题循环
if (i>nr) break
pagenum<-as.numeric(info.title[i,'rppagenum'])
for (j in 1:pagenum){#----回帖页循环
#-------------------------------拼页面链接
urlstr<-as.character(info.title[i,'link'])
id<-gregexpr('-',urlstr)[[1]]
urlstr<-paste(substring(urlstr,1,id[2]),j,substring(urlstr,id[3],nchar(urlstr)),sep='')

conts<-getRpCont(urlstr,info.title[i,'topicid'])
if (is.null(conts)) next
info.rp<-rbind(info.rp,conts)
rm(conts)
Sys.sleep(1)
}#end for 回帖页
}#end for 主题循环
dbWriteTable(con, "55bbs_rp", info.rp,append=TRUE)
#dbCommit(con)
rm(info.rp)
}#end for pid
dbDisconnect(con)