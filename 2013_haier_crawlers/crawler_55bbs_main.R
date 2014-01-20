
#��ȡ55bbs��̳���⣨���ơ����ߡ�ʱ��ȣ����������������ӵ����ݺͷ���ʱ�䣬��������������ӵĶ�Ӧ��ϵ
require(XML)
require(RCurl)
source('crawler_55bbs_fun.r')
options(error=recover)


#-----����ȫ�ֱ���
#-----------------55bbs
listUrl ="http://www.55bbs.com/"

#-----------------����ץȡ�����ҳ��
pages<-1:10

#------------------����ץȡ��ÿ�������������ҳ��
pages.comm<-1:10




#������

#------------��ȡ�������


chanLink<-getChanList(listUrl)
write.csv(chanLink,paste('55bbs_chanlist_',Sys.Date(),'.csv',sep=''),row.names = F)

if (!is.null(chanLink)){
info.title<-data.frame()
nr<-nrow(chanLink)
#------------��ȡ��飨��ҳ�����������Ϣ
for (i in 1:nr){#ѭ����ȡ���
chanurl<-chanLink[i,'�������']
for (pid in pages){#ȡ����ڶ�ҳ

#--------------------ƴҳ������
id<-gregexpr('-',chanurl)[[1]]
chanurl<-paste(substring(chanurl,1,id[2]),pid,'.html',sep='')
nodes.title<-getTitleNode(chanurl)#��ȡ���������
if (is.null(nodes.title)|length(nodes.title)==0) next
titles<-sapply(nodes.title,getTitleDetail)#��ȡ���������Ϣ
titles<-as.data.frame(t(titles))
titles$'ChanName'<-rep(chanLink[i,'�������'],nrow(titles))
info.title<-rbind(info.title,titles)
Sys.sleep(3)
}#end for ҳ��ѭ��
}#end for ���ѭ��
ntitle<-nrow(info.title)
info.title$'TopicID'<-1:ntitle
}#end for if !is.null(chanLink)
write.csv(info.title,paste('55bbs_titles_',Sys.Date(),'.csv',sep=''),row.names = F)


#��csv�ļ��ж�����
#info.title<-read.table('55bbs_titles_2013-10-18.csv',header=T,sep=',')

#------------���ݷֿ�:��ȡ������ػ���
nr<-nrow(info.title)

partition<-100
nump<-ceiling(nr/partition)
for (pid in 1:nump){
info.rp<-data.frame()
for (i in ((pid-1)*partition+1):(pid*partition)){#-----����ѭ��
if (i>nr) break
pagenum<-as.numeric(info.title[i,'RpPageNum'])
for (j in 1:pagenum){#----����ҳѭ��
#-------------------------------ƴҳ������
urlstr<-as.character(info.title[i,'Link'])
id<-gregexpr('-',urlstr)[[1]]
urlstr<-paste(substring(urlstr,1,id[2]),j,substring(urlstr,id[3],nchar(urlstr)),sep='')

conts<-getRpCont(urlstr,info.title[i,'TopicID'])
if (is.null(conts)) next
info.rp<-rbind(info.rp,conts)
rm(conts)
Sys.sleep(1)
}#end for ����ҳ
}#end for ����ѭ��
write.csv(info.rp,paste('55bbs_com_',pid,'_',Sys.Date(),'.csv',sep=''),row.names = F)

rm(info.rp)
}#end for pid



#�����ݿ��ж�����
library(RMySQL)
con=dbConnect(MySQL(),user="hez",password="hez",dbname="test") #������������
info.title<-dbReadTable(con, "55bbs_title") # ֱ��ץ��


#------------���ݷֿ�:��ȡ������ػ���
nr<-nrow(info.title)

partition<-5
nump<-ceiling(nr/partition)
for (pid in 1:nump){
info.rp<-data.frame()
for (i in ((pid-1)*partition+1):(pid*partition)){#-----����ѭ��
if (i>nr) break
pagenum<-as.numeric(info.title[i,'rppagenum'])
for (j in 1:pagenum){#----����ҳѭ��
#-------------------------------ƴҳ������
urlstr<-as.character(info.title[i,'link'])
id<-gregexpr('-',urlstr)[[1]]
urlstr<-paste(substring(urlstr,1,id[2]),j,substring(urlstr,id[3],nchar(urlstr)),sep='')

conts<-getRpCont(urlstr,info.title[i,'topicid'])
if (is.null(conts)) next
info.rp<-rbind(info.rp,conts)
rm(conts)
Sys.sleep(1)
}#end for ����ҳ
}#end for ����ѭ��
dbWriteTable(con, "55bbs_rp", info.rp,append=TRUE)
#dbCommit(con)
rm(info.rp)
}#end for pid
dbDisconnect(con)