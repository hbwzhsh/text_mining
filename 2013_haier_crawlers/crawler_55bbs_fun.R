require(XML)
require(RCurl)


#取55bbs论坛中的板块列表，输入参数为网站首页地址；返回选中的板块地址向量
getChanList<-function(listUrl){
listExist= url.exists(listUrl)
if(listExist) {

# ----------------------解析页面
url1<-htmlTreeParse(listUrl,useInternalNodes=TRUE,encoding='gbk')

#----------------------取主板名称和链接
chanlist <- getNodeSet(url1, "//body//div[@class='subnav clearfix']/ul/li/a")
chanName<-unlist(lapply(chanlist,xmlValue,encoding='utf-8'))
t<-unlist(lapply(chanlist,xmlAttrs))
chanLink<-t[names(t)=='href']

#df<-data.frame(板块名称=chanName,板块地址=chanLink)
#write.csv(df,'55bbs板块列表.csv',row.names = TRUE)

#------------------------选择需要的板块链接
id<-match(c('丽人妆颜','活色生香','男人帮','房子','数码','汽车','厨房','中餐','异国美食','孕宝亲子','备孕','育儿心得','麻辣婆媳','杂谈生活','文化消费','旅游户外','家有宠物'),chanName)
selChanLink<-data.frame(版块链接=chanLink[id],版块名称=chanName[id])
selChanLink

}else{
print('55bbs网页无法连接！')
NULL
}
}


#获取所有话题结点
getTitleNode<-function(urlstr){#获取所有标题结点，输入参数为板块地址；返回标题结点列表
nodes.title<-list()
isex <- url.exists(urlstr)
if(isex){
url1<-NULL
#--------------获取标题结点
try(url1<-htmlTreeParse(urlstr, useInternalNodes=TRUE,encoding='gbk'),silent = T)
if (!is.null(url1)){

node.start<-getNodeSet(url1, "//body//table/thead[@class='category']")
node<-getSibling(node.start[[1]],after=T)
i<-1

while (!is.null(node)){
id<-xmlAttrs(node,'id')
if (!is.null(id)&&(regexpr('thread',id)>0)) {
nodes.title[[i]]<-xmlChildren(node)[[1]]
i<-i+1
}
node<-getSibling(node,after=T)
}#-------------nodes.title为标题结点列表
}#end for if url1
}#end for if(isex)
nodes.title
}# end for function 'getTileNode'


#获取话题内容、链接、作者、发布时间、浏览次数、回复次数、最近回复时间；输入参数为一个话题结点；返回一个字符串向量
getTitleDetail<-function(node.title){#获取单个topic结点的详细相关信息



chd<-xmlChildren(node.title)#获取所有孩子结点
#-----------获取标题
s<-xmlElementsByTagName(chd[[3]],'span')[[1]]
a<-xmlElementsByTagName(s,'a')[[1]]
title<-xmlValue(a,encoding='utf-8')

#-----------获取标题链接
link<-xmlAttrs(a)['href']#链接
link<-paste('http://bbs.55bbs.com/',link,sep='')

#-----------获取标题回复页数
if (length(xmlElementsByTagName(chd[[3]],'span'))>1){
p<-xmlElementsByTagName(chd[[3]],'span')[[2]]
a<-xmlElementsByTagName(p,'a')
pn<-unlist(lapply(a,xmlValue,encoding='utf-8'))
if (is.null(pn)){
rpPageNum<-'0'
}else{
lastPn<-length(pn)
rpPageNum<-pn[lastPn]
if (as.numeric(rpPageNum)<0|is.na(as.numeric(rpPageNum))) browser()
}#end for is.null(pn)
}else{
rpPageNum<-'1'
}#end for if length()>1
names(rpPageNum)<-''

author<-xmlValue(xmlElementsByTagName(chd[[5]],'a',recursive=T)[[1]],encoding='utf-8')#作者
authorLink<-xmlAttrs(xmlElementsByTagName(chd[[5]],'a',recursive=T)[[1]])['href']#AuthorLink

time<-xmlValue(xmlElementsByTagName(chd[[5]],'em',recursive=T)[[1]],encoding='utf-8')#发布时间

str<-xmlValue(chd[[7]])
id<-regexpr('/',str)[1]
rpnum<-substring(str,1,id-1)#回复次数

revnum<-substring(str,id+1,nchar(str))#浏览次数

lastTime<-xmlValue(xmlElementsByTagName(chd[[9]],'a',recursive=T)[[1]],encoding='utf-8')#最近回复时间

c(Topic=title, Link=link, Author=author, AuthorLink=authorLink, PostTime=time, RevNum=revnum,
RepNum=rpnum, RpPageNum = rpPageNum, LastTime=lastTime)

}#end for getTitleDetail

getRpTime<-function(rpinfoStr){
id1<-regexpr('发表于',rpinfoStr)[1]
id2<-regexpr('只看该作者',rpinfoStr)[1]
gsub('[[:cntrl:]]','',substr(rpinfoStr,id1+3,id2-1))
}#end for getRpTime


#getRpCont函数的功能为获取评论正文（包括楼主及回帖,ID=1为楼主）；输入为话题地址；返回为一个数据框：包括 TopicID Author AuthorLink 发布时间 内容
getRpCont<-function(curl,id){
info.rps<-data.frame()
url1<-NULL
try(url1<-htmlTreeParse(curl,useInternalNodes=TRUE,encoding='gbk'),silent = T)
if(!is.null(url1)){

#------------------发帖内容

nodes.rpcont<-getNodeSet(url1, "//body//form/div[@class='mainbox viewthread']/table//tr/
td[@class='postcontent']/div[@class='postmessage defaultpost']/div[@class='t_msgfont']|
//body//form/div[@class='mainbox viewthread']/table//tr/td[@class='postcontent']/
div[@class='postmessage defaultpost']/div[@class='notice']")


rpconts<-lapply(nodes.rpcont,xmlValue,encoding='utf-8')
rpconts<-unlist(lapply(rpconts,function(x) gsub('[[:space:]]','',x)))

#------------------发帖作者
nodes.rpauthor<-getNodeSet(url1, "//body//form/div[@class='mainbox viewthread']/table//tr/td[@class='postauthor']/cite")
rpauthors<-lapply(nodes.rpauthor,xmlValue,encoding='utf-8')
rpauthors<-unlist(lapply(rpauthors,function(x) gsub('[[:space:]]','',x)))#作者ID

getAuthorNode<-function(x){
node<-xmlElementsByTagName(x,'a')
if (length(node)>1){
return (node[[1]])
}else{
return(NULL)
}
}

rpauthorsA<-lapply(nodes.rpauthor,getAuthorNode)
rpauthorsLink<-unlist(lapply(rpauthorsA,function(x) ifelse(is.null(x),'', xmlAttrs(x)['href'])))#AuthorLink

#------------------发帖时间
nodes.rpinfo<-getNodeSet(url1, "//body//form/div[@class='mainbox viewthread']/table//tr/td[@class='postcontent']/div[@class='postinfo']")
rpinfos<-lapply(nodes.rpinfo,xmlValue,encoding='utf-8')
rpinfos<-unlist(lapply(rpinfos,getRpTime))

#-------------------组装返回值
num<-min(c(length(rpauthors),length(rpauthorsLink),length(rpinfos),length(rpconts)))
info.rps<-NULL
if (num>0) info.rps<-data.frame(TopicID=rep(id,num),Author=rpauthors[1:num],AuthorLink=rpauthorsLink[1:num],PostTime=rpinfos[1:num],PostTxt=rpconts[1:num])
rm(url1)
}#end for if url1
info.rps
}#end for getRpCont