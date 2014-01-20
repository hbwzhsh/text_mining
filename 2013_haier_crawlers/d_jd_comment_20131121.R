##京东商品评论获取 by LDD

library(XML)
library(RCurl)
library(RMySQL) 

getscl<-function(cl)
{
  switch(cl,
         空调='空调类别',
         冰箱='门款式',
         平板电视='品类',
         '产品类型'
  )
}
getsize<-function(cl)
{
  switch(cl,
         空调='商品匹数',
         冰箱='总容积',
         平板电视='尺寸',
         洗衣机='洗涤容量',
         热水器='容积(L)',
         '商品毛重'
  )
}


getsku<-function (uli,scl,sbr){
  #uli<-"http://item.jd.com/809061.html";scl<-"冰箱";sbr<-"统帅"
  url1<-htmlTreeParse(uli,useInternalNodes=TRUE,encoding='gbk')
  
  #sku信息
  c <- getNodeSet(url1, "//body//div[@class='right']//ul[@class = 'detail-list']//li")
  txt<-unlist(lapply(c,function(x)xmlValue(x,encoding="utf-8")))
  #商品编号
  ifelse(length(grep('商品编号',txt))>0,sku<-sub('商品编号：','',txt[grep('商品编号',txt)]),sku<-"")
  #商品名称
  ifelse(length(grep('商品名称',txt))>0,sku.name<-sub('商品名称：','',txt[grep('商品名称',txt)]),sku.name<-"")
  #品牌
  #ifelse(length(grep('品牌',txt))>0,sku.brand<-sub('品牌：','',txt[grep('品牌',txt)]),sku.brand<-"")
  #类别
  #ifelse(length(grep('产品类型',txt))>0,sku.type<-sub('产品类型：','',txt[grep('产品类型',txt)]),sku.type<-"")
  ccl<-paste(getscl(scl),'：',sep="")
  ifelse(length(grep(ccl,txt))>0,sku.type<-sub(ccl,'',txt[grep(ccl,txt)]),sku.type<-"")
  #容量
  ssl<-paste(getsize(scl),'：',sep="")
  ifelse(length(grep(ssl,txt))>0,sku.size<-sub(ssl,'',txt[grep(ssl,txt)]),sku.size<-"")
  
  sku.brand<-sbr;sku.cl<-scl
  cbind(sku,sku.name,sku.brand,sku.type,sku.cl,sku.size)
}

getcommon<-function(sku,md)
{
  #sku=809061;md='2013-10-01'
  tcf<-NULL
  for (level in 1:3)
  {
    #level=3
    ul<-paste("http://club.jd.com/review/",sku,"-0-1-",level,".html",sep="")
    
    #最后页码
    #ul<-"http://club.jd.com/review/807004-0-1-0.html"
    url2<-htmlTreeParse(ul,useInternalNodes=TRUE,encoding='gbk')
    c <- getNodeSet(url2, "//body//div[@class='right']//div[@class='pagin fr']//a")
    txt<-unique(unlist(lapply(c,function(x)xmlValue(x,encoding="utf-8"))))
    c <- getNodeSet(url2, "//body//div[@class='item']//div[@class='o-topic']//span")
    txt2<-unlist(lapply(c,function(x)xmlValue(x,encoding="utf-8")))
    if(length(txt2)>0)
    {
      if(length(txt)>1){
        lastpg<-as.integer(sub('...','',txt[length(txt)-1]))
        #ifelse(lastpg>pg,lastpg<-pg,lastpg<-lastpg)
      }else{lastpg<-1}
      i<-1;mtime<-as.character(Sys.Date())
      while((i<=lastpg)&(mtime>=md))
      {
        ul<-paste("http://club.jd.com/review/",sku,"-0-",i,"-",level,".html",sep="")
        url2<-htmlTreeParse(ul,useInternalNodes=TRUE,encoding='gbk')
        #发布时间
        c <- getNodeSet(url2, "//body//div[@class='item']//div[@class='o-topic']//span")
        txt<-unlist(lapply(c,function(x)xmlValue(x,encoding="utf-8")))
        comm.time<-txt[which(txt!='')]
        #评分
        txt<-unlist(lapply(c,xmlAttrs))
        ifelse(length(grep('star',txt))>0,comm.score<-sub('star ','',txt[grep('star',txt)]),comm.score<-"")
        names(comm.score)<-NULL
        #评论内容
        c <- getNodeSet(url2, "//body//div[@class='item']//div[@class='comment-content']")
        txt<-unlist(lapply(c,function(x)xmlValue(x,encoding="utf-8")))
        cont<-gsub('[[:space:]]','',txt)
        comm.cont<-gsub('购买日期.{0,}','',cont)
        #购买日期
        buy.time<-gsub('：','',gsub('.{0,}购买日期','',cont))
        #评论人
        c <- getNodeSet(url2, "//body//div[@class='item']//div[@class='user']//div[@class='u-name']")
        txt<-unlist(lapply(c,function(x)xmlValue(x,encoding="utf-8")))
        user.name<-gsub('[[:space:]]','',txt)
        #评论人级别地址  
        c <- getNodeSet(url2, "//body//div[@class='item']//div[@class='user']//span[@class='u-level']")
        txt<-unlist(lapply(c,function(x)xmlValue(x,encoding="utf-8")))
        
        user.level<-gsub('会员.{0,}','',txt)
        user.adrs<-gsub('.{0,}会员','',txt)
        
        pl<-ifelse(level==1,'差评',ifelse(level==2,'中评','好评'))
        tc<-data.frame(sku,buy.time,comm.time,comm.cont,pl,comm.score,user.name,user.level,user.adrs)
        
        tcf<-rbind(tcf,tc)
        i<-i+1;
        #mtm<-comm.time[-(grep('晒单：',comm.cont))]  #删除晒单帖后比较时间
        ifelse(length(comm.time)>1&(i>2),mtime<-min(comm.time),mtime<-as.character(Sys.Date()))
        rm(url2)
      }
    }
  }
  tcf
}


jdpl<-function(url,sclass,sbrand,ytd,ymd,crid)
{
  #获取所有链接,去重复
  #url<-"http://list.jd.com/737-794-878-22186-0-0-0-0-0-0-1-1-1-1-1-1-4137-0.html";sclass='冰箱';sbrand='统帅';ytd='2013-10-30';ymd='2013-10-31';crid=Sys.Date()
  ul<-htmlTreeParse(url, useInternalNodes=TRUE,encoding='gbk')
  pg <- getNodeSet(ul, "//body//div[@class = 'w main']//div[@class = 'pagin pagin-m']//span[@class='text']")
  txt<-unlist(lapply(pg,xmlValue))
  lastpg<-as.integer(strsplit(txt,'/')[[1]][2])
  title.link<-list()
  jdurl<-unlist(strsplit(url,'-'))
  for (j in 1:lastpg)
  {
    #urlj<-paste("http://list.jd.com/737-794-878-4978-0-0-0-0-0-0-1-1-",j,"-1-1-2801-33.html",sep="")
    
    jdurl[13]<-j
    urlj<-paste(jdurl,collapse='-')
    rl<-htmlTreeParse(urlj, useInternalNodes=TRUE,encoding='gbk')
    m <-getNodeSet(rl, "//body//div[@class = 'w main']//ul[@class = 'list-h']//div[@class='p-img']//a")
    t<-unlist(sapply(m,xmlAttrs))
    title.link[[j]]<-t[rownames(t)=='href']
  }
  allink<-unique(unlist(title.link))
  
  allsku<-NULL;allcommon<-NULL
  skuinfo<-NULL;comminfo<-NULL
  for(k in 1:length(allink))
  {
    try(skuinfo<-getsku(allink[k],sclass,sbrand),silent=T)
    
    sid<-sub('http://item.jd.com/','',allink[k])
    sid<-sub('.html','',sid)
    try(comminfo<-getcommon(sid,ytd),silent=T)
    if(length(skuinfo)>0){
      allsku<-rbind(allsku,skuinfo)
      allcommon<-rbind(allcommon,comminfo)
    }
  }
  allsku<-unique(allsku)
  allcommon<-unique(subset(allcommon,as.character(allcommon$comm.time)>=ytd & as.character(allcommon$comm.time)<ymd))
  allres<-merge(allsku,allcommon,by.x="sku",by.y="sku")
  skures<-as.data.frame(allsku)
  skures$crawid<-crid
  allres$crawid<-crid
  #colnames(allres)<-c("SKU_ID","SKU_NAME","SKU_BRAND","SKU_TYPE","SKU_CLASS","SKU_SIZE","BUY_TIME","COMM_TIME","COMM_CONT","COMM_LEVEL","COMM_SCORE","USER_NAME","USER_LEVEL","USER_ADDRES","CRAW_ID")
  colnames(skures)<-c("skuid","skuname","skubrand","skutype","skuclass","skusize","crawid")
  colnames(allres)<-c("skuid","skuname","skubrand","skutype","skuclass","skusize","buytime","commtime","commcont","commlevel","commscore","username","userlevel","useraddres","crawid")
  con=dbConnect(MySQL(),user="lidd",password="lidd",dbname="test")
  dbWriteTable(con,"ods_jd_sku",skures,append=T,row.names=F)
  dbWriteTable(con,"ods_jd_comment",allres,append=T,row.names=F)
  dbDisconnect(con)
}

#craw<-data.frame(1,"2","1","20001",Sys.time(),url)
#colnames(craw)<-c("CRAW_ID","REQ_ID","WS_ID","KW_ID","CRAW_TIME","CRAW_URL")
#dbWriteTable(con,"CRAW_REC_LDD",craw,append=T,row.names=F)

#url<-"http://list.jd.com/737-794-878-0-0-0-0-0-0-0-1-1-1-1-1-72-4137-0.html";ytd="2013-10-30";ymd="2013-10-31";sclass="冰箱";

main<-function(url,sclass,ytd,ymd)
{
  crid=Sys.Date()
  ul<-htmlTreeParse(url, useInternalNodes=TRUE,encoding='gbk')
  ulink <- getNodeSet(ul, "//body//div[@class = 'w main']//div[@id = 'select']//dl[@id='select-brand']//div[@class='content']//a")
  sbrand<-unlist(lapply(ulink,function(x)xmlValue(x,encoding="utf-8")))
  t<-unlist(sapply(ulink,xmlAttrs))
  levelink<-t[names(t)=='href']
  urlink<-paste('http://list.jd.com/',levelink,sep='')
  for(jj in 2:length(urlink))
  {
    
    try(jdpl(urlink[jj],sclass,sbrand[jj],ytd,ymd,crid),silent=T)
    
  }
}
main("http://list.jd.com/737-794-870-0-0-0-0-0-0-0-1-1-1-1-1-72-4137-0.html","空调",Sys.Date()-1,Sys.Date())
main("http://list.jd.com/737-794-878-0-0-0-0-0-0-0-1-1-1-1-1-72-4137-0.html","冰箱",Sys.Date()-1,Sys.Date())
main("http://list.jd.com/737-794-880-0-0-0-0-0-0-0-1-1-1-1-1-72-4137-0.html","洗衣机",Sys.Date()-1,Sys.Date())
rm(list=ls(all=TRUE))