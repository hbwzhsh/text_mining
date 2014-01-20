#黄橙项目——每日自动运行，问题：大量冗余，需要通过中间处理排重
#备注1 <U+00A0>未解决
#备注2 日起切分以来2013年，跨年需要改代码
#备注3 如果某关键词搜索无结果将会报错停机

library(XML)

#载入俩函数：中文转二进制码和抓一页内容
kw2byte <- function(x){
  kyword <- toupper( iconv( x ,"utf8" , "gb2312", "byte",toRaw=TRUE) [[1]])
  #百度新闻是GBK gb2312 byte, 而大百度是gb2312 utf8 byte！ 服务器版是utf8...
  kyword<-paste(kyword ,collapse="%" ,sep='')
  kyword <- paste( "%", kyword, sep='')
} #单个关键字转为十六进制代码

baidunews1p <- function(x){
  #标题
  m <- getNodeSet(url1,"//body//div[@id = 'content_left']//li/h3/a")
  txt.title<-unlist(lapply(m, xmlValue, encoding="utf8"))
  #链接
  txt.link <- unlist(lapply(m ,xmlAttrs , encoding="utf8" ) )
  txt.link <- txt.link[names(txt.link)=='href']  #<a>中包含多个元素，只取链接  
  #日期和来源_unicode <U+00A0> 暂时没有完美解决方案!!!
  o <- getNodeSet(url1, "//body//div[@id = 'content_left']//li/span")
  txt.aa <- unlist( lapply (o, xmlValue )  )
  ind<-regexpr( " " , txt.aa)  # 检验是否存可恨字符，如果存在就跳出
  if ( length(which(ind==-1)) > 0) # ind由-1,1组成的向量
    browser()
  txt.aa <- gsub(" ","", txt.aa)
  txt.aa <- strsplit( txt.aa , '2013-', fixed = TRUE)
  txt.source <- unlist( lapply(txt.aa , function(x) x[1]) )
  txt.time <- paste( '2013-' , unlist( lapply(txt.aa , function(x) x[2]) ), sep='')  
  #摘要
  o <- getNodeSet(url1, "//body//div[@id = 'content_left']//li/div[@class='c-summary']")
  txt.abs <- unlist( lapply (o, xmlValue , encoding="utf8") )  
  ind<-regexpr( " " , txt.abs)  # 检验是否存可恨字符，如果存在就跳出
  if ( length(which(ind==-1)) > 0) # ind由-1,1组成的向量
    browser()
  txt.abs <- gsub(" ","", txt.abs)
  #合体
  txt <- cbind(txt.title, txt.source, txt.time, txt.abs, txt.link)
} ##抓一页百度新闻的数据


library(RMySQL) #从数据库取出搜索query的列表
con <- dbConnect(MySQL(),user="dingc",password="dingc",dbname="test")
kwlist <- dbReadTable(con, "dim_baidu01")[ ,c(1,3)]
dbDisconnect(con)


lastpg <- 2 #设定一次抓取页数的最大值，每页10条
txtall <- data.frame() #初始化，调试的时候有用，跑通了之后没啥用，下同
txttopic <- data.frame()
txtone <- data.frame()
for ( j in 1:nrow(kwlist) ) { #query循环
  kyword <- kw2byte( kwlist[j,2])
  for (i in 1:lastpg) { #页码循环
    p<-(i-1)*10
    ul<-paste("http://news.baidu.com/ns?si=&tn=news&ie=gb2312&word=",kyword,"&pn=",p, sep="")
    url1<-htmlTreeParse(ul, useInternalNodes=TRUE )
    txtone<- as.data.frame( baidunews1p(url1))  #抓取一页数据
      if( nrow(txtone) > 1 ) { #如果一页抓取成功，则加上附属信息
        txtone$systime <- Sys.Date() #加抓取时间
        #txtone$craw_id <- 3
        txtone$kw <- kwlist[j,2] #加抓取关键字
        names(txtone) <- c("title","source","time","abs","link","systime","kw")
        row.names(txtone) <- NULL
        txttopic <- rbind(txttopic , txtone)
         } else { rm(txtone)} #如果一页抓取不成功，丢弃数据
    }
  txttopic <- unique(txttopic) #对第j个关键字抓取的项目排重         
  txtall <- rbind(txtall, txttopic)
}


#硬过滤
txtall <- txtall [ txtall$time !="2013-NA", ] 
txtall$abs <- gsub( "条相似新闻", "", txtall$abs )
txtall$abs <- gsub( "条相同新闻", "", txtall$abs )
txtall$abs <- gsub( "百度快照", "", txtall$abs )

##写入数据库
library(RMySQL)
con=dbConnect(MySQL(),user="dingc",password="dingc",dbname="test")
dbWriteTable(con,"ods_baidu01_hc", txtall , append=TRUE , row.names=FALSE)
dbDisconnect(con)

rm(list=ls(all=TRUE))
