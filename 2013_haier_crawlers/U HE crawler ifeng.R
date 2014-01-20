###抓取凤凰家电网数据

#获取板块地址

library(XML)

url <- "http://tech.ifeng.com/elec/"
url1 <- htmlTreeParse( url, useInternalNodes=TRUE,encoding='utf8' )
nav <- getNodeSet(url1, "//body//div[@class = 'mainNav']/ul/li")
ban <- sapply( nav,  xmlValue )
nav <- getNodeSet(url1, "//body//div[@class = 'mainNav']/ul/li/a")
link <- sapply( nav, function(el) xmlGetAttr(el, "href") )
banlist <- as.data.frame( cbind( ban , link ))
rm( ban, link, nav , url , url1)

#获取板块内文章数据(仅第一页)
ttlist <- data.frame()

for (i in 2:7) {
  url <- as.character( banlist[i,2] )
  url1 <- htmlTreeParse( url, useInternalNodes=TRUE,encoding='utf8' )
  a <- getNodeSet( url1, "//body//div[@class = 'newsList']//li/a" )
  title <- sapply( a , xmlValue)
  link <- sapply( a, function(el) xmlGetAttr(el, "href") )
  b <- regexpr( "20[0-9]{2}_[0-9]{2}\\/[0-9]{2}" , link)
  time <- substr( link, b, b+9)
  tt <- as.data.frame( cbind( banlist[i,1], title, time, link ))
  ttlist <- rbind( ttlist, tt)
}
rm(url, url1, a, b, title, link, time, i, tt)

##获取全文
url <- as.character("http://tech.ifeng.com/elec/home/detail_2012_04/09/13747768_0.shtml")

ifeng1page <- function(url) {
url1 <- htmlTreeParse( url, useInternalNodes=TRUE,encoding='utf8' )
a <- getNodeSet( url1,  "//body//div[@class = 'pageNum']")
if ( length(a) >0 ) {
  b <- length( xmlElementsByTagName( a[[1]] , "a") )  #获取文章页数
} else b <- 0
vm <- vector()

#获取分页文章的全部内容
if (b > 0) {
  for ( i in 0:b) {
    ul <- paste( substr( url, 1, regexpr( ".shtml" , url)-2 ), i, ".shtml", sep="" )
    ct <- getNodeSet( htmlTreeParse( ul, useInternalNodes=TRUE,encoding='utf8' ), "//body//div[@id = 'artical_real']")
    vm <- paste( vm, sapply( ct, xmlValue) )
  } #多页内容抓取循环结束
}  else  { #只有一页的情况
  ct <- getNodeSet( htmlTreeParse( url, useInternalNodes=TRUE,encoding='utf8' ),  "//body//div[@id = 'artical_real']")
  vm <- sapply( ct, xmlValue)
  } # end of else
out <- vm
#browser()
}

#把文章内容写入数据库
for ( i in 1: nrow(ttlist)) {
  ttlist$content[i] <- ifeng1page( as.character( ttlist[i,4]) )
}

names(ttlist)

head(ttlist$content)

ttlist$content <- gsub( "[[:space:]]+", " ",ttlist$content)

library(RODBC)
con <- odbcConnect("mysql", uid="dingc", pwd="dingc" )
sqlSave(con, ttlist, tablename = "ods_ifeng_tech", append = T, rownames = F, addPK = FALSE)
#新闻正文用varchar(255)会出问题，在mysql中修改成varchar(65535)
close(con)


