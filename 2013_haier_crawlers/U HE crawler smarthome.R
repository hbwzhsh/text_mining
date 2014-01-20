## smarthome forum抓取

library(XML)

#获取分论坛
ul <- "http://www.smarthome.com/forum/default.asp"
url1 <- htmlTreeParse( ul, useInternalNodes=TRUE,encoding='utf8')
tt <- getNodeSet( url1,"//td[@valign = 'top']//span[ @class = 'spnMessageText']/a")
forum <- sapply( tt, xmlValue) [ seq(1,25, by =2)]
link <- sapply( tt, function(el) 
  paste( "http://www.smarthome.com/forum/",  xmlGetAttr(el, "href" ), sep="" )) [ seq(1,25, by =2)]
forumlist <- as.data.frame( cbind( forum, link ) )
forumlist[,2] <- as.character(forumlist[,2] )
rm( forum, link, tt, ul, url1)


#获取分论坛主题页数，加到forumlist
for (i in 1: nrow(forumlist)) {
  ul <- forumlist[i, 2]
  url1 <- htmlTreeParse( ul, useInternalNodes=TRUE,encoding='utf8')
  a <- getNodeSet( url1, "//body/table[2]/tr/td[1]/table[1]/tr[2]//b[2]")
  forumlist$pg[i] <- sapply( a, xmlValue ) [1]
}
forumlist$pg <- as.numeric( gsub( "[^0-9]" , "", forumlist$pg) )
forumlist$pg[ is.na(forumlist$pg)] <- 1

#获取主题列表

smthome.topic.1p <- function( ul ) {  #抓取一页主题帖
ul <- "http://www.smarthome.com/forum/forum.asp?FORUM_ID=9&sortfield=lastpost&sortorder=desc&whichpage=1"
url1 <- NULL
Sys.sleep( round( runif(1, min=1, max=3)) ) 
try( url1 <- htmlTreeParse( ul, useInternalNodes=TRUE,encoding='utf8'),silent = F)
  if ( !is.null( url1))  { 
    
    #抓页面内容
    a <- getNodeSet( url1, "//body/table[2]//td/table[2]//td/table") [[1]] #锚定table
    b <- xmlElementsByTagName( a , "tr")  #锚定表格每一行
    df <- list()  #用两层xmlElementsByTagNames内容取出存到list
    
    for ( i in 2: length(b)-1 ) {  #第一行是标题，最后一行是其他信息
      c <- xmlElementsByTagName( b[[i]] , "td") #锚定行里的每个元素 
      row <- sapply( c, xmlValue , encoding="utf8")
      row <- gsub(  "[[:space:]]+" , " ", row ) #获取内容
      d <- getNodeSet( b[[i]], "td[2]//a") #锚定每一行里的链接所在节点
      link <- sapply( d, function(el)  #获取链接
        paste( "http://www.smarthome.com/forum/" , xmlGetAttr(el, "href") , sep="") ) [1]
      
      df[[i]] <- c( row , link)     # 每一行合体
      rm(row, c, link , d)
    }
    content <- data.frame() #list转换成数据框
    for ( i in 2 : length(df)) {
      for ( j in 1 : length( df[[2]])) {
        content[i,j] =df[[i]][j] } }
    content <- content[ -1 ,-1]
    names( content ) <- c("topic", "author", "replies", "read", "lastpost", "href") 
    content <- content
    
    } # end if url1 is NULL
}

#seesee <- smthome.topic.1p( "http://www.smarthome.com/forum/forum.asp?FORUM_ID=9" )

three <- data.frame()
for (j in 1: nrow(forumlist) ) {

  two <- data.frame()
  for (i in 1 : forumlist[j,3] ) { #单论坛抓10页
    one <- data.frame()
    ul <- forumlist[j, 2] #http://www.smarthome.com/forum/forum.asp?FORUM_ID=9
    ul <- paste( ul, "&sortfield=lastpost&sortorder=desc&whichpage=" ,i , sep="")
    one <- smthome.topic.1p( ul )
    if ( class(one) == "data.frame" ) { #抓取不成功的处理
    one$pg <- i
    two <- rbind(two, one) } else { NULL }
  } #单页循环结束
    two$topic <- forumlist[j, 1]
    three <- rbind( two, three )
}

library(RODBC)

con <- odbcConnect( "mysql", uid="xxx", pwd="xxx")
sqlSave(con, three, tablename = "ods_smarthome_topic", append = T, rownames = F, addPK = FALSE)
close(con)
##以上内容在本地可执行，服务器不可执行


## smarthome forum抓取，服务器版——topic已知，抓取内容部分

library(XML)
library(RMySQL)

con <- dbConnect( MySQL(), username="xxx" , password = "xxx", dbname="test" )
topic <- dbReadTable( con , "ods_smarthome_topic") # 直接抓表
dbDisconnect( con )

names(topic)
topic$id <- 1: nrow(topic)
topic$len <- round( as.numeric( topic$replies )/30)+1  #每个主题取多少页

#ul <- topic[1,6]

#抓取一页
smarthome.1p <- function( ul) {
  
  #Sys.sleep( round( runif(1, min=1, max=3)) ) 
  url1<-NULL
  try( url1 <- htmlTreeParse( ul, useInternalNodes=TRUE,encoding='utf8'),silent = F )
  if (!is.null(url1)){ 
    a <- getNodeSet( url1, "//span[@class = 'spnMessageText' and @id = 'msg' ]") 
    content <- sapply( a, xmlValue)     
    a <- getNodeSet( url1, "//body/table[4]//table//table/tr[1]")
    posttime <- sapply( a , function(x) 
      gsub( '[[:space:]]' , "", xmlValue(x, encoding ="utf8") ) ) [-1]
    a <- getNodeSet( url1, "//span[@class = 'spnMessageText']/a[@onmouseover]") 
    author <- sapply( a , xmlValue)
    rm( url1, a)
    
    try( txt <- as.data.frame( cbind(content, author, posttime) ) )
    
    if ( class(txt) == "data.frame") { txt <- txt} else( NULL) 
    #保证函数输出要么是dataframe，要么是NULL
  } else { NULL}
}

#txt <-  smarthome.1p( 'http://www.smarthome.com/forum/topic.asp?TOPIC_ID=163&whichpage=2')
#a <- txt

#按地址抓内容
a <- data.frame()
b <- data.frame()

for (j in 1: nrow(topic) ) {
  b <- data.frame( )
  for (i in 1: topic$len[j]) {
    try( rm(a) )
    url <- paste( topic[j,6] ,  "&whichpage=", i , sep="" )
    a <- smarthome.1p( url)  # 否则是矩阵会出问题
    if ( nrow(a) >0 )  #异常处理
    {
      a$channel <- as.character( topic$topic[j] )
      a$topicid <- topic$id[j]
      try( b <- rbind( b, a ) )
    } else{ try( rm(a) ) } 
  } # end page roll
  if ( class(b) == "data.frame" & nrow(b) >=3 ){
    con <- dbConnect(MySQL(),user="xxx",password="xxx",dbname="test")
    try( dbWriteTable(con,"ods_smarthome_content", b , append=TRUE , row.names=FALSE) )
    dbDisconnect(con)
    rm(con)
    rm(b)} else{ rm(b)  }
}

#rm(list=ls(all=TRUE))

warnings()


