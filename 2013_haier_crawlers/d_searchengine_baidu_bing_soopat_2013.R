##注意不同环境下的代码转换，搜local 或者 server

########################百度新闻#############################
# 重新封装函数，不考虑排重和清洗的问题，只完成原始数据的积累

#抓一页数据
baidunews.1p <- function(x){
  
  isex <- url.exists(x)
  if(isex){
    url1<-NULL
    try( url1 <- htmlTreeParse( x , useInternalNodes=T, encoding='utf8'),silent = F)
    if (!is.null(url1)){ 
      
      #标题
      o <- getNodeSet(url1,"//body//div[@id = 'content_left']//li/h3/a")
      txt.title<-unlist(lapply(o, xmlValue, encoding="utf8"))
      #链接
      txt.link <- unlist(lapply(o ,xmlAttrs , encoding="utf8" ) )
      txt.link <- txt.link[names(txt.link)=='href']  #<a>中包含多个元素，只取链接  
      
      #日期和来源1 unicode <U+00A0> 暂时没有完美解决方案!!!
      #o <- getNodeSet(url1, "//body//div[@id = 'content_left']//li/span" )
      #txt.aa <- sapply (o, xmlValue , encoding ="utf8")
      #txt.aa <- strsplit( txt.aa , format( Sys.time() , "%Y") , fixed = TRUE) #用今年切
      #txt.source <- sapply(txt.aa , 
      #                  function(x) gsub( '[^dA-Za-z\u3007\u4E00-\u9FCB\uE815-\uE864]', '', x[1]))
      #txt.time <- sapply(txt.aa , function(x) x[2]) 
      
	  #日期和来源2  unicode <U+00A0> &nbsp; 去掉
      o <- getNodeSet(url1, "//body//div[@id = 'content_left']//li/span" )
      txt.aa <- sapply (o, xmlValue , encoding ="utf8")
      #txt.cc <- strsplit( txt.aa ,  '[^0-9A-Za-z\u3007\u4E00-\u9FCB\uE815-\uE864]')
	  txt.cc <- strsplit( txt.aa ,  '[[:space:]]')
      txt.source <- sapply(txt.cc ,  function(x) x[2])
      txt.time <- sapply(txt.cc , function(x) paste( x[3], x[4], sep=" "))

      #摘要
      o <- getNodeSet(url1, "//body//div[@id = 'content_left']//li/div[@class='c-summary']")
      txt.abs <- sapply (o, xmlValue , encoding="utf8") 
      
      #合体
	  rm(o)
      txt <- cbind(txt.title, txt.source, txt.time, txt.abs, txt.link) #如果向量长度不同，会自动循环短的补齐后合并
	  txt <- unique(txt)
	  names(txt) <- c("title","source","time","abs","link") 
      txt <- as.data.frame(txt, row.names= F)
    } #end if
  }
} ##抓一页百度新闻的数据

baidunews.keyword <- function(word) {

	# translate keyword to 16 bit character
	#kyword <- toupper( iconv( word ,"utf8" , "gb2312", "byte",toRaw=TRUE) [[1]])  #server
	kyword <- toupper( iconv( word ,"gb2312" , "gb2312", "byte",toRaw=TRUE) [[1]])  #local 字符集顺序-系统默认，网页，输出
	kyword <- paste(kyword ,collapse="%" ,sep='')
	kyword <- paste( "%", kyword, sep='')

	txt.keyword <- data.frame() # 初始化
	
for (i in 1:lastpg) { #页码循环

  p <- (i-1) * 50
  ul <- paste("http://news.baidu.com/ns?si=&tn=news&ie=gb2312&word=",kyword,"&pn=",p, sep="")
  txt.1p <- baidunews.1p( ul )  #抓取一页数据

  txt.1p$pgnum <- i
  txt.keyword <- rbind(txt.keyword , txt.1p)

  } #页码循环结束
   
#硬过滤
#txt.keyword <- txt.keyword [  !is.na(txt.keyword$time), ] 
#txt.keyword$abs <- gsub( "条相似新闻", "", txt.keyword$abs )
#txt.keyword$abs <- gsub( "条相同新闻", "", txt.keyword$abs )
#txt.keyword$abs <- gsub( "百度快照", "", txt.keyword$abs )

#抓取属性信息
txt.1p$systime <- Sys.Date() #加抓取时间
txt.keyword$channel <- "baidunews"
txt.keyword$kw <- word
data <- txt.keyword

}
#####################抓百度结束##################################

######################抓bing开始###################################

bingnews.1p <- function(x){  #获取所有标题结点，输入参数为板块地址；返回标题结点列表
    
    url1<-NULL
    try( url1<-htmlTreeParse(x, useInternalNodes=TRUE,encoding='utf8'),silent = F)
    
    m <- getNodeSet( url1, "//body//div[@class = 'newstitle']/a") 
    txt.title <- sapply( m , xmlValue , encoding="utf8") #标题
    txt.link <- sapply( m, function(el) xmlGetAttr(el, "href")) #链接
    m <- getNodeSet( url1, "//body//div[@class = 'sn_txt']//span[@class = 'sn_snip']") 
    txt.abs <- sapply( m , xmlValue, encoding = "utf8") #摘要
    m <- getNodeSet( url1, "//body//div[@class ='sn_oi']//cite[@class = 'sn_src']") 
    txt.source <- sapply( m , xmlValue, encoding = 'utf8') #来源
    m <- getNodeSet( url1, "//body//div[@class ='sn_oi']//span/span")
    txt.time <- sapply( m , xmlValue, encoding = 'utf8') #时间
    rm(m)
    txt <- cbind( txt.title, txt.source, txt.time, txt.abs, txt.link)
	txt <- unique(txt)
	names(txt) <- c("title","source","time","abs","link") 
    txt <- as.data.frame(txt, row.names= F)
}
    
    
bingnews.keyword <- function( word ) {  #循环抓取多页数据
  
  #kyword <- toupper( iconv( word ,"utf8" , "utf8", "byte",toRaw=TRUE) [[1]]) #server
	kyword <- toupper( iconv( word ,"gb2312" , "utf8", "byte",toRaw=TRUE) [[1]]) #local
	kyword<-paste(kyword ,collapse="%" ,sep='')
	kyword <- paste( "%", kyword, sep='') #单个关键字转为十六进制代码
  
  txt.keyword <- data.frame() # 初始化
  for (i in 1:lastpg) { #页码循环
  
    p <- (i-1) * 10
    ul <- paste("http://cn.bing.com/news/search?q=",kyword,"&first=",p, "&FORM=PONR", sep="")
    txt.1p<- bingnews.1p( ul )  #抓取一页数据
	txt.1p$pgnum <- i
	txt.keyword <- rbind(txt.keyword , txt.1p)
    } #页码循环结束
	
#抓取属性信息
txt.1p$systime <- Sys.Date() #加抓取时间
txt.keyword$channel <- "bingnews"
txt.keyword$kw <- word
data <- txt.keyword
}  
    
###################抓bing结束
    

########################抓soopat

#Warning messages:
#  1: In cbind(txt.title, txt.abs, txt.type, txt.status, txt.link, txt.author,  :
#                number of rows of result is not a multiple of vector length (arg 1)
#  2: In cbind(txt.title, txt.abs, txt.type, txt.status, txt.link, txt.author,  :
#                number of rows of result is not a multiple of vector length (arg 1)


#抓一页数据
soopat.1p <- function(x){
  
  Sys.sleep( round( runif(1, min=10, max=15)) )  #防抓取比较严，睡的时间长一点
  
  #isex <- url.exists(x) #ul存在与否的验证
  #if(isex){
    url1<-NULL
    try( url1 <- htmlTreeParse( x , useInternalNodes=T, encoding='utf8'),silent = F)
    if (!is.null(url1)){ #地址解析不成功的处理 
      
      #字段包含：类型，标题，评审状态，申请人，申请日，主分类，摘要，链接
      m <- getNodeSet(url1,"//h2[@class = 'PatentTypeBlock']/font")
      txt.type <- sapply(m, xmlValue, encoding="utf8")
      txt.type <- gsub( "[[:space:]]", "", txt.type) #类型
      
      m <- getNodeSet(url1,"//h2[@class = 'PatentTypeBlock']/a")
      ( txt.title <- sapply(m, xmlValue, encoding="utf8") ) #标题
      
      m <- getNodeSet(url1,"//h2[@class = 'PatentTypeBlock']/div")
      txt.status <- sapply(m ,xmlValue , encoding="utf8" ) #评审状态
      
      m <- getNodeSet(url1, "//span[@class = 'PatentAuthorBlock']" )
      txt.aa <- sapply (m, xmlValue , encoding ="utf8") 
      txt.aa <- gsub( "[[:space:]]" , "" , txt.aa)
      txt.aa <- strsplit( txt.aa, "-")
      txt.author <- sapply( txt.aa,  function(x)  x[1] ) #申请人
      txt.time <- sapply( txt.aa, function(x) paste(x[2] , x[3], x[4], sep="-" )) #申请日期
      txt.num <- sapply( txt.aa, function(x) x[5]) #主分类号
      
      m <- getNodeSet(url1, "//span[@class = 'PatentContentBlock']" )
      txt.abs <- sapply (m, xmlValue , encoding ="utf8") #摘要
      
      m <- getNodeSet(url1, "//h2[@class = 'PatentTypeBlock']/a" )
      txt.link <- sapply (m, function(el) xmlGetAttr(el, "href")) #摘要     
      txt.link <- paste(  "http://www.soopat.com/" , txt.link, sep="" )
      
      rm(m)
      
      #合体
      txt <- cbind( txt.title, txt.abs, txt.type, txt.status, 
                    txt.link, txt.author, txt.num)
	  txt <- unique(txt)
      if ( nrow( txt) > 0 ) {  txt <- as.data.frame(txt) } else {rm(txt)} #合体不成功的处理
    } else { rm(url1)} #end if 地址解析不成功的处理
 # }
} ##抓一页

soopat.keyword <- function( word ) {
  
  allpage <- data.frame()
  kyword <- toupper( iconv( word ,"gb2312" , "utf8", "byte",toRaw=TRUE) [[1]]) #local
  #kyword <- toupper( iconv( word ,"utf8" , "utf8", "byte",toRaw=TRUE) [[1]]) #server
  kyword<-paste(kyword ,collapse="%" ,sep='')
  kyword <- paste( "%", kyword, sep='')  #单个关键字转为十六进制代码
  
  for (i in 1:lastpg) { #页码循环
    
    p <- (i-1) * 10
    ul <- paste( "http://www.soopat.com/Home/Result?SearchWord=",
                 kyword, "&PatentIndex=", p, sep="")
    try( onepg <- soopat.1p( ul ) ) #抓取一页数据
    if ( class(onepg) == "data.frame" & nrow(onepg) > 1 ) { #抓取不成功的处理
      names(onepg) <- c("title","abs","type","status","link","author","num")
      onepg$kw <- word #函数输入的关键词，非16进制代码!
      onepg$systime <- Sys.Date() #加抓取时间
      row.names(onepg) <- NULL
      allpage <- rbind(allpage , onepg)
    } else { rm(onepg )} # endif 抓取不成功的处理
  } #页码循环完毕
  allpage <- unique(allpage) #对第j个关键字抓取的项目排重         
}

#####################抓soopat结束



