##ע�ⲻͬ�����µĴ���ת������local ���� server

########################�ٶ�����#############################
# ���·�װ���������������غ���ϴ�����⣬ֻ���ԭʼ���ݵĻ���

#ץһҳ����
baidunews.1p <- function(x){
  
  isex <- url.exists(x)
  if(isex){
    url1<-NULL
    try( url1 <- htmlTreeParse( x , useInternalNodes=T, encoding='utf8'),silent = F)
    if (!is.null(url1)){ 
      
      #����
      o <- getNodeSet(url1,"//body//div[@id = 'content_left']//li/h3/a")
      txt.title<-unlist(lapply(o, xmlValue, encoding="utf8"))
      #����
      txt.link <- unlist(lapply(o ,xmlAttrs , encoding="utf8" ) )
      txt.link <- txt.link[names(txt.link)=='href']  #<a>�а������Ԫ�أ�ֻȡ����  
      
      #���ں���Դ1 unicode <U+00A0> ��ʱû�������������!!!
      #o <- getNodeSet(url1, "//body//div[@id = 'content_left']//li/span" )
      #txt.aa <- sapply (o, xmlValue , encoding ="utf8")
      #txt.aa <- strsplit( txt.aa , format( Sys.time() , "%Y") , fixed = TRUE) #�ý�����
      #txt.source <- sapply(txt.aa , 
      #                  function(x) gsub( '[^dA-Za-z\u3007\u4E00-\u9FCB\uE815-\uE864]', '', x[1]))
      #txt.time <- sapply(txt.aa , function(x) x[2]) 
      
	  #���ں���Դ2  unicode <U+00A0> &nbsp; ȥ��
      o <- getNodeSet(url1, "//body//div[@id = 'content_left']//li/span" )
      txt.aa <- sapply (o, xmlValue , encoding ="utf8")
      #txt.cc <- strsplit( txt.aa ,  '[^0-9A-Za-z\u3007\u4E00-\u9FCB\uE815-\uE864]')
	  txt.cc <- strsplit( txt.aa ,  '[[:space:]]')
      txt.source <- sapply(txt.cc ,  function(x) x[2])
      txt.time <- sapply(txt.cc , function(x) paste( x[3], x[4], sep=" "))

      #ժҪ
      o <- getNodeSet(url1, "//body//div[@id = 'content_left']//li/div[@class='c-summary']")
      txt.abs <- sapply (o, xmlValue , encoding="utf8") 
      
      #����
	  rm(o)
      txt <- cbind(txt.title, txt.source, txt.time, txt.abs, txt.link) #����������Ȳ�ͬ�����Զ�ѭ���̵Ĳ����ϲ�
	  txt <- unique(txt)
	  names(txt) <- c("title","source","time","abs","link") 
      txt <- as.data.frame(txt, row.names= F)
    } #end if
  }
} ##ץһҳ�ٶ����ŵ�����

baidunews.keyword <- function(word) {

	# translate keyword to 16 bit character
	#kyword <- toupper( iconv( word ,"utf8" , "gb2312", "byte",toRaw=TRUE) [[1]])  #server
	kyword <- toupper( iconv( word ,"gb2312" , "gb2312", "byte",toRaw=TRUE) [[1]])  #local �ַ���˳��-ϵͳĬ�ϣ���ҳ�����
	kyword <- paste(kyword ,collapse="%" ,sep='')
	kyword <- paste( "%", kyword, sep='')

	txt.keyword <- data.frame() # ��ʼ��
	
for (i in 1:lastpg) { #ҳ��ѭ��

  p <- (i-1) * 50
  ul <- paste("http://news.baidu.com/ns?si=&tn=news&ie=gb2312&word=",kyword,"&pn=",p, sep="")
  txt.1p <- baidunews.1p( ul )  #ץȡһҳ����

  txt.1p$pgnum <- i
  txt.keyword <- rbind(txt.keyword , txt.1p)

  } #ҳ��ѭ������
   
#Ӳ����
#txt.keyword <- txt.keyword [  !is.na(txt.keyword$time), ] 
#txt.keyword$abs <- gsub( "����������", "", txt.keyword$abs )
#txt.keyword$abs <- gsub( "����ͬ����", "", txt.keyword$abs )
#txt.keyword$abs <- gsub( "�ٶȿ���", "", txt.keyword$abs )

#ץȡ������Ϣ
txt.1p$systime <- Sys.Date() #��ץȡʱ��
txt.keyword$channel <- "baidunews"
txt.keyword$kw <- word
data <- txt.keyword

}
#####################ץ�ٶȽ���##################################

######################ץbing��ʼ###################################

bingnews.1p <- function(x){  #��ȡ���б����㣬�������Ϊ����ַ�����ر������б�
    
    url1<-NULL
    try( url1<-htmlTreeParse(x, useInternalNodes=TRUE,encoding='utf8'),silent = F)
    
    m <- getNodeSet( url1, "//body//div[@class = 'newstitle']/a") 
    txt.title <- sapply( m , xmlValue , encoding="utf8") #����
    txt.link <- sapply( m, function(el) xmlGetAttr(el, "href")) #����
    m <- getNodeSet( url1, "//body//div[@class = 'sn_txt']//span[@class = 'sn_snip']") 
    txt.abs <- sapply( m , xmlValue, encoding = "utf8") #ժҪ
    m <- getNodeSet( url1, "//body//div[@class ='sn_oi']//cite[@class = 'sn_src']") 
    txt.source <- sapply( m , xmlValue, encoding = 'utf8') #��Դ
    m <- getNodeSet( url1, "//body//div[@class ='sn_oi']//span/span")
    txt.time <- sapply( m , xmlValue, encoding = 'utf8') #ʱ��
    rm(m)
    txt <- cbind( txt.title, txt.source, txt.time, txt.abs, txt.link)
	txt <- unique(txt)
	names(txt) <- c("title","source","time","abs","link") 
    txt <- as.data.frame(txt, row.names= F)
}
    
    
bingnews.keyword <- function( word ) {  #ѭ��ץȡ��ҳ����
  
  #kyword <- toupper( iconv( word ,"utf8" , "utf8", "byte",toRaw=TRUE) [[1]]) #server
	kyword <- toupper( iconv( word ,"gb2312" , "utf8", "byte",toRaw=TRUE) [[1]]) #local
	kyword<-paste(kyword ,collapse="%" ,sep='')
	kyword <- paste( "%", kyword, sep='') #�����ؼ���תΪʮ�����ƴ���
  
  txt.keyword <- data.frame() # ��ʼ��
  for (i in 1:lastpg) { #ҳ��ѭ��
  
    p <- (i-1) * 10
    ul <- paste("http://cn.bing.com/news/search?q=",kyword,"&first=",p, "&FORM=PONR", sep="")
    txt.1p<- bingnews.1p( ul )  #ץȡһҳ����
	txt.1p$pgnum <- i
	txt.keyword <- rbind(txt.keyword , txt.1p)
    } #ҳ��ѭ������
	
#ץȡ������Ϣ
txt.1p$systime <- Sys.Date() #��ץȡʱ��
txt.keyword$channel <- "bingnews"
txt.keyword$kw <- word
data <- txt.keyword
}  
    
###################ץbing����
    

########################ץsoopat

#Warning messages:
#  1: In cbind(txt.title, txt.abs, txt.type, txt.status, txt.link, txt.author,  :
#                number of rows of result is not a multiple of vector length (arg 1)
#  2: In cbind(txt.title, txt.abs, txt.type, txt.status, txt.link, txt.author,  :
#                number of rows of result is not a multiple of vector length (arg 1)


#ץһҳ����
soopat.1p <- function(x){
  
  Sys.sleep( round( runif(1, min=10, max=15)) )  #��ץȡ�Ƚ��ϣ�˯��ʱ�䳤һ��
  
  #isex <- url.exists(x) #ul����������֤
  #if(isex){
    url1<-NULL
    try( url1 <- htmlTreeParse( x , useInternalNodes=T, encoding='utf8'),silent = F)
    if (!is.null(url1)){ #��ַ�������ɹ��Ĵ��� 
      
      #�ֶΰ��������ͣ����⣬����״̬�������ˣ������գ������࣬ժҪ������
      m <- getNodeSet(url1,"//h2[@class = 'PatentTypeBlock']/font")
      txt.type <- sapply(m, xmlValue, encoding="utf8")
      txt.type <- gsub( "[[:space:]]", "", txt.type) #����
      
      m <- getNodeSet(url1,"//h2[@class = 'PatentTypeBlock']/a")
      ( txt.title <- sapply(m, xmlValue, encoding="utf8") ) #����
      
      m <- getNodeSet(url1,"//h2[@class = 'PatentTypeBlock']/div")
      txt.status <- sapply(m ,xmlValue , encoding="utf8" ) #����״̬
      
      m <- getNodeSet(url1, "//span[@class = 'PatentAuthorBlock']" )
      txt.aa <- sapply (m, xmlValue , encoding ="utf8") 
      txt.aa <- gsub( "[[:space:]]" , "" , txt.aa)
      txt.aa <- strsplit( txt.aa, "-")
      txt.author <- sapply( txt.aa,  function(x)  x[1] ) #������
      txt.time <- sapply( txt.aa, function(x) paste(x[2] , x[3], x[4], sep="-" )) #��������
      txt.num <- sapply( txt.aa, function(x) x[5]) #�������
      
      m <- getNodeSet(url1, "//span[@class = 'PatentContentBlock']" )
      txt.abs <- sapply (m, xmlValue , encoding ="utf8") #ժҪ
      
      m <- getNodeSet(url1, "//h2[@class = 'PatentTypeBlock']/a" )
      txt.link <- sapply (m, function(el) xmlGetAttr(el, "href")) #ժҪ     
      txt.link <- paste(  "http://www.soopat.com/" , txt.link, sep="" )
      
      rm(m)
      
      #����
      txt <- cbind( txt.title, txt.abs, txt.type, txt.status, 
                    txt.link, txt.author, txt.num)
	  txt <- unique(txt)
      if ( nrow( txt) > 0 ) {  txt <- as.data.frame(txt) } else {rm(txt)} #���岻�ɹ��Ĵ���
    } else { rm(url1)} #end if ��ַ�������ɹ��Ĵ���
 # }
} ##ץһҳ

soopat.keyword <- function( word ) {
  
  allpage <- data.frame()
  kyword <- toupper( iconv( word ,"gb2312" , "utf8", "byte",toRaw=TRUE) [[1]]) #local
  #kyword <- toupper( iconv( word ,"utf8" , "utf8", "byte",toRaw=TRUE) [[1]]) #server
  kyword<-paste(kyword ,collapse="%" ,sep='')
  kyword <- paste( "%", kyword, sep='')  #�����ؼ���תΪʮ�����ƴ���
  
  for (i in 1:lastpg) { #ҳ��ѭ��
    
    p <- (i-1) * 10
    ul <- paste( "http://www.soopat.com/Home/Result?SearchWord=",
                 kyword, "&PatentIndex=", p, sep="")
    try( onepg <- soopat.1p( ul ) ) #ץȡһҳ����
    if ( class(onepg) == "data.frame" & nrow(onepg) > 1 ) { #ץȡ���ɹ��Ĵ���
      names(onepg) <- c("title","abs","type","status","link","author","num")
      onepg$kw <- word #��������Ĺؼ��ʣ���16���ƴ���!
      onepg$systime <- Sys.Date() #��ץȡʱ��
      row.names(onepg) <- NULL
      allpage <- rbind(allpage , onepg)
    } else { rm(onepg )} # endif ץȡ���ɹ��Ĵ���
  } #ҳ��ѭ�����
  allpage <- unique(allpage) #�Ե�j���ؼ���ץȡ����Ŀ����         
}

#####################ץsoopat����


