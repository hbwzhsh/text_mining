

getTitleNode <- function(x){  #取主题列表，输入各板块url，返回每页的全部主题
  
  url1<-NULL
  try( url1 <- htmlTreeParse( x , useInternalNodes=T, encoding='utf8') ,silent = T)
  if (!is.null(url1)){ 
    #title
    o <- getNodeSet( url1, "//body//li[@class='ui-list-item']/div/a[@class='fn-highlight']")
    title <- sapply( o, xmlValue, encoding="utf8")
    title <- gsub('[^d0-9A-Za-z\u3007\u4E00-\u9FCB\uE815-\uE864]',' ',title) #非汉字字母数字以外
    
    #link
    o <- getNodeSet( url1, "//body//li[@class='ui-list-item']/div/a[@class='fn-highlight']")
    link <- sapply( o, function(el) xmlGetAttr(el, "href")) #链接需加前缀"http://www.liba.com/"
    link <- paste( "http://www.liba.com/", link, sep=""  )
    
    #author
    o <- getNodeSet( url1, "//body//li[@class='ui-list-item']/div[@class='ui-list-item-author']/a")
    author <- sapply( o, xmlValue )
    
    #update
    o <- getNodeSet( url1, "//body//li[@class='ui-list-item']/div[@class='ui-list-item-author']/a")
    date <- sapply( o, function(el) xmlValue( getSibling(el)) )  ###
    
    #rev&rpl
    o <- getNodeSet( url1, "//body//li[@class='ui-list-item']/div[@class='ui-list-item-click']/span")
    rpl <- sapply( o, xmlValue )
    
    #o <- getNodeSet( url1, "//body//li[@class='ui-list-item']/div[@class='ui-list-item-click']")
    #rev <- sapply( o, function(el) xmlValue(el ) ) #不会搞！！！！
    com <- as.data.frame( cbind(title, link, author, date, rpl)  )
    
  } #end if
}

getTitleDetail <- function(x) {  #输入主题url，输入主题帖子或回帖内容
  
  isex <- url.exists(x)
  if(isex){
    url1<-NULL
    try( url1 <- htmlTreeParse( x , useInternalNodes=T, encoding='utf8'),silent = F)
    #不try的话，由于网络问题，帖子无法访问时，程序会报错终止
    if (!is.null(url1)){ 
      #content
      o <- getNodeSet( url1, "//body//div[@class='ui-box-content']//div[@class='ui-topic-container']")
      content <- sapply( o, xmlValue, encoding="utf8")
      content <- gsub('[[:punct:]]','.',content) #各种标点符号
      content <- gsub('[[:space:]]' , '', content)
      #author
      o <- getNodeSet( url1, "//body//div[@class='ui-box-content']//a[@class='ui-topic-author-name']")
      author <- sapply( o, xmlValue, encoding="utf8")
      #time
      o <- getNodeSet( url1, "//body//div[@class='ui-topic-operate']/div[@class='fn-left']")
      time <- sapply( o, xmlValue, encoding="utf8")  
      #floor
      o <- getNodeSet( url1, "//body//div[@class='ui-topic-operate']/div[@class='fn-right']//div/span")
      floor <- sapply( o, xmlValue, encoding="utf8")  
      rm(url1) #这货非常大，不rm掉就会一直存，多了报错，注意不能放在最后一行
      data <- cbind( content, author, time, floor)
      
    } #end if
  }
  else{ return(NULL)}
}
