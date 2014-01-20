

getTitleNode <- function(x){  #ȡ�����б�����������url������ÿҳ��ȫ������
  
  url1<-NULL
  try( url1 <- htmlTreeParse( x , useInternalNodes=T, encoding='utf8') ,silent = T)
  if (!is.null(url1)){ 
    #title
    o <- getNodeSet( url1, "//body//li[@class='ui-list-item']/div/a[@class='fn-highlight']")
    title <- sapply( o, xmlValue, encoding="utf8")
    title <- gsub('[^d0-9A-Za-z\u3007\u4E00-\u9FCB\uE815-\uE864]',' ',title) #�Ǻ�����ĸ��������
    
    #link
    o <- getNodeSet( url1, "//body//li[@class='ui-list-item']/div/a[@class='fn-highlight']")
    link <- sapply( o, function(el) xmlGetAttr(el, "href")) #�������ǰ׺"http://www.liba.com/"
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
    #rev <- sapply( o, function(el) xmlValue(el ) ) #����㣡������
    com <- as.data.frame( cbind(title, link, author, date, rpl)  )
    
  } #end if
}

getTitleDetail <- function(x) {  #��������url�������������ӻ��������
  
  isex <- url.exists(x)
  if(isex){
    url1<-NULL
    try( url1 <- htmlTreeParse( x , useInternalNodes=T, encoding='utf8'),silent = F)
    #��try�Ļ��������������⣬�����޷�����ʱ������ᱨ����ֹ
    if (!is.null(url1)){ 
      #content
      o <- getNodeSet( url1, "//body//div[@class='ui-box-content']//div[@class='ui-topic-container']")
      content <- sapply( o, xmlValue, encoding="utf8")
      content <- gsub('[[:punct:]]','.',content) #���ֱ�����
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
      rm(url1) #����ǳ��󣬲�rm���ͻ�һֱ�棬���˱�����ע�ⲻ�ܷ������һ��
      data <- cbind( content, author, time, floor)
      
    } #end if
  }
  else{ return(NULL)}
}