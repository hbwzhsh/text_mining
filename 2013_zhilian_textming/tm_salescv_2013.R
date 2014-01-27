#概述：999份文本简历的挖掘，
#1按照htm结构抓简历相应部分内容，例：简历自我评价，XML
#2语料库的清洗整理，生成dtm矩阵，用统计方法筛选词
#3描述统计分析和词云图，词关系图
#4主题模型探索


##data prepare
library(XML)
library(tm)
l=999
p_basic <- "//div[@class='baseinfo']" #xmlNodes
p_comment <-"//div[@class='resume_p']"
p_ind <- "//div[@class='itemCon']//td[6]"
#p_work <-"//div[@class='zpResumeS']//td[@class='line150']" # working exp.
basicinfo<-vector(length = l)
comment <- vector(length = l)
industry <- vector(length = l)
for (i in 1:l){t<-paste("D:/百度云同步盘/data/20130722 999salescv/第",i,"份简历.htm",sep='')       
  basicinfo[i] <- sapply( getNodeSet(htmlParse(t,encoding='utf8'), p_basic),xmlValue)
  comment[i] <- sapply( getNodeSet(htmlParse(t,encoding='utf8'), p_comment),xmlValue)
  industry[i] <- sapply( getNodeSet(htmlParse(t,encoding='utf8'), p_ind),xmlValue)
}
basicinfo <- gsub( "[ |\r\n\t]+" , ",", basicinfo)
basicinfo <- sapply(basicinfo, function(x) strsplit(x ,",") )
basicinfo <- do.call(rbind, basicinfo) #tern list to dataframe
row.names(basicinfo) <- NULL
for ( i in 1: nrow(basicinfo)) {
  if (  basicinfo[i,3] !="未婚" & basicinfo[i,3] !="已婚") 
  { 
    for ( j in ncol(basicinfo): 4)
          { basicinfo[i,j] = basicinfo[i,j-1]} #每一行数据向前窜一格
  }
}
gender <- basicinfo[,2]
age <- as.numeric( format(Sys.Date(), "%Y"))-as.numeric( basicinfo[,4] )
local <- gsub( "现居住于","",basicinfo[,7])
workyear <- as.numeric( gsub( "年工作经验", "", basicinfo[,8]))
industry1 <- as.character( lapply( industry, function(x) strsplit(x,"、")[[1]][1])) #last work
rm(basicinfo, i, j, p_basic, p_comment, t,l, industry, p_ind )


## deal with text
#分词
library(Rwordseg)
seg.txt <- segmentCN( comment , nature = T )
#source textmining.R
wordstyle(seg.txt)
keep <- "n,a,v,vn,ad, l, i, j, b, nz, s, z, an, "
seg.txt <- wordfilter.keep( seg.txt, keep)
seg.txt <- gsub("^c","", as.character(seg.txt))
##tm准备
corpus <- vector2corpus(seg.txt) 
#由语料库变成dtm矩阵
dtm <- DocumentTermMatrix( corpus , control = list(
  #weighting = weightTfIdf, #出于获得原始数据考虑，不加权
  wordLengths = c (2, Inf ), #长度为1的词去掉
  stopwords = c(""), #
  removePunctuation = TRUE,
  removeNumbers=TRUE
))

#dtm降维——基于tfidf
dtm.data <- as.data.frame( as.matrix( dtm))
#terms <- tf.idf(dtm.data)
terms <- atf.pdf(dtm.data)
terms <- terms[ terms > quantile(terms, prob=.25) ]
dtm.data <- dtm.data[, colnames(dtm.data) %in% names(terms)]


#wordcloud图
library(wordcloud)
#单图
dm<-t(as.matrix(dtm.data)) #转置，排序，计算频数，然后绘图
v<-sort(rowSums(dm),decreasing=T)
d<-data.frame(word=names(v),freq=v)
col<-rep( c(rgb(0,0.32,0.61), rgb(1,0.6,0),"black", rgb(0.6,0.8,0)),20)
#标准御四色蓝、绿、黑、桔
wc<-wordcloud(d$word,d$freq,colors=col, min.freq=5,max.words=80  )

#分组图
#性别分类
wordcloud.class( dtm.data, gender )
#年龄分类
year <- cut(age, quantile(age , prob =c(0,.25,.5,.75,1)) )
wordcloud.class( dtm.data, year )

#词的关系图
library(igraph);library(tm) #参数要尝试
dtmwc<-as.matrix(removeSparseTerms(dtm,0.8))
cordtm <- cor(dtmwc) #词相关矩阵
g=graph.adjacency((cordtm>(0.05)),mode="undirected",diag=F)
plot(g,layout=layout.fruchterman.reingold, edge.color=grey(0.5),vertex.size=5)


#做topicmodel
dim(dtm.data) #999 1161
dtm.tp <- dtm.data[ rowSums(dtm.data) >0 ,]
library("topicmodels")
k<-10 #num of topics, 如何确定，不知道
SEED<-2013

r_tm<-list( VEM=LDA(dtm.tp,k=k,control=list(seed=SEED)) ,
            VEM_fixed=LDA(dtm.tp,k=k, control=list(estimate.alpha=FALSE,seed=SEED)),
            Gibbs=LDA(dtm.tp,k=k,method="Gibbs", control=list(seed=SEED,burnin=1000, thin=100,iter=1000)),
            CTM=CTM(dtm.tp,k=k, control=list(seed=SEED, var=list(tol=10^-4),em=list(tol=10^-3)))
)
sapply(r_tm[1:3], slot, "alpha")
#a数值越小，更多文档被分在一个主题下的概率越高-k的选择越没用???
#the most likely topic for each doc is obtained by
Topic<-topics(r_tm[["VEM"]],2)
#最后的常数表示每个doc有几个topic，1就是唯一概率最大的那个topic，类推
term.topic<-terms(r_tm[["VEM"]],10)


