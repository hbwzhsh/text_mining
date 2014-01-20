setwd("d:/data/0722999cv")

memory.size(F)
memory.size(T)
memory.size(NA)

#概述：999份文本简历的挖掘，
#1按照htm结构抓简历相应部分内容，例：简历自我评价，XML
#2语料库的清洗整理，生成dtm矩阵，全量和加权
#3描述统计分析和词云图，词关系图
#4主题模型探索
#问题1 数据导入泛适应；2正则优化-分词套用词库；3topic模型评价
#4.sougou研究报告的深入挖掘。。。


library(XML)
library(tm)
#数据导入1——依照htm结构抓取对应信息
l=999
p_comment <-"//div[@class='resume_p']"
p_work <-"//div[@class='zpResumeS']//td[@class='line150']" #工作经验的nodeset

basicinfo<-vector(length = l)
comment <- vector(length = l)
work1 <-vector(length = l)
work2 <-vector(length = l)
work3 <-vector(length = l)
work4 <-vector(length = l)

for (i in 1:l){t<-paste("第",i,"份简历.htm",sep='') 
               #抓自我评论
               comment[i]<-sapply(
                 getNodeSet(htmlParse(t,encoding='utf8'), p_comment),xmlValue)
               #抓工作经历             
               #doc<-htmlParse(t,encoding='utf8')
               #work1[i] <- as.character(sapply( getNodeSet(htmlParse(t,encoding='utf8'), p_work),xmlValue)[1])
               #work2[i] <- as.character(sapply( getNodeSet(htmlParse(t,encoding='utf8'), p_work),xmlValue)[2])
               #work3[i] <- as.character(sapply( getNodeSet(htmlParse(t,encoding='utf8'), p_work),xmlValue)[3])
               #work4[i] <- as.character(sapply( getNodeSet(htmlParse(t,encoding='utf8'), p_work),xmlValue)[4])
}

#定义一个函数，输入文本vector，切词，生成语料库,移除空格，标点，小写

vector2corpus <- function(x)
{
  library(rmmseg4j)
  corpus <- Corpus(VectorSource(mmseg4j(x)))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords,stopwords("english"))
  tm_map(corpus, stripWhitespace)
}

cmt <- vector2corpus(comment) #自我评价的语料库


#由语料库变成dtm矩阵

dtm_all<-DocumentTermMatrix(cmt, control=list(removeNumbers = TRUE) ) # 全量dtm
dtm_w1<-DocumentTermMatrix(cmt, control=list(
  removeNumbers = TRUE, #移除数字
  stopwords = FALSE, #中文里没定义字典的话没有用处
  weighting = weightTfIdf  #可选 weightTf, weightTfIdf, weightBin, and weightSMART
)   )

##描述统计分析

#词频选取
length( findFreqTerms(dtm_all, 1)) #全量dtm包含词的个数

lterm <- 30
terms_all <-vector(length = lterm)
terms_w1 <-vector(length =lterm)
for (i in 1:lterm) {
 terms_all[i] <-length (findFreqTerms(dtm_all, i))
 terms_w1[i] <-length (findFreqTerms(dtm_w1, i))}
plot(terms_all, xlab="freq of words", ylab="n of words", main="unweighted vs tf-idf",
     type="l", col="grey", lwd=3);
lines(terms_w1, col="blue" , lwd=3) #可见，加权的dtm在中频词的发现能力要强于不加权dtm

#高频词
findFreqTerms(dtm_w1,15) #频率参数由前面图参考确定
highfreqterms <- findFreqTerms(dtm_w1,15)

#把高频词导出来
freq <- as.data.frame( colSums( as.matrix(dtm_all)))
freq <- cbind(row.names(freq), freq)
names(freq) <- c("term","n")
library(sqldf)
freq <- sqldf("select * from freq where n>5 order by n desc")
write.csv(freq, "output/freqterms.csv"); rm(freq)


#wordcloud图
library(wordcloud)
dm<-t(as.matrix(dtm_w1)) #转置，排序，计算频数，然后绘图
v<-sort(rowSums(dm),decreasing=T)
d<-data.frame(word=names(v),freq=v)
col<-rep( c(rgb(0,0.32,0.61), rgb(1,0.6,0),"black", rgb(0.6,0.8,0)),20)
#标准御四色蓝、绿、黑、桔
wc<-wordcloud(d$word,d$freq,colors=col, min.freq=5,max.words=80  )

??wordcloud

#词的关系图
library(igraph) #参数要尝试
dtmwc<-as.matrix(removeSparseTerms(dtm_w1,0.98))
cordtm <- cor(dtmwc)
g=graph.adjacency((cordtm>(0.05)),mode="undirected",diag=F)
plot(g,layout=layout.fruchterman.reingold, edge.color=grey(0.5),vertex.size=5)

#高频词的相关词矩阵
hft <- sapply(highfreqterms, FUN=function(x) list(findAssocs(dtm_w1, x, 0.1)[1:5] ) )
hft #只能在本地看，没有合适的展示方式

#做topicmodel
dim(dtm_all) #999 1161
#预处理,using tf-idf value to choose terms
library("slam") #稀疏矩阵计算用的
summary(col_sums(dtm_all))
term_tfidf<-
  tapply(dtm_all$v/row_sums(dtm_all)[dtm_all$i],dtm_all$j,mean)*
  log2(nDocs(dtm_all)/col_sums(dtm_all>0))
summary(term_tfidf)#全部term的dfidf值的分布，用来做筛选
dtm4tp<-dtm_all[,term_tfidf>=1.4] #筛选term的参数参考中位数
dtm4tp<-dtm4tp[row_sums(dtm4tp)>0,]
summary(col_sums(dtm4tp));dim(dtm4tp) #降维之后的矩阵

library("topicmodels")
k<-10 #num of topics, 如何确定，不知道
SEED<-2013

r_tm<-list( VEM=LDA(dtm4tp,k=k,control=list(seed=SEED)) ,
          VEM_fixed=LDA(dtm4tp,k=k, control=list(estimate.alpha=FALSE,seed=SEED)),
         Gibbs=LDA(dtm4tp,k=k,method="Gibbs", control=list(seed=SEED,burnin=1000, thin=100,iter=1000)),
         CTM=CTM(dtm4tp,k=k, control=list(seed=SEED, var=list(tol=10^-4),em=list(tol=10^-3)))
            )
sapply(r_tm[1:3], slot, "alpha")
#a数值越小，更多文档被分在一个主题下的概率越高-k的选择越没用???

#the most likely topic for each doc is obtained by
Topic<-topics(r_tm[["VEM"]],2)
#最后的常数表示每个doc有几个topic，1就是唯一概率最大的那个topic，类推
terms<-terms(r_tm[["VEM"]],10)
write.csv(t(Topic),file="output/tpic.csv")
write.csv(terms,file="output/terms.csv")

