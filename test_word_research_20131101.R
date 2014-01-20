#全流程模块化——改造中-服务器数据访问未解决

#实验1009：全流程模块化——指定词，抓取内容，分词整理，过滤
#1.从相关数据源（专业网站）按照指定关键词获取文献数据；
#.提取并整理有价值的技术信息标签，包括所属公司、产品名称、具体型号、成熟程度、时间、评价等内容；
#.通过文本挖掘技术进行过滤和分类

setwd("D:/data/20130912se1") #win
setwd("/home/dingc") #ubuntu

# 1.1 获取数据源

library(RODBC)
con <- odbcConnect("mysql", uid="dingc", pwd="dingc" ,DBMSencoding = "UTF-8")
txtall <- sqlQuery(con, "select * from tech2")
close(con)
#write.csv(txtall, "aa.csv")

#2 提取文本标签

#文本加工过滤：用到两个字典，一个是技术词，一个是虚词表(虚词表去掉了因为会误伤，如识别的别)

t <- paste( txtall[,1], txtall[,4])
x <- t

require(Rwordseg)
#installDict( "D:/data/20130912se1/techword.txt", dictname="techword") #安装技术词典
library(tm)
x <- gsub( "[0-9]", "", x) 
x <- segmentCN(x, nature=TRUE)
x <- sapply( x , function(x)  
  x[ names(x)!="uj" & names(x)!="b" & names(x)!="f" & names(x)!="a"]) #安全起见，只做黑名单，删除虚词
x <- as.character( x )
x <- sub( 'c' , "", x,  fix=T) #分词结束，列表转向量的时候剩下了c(/)
x <- Corpus( VectorSource( x))
x <- tm_map(x, tolower)#小写规范
x <- tm_map(x, removePunctuation)#除标点
x <- tm_map(x, stripWhitespace)#移除多余空格
x <- as.character(x)


### 文本过滤完成

##提取标签，首选，制作标签词候选集

#基于词频的关键词提取
dtm <- DocumentTermMatrix( Corpus( VectorSource( x)) )
dtm.data <- as.data.frame( as.matrix( dtm))

# 对比tf-idf, 似乎ATF*pdf性能略好

tf.idf <- function(y) {
  t <- colSums(y) * 
    log2( nrow(y)/ apply(y , 2, function(x) sum( (x >0)==TRUE)) )
  t <- sort(t, decreasing = TRUE)
}

atf.pdf <- function(y) {
  tf.weight <-  sqrt( apply( y, 2, crossprod )) # 矩阵列向量求自己的内积再开方，得到|tfji|系数的分母
  abs.w <- t(apply( y, 1, function(x)  x/tf.weight ) )# |tfji| 得到
  ni <- apply( abs.w , 2, function(x) sum(x>0)) #包含词i的文档数的向量
  wi <- colSums(abs.w)/ dim(abs.w)[1] * exp( ni /dim(abs.w)[1])
  w.atfpdf <- sort(wi, decreasing = TRUE)
}

atf <- atf.pdf(dtm.data) #摘要关键词
tf <- tf.idf(dtm.data)

atf[1:30]
tf[1:30]

#基于词序的关键词提取

#生成路径（边、投票、link）集E-矩阵
require(tau)

tr.abs <- x
vote <- as.data.frame( as.matrix( textcnt( tr.abs[1], split = ' ', method = 'string', n=2L) )) #两两组词
for ( i in 2:length(tr.abs)) {
  b <- as.data.frame( as.matrix( textcnt( tr.abs[i], split = ' ', method = 'string', n=2L) ))
  vote <- rbind(b, vote)
} #所有文档的词对和频数堆在一起
#in 和out的不加区分
vote.name <- gsub("[0-9]", "", row.names(vote) )  #重复的词被标记了不同数字，要把数字去掉
vote.name <-  strsplit( vote.name , ' ')
vote.n1 <- unlist( lapply(vote.name, function(x) x[1]))
vote.n2 <- unlist( lapply(vote.name, function(x) x[2]))
row.names(vote) <- NULL
vote <- cbind( vote.n1,vote.n2, vote)
names(vote) <- c("kw1", "kw2" , "freq")

vote <- vote[which(vote[,1] != '' & vote[,2] != ''),]
vote <- vote[ vote[,1] %in% names(abs.atfpdf) & vote[,2] %in% names(abs.atfpdf), ]  #atfpdf
head(vote)
trdata <- as.matrix( vote)


# 计算rank系数至收敛
### Importing data as a matrix with columns ["KW1", "KW2", "Freq"] ###
### Here the following matrix "a" is considered as an directed graph, 
### which means rows [x,y,n] and [y,x,n] have different directions. 
### A row [x,y,n] means there is a link from x to y with weight n.

# NOTICE: If "a" is NOT directed, run the following line
trdata = rbind(trdata, cbind(trdata[,2],trdata[,1],trdata[,3]));

### Constructing data describing the network ###
# sortedOriginalData = a[order(a[,1],a[,2]),]; # not needed
vertexData = unique(as.vector(trdata[,c(1,2)])); # This is a vector of vertices
vertexDataID = cbind(vertexData, vID=1:length(vertexData)); # This is a vector of vertices together with a vector of their IDs

# An ID version of matrix "a":
aID = matrix(, nrow = nrow(trdata), ncol = ncol(trdata));
for (i in 1:nrow(trdata)) {
  aID[i,1] = vertexDataID[vertexDataID[,1] == trdata[i,1],2];
  aID[i,2] = vertexDataID[vertexDataID[,1] == trdata[i,2],2];
  aID[i,3] = trdata[i,3];
}

inListID = list(); # list of predecessors' IDs of each vertex 
for (i in 1:length(vertexData)) {
  inListID[[i]] = aID[aID[,2] == vertexDataID[i,2],1];
}
inListW = list(); # list of predecessors' weights of each vertex 
for (i in 1:length(vertexData)) {
  inListW[[i]] = aID[aID[,2] == vertexDataID[i,2],3];
}

# A list of the sum of outgoing link weights of each vertex:
outSum = array(0, dim = length(vertexData));
for (i in 1:length(vertexData)) {
  outSum[i] = sum(as.integer(trdata [ trdata [,1]==vertexData[i],3]));
}

### Evaluating the weighted score vector ###
d = 0.85; # damping factor
epsilon = 0.000001; # precision of convergence
maxIteration = 100; # maximum number of iteration steps
WS = array(1, dim = length(vertexData)); # weighted score vector
errorSeries = array(0, dim = maxIteration); # error series

for (n in 1:maxIteration) {
  WS_temp = array(0, dim = length(vertexData)); # temporary weighted score vector
  for (i in 1:length(vertexData)) {
    WS_temp[i] = (1-d) + d * ((as.integer(inListW[[i]]) / outSum[as.integer(inListID[[i]])]) %*% WS[as.integer(inListID[[i]])]);
  }
  errorSeries[n] = max(abs(WS - WS_temp));
  WS = WS_temp;
  if (errorSeries[n] < epsilon) {break;}
}

### Output as a ranking of words ###
scoredData = as.data.frame(cbind(vertexData,WS)); # combining the keywords and their scores
sortedData = scoredData[order(WS, decreasing=TRUE),]; # sorting.
sortedData
# sortedData is the full ranked keywords list
rm(aID, b, dtm.data, scoredData, trdata, vertexDataID, vote, WS, WS_temp, con, d , dtm,
   epsilon, errorSeries, i, inListID, inListW, maxIteration, n, outSum , t, tr.abs,
   vertexData, vote.n1, vote.n2, vote.name, x)

#几种方法得到的关键词
names(atf)[1:10]
names(tf)[1:10]
sortedData[1:10,1]

#取合集，得到标签超集（标签集）
i <- 200
lables0 <- names(atf) [names(atf)[1:i] %in% names(tf)[1:i]] 
lables0 <- lables0[ lables0 %in% sortedData[1:i,1] ]

#标签集并dtm变量集
out1 <- dtm.data[, names(dtm.data) %in% lables0]




##########关键词的可视化展示

library(sqldf)
vote.stat <- sqldf(" select kw1,kw2,sum(freq) as n from vote 
                   group by kw1,kw2 having kw1 != '' and sum(freq) >1 order by sum(freq) desc")

require(igraph)
#滤掉关系不大的词
vote1 <- vote[ vote[,1] %in% names(abs.atfpdf[1:50]) & vote[,2] %in% names(abs.atfpdf[1:50]), ]  #atfpdf
g <- graph.data.frame( vote1 , directed =F)
plot(g, edge.width = E(g)$weight)


