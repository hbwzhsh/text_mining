
Book1 <- read.csv("D:/cloud/data/20131113 openquestion/Book1.csv")

library(Rwordseg)
library(tm)
comment <- as.character( Book1[,1] )

x <- segmentCN( comment, nature=TRUE)

#依照词性判断留那些

x <- sapply( x , function(x) 
  x[ names(x)!="uj" & names(x)!="c" & names(x)!="y"  & names(x)!="e"  & names(x)!="ud"
     & names(x)!="ul" & names(x)!="p"  & names(x)!="o"  & names(x)!="uv"  & names(x)!="uz"
     & names(x)!="d"  & names(x)!="m"  & names(x)!="en"  & names(x)!="t" 
     & names(x)!="f"  & names(x)!="y"  & names(x)!="u"  & names(x)!="ug"  & names(x)!="k"
     ]) 

x <- as.character( x )
x <- sub( '^c' , "", x) #分词结束，列表转向量的时候剩下了c(/)
x <- Corpus( VectorSource( x))
x <- tm_map(x, tolower)#小写规范
x <- tm_map(x, removePunctuation)#除标点
x <- tm_map(x, stripWhitespace)#移除多余空格
x <- as.character(x)


dtm <- DocumentTermMatrix( Corpus( VectorSource( x)) , control = list(
  wordLengths = c (2, Inf ), #只出现一次的词干掉
  removeNumbers=TRUE
))

findAssocs(dtm, "环保", 0.1)

data <-  as.data.frame ( as.matrix (dtm ))
names(data)

summary( colSums(data) )

data <- data[ , colSums(data) > 10]
names(data)
dist <- dist( data , method="euclidean" )
hc <- hclust( dist, "ward")
plclust ( hc, hang =-1  )
re <- rect.hclust( hc, k= 8 , border="red")
out.id <- cutree( hc , k = 8) 

table( out.id)

library(wordcloud)
stype <- out.id #类别变量
zz1 <- 1:length( stype )
tdm_matrix<-t( data )
unique_type<-unique(stype)
names(unique_type) <- c(1:8)

cluster_matrix<-sapply(unique_type,
                       function(type){apply(tdm_matrix[,zz1[stype==type]],1,sum)})


#colnames(cluster_matrix)<-unique_type
#colnames(cluster_matrix) <- c("智慧","健康","绿色","个性","舒适")
#pmatrix<-cluster_matrix[,c(1:5)]
comparison.cloud(  cluster_matrix ,
                   scale=c(2,1) , #字体最大及最小值
                   max.words=300,
                   random.order=FALSE,rot.per=.1,
                   colors=brewer.pal(ncol(cluster_matrix),"Dark2"),use.r.layout=FALSE,
                   title.size=1)



