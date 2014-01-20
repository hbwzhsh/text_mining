#文本挖掘相关函数

wordstyle <- function(x) { #输入分词后的文本list，输出每个词性对应的词
  x <- unlist(x)
  x <-as.data.frame( cbind( x, names(x)))
  row.names(x) <- NULL  
  y <- unique( as.character( x[,2]) )
  for ( i in 1: length(y) )  {
    z <- unique( as.character( x[ x$V2 == y[i] , 1] ) ) [1:10]
    cat( y[i] , z, "\n")
  } #end for                      
}


charclear <- function(x) {  #去除标点，分词，以及多余空格
  library(tm)
  x <- as.character( x )
  x <- sub( '^c' , "", x) #分词结束，列表转向量的时候剩下了c(/)
  x <- Corpus( VectorSource( x))
  x <- tm_map(x, tolower)#小写规范
  x <- tm_map(x, removePunctuation)#除标点
  x <- tm_map(x, stripWhitespace)#移除多余空格
  x <- as.character(x)
}

#词性过滤
txt.reg <- sapply( txt.reg , function(x) #
  x[ names(x)!="uj" & names(x)!="c" & names(x)!="y"  & names(x)!="e"  & names(x)!="ud"
     & names(x)!="ul" & names(x)!="p"  & names(x)!="o"  & names(x)!="uv"  & names(x)!="uz"
     & names(x)!="d"  & names(x)!="m"  & names(x)!="en"  
     & names(x)!="f"  & names(x)!="y"  & names(x)!="u"  & names(x)!="ug"  & names(x)!="k"
     ]) 



################################################################3



charclear <- function(x) {  #去除标点，分词，以及多余空格
  library(tm)
  x <- as.character( x )
  x <- sub( '^c' , "", x) #分词结束，列表转向量的时候剩下了c(/)
  x <- Corpus( VectorSource( x))
  x <- tm_map(x, tolower)#小写规范
  x <- tm_map(x, removePunctuation)#除标点
  x <- tm_map(x, stripWhitespace)#移除多余空格
  x <- as.character(x)
}

# textrank
term.rank <- function(x) {
  
  require(tau)
  #顺序敏感的词共现
  vote <- textcnt( x, split=" ", method = "string", n=2L, decreasing = T)
  vnn <- strsplit( names(vote) , split=" ")
  v1 <- sapply( vnn, function(x) x[1])
  v2 <- sapply( vnn, function(x) x[2])
  vote <-as.data.frame( cbind( v1, v2, vote), row.names=F)
  names(vote) <- c("kw1", "kw2" , "freq")
  #vote <- vote[ vote[,1] %in% names(atf) & vote[,2] %in% names(tf), ]  #atfpdf
  vote <- vote
}



wordstyle <- function(x) { #输入分词后的文本list，输出每个词性对应的词
  x <- unlist(x)
  x <-as.data.frame( cbind( x, names(x)))
  row.names(x) <- NULL
 
  y <- unique( as.character( x[,2]) )
  for ( i in 1: length(y) )  {
    z <- unique( as.character( x[ x$V2 == y[i] , 1] ) ) [1:10]
    cat( y[i] , z, "\n")
  } #end for                     
}


xxx <- function(x) {
  
  # 计算rank系数至收敛
  ### Importing data as a matrix with columns ["KW1", "KW2", "Freq"] ###
  ### Here the following matrix "a" is considered as an directed graph,
  ### which means rows [x,y,n] and [y,x,n] have different directions.
  ### A row [x,y,n] means there is a link from x to y with weight n.
  
  # NOTICE: If "a" is NOT directed, run the following line
  #trdata = rbind(trdata, cbind(trdata[,2],trdata[,1],trdata[,3]));
  
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
  rm(aID, b, scoredData, trdata, vertexDataID, WS, WS_temp, con, d , dtm,
     epsilon, errorSeries, i, inListID, inListW, maxIteration, n, outSum , t, tr.abs,
     vertexData, vote.n1, vote.n2, vote.name, x)
  
} 
