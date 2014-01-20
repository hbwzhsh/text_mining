#有一个文件index.txt含有搜索的关键字 苹果手机 和对应的网页上的代码   %C6%BB%B9%FB%CA%D6%BB%FA

library(XML)
library(RCurl)
library(stringr)
setwd('D:/Dropbox/doc/public training/2.R Fundamentals/case')
id <- read.table('index.txt')
url <- paste('http://mini.s.taobao.com/search?tab=all&sort=sale-desc&fs=0&q=', as.character(id[1,2]), '&sortn=3&sort2=price', sep = '')
pagetree <- htmlParse(url, encoding = "GBK")
## 商品名称
nname <- iconv(sapply(getNodeSet(pagetree, "//body//h3[@class = 'summary']"), xmlValue),'UTF-8','GB2312')
nname <- str_trim(nname)
## 商品ID
itemurl <- sapply(getNodeSet(pagetree, "//body//h3[@class = 'summary']//a[@href]"), function(el) xmlGetAttr(el, "href"))
itemid <- gsub('.*\\?id=(\\d{8,11}).*', '\\1', itemurl)
## 价格
price <- iconv(sapply(getNodeSet(pagetree, "//body//div[@class = 'col price']"), xmlValue),'UTF-8','GB2312')
price <- str_trim(price)
price <- as.numeric(as.character((gsub('￥', '', price))))
## 最近成交额
sale <- iconv(sapply(getNodeSet(pagetree, "//body//div[@class = 'col dealing']"), xmlValue),'UTF-8','GB2312')
sale <- as.numeric(gsub('最近(\\d{0,5})人成交', '\\1', sale))
## 产地
place <- iconv(sapply(getNodeSet(pagetree, "//body//div[@class = 'col end loc']"), xmlValue),'UTF-8','GB2312')

data.frame(nname, itemid, price, sale, place)



## 京东
getJDName <-
function (item)
{
    Sys.sleep(0.5)
    url <- paste("http://item.jd.com/", item, ".html", sep = "")
    url1 <- htmlParse(url, encoding = "gbK")
    m <- getNodeSet(url1, "//body//div//h1")
    name <- iconv(xmlValue(m[[1]]), "utf-8", "gbk")
    purl <- paste("http://p.3.cn/prices/get?skuid=J_", item,
        "&type=1&callback=changeImgPrice2Num", sep = "")
    pr <- readLines(purl)
    price <- gsub(".*\\\"p\\\":\"(-{0,1}\\d{1,4})\\.\\d{2}.*",
        "\\1", pr)
    return(c(item, name, price))
}