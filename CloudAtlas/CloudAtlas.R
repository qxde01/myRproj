library(Rdouban)
library(Rwordseg)
setwd("E:/myRproj/trunk/CloudAtlas/")
####获取电影《云图》的豆瓣影评500篇
#### http://movie.douban.com/subject/3530403/reviews
cloudAtlas<-get_movie_reviews(movieid=3530403,results=1000,count=20,verbose=TRUE)
save(cloudAtlas,file='cloudAtlas.rda')
cloudAtlas500<-cloudAtlas[1:500,]
stopwords=readLines('stopwords.txt')
positive=readLines('positive.txt')
negative=readLines('negative.txt')
insertWords("云图")
####中文分词，去掉停止词，提取情感词
f_seg<-function(x,stopwords,positive,negative){
  x<-gsub('\r|--','',x)
  seg<-segmentCN(x)
  seg<-seg[!seg %in% stopwords]
  wordPos<-seg[seg %in% positive]
  wordNeg<-seg[seg %in% negative]
  word<-paste(seg,collapse=' ')
  wordPos<-paste(wordPos,collapse=' ')
  wordNeg<-paste(wordNeg,collapse=' ')
  
  c(word,wordPos,wordNeg)
}

w3<-sapply(cloudAtlas500$review,f_seg,stopwords,positive,negative)
w3<-t(w3)
row.names(w3)<-NULL
####
cloudAtlas500<-cbind(cloudAtlas500,word=w3[,1],wordPos=w3[,2],wordNeg=w3[,3])
row.names(cloudAtlas500)<-NULL
#cloudAtlas<-cloudAtlas[!is.na(cloudAtlas$word),]
cloudAtlas500$word<-as.character(cloudAtlas500$word)
cloudAtlas500$wordPos<-as.character(cloudAtlas500$wordPos)
cloudAtlas500$wordNeg<-as.character(cloudAtlas500$wordNeg)
save(cloudAtlas500,file='rda/cloudAtlas500.rda')

#names(cloudAtlas)
rating=cloudAtlas500$rating
 barplot(table(rating[!is.na(rating)]))
###############################################
#### http://movie.douban.com/subject/2049435/
#### 超人：钢铁之躯 Man of Steel
#### 840

###############################################
##582，三体1：http://book.douban.com/subject/2567698/
##335 三体2：http://book.douban.com/subject/3066477/
##855 三体3：http://book.douban.com/subject/5363767/
threeBody1<-get.book.review(bookid=2567698,results=600)
save(threeBody1,file="threebody1.rda")
threeBody2<-get.book.review(bookid=3066477,results=400)
save(threeBody2,file="threebody2.rda")

threeBody3<-get.book.review(bookid=5363767,results=900)
save(threeBody3,file="threebody3.rda")

threeBody<-rbind(threeBody1,threeBody2,threeBody3)
save(threeBody,file="threebody.rda")
