library(Rdouban)
library(Rwordseg)
####获取电影《云图》的豆瓣影评500篇
#### http://movie.douban.com/subject/3530403/reviews
reviews<-get_movie_reviews(movieid=3530403,n=500,verbose=TRUE)
reviews_info<-reviews$reviews_info

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

w3<-sapply(reviews_info$review,f_seg,stopwords,positive,negative)
w3<-t(w3)
row.names(w3)<-NULL
####
cloudAtlas<-cbind(reviews_info,word=w3[,1],wordPos=w3[,2],wordNeg=w3[,3])
row.names(cloudAtlas)<-NULL
#cloudAtlas<-cloudAtlas[!is.na(cloudAtlas$word),]
save(cloudAtlas,file='rda/cloudAtlas.rda')

#names(cloudAtlas)
rating=cloudAtlas$rating
 barplot(table(rating[!is.na(rating)]))
###############################################
#### http://movie.douban.com/subject/2049435/
#### 超人：钢铁之躯 Man of Steel
#### 840
superman<-get_movie_reviews(movieid=2049435,n=900,verbose=TRUE)
superman<-superman$reviews_info

s3<-sapply(superman$review,f_seg,stopwords,positive,negative)
s3<-t(s3)
superman<-cbind(superman,word=s3[,1],wordPos=s3[,2],wordNeg=s3[,3])
save(superman,file='rda/superman.rda')
rating=superman$rating
table(rating)