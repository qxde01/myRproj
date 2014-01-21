#################################################################
## 数据获取
setwd('D:/2013')
source('movie_tag_get.R')
movie_china<-movie_tag_get(tag='中国电影',start=1,end=600)
movie_tw<-movie_tag_get(tag='台湾电影',start=1,end=138)
movie_HK<-movie_tag_get(tag='香港电影',start=1,end=372)
movie_zh<-movie_tag_get(tag='大陆电影',start=1,end=50)
movie_ch02<-movie_tag_get(tag='国产电影',start=91,end=150)
##225
#
#save(movie_HK,file='movie_HK.rda')

movie_china<-unique(rbind(movie_china,movie_HK,movie_tw,movie_zh,movie_ch))
movie_china<-movie_china[order(movie_china$movieid),]
dim(movie_china)
a<-sort(table(movie_china$movieid),decreasing=T)
b=row.names(a[a>1])
m1<-movie_china[!movie_china$movieid %in% b,]
m2<-movie_china[movie_china$movieid %in% b,]
library(sqldf)
m2<-data.frame(id=1:nrow(m2),m2)

m=sqldf('select * from m2 A  where 
        id=(select max(id) from m2 where movieid=A.movieid)
        order by id')

m=m[,-1]  
for (i in 1:17){
  Encoding(m[,i])<-'UTF-8'
}

movie_china<-rbind(m1,m)
row.names(movie_china)<-NULL
save(movie_china,file='movie_china.rda')
#movie_china$episodes<-gsub('[^0-9]','',movie_china$episodes)
#movie_china$score<-as.numeric(movie_china$score)
#movie_china$collect_count<-as.numeric(movie_china$collect_count)
movie_china$episodes<-as.numeric(movie_china$episodes)
save(movie_china,file='movie_china.rda')
##########################################
load('movie_china.rda')

movie<-movie_china[grep('中国电影|台湾电影|香港电影|大陆电影|国产电影',
                        movie_china$tags),]
movie<-movie[-grep('电视剧',movie$tags),]
movie<-movie[!(movie$episodes>1 & !is.na(movie$episodes)),]
names(movie_china);dim(movie)
movie=movie[-grep('澳大利亚电影|日本电影|德国电影|印度电影|外国电影|联邦电影',
                  movie$tags),]
movie=movie[-grep('西班牙电影|新加坡电影|法国电影|韩国电影|苏联电影|英国电影',
                  movie$tags),]
row.names(movie)<-NULL
save(movie,file='movie.rda')
############################################################
library(wordcloud)
library("RJSONIO")
library("igraph")
## 根据年代统计电影数量
##########################################################
date_extract<-function(x){
  x<-unlist(strsplit(x,','))
  if(length(grep('中国大陆|中国内地',x))>0){
    x<-x[grep('中国大陆',x)][1]
  }
  else if(length(grep('香港',x))>0){
    x<-x[grep('香港',x)][1]
  }
  else if(length(grep('台湾',x))>0){
    x<-x[grep('台湾',x)][1]
  }
  else if(length(x)>0){
    x<-x[1]
  }
  else{
    x<-NA
  }
  x<-substr(x,1,4)
  x<-gsub('[^0-9]','',x)
  names(x)<-NULL
  return(unique(x))
}
##########################################################
pubdate<-sapply(movie$pubdate,date_extract)
names(pubdate)<-NULL
year<-gsub('[^0-9]','',movie$year)
year<-year[nchar(year)==4]
year.count<-table(year[year>1944 & year<2014])
#YY<-cbind(pubdate,substr(year,1,4))
#yy=YY[YY[,1]!=YY[,2],]
#yy=yy[!is.na(yy[,1]) & !is.na(yy[,2]),]
#pubdate<-pubdate[!is.na(pubdate)]
#pubdate<-pubdate[nchar(pubdate)==4]
#sum(table(pubdate))

date.count<-table(pubdate[!is.na(pubdate)& pubdate>1944 & pubdate<2014])
png('movie_number.png',width=720,height=540)
plot(date.count,type='o',ylab='数量',xlab='年代',col=2,ylim=c(0,500),
     main='中国电影数量趋势(1945-2013)')
#text(2010,690,paste('2010年',max(date.count),'部',sep=''),col='red')
#text(2012.1,530,paste('2013年',533,'部',sep=''),col='red')
lines(year.count,type='o',col='blue')
legend("topleft", lty = 1, xjust = 1, yjust = 1,col=c('red','blue'),
       legend = c(paste("首映时间--",sum(date.count),'部',sep=''),
                  paste("拍摄时间--",sum(year.count),'部',sep='')),
       title = "图例")
dev.off()
#hist(table(pubdate[pubdate>1970 & pubdate<2014]))

gdp<-read.csv('gdp.csv',stringsAsFactors =F)
plot(gdp$Year,gdp$GDP,type='o',col=2,xlab='年代',ylab='GDP',
     main='中国GDP 1980-2012')
#lines(gdp$Year,gdp$GNI,type='o',col=3)
#plot(log(gdp$GDP),date.count[-34],type='o',xlab='log(GDP)')
####################################################################
## 类型
extract<-function(x){
  unlist(strsplit(x,'[饰,— ]|/|-|、'))
}
##################################
cloud<-function(x=movie$type,width=400,height=400,filename=NULL){
  type=unlist(sapply(x,extract))
  names(type)<-NULL
  type.count<-sort(table(type),decreasing=T)
  df<-data.frame(words=names(type.count),
                freq=type.count[],stringsAsFactors =F)
  df<-df[nchar(df$words)>1,]
  row.names(df)<-NULL
  library(wordcloud)
  png(paste0('movie_',filename,'.png'),width,height)
  wordcloud(words=df$words,
            freq=df$freq,scale=c(7,1.5),
            min.freq=10,colors=rainbow(length(table(df[,2]))),
            random.order=F)
  dev.off()
  return(df)
}
type.df=cloud(x=movie$type,filename='type')
type.df2013=cloud(x=movie$type[pubdate=='2013'],filename='type_2013')
png('movie_type_bar.png',width=1200,height=640)
par(mfrow=c(1,2),mar=c(2,2,1,1))
barplot(type.df[1:30,2],col=rainbow(30),main='电影类型')
barplot(type.df2013[,2],col=rainbow(30),main='2013年电影类型')
dev.off()
#################################################
##导演 & 编剧 & 演员
director<-gsub('[a-zA-Z0-9]|（[^）（]*.）|\\([^\\)\\(]*.\\)|\\.|-',
               '',movie$director)
director.df=cloud(x=director,width=1200,
                  height=1200,filename='director')
director.df2013=cloud(x=director[pubdate=='2013'],
                      width=1500,height=1500,filename='director_2013')

writer<-gsub('[a-zA-Z0-9]|（[^）（]*.）|\\([^\\)\\(]*.\\)|-|\\.',
               '',movie$writer)
writer.df=cloud(x=writer,width=720,height=720,filename='writer')
writer.df2013=cloud(x=writer[pubdate=='2013'],width=1200,
                    height=1200,filename='writer_2013')

cast<-gsub("[a-zA-Z0-9]|（[^）（]*.）|\\([^\\)\\(]*.\\)|-|\\.|'",
             '',movie$cast)
cast.df=cloud(x=cast,width=1500,height=1500,filename='cast')
cast.df2013=cloud(x=cast[pubdate=='2013'],
                  width=1500,height=1500,filename='cast_2013')
#################################################
df.table<-data.frame(type=type.df[1:20,1],
                     type_freq=type.df[1:20,2],
                     director=director.df[1:20,1],
                     director_freq=director.df[1:20,2],
                     writer=writer.df[1:20,1],
                     writer_freq=writer.df[1:20,2],
                     cast=cast.df[1:20,1],
                     cast_freq=cast.df[1:20,2],
                     stringsAsFactors =F)
row.names(df.table)<-NULL

df.table2013<-data.frame(type=type.df2013[1:20,1],
                     type_freq=type.df2013[1:20,2],
                     director=director.df2013[1:20,1],
                     director_freq=director.df2013[1:20,2],
                     writer=writer.df2013[1:20,1],
                     writer_freq=writer.df2013[1:20,2],
                     cast=cast.df2013[1:20,1],
                     cast_freq=cast.df2013[1:20,2],
                     stringsAsFactors =F)
row.names(df.table2013)<-NULL
write.csv(df.table,'table.csv',row.names=F,fileEncoding = "GB2312")
write.csv(df.table2013,'table2013.csv',row.names=F,fileEncoding = "GB2312")
hk<-movie[grep('香港|HK|hk',movie$tags),];dim(hk)
tw<-movie[grep('台湾|台灣',movie$tags),];dim(tw)
#######################################################################
## 评分
score<-movie$score
score2013<-score[pubdate==2013 & !is.na(pubdate)]
score2012<-score[pubdate==2012 & !is.na(pubdate)]
summary(score);summary(score2013)
png('movie_score_hist.png',width=720,height=480)
par(mar=c(4,4,2,2))
hist(score,freq=F,col='gray60',main='豆瓣电影评分分布',
     xlab='评分',ylab='概率密度')
lines(density(score),lwd=2)
lines(density(score2013),lwd=2,col=2)
lines(density(score2012),lwd=2,col=3)
legend("top", lty = 1, xjust = 1, yjust = 1,col=1:3,
       legend = c("全     体","2013年","2012年"),
       title = "图例")
dev.off()
#######################################################################
## 观看人数
collect_count<-movie$collect_count
m_collect=tapply(collect_count,pubdate,sum)
m_collect=m_collect[names(m_collect) %in% 1945:2013]
m_score=tapply(score,pubdate,mean)
m_score=m_score[names(m_score) %in% 1945:2013]
png('score_collect.png',width=800,height=540)
par(mar=c(4,0,2,0.1))
plot(1945:2013,m_score/max(m_score),type='o',yaxt='n',lwd=2,
     main='平均评分及观影人数',ylab='',xlab='年代')
par(new=TRUE)
plot(1945:2013,m_collect/max(m_collect),lwd=2,
     col=2,type='o',ylab='',xlab='',yaxt='n')
legend("topleft", lty = 1, xjust = 1, yjust = 1,col=1:2,
       legend = c("平均评分","观影人数"),
       title = "图例")
dev.off()
##########################################################
duration<-movie$duration
duration=duration[!is.na(duration)]
duration=unlist(strsplit(duration,','))
duration<-as.numeric(duration)
duration=duration[duration<200]
hist(duration)
#######################################################################
## 标签
library("RJSONIO")

tagtoDF<-function(x){
  JSONtoDF<-function(x){
    x<-fromJSON(x)
    df<-cbind(words=x$tag_label,freq=x$tag_freq)
    row.names(df)<-NULL
    return(df)
  }
  tag<-sapply(x,JSONtoDF)
  names(tag)<-NULL
  n=length(tag)
  tagg<-c()
  for(i in 1:n){
    tagg<-rbind(tagg,tag[[i]])
  }
  tagg<-as.data.frame(tagg,stringsAsFactors=F)
  tagg$words<-gsub('-|[/？%\\^$*：￥！、—#!&。.，★]','',tagg$words)
  tagg$freq<-as.integer(tagg$freq)
  all.tags<-tapply(tagg$freq,tagg$words,sum)
  all.tags<-data.frame(words=names(all.tags),freq=all.tags)
  all.tags[1:50,]
  all.tags<-all.tags[order(all.tags[,2],decreasing=T),]
  row.names(all.tags)<-NULL
  all.tags$words<-as.character(all.tags$words)
  return(all.tags)
}
#####################################
tags<-movie$tags
all_tags=tagtoDF(x=tags)
tags2013=tagtoDF(x=tags[pubdate==2013 & !is.na(pubdate)])
#all_tags[grep('电影',all_tags[,1]),]
save(all_tags,file='all_tags.rda')

png('movie_all_tags.png',width=1200,height=1200)
wordcloud(words=all_tags$words,
          freq=all_tags$freq,scale=c(7,1.5),max.words=1000,
          min.freq=10,colors=rainbow(length(table(all_tags[,2]))),
          random.order=F)
dev.off()
png('movie_tags2013.png',width=1200,height=1200)
wordcloud(words=tags2013$words,
          freq=tags2013$freq,scale=c(7,1.5),max.words=1000,
          min.freq=10,colors=rainbow(length(table(tags2013[,2]))),
          random.order=F)
dev.off()

#######################################################################
## 导演与演员
.todf<-function(x){
  x<-as.character(x)
  x<-gsub('[a-zA-Z0-9]|（[^）（]*.）|\\([^\\)\\(]*.\\)','',c(x[1],x[2]))
  left0=unlist(strsplit(x[1],' |,|/|——|、|，'))
  left0<-gsub('[\'　]| |-','',left0)
  left0<-left0[nchar(left0)>1]
  right0<-unlist(strsplit(x[2],',|、|，|/|饰|、|\\.\\.'))
  right0<-gsub('[\'　]| |-','',right0)
  right0<-right0[nchar(right0)>1]
  ''
  right<-rep(right0,length(left0))
  left<-rep(left0,rep(length(right0),length(left0)))
  df<-unique(cbind(left,right))
  df<-df[df[,1]!=df[,2],]
  return(df)
}
toDF<-function(x){
  x<-x[nchar(x[,1])>0 & nchar(x[,2])>0,]
  n=nrow(x)
  df<-c()
  for(i in 1:n){
    df0<-.todf(x[i,])
    #cat(i,'\n')
    df<-rbind(df,df0)
  }
  row.names(df)<-NULL
  return(unique(df))
}
#X=movie_china[1:1500,c('director','cast')]
#X<-X[nchar(X[,1])>0 & nchar(X[,2])>0,]
DCDF<-toDF(x=movie[,c('director','cast')])
save(DCDF,file='DCDF.rda')
WCDF<-toDF(x=movie[,c('writer','cast')])
save(WCDF,file='WCDF.rda')
DWDF<-toDF(x=movie[,c('director','writer')])
save(DWDF,file='DWDF.rda')
#############
library(igraph)

s1=c('王晶','陈凯歌','张艺谋','冯小刚','李安','姜文','徐克',
     '周星驰','成龙')
w1<-DCDF[DCDF[,1] %in% s1,]
g<-graph.data.frame(w1,directed=F)
dirctors<-unique(w1[,1])
actors<-unique(w1[,2])
col=rep(2,length(V(g)$name))
vcex=col
col[V(g)$name %in% dirctors]<-4
vcex[V(g)$name %in% actors]=1
png('Movie_director_cast_Graph.png',width=1200,height=1200)
#par(bg='black')
plot(g, layout=layout.kamada.kawai, vertex.size=col,
     vertex.label.dist=0, vertex.color=col+1, 
     edge.arrow.size=0.5,vertex.label=V(g)$name,vertex.label.cex=vcex,
     vertex.label.color=col+2)
title(main=list("导演与演员的合作关系",
                font=2,cex=2,col="blue"))
dev.off()
##########
s2=c('李连杰','刘德华','葛优','梁朝伟','姜文','成龙','柳岩','章子怡',
     '周润发','甄子丹','刘亦菲','王祖贤','周星驰','文章','曾志伟')
w2<-DCDF[DCDF[,2] %in% s2,]
g<-graph.data.frame(w2,directed=F)
dirctors<-unique(w2[,1])
actors<-unique(w2[,2])
col=rep(2,length(V(g)$name))
vcex=col-1
col[V(g)$name %in% s2]<-4
#vcex[match(V(g)$name,dirctors)]=1
vcex[V(g)$name %in% s2]=2.0
png('Movie_cast_director_Graph.png',width=1200,height=1200)
#par(bg='black')
plot(g, layout=layout.kamada.kawai, vertex.size=col,
     vertex.label.dist=0, vertex.color=col+1, 
     edge.arrow.size=0.5,vertex.label=V(g)$name,
     vertex.label.cex=vcex,
     vertex.label.color=col+2)
title(main=list("演员与导演的合作关系",
                font=2,cex=2,col="blue"))
dev.off()

w=unique(rbind(w1,w2))
dirctors<-unique(w[,1])
actors<-unique(w[,2])
g<-graph.data.frame(w,directed=F)
col=rep(2,length(V(g)$name));vcex=col

col[V(g)$name %in% dirctors]<-3
vcex[V(g)$name %in% dirctors]=2
vcex[V(g)$name %in% actors]=1.5
label.col=col
label.col[label.col==2]='red'
label.col[label.col==3]='yellow'
label=V(g)$name
label[!label %in% c(s1,s2)]=NA
png('Movie_cast_director_merge.png',width=1200,height=1200)
par(bg='black')
plot(g, layout=layout.kamada.kawai, vertex.size=2,
     vertex.label.dist=0, vertex.color=col+1, 
     edge.arrow.size=0.5,vertex.label=label,vertex.label.cex=vcex,
     vertex.label.color=label.col)
dev.off()

g1=graph.data.frame(DCDF,directed=F)
deg=centralization.degree(g1)
degdf=data.frame(name=V(g1)$name,degree=deg$res)
degdf<-degdf[order(degdf[,2],decreasing=T),]
####随机抽取节点
gd<-DCDF[sample(1:nrow(DCDF),6000),]
gg<-graph.data.frame(gd,directed=F)
dirctors<-unique(gd[,1])
actors<-unique(gd[,2])
col=rep(2,length(V(gg)$name))
col[V(gg)$name %in% dirctors]<-4
#col[V(gg)$name %in% dirctors]=4
#V(gg)$name
png('Movie_sample_Graph.png',width=2000,height=2000)
par(bg='black')
plot(gg, layout=layout.fruchterman.reingold, vertex.size=1.3,
    vertex.color=col,edge.arrow.size=0.5,vertex.label=NA)
dev.off()
#######################################################################
movie_high<-movie[movie$score>=8,];dim(movie_high)
#high.df=cloud(x=movie_high$type,filename='type_high')
high_tags=tagtoDF(x=movie_high$tags)
png('movie_high_tags.png',width=1200,height=1200)
wordcloud(words=high_tags$words,
          freq=high_tags$freq,scale=c(7,1.5),max.words=1000,
          min.freq=10,colors=rainbow(length(table(high_tags[,2]))),
          random.order=F)
dev.off()

highDCDF<-toDF(x=movie_high[,c('director','cast')])
#movie_wuxia<-movie[grep('武侠',movie$tags),];dim(movie_wuxia)
#highDCDF<-toDF(x=movie_wuxia[,c('director','cast')])
#wuxia.df=cloud(x=movie_wuxia$type,filename='type_wuxia')
gg<-graph.data.frame(highDCDF,directed=F)
dirctors<-unique(highDCDF[,1])
actors<-unique(highDCDF[,2])
col=rep(2,length(V(gg)$name))
col[V(gg)$name %in% dirctors]<-4
degree=centralization.degree(gg)$res
label.name=V(gg)$name
label.name[degree<15]=NA
png('Movie_high_Graph.png',width=1600,height=1600)
par(bg='black')
plot(gg, layout=layout.fruchterman.reingold, vertex.size=1.3,
     vertex.color=col,edge.arrow.size=0.5,
     vertex.label=label.name,vertex.label.cex=1.5,
     vertex.label.color='green')
dev.off()
wudf=data.frame(name=V(gg)$name,degree=degree)
wudf=wudf[order(wudf[,2],decreasing=T),]
wudf[1:10,1]
