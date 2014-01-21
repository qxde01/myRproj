library(Rdouban)
toDF<-function(x){
  duration=x$attribute$movie_duration
  episodes=gsub('[^0-9]','',x$attribute$episodes)
  if(length(duration)>0) {
    duration=unlist(strsplit(duration,' |分钟'))
    duration=gsub('[^0-9]','',duration)
    duration=paste(duration[nchar(duration)>0],collapse=',')
  }
  if(length(duration)==0){
    duration=NA
  }
  if(length(episodes)==0)episodes=NA
c(movieid=gsub('[^0-9]','',x$href),
  title=x$title[1],
  director=paste(x$attribute$director,collapse=','),
  writer=paste(x$attribute$writer,collapse=','),
  cast=paste(x$attribute$cast,collapse=','),
  pubdate=paste(x$attribute$pubdate,collapse=','),
  country=paste(x$attribute$country,collapse=','),
  language=paste(x$attribute$language,collapse=','),
  duration=duration,
  episodes=episodes,
  year=paste(x$attribute$year,collapse=','),
  type=paste(x$attribute$movie_type,collapse=','),
  score=as.numeric(x$rating[2]),
  collect_count=as.numeric(x$rating[3]),
  summary=x$summary,
  tags=toJSON(x$tags),
  image=x$image)
}

movie_tag_get<-function(tag='中国电影',start=1,end=10){
  #pages=as.integer(results/count)
  df<-matrix(nrow=(end-start+1)*20,ncol=17)
  k=1
  Encoding(tag)<-"GBK"
  tag<-URLencode(iconv(tag,to='UTF-8',toRaw=F))
  for (pg in start:end){
    u=paste0('https://api.douban.com/v2/movie/search?tag=',tag,
             '&start=',(pg-1)*20)
    cat('Getting from page:',pg,' ',u,'\n')
    p=.refreshURL(u,ssl.verifypeer = F)
    p=fromJSON(p)$subjects
    ids<-sapply(p,function(x)x$id)
    n=length(ids)
    for(i in 1:n){
      cat(' ',i,'  movieID=',ids[i],'\n')
      info<-toDF(get.movie.info(ids[i]))
      df[k,]<-info
      k=k+1
    }  
  }
  colnames(df)<-names(info)
  df<-as.data.frame(df,stringsAsFactors=F)
  #df$score<-as.numeric(df$score)
  #df$collect_count<-as.numeric(df$collect_count)
  return(df)
}