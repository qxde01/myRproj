library(RCurl)
library(XML)
#iconv(title,from='UTF-8', to='GB2312')<[^<>]*>
#####################
#####获取wiki的一个词条下有连接的词条
#####
##keyword='Data_mining';lang='en'
##depth,连接深度
wiki_get<-function(keyword='Data_mining',lang='en',depth=3){
  link<-c()
  .get<-function(keyword=keyword,lang=lang){
    strurl=paste0('http://',lang,'.wikipedia.org/wiki/',keyword)
    cat('  Getting',strurl,'...\n')
    if(url.exists(strurl)){
      pagetree<- tryCatch(htmlParse(getURL(strurl,.encoding="UTF-8"),
                         encoding="UTF-8",asText = TRUE), 
                          error = function(e){NULL})
    }
    #pagetree <- htmlParse(getURL(strurl,.encoding="UTF-8"),
    #                      encoding="UTF-8",asText = TRUE)
    if(!is.null(pagetree)){
      title<-sapply(getNodeSet(pagetree, '//head//title'), xmlValue)
      title<-unlist(strsplit(title,' - 维基百科| - Wikipedia'))[1]
      node <- getNodeSet(pagetree, '//div//p//a')
      words<-data.frame(word=sapply(node, xmlValue),
                        url=sapply(node,function(x) xmlGetAttr(x, "href")),
                        stringsAsFactors=F)  
      sp<-'/wiki/Special:|/w/index.php|/wiki/Wikipedia:|#|/wiki/http:'
      words<-unique(words[-grep(sp,words$url) ,])
      row.names(words)<-NULL
      b<-sapply(gregexpr('/',words$url),length)
      words<-words[b<3,];row.names(words)<-NULL
      a<-grep("http:|https:|www.",words$word)
      if(length(a)>0){
        words<-words[-a,]
      }
      
      n=nrow(words)
      cat('There are',n,'word term link about ... ',keyword,' ...\n')
      link<-data.frame(left=rep(title,n),right=words$word,stringsAsFactors=F)
      urlword=gsub('/wiki/','',words$url) 
    }
    if(is.null(pagetree)){
      link=NULL
      urlword=NULL
    }
    list(link=link,urlword=urlword)
  }

  .get_bat<-function(keyword=keyword,lang=lang){
    one<-.get(keyword=keyword,lang=lang)
    link<-one$link
    urlword<-one$urlword
    url<-c()
    nn=length(urlword)
    #cat('There are',nn,'word term link about ... ',keyword,' ...\n')
    if(nn>0){
      for(i in 1:nn){
        if(lang=="zh"){
          z=iconv(URLdecode(urlword[i]),"UTF-8","gb2312")
          cat('Getting ',i,'word term about __ ',z,' __ \n')   
        }    
        else{
          cat('Getting ',i,'word term about __ ',urlword[i],' __ \n')
        }
        two<-.get(keyword=urlword[i],lang=lang)
        link<-rbind(link,two$link)
        url0<-two$urlword
        url<-unique(c(url0,url))
      } 
    }

  list(link,urlword=url)  
  }

  three<-.get_bat(keyword=keyword,lang=lang)
  link<-rbind(link,three$link)
  urlword<-three$urlword
  n=length(urlword)
  k=1
  while(k<=depth){
    k=k+1
    for(i in 1:n){
      four<-.get_bat(keyword=urlword[i],lang=lang)
      link<-rbind(link,four$link)
      urlword<-four$urlword
    }
  }
link
}

#dataming<-wiki_get(keyword='Data_mining',lang='en',depth=2)
#datamingzh<-wiki_get(keyword='数学挖掘',lang='zh',depth=2)
