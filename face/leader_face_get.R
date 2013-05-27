##第十二届全国人民代表大会常务委员会 15
#http://www.gov.cn/test/2013-03/14/content_2353702.htm
##中华人民共和国主席 2
#http://www.gov.cn/test/2013-03/14/content_2353936.htm
## 中华人民共和国国务院 11
#http://www.gov.cn/test/2013-03/16/content_2355707.htm
## 中华人民共和国中央军事委员会 3
#http://www.gov.cn/test/2013-03/14/content_2354022.htm
## 人民法院和人民检察院 1
#http://www.gov.cn/test/2013-03/15/content_2354748.htm
## 人民法院和人民检察院 1
#http://www.gov.cn/test/2013-03/15/content_2354748_2.htm
##政协第十二届全国委员会
#http://www.cppcc.gov.cn/zxww/zxww/zx/index.shtml
#http://www.cppcc.gov.cn/zxww/zxww/fzx/index.shtml
#http://www.cppcc.gov.cn/zxww/zxww/msz/index.shtml
source('sliding.R',encoding='utf-8')
library(EBImage)
library(RCurl)
library(XML)
library(jpeg)

strurl=c('http://www.gov.cn/test/2013-03/14/content_2353702.htm',
         'http://www.gov.cn/test/2013-03/14/content_2353936.htm',
         'http://www.gov.cn/test/2013-03/16/content_2355707.htm',
         'http://www.gov.cn/test/2013-03/14/content_2354022.htm',
         'http://www.gov.cn/test/2013-03/15/content_2354748.htm',
         'http://www.gov.cn/test/2013-03/15/content_2354748_2.htm')
imurls<-c()
for(i in 1:6){
  pagetree<-htmlParse(getURL(strurl[i],.encoding="UTF-8"),
                      encoding="UTF-8",asText = TRUE)
  node <- getNodeSet(pagetree, '//tr//p//img')
  imurl=sapply(node,function(x) xmlGetAttr(x, "src"))
  imurl=gsub('../../images/','',imurl)
  imurl=imurl[grep('.jpg',imurl)]
  cat(i,imurl,'\n')
  imurls<-unique(c(imurls,imurl))
}         
imurls<-imurls[-grep('http',imurls)]
n=length(imurls)
for(i in 1:n){
  headurl=paste0('http://www.gov.cn/test/images/',imurls[i])
  im<-readImage(headurl)
  im<-resize(im,96,128)
  #display(im,method="raster")
  imname<-paste0('leader/',100+i,'.png')
  png(imname,width=96,height=128)
  par(mar=rep(0,4))
  display(im,method="raster")
  dev.off()
}
##政协
strurl2=c('http://www.cppcc.gov.cn/zxww/zxww/zx/index.shtml',
         'http://www.cppcc.gov.cn/zxww/zxww/fzx/index.shtml',
         'http://www.cppcc.gov.cn/zxww/zxww/msz/index.shtml')
imurls2<-c()
for(i in 1:3){
  pagetree<-htmlParse(getURL(strurl2[i],.encoding="UTF-8"),
                      encoding="UTF-8",asText = TRUE)
  node <- getNodeSet(pagetree, '//div//img')
  imurl=sapply(node,function(x) xmlGetAttr(x, "src"))
  #imurl=gsub('../../images/','',imurl)
  imurl=imurl[grep('performance',imurl)]
  cat(i,imurl,'\n')
  imurls2<-unique(c(imurls2,imurl))
} 
n=length(imurls2)
for(i in 1:n){
  map_jpg <- getBinaryURL(imurls2[i])
  im<-readJPEG(map_jpg)
  writeJPEG(im,'im.jpg')
  im<-readImage('im.jpg')
  im<-resize(im,96,128)
  imname<-paste0('leader/',200+i,'.png')
  png(imname,width=96,height=128)
  par(mar=rep(0,4))
  display(im,method="raster")
  dev.off()
  file.remove('im.jpg')
}
#####
##对部分图片进行手工调整
## 顺序调整，前三位：主席、总理、副主席
## 使用OpenCV检测脸，并提取，48X48
##从图片中提取脸
face.extract<-function(path='leader/',w=48,h=48){
  files=dir(path,'.png')
  n=length(files)
  faces<-vector('list')
  for(i in 1:n){
    im<-readImage(paste0(path,files[i]))
  #  if(type==1){
  #    im<-im[18:84,10:92,]
  #    im<-resize(im,w=w,h=h)
  #  }
  #  if(type==2)
    im<-resize(im,w=w,h=h)
    faces[[i]]<-im
  }
  faces
}


leader.face<-face.extract(path='leader_face/',w=48,h=48)
png('leader.face.png',width=48*8,height=48*8)
display(combine(leader.face),method="raster",all=T)
dev.off()
save('leader.face',file='leader.face.rda')


test.face<-face.extract(path='test/',w=48,h=48)
png('test.face.png',width=48*5,height=48*5)
display(combine(test.face),method="raster",all=T)
dev.off()
save(test.face,file='test.face.rda')


head<-face.extract(path='leader/',w=96,h=128,type=2)
png('head.png',width=96*8,height=128*8)
display(combine(head),method="raster",all=T)
dev.off()

##### 测试脸
#testf<-dir('./test','.jpg')
#nn=length(testf)
#fn<-gsub('.jpg','',testf)
#for(i in 1:nn){
#  im<-readImage(paste0('test/',testf[i]))
#  im<-resize(im,48,64)
#  png(paste0('test/',10+i,'_',fn[i],'.png'),width=48,height=64)
#  par(mar=rep(0,4))
# display(im,method="raster")
#  dev.off()
#}

#x<-readImage('test/35_hu_jin_tao.jpg')
#y<-x[17:85,10:90,];dim(y)
#y<-resize(x,48,64)
#display(face.mean0,method="raster")
#png('35_hu_jin_tao.png',width=48,height=64)
#par(mar=rep(0,4))
#display(y,method="raster")
#dev.off()