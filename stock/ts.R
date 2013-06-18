library(quantmod)
#setwd('E:/myRproj/trunk/stock')

#setSymbolLookup(King.Long=list(name="600686.ss",src="yahoo"))
#getSymbols("King.Long")
#getSymbols('CSI300',src='google')

###################################
#### 将一个向量化分为长度为k的区间段
#### 每个区间段为矩阵的列向量
ts.sliding<-function(x,k=5,rnd=FALSE,rnd.num=1000){
  x=as.vector(x)
  nlen=length(x)-k
  mat<-c()
  if(rnd==FALSE){
    for(i in 1:nlen){
      xs<-x[i:(i+k-1)]
      mat<-cbind(mat,xs)
    }
  }
  if(rnd==TRUE){
    ind<-sample(1:nlen,min(nlen,rnd.num))
    ind<-sort(ind)
    nn=length(ind)
    for(i in 1:nn){
      xs<-x[ind[i]:(ind[i]+k-1)]
      mat<-cbind(mat,xs)
    }
  }
  colnames(mat)<-NULL
return(mat)
  
}

#########################################
#### 在一幅图中显示矩阵x每一列的折线图
jigsaw.plot<-function(x){
  nc=ncol(x)
  m=sqrt(nc)
  if(m<floor(m)+0.5){
    m1=ceiling(m)
    m2=floor(m)
  }
  if(m>=floor(m)+0.5){
    m1=ceiling(m)
    m2=m1
  }
  op<-par(mar=rep(0,4),mfrow=c(m2,m1),bg='grey80')
  for(i in 1:nc){
    plot(x[,i],type='o',col=i%%2+2, axes = F,frame.plot=T)
  }
  par(op)
}
##########################################
mse<-function(x,y){
  nc=ncol(x)
  nr=nrow(x)
  x<-as.vector(x)
  y<-as.vector(y)
  sum((x-y)^2)/(nc*nr)
}