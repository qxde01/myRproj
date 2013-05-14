library(EBImage)
#setwd('E:/myRproj//trunk//face')

#x=readImage('baby.jpg')
#x=resize(x,480,448)
#display(x,method="raster")
######################################
#### 将一副图片按从上到下、从左至右的顺序划分为m*n小块，
#### 将小块转化为列向量，每一列代表一个小块
#### pat<-image.sliding(x,m=16,n=16)
#### patgrey<-image.sliding(x,m=16,n=16,type="grey")
#image.sliding<-function(x,w_size=512,h_size=448,m=8,n=8,type="RGB"){
image.sliding<-function(x,m=8,n=8,type="RGB"){
  .sliding<-function(x,m=8,n=8){
    w=nrow(x);h=ncol(x)
    w_w=ceiling(w/m);h_h=ceiling(h/n)
    mat<-matrix(0,nrow=m*n,ncol=w_w*h_h)
    k=1
    for(i in 1:w_w){
      for(j in 1:h_h){
        pat<-x[((i-1)*m+1):(i*m),((j-1)*n+1):(j*n)]
        mat[,k]<-as.vector(pat)
        k=k+1
      }
    }
    mat
  }  
  #x=resize(x,w_size,h_size)
  if(length(dim(x))==3){
    R=x[,,1]
    G=x[,,2]
    B=x[,,3]
  }
  if(type=="RGB"|type=="rgb"){
    if(length(dim(x))<3)
      stop('This is not a RGB image !')
    R_pat<-.sliding(R,m=m,n=n)
    G_pat<-.sliding(G,m=m,n=n)
    B_pat<-.sliding(B,m=m,n=n) 
    x_pat<-rbind(R_pat,G_pat,B_pat)
  }
  if(type=="grey"|type=="GREY"){
    if(length(dim(x))<3)
      x_pat<-.sliding(x,m=m,n=n)
    if(length(dim(x))==3){
      x=0.3*R+0.59*G+0.11*B
      x_pat<-.sliding(x,m=m,n=n)
    }    
  }
 return(x_pat)   
}
####################################
#### 将图像块合并成一幅图像
#### raw<-sliding.merge(x=pat,y=x,m=16,n=16)
#### display(raw,method="raster")
#### raw2<-sliding.merge(x=patgrey,y=x[,,1],m=16,n=16,type="grey")
#### display(raw2,method="raster")
sliding.merge<-function(x,y,m=8,n=8,type="RGB"){
  .merge<-function(x,y,m=8,n=8){
    n_col=ncol(x)
    w_w=ceiling(w/m);h_h=ceiling(h/n)
    #mat<-matrix(0,nrow=m*n,ncol=w_w*h_h)
    k=1
    for(i in 1:w_w){
      for(j in 1:h_h){
        pat<-x[,k]
        pat<-matrix(pat,nrow=m,ncol=n)
        y[((i-1)*m+1):(i*m),((j-1)*n+1):(j*n)]<-pat
        k=k+1
      }
    }
    y
  }
  w=nrow(y);h=ncol(y)
  yy=matrix(0,nrow=w,ncol=h)
  if(type=="RGB"|type=="rgb"){
    if(length(dim(y))<3)
      stop('This is not a RGB image !')
    if(length(dim(y))==3){
      r_pat<-x[1:(m*n),]
      g_pat<-x[(1+m*n):(2*m*n),]
      b_pat<-x[(1+2*m*n):(3*m*n),]
      im<-array(0,dim=c(w,h,3))
      im[,,1]<-.merge(x=cbind(r_pat),y=yy,m=m,n=n)
      im[,,2]<-.merge(x=cbind(g_pat),y=yy,m=m,n=n)
      im[,,3]<-.merge(x=cbind(b_pat),y=yy,m=m,n=n)
      im<-as.Image(im)
      colorMode(im)<-2
    } 
  }
  if(type=="grey"|type=="GREY"){
    if(length(dim(y))<3){
      im<-.merge(x=x,y=yy,m=m,n=n)
      im<-as.Image(im)
      colorMode(im)<-0
    }

    if(length(dim(y))==3){
      im=0.3*im[,,1]+0.59*im[,,2]+0.11*im[,,3]
      im<-as.Image(im)
      colorMode(im)<-0
    }    
  }
  return(im)
}