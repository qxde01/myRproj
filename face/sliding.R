library(EBImage)
#x=readImage('baby.jpg')
#x=resize(x,480,448)
#display(x,method="raster")
rgb2grey<-function(x){
  x<-0.3*x[,,1]+0.59*x[,,2]+0.11*x[,,3]
  x<-as.Image(x)
  colorMode(x)<-0
  x
}
######################################
#### 将一副图片按从上到下、从左至右的顺序划分为m*n小块，
#### 将小块转化为列向量，每一列代表一个小块
#### pat<-image.sliding(x,m=16,n=16)
#### patgrey<-image.sliding(x,m=16,n=16,type="grey")
#image.sliding<-function(x,w_size=512,h_size=448,m=8,n=8,type="RGB"){
image.sliding<-function(x,m=8,n=8,type="RGB",rnd=FALSE,samp=3000){
  ##顺序分割
  .sliding<-function(x,m=8,n=8){
    w=nrow(x);h=ncol(x)
    w_w=ceiling(w/m);h_h=ceiling(h/n)
    if(length(dim(x))==3){
      mat<-matrix(0,nrow=m*n*3,ncol=w_w*h_h)
      k=1
      for(i in 1:w_w){
        for(j in 1:h_h){
          pat<-x[((i-1)*m+1):(i*m),((j-1)*n+1):(j*n),]
          mat[,k]<-as.vector(pat)
          k=k+1
        }
      }
    }
    if(length(dim(x))==2){
      mat<-matrix(0,nrow=m*n,ncol=w_w*h_h)
      k=1
      for(i in 1:w_w){
        for(j in 1:h_h){
          pat<-x[((i-1)*m+1):(i*m),((j-1)*n+1):(j*n)]
          mat[,k]<-as.vector(pat)
          k=k+1
        }
      }
    }
    mat
  } 
  ##随机采样，仅支持RGB
  rnd.sliding<-function(x,m=8,n=8,samp=2000){
    w=nrow(x);h=ncol(x)
    ind1=sample(1:(w-m-1),samp,replace=T)
    ind2=sample(1:(h-n-1),samp,replace=T)
    ind<-unique(cbind(ind1,ind2))
    nn=nrow(ind)
    if(length(dim(x))==3){
      mat<-matrix(0,nrow=m*n*3,ncol=nn)
      for(i in 1:nn){ 
        mat[,i]<-as.vector(x[ind[i,1]:(ind[i,1]+m-1),ind[i,2]:(ind[i,2]+n-1),])   
      } 
    }
    if(length(dim(x))==2){
      mat<-matrix(0,nrow=m*n,ncol=nn)
      for(i in 1:nn){ 
        mat[,i]<-as.vector(x[ind[i,1]:(ind[i,1]+m-1),ind[i,2]:(ind[i,2]+n-1)])   
      } 
    }   
    return(mat)    
  }
  
  if(rnd==FALSE){
    if(length(dim(x))==3 & (type=="RGB"|type=="rgb")){
      pat<-.sliding(x,m=m,n=n)
    }
    if(length(dim(x))==3 & (type=="grey"|type=="GRAY")){
      x=0.3*x[,,1]+0.59*x[,,2]+0.11*x[,,3]
      pat<-.sliding(x,m=m,n=n)
    }
    if(length(dim(x))==2)  
      pat<-.sliding(x,m=m,n=n)
    
    return(pat)   
  }
  if(rnd==TRUE){
    pat<-rnd.sliding(x,m=m,n=n,samp=samp)
    return(pat)
  }
  
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
  x<-cbind(as.matrix(x))
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

####################################
#### 矩阵每列为一幅图像，转化为图像list
unit.image<-function(mat,m,n,type='RGB'){
  nc=ncol(mat)
  y=array(NA,dim=c(m,n,3))
  unitface<-vector('list')
  if(type=='grey'|type=='GREY')
    y=matrix(NA,nrow=m,ncol=n)
  for(i in 1:nc){
    unitface[[i]]<- sliding.merge(mat[,i],y=y,m=m,n=n,type=type)
  }
  unitface
}

###############################
#### 将由Image组成的list转化为列矩阵
list2mat<-function(x){
  n=length(x)
  p=nrow(x[[1]]);q=ncol(x[[1]])
  if(length(dim(x[[1]]))==3){
    mat<-matrix(0,nrow=p*q*3,ncol=n)
    for(i in 1:n){
      mat[,i]<-as.vector(image.sliding(x[[i]],m=p,n=q))
    }
  }
  if(length(dim(x[[1]]))==2){
    mat<-matrix(0,nrow=p*q,ncol=n)
    for(i in 1:n){
      mat[,i]<-as.vector(image.sliding(x[[i]],m=p,n=q))
    }
  }
  mat
}