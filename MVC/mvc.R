############################################
# A=cood[100,];B=contours[1,];C=contours[2,]
# compute tan(A/2)
tanAlpha<-function(A,B,C){
  a2=sum((B-C)^2)
  b2=sum((A-C)^2)
  c2=sum((A-B)^2)
  if (c2*b2==0) {out=100}
  else{
    cosA=(c2+b2-a2)/(2*sqrt(c2)*sqrt(b2))
    if (cosA==-1){out=100}
    else{
      out<-((1-cosA)/(1+cosA))^0.5
    } 
  } 
  return( out) 
}
############################################
## boudary and pixel points
imContour<-function(im,k=5){
  cols=nrow(im);rows=ncol(im)
  topX=1:cols;topY=rep(1,cols)
  bottomX=1:cols;bottomY=rep(rows,cols)
  leftX=rep(1,rows-2);leftY=2:(rows-1)
  rightX=rep(cols,rows-2);rightY=2:(rows-1)
  contours=cbind(X=c(topX,rightX,bottomX,leftX),
                 Y=c(topY,rightY,bottomY,leftY))
  contours=contours[seq(1,nrow(contours),by=k),]
  cood=matrix(NA,nrow=cols*rows,ncol=2)
  k=1
  for (i in 1:cols){
    for ( j in 1:rows){
      cood[k,]=c(i,j)
      k=k+1
    }
  }
  return(list(contours=contours,cood=cood))
}
############################################
## MVC
mvc<-function(x,contours){
  contours=rbind(contours[nrow(contours),],contours,contours[1,])
  n=nrow(contours)
  w<-c() 
  for(i in 2:(n-1)){
    w0<-(tanAlpha(x,contours[i-1],contours[i,])+
           tanAlpha(x,contours[i],contours[i+1,]))/sqrt(sum((x-contours[i,])^2))
    if(is.nan(w0) | is.na(w0)) {w0=0.0}
    w<-c(w,w0)
  }
  w[is.nan(w)]=1
  lamda=w/sum(w)
  #cat(w)
  return(lamda)
}
############################################
Collage<-function(objIm,bgIm,k=5){
  ccd=imContour(im=objIm,k=k)
  contours=ccd$contours
  cood=ccd$cood
  nr=nrow(contours);nc=ncol(contours) 
  m=nrow(cood)
  cat(' boundry points:',nr,'\n')
  cat(' mesh points:',m,'\n')
  lamda=matrix(0,nrow=m,ncol=nr)
  for(i in 1:m){
    lamda[i,]<-mvc(x=cood[i,],contours)
  }
  diff<-matrix(0,nrow=nr,ncol=3)
  for(i in 1:nr){
    diff[i,]=bgIm[contours[i,1],contours[i,2],]-
      objIm[contours[i,1],contours[i,2],]
  }
  resultIm=objIm
  na_seq<-c()
  kk=1
  for( i in 1:m){
    res=lamda[i,] %*% diff
    if(length(res[is.na(res)])>0){
      na_seq[kk]<-i
     # cat(i,'\n')
      kk=kk+1
    }
    else{
      res=as.vector(objIm[cood[i,1],cood[i,2],])+res
      res[res<0]=0;res[res>1]=1
      resultIm[cood[i,1],cood[i,2],]=res
    }
  }
  for(i in 1:(kk-1)){
    resultIm[cood[na_seq[i],1],cood[na_seq[i],2],]=(resultIm[cood[na_seq[i]+1,1],cood[na_seq[i]+1,2],]
                                                    +resultIm[cood[na_seq[i]+2,1],cood[na_seq[i]+2,2],])/2
  }
  return(resultIm)
}