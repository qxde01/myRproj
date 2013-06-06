setwd('E:/myRproj//trunk/face')
source('sliding.R',encoding='utf-8')
library(spams)
load('rdata/leader.face.rda')
load('rdata/test.face.rda')
###############################
## 对批量图像随机取样或顺序分割
image.batch<-function(xlist,m=8,n=8,samp=1000,rnd=TRUE){
  nn=length(xlist)
  X<-c()
  if(rnd==TRUE){
    for(i in 1:nn){
      cat(i,'\n')
      X0<-image.sliding(x=xlist[[i]],m=m,n=n,type="RGB",rnd=rnd,samp=samp)
      X<-cbind(X,X0)
    }
  }
  if(rnd==FALSE){
    for(i in 1:nn){
      cat(i,'\n')
      X[[i]]<-image.sliding(x=xlist[[i]],m=m,n=n,type="RGB",rnd=rnd)
    }
  }
  X
}
####################################
##用字典D重建图像
raw.recon<-function(D,xlist,y=array(0,dim=c(48,48,3)),m=8,n=8){
  nn=length(xlist)
  raw<-vector('list')
  for(i in 1:nn){
    a=spams.omp(X=xlist[[i]],D=D,lambda1=0.15)
    recx<-D%*%a
    raw[[i]]<-sliding.merge(recx,y=y,m,n)
  }
  raw
}
m=4;n=4
X=image.batch(x=leader.face,m=m,n=n,samp=1500)
## nnsc 字典学习，K=256,ncol(X)>50000
nnsc_D<-spams.nnsc(X=X,K=256,lambda1=0.05)

#save(nnsc_D,file='rdata/nnsc_D_face.rda')
nnsc_face<-unit.image(nnsc_D,m=m,n=n,type='RGB')
png('rpng/nnsc_Feature_Face.png',width=m*16,height=n*16)
display(combine(nnsc_face)*1/max(nnsc_D),method="raster",all=T)
dev.off()


library("ttutils")
## 合并训练图像和测试图像
all.face<-merge(leader.face,test.face)
## 每幅图片顺序分割
all_slide<-image.batch(x=all.face,m=m,n=n,rnd=F)
## 用字典D重建测试图像
rec.all.face<-raw.recon(D=nnsc_D,xlist=all_slide,
                    y=array(0,dim=c(48,48,3)),m=m,n=n)
png('rpng/all_rec_nnsc.png',width=48*10,height=48*9)
display(combine(rec.all.face),method="raster",all=T)
dev.off()
###########################
## 使用字典D重建的均方误差
DLrec.error<-function(x,y){
  n=length(x)
  p=nrow(x[[1]]);q=ncol(x[[1]])
  mse<-c()
  if(length(dim(x[[1]]))==3)N=p*q*3
  if(length(dim(x[[1]]))==2)N=p*q
  for(i in 1:n){
    mse[i]<-sum((x[[i]]-y[[i]])^2)/N
  }
  mse  
}

dlerr<-DLrec.error(x=all.face,y=rec.all.face)
indx<-order(dlerr) 
dlerr<-dlerr[indx]
cl=indx;cl[cl<58]=2;cl[cl>57]=3
s=mean(dlerr)*0.03

png('rpng/nnsc_rec_err.png',width=800,height=480)
par(mar=c(2,4,2,0.5))
plot(dlerr,col=cl,ylab='重建MSE',xlab='',
     main='使用NNSC重建训练和测试图像')
legend('topleft',col=2:3,c('训练集','测试集'),pch = 1)
text(which(indx==1),dlerr[indx==1]+s,'主席',col=4,cex=1)
text(which(indx==2),dlerr[indx==2]+s,'总理',cex=1,col=4)
text(which(indx==3),dlerr[indx==3]-s,'副主席',cex = 1,col=4)
text(which(indx==4),dlerr[indx==4],'委员长',cex = 1,col=4)

text(which(indx==58),dlerr[indx==58],'薄熙来',cex = 1,col=3)
text(which(indx==59),dlerr[indx==59]-1.5*s,'苍井空',cex = 1,col=3)
text(which(indx==63),dlerr[indx==63]+s,'郭德纲',cex = 1,col=3)
text(which(indx==64),dlerr[indx==64],'蒋介石',cex = 1,col=3)
text(which(indx==65),dlerr[indx==65]-s,'乔布斯',cex = 1,col=3)
text(which(indx==66),dlerr[indx==66],'雷军',cex = 1,col=3)
text(which(indx==68),dlerr[indx==68],'李连杰',cex = 1,col=3)
text(which(indx==70),dlerr[indx==70],'马英九',cex = 1,col=3)
text(which(indx==71),dlerr[indx==71]+s,'马云',cex = 1,col=3)
text(which(indx==72),dlerr[indx==72],'毛泽东',cex = 1,col=2)
text(which(indx==74),dlerr[indx==74]+s,'奥巴马',cex = 1,col=3)
text(which(indx==77),dlerr[indx==77],'章子怡',cex = 1,col=3)
text(which(indx==82),dlerr[indx==82]+1.5*s,'胡锦涛',cex = 1,col=2)
text(which(indx==80),dlerr[indx==80]-s,'周鸿祎',cex = 1,col=3)
dev.off()

## 每幅图片顺序分割
#face_slide<-image.batch(x=leader.face,m=m,n=n,rnd=F)
## 用字典D重建图像
#raw.face<-raw.recon(D=nnsc_D,xlist=face_slide,y=array(0,dim=c(48,48,3)),m=8,n=8)
  
#display(combine(raw.face),method="raster",all=T)
## 测试图像顺序分割
#test_slide<-image.batch(x=test.face,m=m,n=n,rnd=F)
## 用字典D重建测试图像
#test.face<-raw.recon(D=nnsc_D,xlist=test_slide,y=array(0,dim=c(48,48,3)),m=8,n=8)

#display(combine(test.face),method="raster",all=T)



