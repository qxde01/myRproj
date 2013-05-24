#setwd('E:/myRproj//trunk/face')
source('sliding.R',encoding='utf-8')
library(EBImage)
#leader.face<-face.extract(path='leader/',w=48,h=64)
#save(leader.face,file='leader.face.rda')
#str(leader.face)
load('leader.face.rda')
png('faces.png',width=8*48,height=8*64)
display(combine(leader.face),method="raster",all=T)
dev.off()
######
face_mat<-matrix(0,nrow=48*64*3,ncol=57)
for(i in 1:57){
  face_mat[,i]<-as.vector(image.sliding(leader.face[[i]],m=48,n=64))
}
#####计算几种统计脸：0.25分位数、平均、中值、
#####                0.75分位数、标准差、中值绝对偏差
face.mean<-rowMeans(face_mat)
face.median<-apply(face_mat,1,median)
face.qu25<-apply(face_mat,1,function(x){quantile(x,0.25)})
face.qu75<-apply(face_mat,1,function(x){quantile(x,0.75)})
face.sd<-apply(face_mat,1,sd)
face.mad<-apply(face_mat,1,mad)
y=leader.face[[1]]
face.qu25<-sliding.merge(x=cbind(face.qu25),y=y,m=48,n=64)
face.mean<-sliding.merge(x=cbind(face.mean),y=y,m=48,n=64)
face.median<-sliding.merge(x=cbind(face.median),y=y,m=48,n=64)
face.qu75<-sliding.merge(x=cbind(face.qu75),y=y,m=48,n=64)
face.sd<-sliding.merge(x=cbind(face.sd),y=y,m=48,n=64)
face.mad<-sliding.merge(x=cbind(face.mad),y=y,m=48,n=64)

face.stat<-combine(face.qu25,face.mean,face.median,
                   face.qu75,face.sd,face.mad)

png('stats_face.png',width=144,height=136)
display(face.stat,method="raster",all=T)
dev.off()
#######
squre.sum<-function(x,y){
  x<-as.vector(x)
  y<-as.vector(y)
  n=length(x)
  sum((x-y)^2)/n
}
cosine.sim<-function(x,y){
  x<-as.vector(x)
  y<-as.vector(y)
  sum(x*y)/(sqrt(sum(x^2))*sqrt(sum(y^2)))
}
#######
m1<-apply(face_mat,2,squre.sum,face.mean)
m2<-apply(face_mat,2,squre.sum,face.median)
sim1<-apply(face_mat,2,cosine.sim,face.mean)
sim2<-apply(face_mat,2,cosine.sim,face.median)

ms<-cbind(mean.err=m1,mean.sim=1-sim1,median.err=m2,median.sim=1-sim2)
ind<-order(ms[,1])
ms<-ms[ind,]

########################################################
png('sim_err.png',width=720,height=320)
par(mfrow=c(1,2),mar=c(2,4,2,0.5))
##57位领导人的脸与平均脸的均方误差和余玄相似度
plot(ms[,1],type='o',col=2,ylim=c(min(ms[,1:2]),max(ms[,1:2])),
     ylab='MSE and cosine',xlab='',
     main='与平均脸的均方误差和余玄相似度')
lines(ms[,2],type='o',col=3)
text(which(ind==1),ms[ind==1,1],'主席',cex = 1.5)
text(which(ind==2),ms[ind==2,1],'总理',cex = 1.5)
legend('topleft',col=2:3,c('MSE','cosine'),pch = 1)
#text(1.2,ms[1,2],'张宝文')
#text(57,ms[57,2],'严隽琪')
##中值脸的均方误差和余玄相似度
plot(ms[,3],type='o',col=2,ylim=c(min(ms[,3:4]),max(ms[,3:4])),
     ylab='MSE and cosine',xlab='',
     main='与中值脸的均方误差和余玄相似度')
lines(ms[,4],type='o',col=3)
text(which(ind==1),ms[ind==1,3],'主席',cex = 1.5)
text(which(ind==2),ms[ind==2,4],'总理',cex = 1.5)
legend('topleft',col=2:3,c('MSE','cosine'),pch = 1)
#text(1,ms[1.2,3],'张宝文')
#text(57,ms[57,4],'严隽琪')
dev.off()

################################
#### 利用svd重建face
svd.recon<-function(x){
  n=length(x)
  high.face<-vector('list')
  lower.face<-vector('list')
  grey.face<-vector('list')
  p=nrow(x[[1]]);q=ncol(x[[1]])
  dd<-matrix(0,nrow=n,ncol=p)
  error<-c()
  for( i in 1:n){
    tmp<-rgb2grey(x[[i]])
    grey.face[[i]]<-tmp
    s<-svd(tmp)
    d<-s$d
    d1=d2=d
    ##最大的5个特征值重建
    d1[6:p]=0
    d2[1:5]=0
    high<-Image(s$u %*% diag(d1) %*% t(s$v))
    lower<-Image(s$u %*% diag(d2) %*% t(s$v))
    high.face[[i]]<-high
    lower.face[[i]]<-lower
    dd[i,]<-d
    error[i]<-sum(lower^2)/(p*q)
  }
  list(grey.face=grey.face,
       high.face=high.face,
       lower.face=lower.face,
       d=dd,error=error)
}
#######################
rec<-svd.recon(x=leader.face)
## 高频部分重建
high.face=rec$high.face
display(combine(high.face),method="raster",all=T)
## 低频部分重建
lower.face=rec$lower.face
display(1-combine(lower.face),method="raster",all=T)
## 特征值
face.d<-rec$d
#plot(face.d[,1],face.d[,2])
#plot(face.d[1,]);lines(face.d[2,],col=2)
#####svd 重建误差
err<-rec$error
ind2<-order(err);err<-err[ind2]

########################################################
png('svd_face.png',width=720,height=320)
par(mfrow=c(1,2),mar=c(2,4,2,0.5))
##SVD 重建误差
plot(err,col=2,type='o',xlab='',ylab='误差平方和',main='SVD 重建误差')
text(which(ind2==1),err[ind2==1],'主席',cex = 1.5)
text(which(ind2==2),err[ind2==2],'总理',cex = 1.5)
####特征值散点图
dc=rep(3,57);dc[1]=2;dc[2]=2
plot(face.d[,1],face.d[,2],col=dc,main='灰度脸svd特征值散点图')
text(face.d[1,1],face.d[1,2],'主席',cex = 1.5)
text(face.d[2,1],face.d[2,2],'总理',cex = 1.5)
points(mean(face.d[,1]),mean(face.d[,2]),col=2,pch=3)
#text(mean(face.d[,1]),mean(face.d[,2]),'中心')
dev.off()