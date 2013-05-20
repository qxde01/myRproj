leader.face<-face.extract(path='leader/',w=48,h=64)
#save(leader.face,file='leader.face.rda')
#str(leader.face)
load('leader.face.rda')
png('faces.png',width=8*48,height=8*64)
display(combine(leader.face),method="raster",all=T)
dev.off()
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
display(face.stat,method="raster",all=T)


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

m<-apply(face_mat,2,squre.sum,face.mean)
s<-apply(face_mat,2,cosine.sim,face.mean)


ms<-cbind(m,s=1-s)
ind<-order(ms[,1])
ms<-ms[ind,]
plot(ms[,1],type='o',col=2,ylim=c(min(ms),max(ms)),ylab='MSE and cosine')
lines(ms[,2],type='o',col=3)
text(which(ind==1),ms[ind==1,1],'主席')
#plot(ms,col=1:57)
#svd_face<-svd(face_mat)
library("fastICA")
###重建误差
error.recon<-function(pre,rec){
  nc=ncol(pre)
  nr=nrow(pre)
  error<-c()
  for(i in 1:nc){
    error[i]<-sum((pre[,i]-rec[,i])^2)/nr
  }
  cat('error of sum :',sum(error),'\n')
  error
}

feat<-fastICA(face_mat,15)
#face_col_mean<-apply(face_mat,2,mean)
#face_col_sd<-apply(face_mat,2,sd)
#### 白化后的数据
pre_face<-feat$X
#pre_face[,1]=pre_face[,1]*face_col_sd[1]+face_col_mean[1]
#### 主成份特征
pca_face<-feat$X %*% feat$K
#plot(feat$X%*%feat$K, main = "PCA components")
#### ICA特征
ica_face<-feat$S
##ICA 重建，A为ICA系数
ica_rec<-feat$S%*%feat$A

#pre<-unit.image(pre_face,m=48,n=64)
#png('pre_faces.png',width=8*48,height=8*64)
#display(combine(pre),method="raster",all=T)
#dev.off()
#pca<-unit.image(pca_face,m=48,n=64)
#ica<-unit.image(ica_face,m=48,n=64)
#rec_rec<-unit.image(ica_rec,m=48,n=64)
#display(combine(pca),method="raster",all=T)
#pri<-princomp(face_mat);summary(pri)

err<-error.recon(pre=pre_face,rec=ica_rec);sum(err)
#ec=as.integer(err*1000)+1
plot(err,type='o',col=as.integer(err*1000))
text(1,err[1],'主席' ,col=2)
text(2,err[2],'总理' ,col=2)