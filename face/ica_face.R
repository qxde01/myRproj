library("fastICA")
source('sliding.R',encoding='utf-8')
library(EBImage)
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

feat<-fastICA(face_mat,20)
#face_col_mean<-apply(face_mat,2,mean)
#face_col_sd<-apply(face_mat,2,sd)
#### 白化后的数据
pre_face<-feat$X
#pre_face[,1]=pre_face[,1]*face_col_sd[1]+face_col_mean[1]
#### 主成份特征
pca_face<-feat$X %*% feat$K
#plot(feat$X%*%feat$K, main = "PCA components")
#### ICA特征脸
ica_face<-feat$S
ica<-unit.image(ica_face,m=48,n=64)
png('ICA_Feature_Face.png',width=5*48,height=64*4)
display(combine(ica),method="raster",all=T)
dev.off()
##ICA 重建，A为ICA系数
ica_rec<-feat$S%*%feat$A
rec_rec<-unit.image(ica_rec,m=48,n=64)
display(combine(rec_rec),method="raster",all=T)
#pre<-unit.image(pre_face,m=48,n=64)
#png('pre_faces.png',width=8*48,height=8*64)
#display(combine(pre),method="raster",all=T)
#dev.off()
#pca<-unit.image(pca_face,m=48,n=64)
#rec_rec<-unit.image(ica_rec,m=48,n=64)
#pri<-princomp(face_mat);summary(pri)
##重建误差
err<-error.recon(pre=pre_face,rec=ica_rec);sum(err)
ind3=order(err);err<-err[ind3]
#ec=as.integer(err*1000)+1
png('ica_err.png',width=480,height=320)
par(mar=c(2,4,2,0.5))
plot(err,type='o',col=as.integer(err*1000)+1,
     ylab='重建误差',main='ICA特征脸重建误差')
text(which(ind3==1),err[ind3==1],'主席' ,col=2,cex=1.5)
text(which(ind3==2),err[ind3==2],'总理' ,col=2,cex=1.5)
dev.off()

#############################
## 用ICA特征脸重建测试脸
## 对测试脸数据白化
load('test.face.rda')
n_test=length(test.face)
test_mat<-matrix(0,nrow=48*64*3,ncol=n_test)
for(i in 1:n_test){
  test_mat[,i]<-as.vector(image.sliding(test.face[[i]],m=48,n=64))
  #tmp<-(tmp-mean(tmp))/sd(tmp)
}
#####
test.white<-fastICA(test_mat,2)$X
test<-unit.image(test.white,m=48,n=64)
display(combine(test),method="raster",all=T)
display(combine(test.face),method="raster",all=T)

library(spams)
a=spams.lasso(X=test.white,D=ica_face,lambda1=0.05)
test_rec<-ica_face %*% a
test_recon<-unit.image(test_rec,m=48,n=64)
display(combine(test_recon),method="raster",all=T)
test.name<-dir('test','png')
test.name<-gsub('[0-9][0-9]_|.png','',test.name)
test_err<-error.recon(pre=test.white,rec=test_rec)
ind4=order(test_err)
test_err=test_err[ind4];test.name=test.name[ind4]
#plot(test_err,col=as.integer(test_err*100))
#text(1:24,test_err,test.name,cex=0.8)

png('ica_test_err.png',width=640,height=540)
par(mar=c(2,4,2,0.5))
plot(err,type='o',col=as.integer(err*1000)+1,ylim=c(0,max(test_err)),
     ylab='重建误差',main='测试脸使用ICA特征脸重建的重建误差')
text(which(ind3==1),err[ind3==1],'主席' ,col=2,cex=1.5)
text(which(ind3==2),err[ind3==2],'总理' ,col=2,cex=1.5)
points(seq(1,48,2),test_err,col=as.integer(test_err*100)+1)
text(seq(1,48,2),test_err,test.name,cex=0.8)
dev.off()
test_svd<-svd.recon(x=test.face)