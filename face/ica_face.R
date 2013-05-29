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
## k=20 pca 95%
ica_leader_face<-fastICA(face_mat,20)
#### 白化后的数据
pre_face<-ica_leader_face$X
pre_face0<-unit.image(pre_face,m=48,n=48)
png('rpng/whiten_leader_Face.png',width=8*48,height=48*8)
display(combine(pre_face0),method="raster",all=T)
dev.off()

#### 主成份特征
pca_face<-ica_leader_face$X %*% ica_leader_face$K
pca<-unit.image(pca_face,m=48,n=48)
display(combine(pca),method="raster",all=T)
#plot(feat$X%*%feat$K, main = "PCA components")
#### ICA特征脸
ica_face<-ica_leader_face$S
#save(ica_face,file='rdata/ica_feature_face.rda')
ica_face0<-unit.image(ica_face,m=48,n=48)
png('rpng/ICA_Feature_Face.png',width=5*48,height=48*4)
display(combine(ica_face0),method="raster",all=T)
dev.off()

##ICA 重建，A为ICA系数
rec_ica_face<-ica_leader_face$S%*%ica_leader_face$A
rec_ica<-unit.image(rec_ica_face,m=48,n=48)
png('rpng/rec_ica_leader_face.png',width=8*48,height=8*48)
display(combine(rec_ica),method="raster",all=T)
dev.off()
 
#pri<-princomp(face_mat);summary(pri)
##重建误差
ica_rec_err<-error.recon(pre=pre_face,rec=rec_ica_face);sum(ica_rec_err)
ind3=order(ica_rec_err);ica_rec_err<-ica_rec_err[ind3]
#ec=as.integer(err*1000)+1
png('rpng/ica_leader_recon_error.png',width=640,height=320)
par(mar=c(2,4,2,0.5))
plot(ica_rec_err,type='o',col=as.integer(err*1000)+1,
     ylab='ICA重建误差',main='ICA特征脸重建误差')
text(which(ind3==1),ica_rec_err[ind3==1],'主席' ,col=2,cex=1.5)
text(which(ind3==2),ica_rec_err[ind3==2],'总理' ,col=2,cex=1)
text(which(ind3==3),ica_rec_err[ind3==3],'副主席',cex = 1)
text(which(ind3==4),ica_rec_err[ind3==4],'委员长',cex = 1)
dev.off()

#############################
## 用ICA训练的特征脸重建测试脸
## 对测试脸数据白化
load('rdata/test.face.rda')
test_mat<-list2mat(test.face)
save(test_mat,file='rdata/test_mat.rda')
#####
test.white<-fastICA(test_mat,2)$X
test<-unit.image(test.white,m=48,n=48)
display(combine(test),method="raster",all=T)
#display(combine(test.face),method="raster",all=T)

library(spams)
a=spams.lasso(X=test.white,D=ica_face,lambda1=0.25)
test_rec<-ica_face %*% a
test_recon<-unit.image(test_rec,m=48,n=48)
png('rpng/rec_ica_test_face.png',width=5*48,height=5*48)
display(combine(test_recon),method="raster",all=T)
dev.off()

test.name<-gsub('[0-9][0-9]_|.png','',dir('test','png'))
test_err<-error.recon(pre=test.white,rec=test_rec)
ind4=order(test_err)
test_err=test_err[ind4];test.name=test.name[ind4]
#plot(test_err,col=as.integer(test_err*100))
#text(1:24,test_err,test.name,cex=0.8)

png('rpng/ica_test_rec_err.png',width=960,height=640)
par(mar=c(2,4,2,0.5))
plot(ica_rec_err,type='o',col=as.integer(ica_rec_err*1000)+1,
     ylim=c(0,max(test_err,ica_rec_err)),
     ylab='重建误差',main='测试脸使用ICA特征脸重建的重建误差')
text(which(ind3==1),ica_rec_err[ind3==1],'主席' ,col=2,cex=1.5)
text(which(ind3==2),ica_rec_err[ind3==2],'总理' ,col=2,cex=1.5)
text(which(ind3==3),ica_rec_err[ind3==3],'副主席',cex = 1,col=2)
text(which(ind3==4),ica_rec_err[ind3==4],'委员长',cex = 1,col=2)
lines(seq(1,49,2),test_err,col=as.integer(test_err*1000)+1,type='o')
#points(seq(1,49,2),test_err,col=as.integer(test_err*1000)+1)
text(seq(1,49,2),test_err+mean(test_err)*0.04,test.name,cex=0.8)
dev.off()


###################################

test_svd<-svd.recon(x=test.face)
test.eigen<-test_svd$d
class=c(rep(1,nrow(leader.eigen)),rep(2,nrow(test.eigen)))
eigen<-rbind(leader.eigen,test.eigen)
#eid<-cbind(eid,class)
png('rpng/leader_test_svd_eigen.png',width=720,height=640)
par(mar=c(4,4,2,0.5))
plot(eigen[,1],eigen[,2],col=class+1,ylab='第二特征值',xlab='第一特征值',
     main='57个训练脸和25个测试脸的特征值散点图')
points(mean(leader.eigen[,1]),mean(leader.eigen[,2]),col=2,pch=3)
points(mean(test.eigen[,1]),mean(test.eigen[,2]),col=3,pch=3)
text(eigen[1,1],eigen[1,2],'主席',cex = 1.2)
text(eigen[2,1],eigen[2,2],'总理',cex = 1.2)
text(eigen[3,1],eigen[3,2],'副主席',cex = 1)
text(eigen[4,1],eigen[4,2],'委员长',cex = 1)
text(eigen[58,1],eigen[58,2],'薄熙来',cex = 1,col=3)
text(eigen[59,1],eigen[59,2],'苍井空',cex = 1,col=4)
text(eigen[63,1],eigen[63,2],'郭德纲',cex = 1,col=4)
text(eigen[70,1],eigen[70,2],'马英九',cex = 1,col=3)
text(eigen[72,1],eigen[72,2],'马云',cex = 1,col=6)
text(eigen[73,1],eigen[73,2],'毛泽东',cex = 1,col=5)
text(eigen[74,1],eigen[74,2],'奥巴马',cex = 1,col=3)
text(eigen[82,1],eigen[82,2],'胡锦涛',cex = 1,col=5)
text(eigen[80,1],eigen[80,2],'周鸿祎',cex = 1,col=6)
dev.off()
