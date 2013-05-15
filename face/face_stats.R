leader.face<-face.extract(path='leader/',w=48,h=64)
#save(leader.face,file='leader.face.rda')
#str(leader.face)

display(combine(leader.face),method="raster",all=T)
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