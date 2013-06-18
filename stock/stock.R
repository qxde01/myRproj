library(quantmod)
library(spams)
setwd('E:/myRproj/trunk/stock')
source('ts.R',encoding='utf-8')
##沪深300
setSymbolLookup(CSI300=list(name="000300.ss",src="yahoo"))
getSymbols("CSI300")
#x=Cl(CSI300)

open_price<-CSI300[,1]
X=ts.sliding(x=open_price,k=10,rnd=F,rnd.num=2000)
nnsc_D<-spams.nnsc(X=X,K=64,lambda1=0.05)
jigsaw.plot(x=nnsc_D)
#pr<-prcomp(nnsc_D);summary(pr)
a=spams.omp(X=X,D=nnsc_D,lambda1=0.15)
x2=nnsc_D %*% a
mse(X,x2)


