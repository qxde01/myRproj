setwd('E:/myRproj/trunk/CloudAtlas/')
load('rda/cloudAtlas500.rda')
source('postProcess.R',encoding="UTF-8")
#去掉一些字符a-z0-9
cloudAtlas500$word=sapply(cloudAtlas500$word,remove.chars,n=1,"[0-9a-zA-Z\\.%\\']|—|云图|-")
#cloudAtlas$wordPositive=sapply(cloudAtlas$wordPos,remove.chars,n=1,"[—0-9a-z\\.%\\-\\']")
#cloudAtlas$wordNegative=sapply(cloudAtlas$wordNeg,remove.chars,n=1,"[—0-9a-z\\.%\\-\\']")
####有效346+149=495 篇
train_dtm<-df2dtm(cloudAtlas500[1:350,],content='word',word.min=2)
test_dtm<-df2dtm(cloudAtlas500[351:500,],content='word',word.min=2)
t1<-colnames(train_dtm);t2<-colnames(test_dtm)
table(t1 %in% t2);length(table(c(t1,t2)))
dim(train_dtm);dim(test_dtm)

#### 将主题设定为2～25
ldas<-lda.rep(train=train_dtm,test=test_dtm,n=25,method='VEM')

#save(ldas,file='rda/ldas.rda')

####困惑度的计算
train_perp=ldas$train_perp
test_perp=ldas$test_perp
loglik=ldas$loglik
est=data.frame(x=c(1:25,1:25),
               y=c(test_perp,train_perp),
               type=c(rep('test_perplexity',25),
                      rep('train_perplexity',25) ))

png('png/perplextity.png',width=720,height=480)
ggplot(est, aes(x, y, color=type)) + 
  geom_line() + geom_point()+ 
  facet_wrap(~ type, ncol=2)+
  labs(list(title ='', x = "number of topic", y = "perplexity"))
dev.off()
####熵计算
alpha=sapply(ldas[[1]], slot, "alpha")
entropy=sapply(ldas[[1]], function(x) mean(apply(posterior(x)$topics, 1, 
                                                 function(z) - sum(z * log(z)))))
ae=data.frame(x=c(1:24,1:24),y=c(alpha,entropy),
              type=c(rep('alpha',24),rep('entropy',24)))

png('png/entropy.png',width=720,height=480)
ggplot(ae, aes(x, y, color=type)) + 
  geom_line() + geom_point()+ 
  facet_wrap(~ type, ncol=2)+
  labs(list(title ='', x = "number of topic", y = "alpha and entropy"))
dev.off()
# png('perplextity.png',width=640,height=320)
# par(mfrow=c(1,3),mar=c(2,4,3,1))
# plot(train_perp,type='o',col=2,main='perplexity of training data',ylab='perplexity')
# plot(test_perp,type='o',col=3,main='perplexity of testing data',ylab='perplexity')
# plot(loglik,type='o',col=4,main='loglik of training data',ylab='loglik')
# dev.off()
####
#library(textir)
#summary(simselect <- topics(dtm_pos, K=10+c(-8:10)), nwrd=0)
# => k=2
###########################################################
## k=8 ,文档-主题概率分布
#model<-LDA(train_dtm, control = list(alpha=0.015,verbose=1),method='VEM', k = 8)
model=ldas$model[[7]]
###
train_topic<-posterior(model)$topics
test_topic<-posterior(model,newdata=test_dtm)$topics
png('png/topic_dist.png',width=1280,height=640)
par(mfrow=c(1,2))
topic.image(train_topic,main='Training data ')
topic.image(test_topic,main='Testing data ')
dev.off()
#########################################################
####随机抽取10个样本，显示主题分布
prop=prop2df(lda=model,terms.top.num=5,doc.num=10)
propor=prop$lda.prop
png('png/topic_rnd10.png',width=640,height=480)
qplot(y=proportion,x=topic, fill=doc, position="dodge",
      data=propor, geom="bar",stat="identity",facets=~doc)+
  coord_flip()+
  facet_wrap(~ doc, ncol=5)
dev.off()
prop2=prop2df(lda=model,newdata=test_dtm,terms.top.num=5,doc.num=10)
propor2=prop2$lda.prop
png('png/topic_rnd10_test.png',width=640,height=480)
qplot(y=proportion,x=topic, fill=doc, position="dodge",
      data=propor2, geom="bar",stat="identity",facets=~doc)+
  coord_flip()+
  facet_wrap(~ doc, ncol=5)
dev.off()

####################################
#### 抽取每个主题的前10个词汇，绘制网络图
top_terms<-terms(model, k=10, threshold=0.002)
g<-topic.graph(top_terms)
g2<-topic.graph(terms(ldas$model[[1]], k=10, threshold=0.002))
g4<-topic.graph(terms(ldas$model[[4]], k=10, threshold=0.002))
gg<-graph.data.frame(g[,1:2])
gg2<-graph.data.frame(g2[,1:2])
gg4<-graph.data.frame(g4[,1:2])
png('png/top_terms_graph.png',width=960,height=320)
par(mfrow=c(1,3),mar=c(0,0,2,0))
plot(gg2, layout=layout.fruchterman.reingold,
     vertex.size=10,
     vertex.color='gray90',
     vertex.label.cex=2,
     edge.color=as.integer(g4[,3]),
     edge.arrow.size=0.5,
     main='2 Topics')
plot(gg4, layout=layout.fruchterman.reingold,
     vertex.size=10,
     vertex.color='gray90',
     vertex.label.cex=2,
     edge.color=as.integer(g4[,3]),
     edge.arrow.size=0.5,
     main='5 Topics')
plot(gg, layout=layout.fruchterman.reingold,
     vertex.size=10,
     vertex.color='gray90',
     vertex.label.cex=1.8,
     edge.color=as.integer(g[,3]),
     edge.arrow.size=0.5,
     main='8 Topics')
dev.off()
#####################################################################
dtm<-df2dtm(cloudAtlas,content='word',word.min=2)
dim(dtm)
k <- 8;SEED <- 2013
CA_TM <-list(VEM = LDA(dtm, k = k, control = list(seed = SEED)),
       VEM_fixed = LDA(dtm, k = k,control = list(estimate.alpha = FALSE, seed = SEED)),
       Gibbs = LDA(dtm, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
       CTM = CTM(dtm, k = k, control = list(seed = SEED, 
                                var = list(tol = 10^-4), em = list(tol = 10^-3))))
#save(CA_TM,file='rda/CA_TM.rda')
load('rda/CA_TM.rda')
#### 文档分配给最有可能的话题的概率分布
sapply(CA_TM[1:2], slot, "alpha")
methods <- c("VEM", "VEM_fixed", "Gibbs", "CTM")
DF <- data.frame(posterior = unlist(lapply(CA_TM, 
                                           function(x) apply(posterior(x)$topics, 1, max))),
                 method = factor(rep(methods, each = nrow(posterior(CA_TM$VEM)$topics)), 
                                 methods))
 png('png/maxProb.png',width=720,height=320)
ggplot(DF, aes(x=posterior, fill=method))+
  geom_histogram(binwidth = 0.05)+facet_wrap(~ method,ncol=4) +
  ylab("Frequency")+
  xlab ("Probability of assignment to the most likely topic")
dev.off()

DF2 <- data.frame(topic_class = unlist(lapply(CA_TM, 
                                           function(x) apply(posterior(x)$topics, 1, which.max))),
                 method = factor(rep(methods, each = nrow(posterior(CA_TM$VEM)$topics)), 
                                 methods))

ggplot(DF2, aes(x=topic_class, fill=method))+
  geom_histogram(binwidth = 1)+facet_wrap(~ method,ncol=4)+
  ylab("Frequency")
  #xlab ("Probability of assignment to the most likely topic")
vem_class=DF2[1:495,1]
library(slam)
####################################
#### 合并同一主题的词频
vemsum<-DTM_Topic_SUM(dtm,vem_class)
Gibbssum<-DTM_Topic_SUM(dtm,DF2[991:1485,1])
dim(vemsum)
vemsum<-as.matrix(vemsum)
Gibbssum<-as.matrix(Gibbssum)
#########################################
#### 余玄相似度
sim<-cosine(vemsum)
sim2<-cosine(Gibbssum)
png('png/cosine.png',width=950,height=480)
par(mfrow=c(1,2),mar=c(2,2,2,0))
image(sim,xaxt="n", yaxt="n",main="LDA_VEM")
axis(1, at=seq(0,1,length=8),labels=paste('Topic',1:8), tick=F ,line=-.5)
axis(2, at =seq(0,1,length=8),label=paste('Topic',1:8))
image(sim2,xaxt="n", yaxt="n",main="LDA_Gibbs")
axis(1, at=seq(0,1,length=8),labels=paste('Topic',1:8), tick=F ,line=-.5)
axis(2, at =seq(0,1,length=8),label=paste('Topic',1:8))
dev.off()
#################################################################
dtm_pos<-df2dtm(cloudAtlas,content='wordPos',word.min=2)
ldas_pos<-lda.rep(train=dtm_pos,n=25,method='VEM')

entropy=sapply(ldas_pos[[1]], function(x) mean(apply(posterior(x)$topics, 1, 
                                                 function(z) - sum(z * log(z)))))

