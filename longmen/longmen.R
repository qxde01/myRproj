library(Rdouban)
library(Rwordseg)
#setwd("E:/myRproj/trunk/longmen/")
longmen<-get_movie_reviews(movieid=11627047,results=300,
                           count=20,verbose=TRUE)
lg_short<-get_movie_comments(movieid=11627047,results=11000)

#insertWords("龙门镖局")
data(stopwords)
f_seg<-function(x,stopwords){
  x<-gsub('[\ra-z]|--','',x)
  seg<-segmentCN(x)
  seg<-seg[!seg %in% stopwords]
  seg<-gsub('[a-zA-Z0-9]|\\.',"",seg)
  seg<-seg[nchar(seg)>1]
  word<-paste(seg,collapse=' ')
  word
}

w<-sapply(longmen$review,f_seg,stopwords)
names(w)<-NULL
longmen<-data.frame(longmen,word=w,stringsAsFactors=F)
row.names(longmen)<-NULL
#save(longmen,file='longmen.rda')
s<-sapply(lg_short$comment,f_seg,stopwords)
names(s)<-NULL
lg_short<-data.frame(lg_short,word=s,stringsAsFactors=F)
#save(lg_short,file="lg_short.rda")
lg_short2<-lg_short[nchar(lg_short$word)>1,]
lg_short2<-lg_short2[!is.na(lg_short2$rating),]
save(lg_short2,file="lg_short2.rda")
#load("longmen.rda")
#################################################
library(RTextTools)
longmen<-longmen[nchar(longmen$word)>0,]
myReader <- readTabular(mapping =list(Content = "word"))
corpus <- Corpus(DataframeSource(longmen),
                 readerControl = list(reader = myReader,
                                      language = "zh_cn"))
mat<-TermDocumentMatrix(corpus, control =list(wordLengths=c(2,Inf)))
mat<-weightTfIdf(mat,normalize = TRUE)
mat<-t(removeSparseTerms(mat, sparse=0.99))

#mat2<- create_matrix(longmen$word,removeNumbers=TRUE, stemWords=FALSE, 
#                    weighting=weightTfIdf,removeSparseTerms=.99)
#mat2<- create_matrix(lg_short2$word,removeNumbers=TRUE, stemWords=FALSE, 
#                    weighting=weightTfIdf,removeSparseTerms=.99)

container <- create_container(mat,longmen$rating,trainSize=1:250, 
                              testSize=251:323, virgin=FALSE)
SVM <- train_model(container,"SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
#SLDA <- train_model(container,"SLDA")
#BAGGING <- train_model(container,"BAGGING")
BOOSTING <- train_model(container,"BOOSTING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")

# save(TREE,file="TREE.rda")

SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
# SLDA_CLASSIFY <- classify_model(container,SLDA)
# BAGGING_CLASSIFY <- classify_model(container,  BAGGING)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container,NNET)
TREE_CLASSIFY <- classify_model(container, TREE)

analytics<-create_analytics(container,
                            cbind(SVM_CLASSIFY, BOOSTING_CLASSIFY,
                                  RF_CLASSIFY, GLMNET_CLASSIFY,
                                  NNET_CLASSIFY, TREE_CLASSIFY,
                                  MAXENT_CLASSIFY))
summary(analytics)
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary
create_ensembleSummary(analytics@document_summary)

N=3
cross_SVM <- cross_validate(container,N,"SVM")
cross_GLMNET <- cross_validate(container,N,"GLMNET")
cross_MAXENT <- cross_validate(container,N,"MAXENT")
# cross_SLDA <- cross_validate(container,N,"SLDA")
# cross_BAGGING <- cross_validate(container,N,"BAGGING")
cross_BOOSTING <- cross_validate(container,N,"BOOSTING")
cross_RF <- cross_validate(container,N,"RF")
cross_NNET <- cross_validate(container,N,"NNET")
cross_TREE <- cross_validate(container,N,"TREE")
cross<-c(cross_SVM[[2]],cross_GLMNET[[2]],
  cross_MAXENT[[2]],cross_BOOSTING[[2]],
  cross_RF[[2]],cross_NNET[[2]],cross_TREE[[2]])

#pmml(RF,model.name="RF_Model")
write.csv(analytics@document_summary,
          "DocumentSummary.csv")