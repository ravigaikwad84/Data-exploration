#reading data
hr<-read.csv(file.choose(), header = T)
dim(hr)
str(hr)
View(hr)

colSums(is.na(hr))

unique(hr$sales)

#rename "sales" column to "department"
names(hr)[names(hr)=="sales"]<-"department"
View(hr)
unique(names(hr))
unique(hr$department)

#data partitioning
index = sort(sample(nrow(hr), nrow(hr)*0.8))
View(index)

train <-hr[index,]
test<-hr[-index,]

#find location of target variable
which(colnames(hr)=="left")  #7th position

X_train<-train[,-7] #matrix
Y_train<-train$left #vector

X_test<-test[,-7]
Y_test<-test$left

install.packages("caret")
library(caret)
install.packages("gbm")
?gbm
library(gbm)
set.seed(1234)

control <-trainControl(method = "cv", number=10)  #10-fold cross-validation
tune<-expand.grid(n.trees=seq(70,300,10), shrinkage=c(0.002, 0.01, 0.1),
                  interaction.depth=c(1,3,9), n.minobsinnode=rep(3,14))
View(tune)

#parameter selection
mod=train(X_train, as.factor(Y_train),method="gbm",trControl=control, tuneGrid=tune, verbose=F)
mod

tune1<-expand.grid(n.trees=300, .shrinkage=0.1,.interaction.depth=9,n.minobsinnode=3)
View(tune1)

#final model

model<-train(X_train, as.factor(Y_train), method = "gbm", tuneGrid=tune1, verbose=F)

#accuracy
#Accuracy
pred=predict(model,newdata=X_test,type="prob")
library(ROCR)

predicted=prediction(pred[,2],y_test)
auc=performance(predicted,"auc")
auc=unlist(slot(auc,"y.values"))
auc

#Feature Importance

summary(model)



imp=as.data.frame(as.matrix(summary(model)))



library(ggplot2)

Featureimp=ggplot(imp,aes(x=var,y=rel.inf,fill=var))

Featureimp+geom_bar(stat="identity")+geom_text(label=imp$rel.inf,vjust=-1)



#PDP

plot.gbm(model$finalModel,i.var=3,col="blue")


