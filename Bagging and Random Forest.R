#read data
hr<-read.csv(file.choose(), header = T)
head(hr)
View(hr)
tail(hr)
dim(hr)
str(hr)

#NA values
colSums(is.na(hr))

#need to rename "sales"column as "department"
attach(hr)

names(hr)[names(hr)=="sales"]<-"department"
View(hr)
unique(hr$department)

#Data Partitioning
index = sort(sample(nrow(hr), nrow(hr)*0.8))
View(index)
train = hr[index,]
test= hr[-index,]

which(colnames(hr)=="left")

X_train <- train[,-7]#matrix
Y_train <- train$left #vector
View(X_train)
View(Y_train)

X_test<-test[, -7]#matrix
Y_test<-test$left#vector
View(X_test)
View(Y_test)

install.packages("caret")#for confusion matrix
library(caret)
install.packages("ipred")
library(ipred)#improved prediction version for bagging 

set.seed(1234)

#bagging hyperparameters
control<-trainControl(method = "cv", number=10)#10-fold cross-validation
View(control)

btree=train(X_train, as.factor(Y_train), method="treebag", trControl=control, verbose=F, keepX = TRUE, coob=TRUE)  
View(btree)

#prediction and accuracy
predb = predict(btree$finalModel, newdata = X_test, type = "prob")
View(predb)#2nd column is 

install.packages("ROCR")
library(ROCR)

predicted=prediction(predb[,2], Y_test)
auc=performance(predicted, "auc")
auc = unlist(slot(auc,"y.values"))
auc

#OOB ESTIMATE
print(btree$finalModel)

#feature importance
bagimp<-varImp(btree)
bagimp
plot(bagimp)

#Random Forest
install.packages("randomForest")
library(randomForest)

control=trainControl(method = "cv", number = 10)
tune=expand.grid(mtry = c(3,6,9))
rf<-train(X_train, as.factor(Y_train), method = "rf", trControl = control, tuneGrid = tune, verbose=F)
rf#final value of mtry=3

tune1<-expand.grid(mtry=3)
rfmod<-train(X_train, as.factor(Y_train), method = "rf",tuneGrid = tune1, verbose=F)

#prediction and accuracy
predrf = predict(rfmod$finalModel, newdata=X_test, type = "prob")

predicted<-prediction(predrf[,2], Y_test)
auc <-performance(predicted, "auc")
auc<- unlist(slot(auc, "y.values"))
auc

#OOB estimate
print(rfmod$finalModel)


#Feature Importance
rfimp<-varImp(rfmod)
rfimp
plot(rfimp)

