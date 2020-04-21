
#75% training set

sam<-sample(150,113)
trained<-iris[sam,]
test<-iris[-sam,]
dtm<-rpart(Species~.,trained,method="class")
rpart.plot(dtm)
p<-predict(dtm,test,type="class")
cn<-confusionMatrix(test[,5],p)$table
print(cn)
accu<-(sum(diag(cn))/sum(cn))*100
cat("The accuracy with 75% training data is ",accu,"%")

#66.6% taining set

s<-sample(150,100)
trained1<-iris[s,]
test1<-iris[-s,]
dtm<-rpart(Species~.,trained1,method="class")
rpart.plot(dtm)
p<-predict(dtm,test1,type="class")
cn<-confusionMatrix(test1[,5],p)$table
print(cn)
accu6<-(sum(diag(cn))/sum(cn))*100
cat("The accuracy with 66.6% training data is ",accu6,"%")

#Holdout method

s<-sample(150,75)
trained2<-iris[s,]
test2<-iris[-s,]
dtm<-rpart(Species~.,trained2,method="class")
rpart.plot(dtm)
p<-predict(dtm,test2,type="class")
cn<-confusionMatrix(test2[,5],p)$table
acchld<-(sum(diag(cn))/sum(cn))*100
cat("The accuracy with hold out method is ",acchld,"%")

#Random subsampling

i<-75
j<-1
acc<-c()
for(i in 75:100)
{
  s<-sample(150,i)
  trained3<-iris[s,]
  test3<-iris[-s,]
  dtm<-rpart(Species~.,trained3,method="class")
  rpart.plot(dtm)
  p<-predict(dtm,test3,type="class")
  cn<-confusionMatrix(test3[,5],p)$table
  acc[j]<-c((sum(diag(cn))/sum(cn))*100,acc)
  j=j+1
}
acrs<-mean(acc)

cat("The accuracy with random subsampling method is ",acrs,"%")

#cross validation method

library(plyr)
library(rpart)
set.seed(123)
form <- "Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
folds <- split(iris, cut(sample(1:nrow(iris)),10))
errs <- rep(NA, length(folds))
i<-1
for (i in 1:length(folds))
{
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- rpart(form , train, method = "class")
  tmp.predict <- predict(tmp.model, newdata = test, type = "class")
  conf.mat <- table(test$Species, tmp.predict)
  errs[i] <-1- sum(diag(conf.mat))/sum(conf.mat)
}
accv<-(1-mean(errs))*100
cat("The average accutracy using k-fold cross validation is",accv,"%")

#comparison of holdout,random subsampling and cross validation

highest<-max(acchld,acrs,accv)
if(highest==acchld)
{
  cat("Holdout method does best classification")
}else if(highest==acrs)
{
  cat("Random subsampling method does best classification")
}else
{
  cat("cross validation method does best classification")
}

#standard normal form

funct <- function(x) 
{
  (x-mean(x))/sd(x)
}
sapply(iris[,-5],funct)
sam<-sample(150,90)
trained4<-iris[sam,]
test4<-iris[-sam,]
dtm<-rpart(Species~.,trained4,method="class")
rpart.plot(dtm)
p<-predict(dtm,test4,type="class")
cn<-confusionMatrix(test4[,5],p)$table
accsd<-(sum(diag(cn))/sum(cn))*100
cat("The accuracy in standardised iris data is ",accsd,"%")