library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(class)

myfunct<-function(x)
{
  (x-mean(x))/(sd(x))
}
norm<-sapply(iris[,-5],myfunct)

#5.1 a) Training set = 75% Test set = 25%

trained<-norm[1:113,]
test<-norm[114:150,]
pred<-knn(trained,test,iris[1:113,5],k=13) #k-nearest neighbour classification for test set from training set
confmat<-table(pred,iris[114:150,5])  # Making Confusion Matrix
accuracy<-((sum(diag(confmat)))/sum(confmat))*100 # finding Accuracy
cat("Accuracy with 75% training set is ",accuracy,"%")

# 5.1 b) Training set = 66.6% (2/3rd of total), Test set = 33.3%

trained1<-norm[1:100,]
test1<-norm[101:150,]
pred1<-knn(trained1,test1,iris[1:100,5],k=13)
confmat1<-table(pred1,iris[101:150,5])
acc1<-((sum(diag(confmat1)))/sum(confmat1))*100
cat("Accuracy with 75% training set is ",acc1,"%")


#5.2 Training set is chosen by i)hold out method ii)Random subsampling iii)Cross-Validation. Compare the accuracy of the classifiers obtained
#holdout method

trained2<-norm[1:75,]
test2<-norm[76:150,]
pred2<-knn(trained2,test2,iris[1:75,5],k=13)
confmat2<-table(pred2,iris[76:150,5])
acc2<-((sum(diag(confmat2)))/sum(confmat2))*100
cat("Accuracy with hold out is ",acc2,"%")


#random subsampling
#training sets are 80

trained3<-norm[1:80,]
test3<-norm[81:150,]
pred3<-knn(trained3,test3,iris[1:80,5],k=13)
confmat3<-table(pred3,iris[81:150,5])
acc3<-((sum(diag(confmat3)))/sum(confmat3))*100
cat("Accuracy with Random Subsampling is ",acc3,"%")


#training sets are 90

trained4<-norm[1:90,]
test4<-norm[91:150,]
pred4<-knn(trained4,test4,iris[1:90,5],k=13)
confmat4<-table(pred,iris[91:150,5])
acc4<-((sum(diag(confmat4)))/sum(confmat4))*100

acc4<-max(acc3,acc2)
cat("Accuracy with random subsampling is",acc4)


#cross validation

x=iris[,-5]
y=as.factor(iris$Species)
res<-knn.cv(x,y,1:length(y))
confmat5<-table(res,y)
acc5<-((sum(diag(confmat5)))/sum(confmat5))*100
cat("Accuracy with cross validation is",acc4)

#comparison of holdout,random subsampling and  cross validation

highest<-max(acc2,acc4,acc5)
if(highest==acc2)
{
  cat("Holdout method does best classification")
}
if(highest==acc4)
{
  cat("Random subsampling method does best classification")
}else
{
  cat("cross validation method does best classification")
}

# Scaling data to standard format

myfunct2 <- function(x) 
{
  (x-mean(x))/sd(x)
}
sapply(iris[,-5],myfunct2)

trainedsd<-norm[1:100,]
testsd<-norm[101:150,]
predsd<-knn(trainedsd,testsd,iris[1:100,5],k=13)
confmatsd<-table(predsd,iris[101:150,5])
accusd<-((sum(diag(confmatsd)))/sum(confmatsd))*100
cat("Accuracy of sd is",accusd)