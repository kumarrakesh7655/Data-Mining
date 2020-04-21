library(klaR)
library(e1071)
library(rpart)
library(rpart.plot)
library(caret)
 # Using Naive bayes
# 5.1 a) Training set = 75% Test set = 25%
sam<-sample(150,113) # taking random subsamples
sam  # printing that
iris_trained<-iris[sam,] 
iris_testset<-iris[-sam,]
iris_testset
model<-naiveBayes(Species~.,data = iris_trained) # applying navie base model
pred<-predict(model,iris_testset)
confmat<-confusionMatrix(pred,iris_testset$Species)$table # deriving Confusion Matrix
acc<-((sum(diag(confmat)))/sum(confmat)*100) #calcluating Confusion Matrix
acc 


#5.1 b) Training set = 66.6% (2/3rd of total), Test set = 33.3%
sam2<-sample(150,100)
sam2
iris_trained2<-iris[sam2,]
iris_testset2<-iris[-sam2,]
model2<-naiveBayes(Species~.,data = iris_trained2)
pred2<-predict(model,iris_testset2)
confmat2<-confusionMatrix(pred2,iris_testset2$Species)$table
acc2<-(sum(diag(confmat2))/sum(confmat2)*100)
acc2  
if(acc>acc2)
{
  cat("Training set of 75% records is better")
}
if(acc < acc2)
{
  cat("Training set of 66.6% records is better")
}


#5.2 Training set is chosen by i)hold out method ii)Random subsampling iii)Cross-Validation. Compare the accuracy of the classifiers obtained.

# i.) holdout method

sam3<-sample(150,75)
iris_trained3<-iris[sam3,]
iris_testset3<-iris[-sam3,]
model<-naiveBayes(Species~.,data=iris_trained3)
pred3<-predict(model,iris_testset3)
confmat3<-confusionMatrix(pred3,iris_testset3$Species)$table
acc3<-((sum(diag(confmat3)))/sum(confmat3))*100
cat("Accuracy of holdout method is ",acc3,"%")


# ii.) Random subsampling

a<-75
b<-1
acc4<-c()
for(a in 75:100)
{
  sam4<-sample(150,a)
  iris_trained4<-iris[sam4,]
  iris_testset4<-iris[-sam4,]
  model<-naiveBayes(Species~.,data=iris_trained4)
  pred4<-predict(model,iris_testset4)
  confmat4<-confusionMatrix(pred4,iris_testset4$Species)$table
  acc4[b]<-c((sum(diag(confmat4))/sum(confmat4))*100,acc4)
  b=b+1
}
accrs<-mean(acc4)
cat("Accuracy of random subsampling is ",accrs,"%")

# iii.) Cross validation method
x=iris[,-5]
y=iris$Species
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
cons<-table(predict(model$finalModel,x)$class,y)
acc5<-(sum(diag(cons))/sum(cons))*100
acc5


#comparing Above 3 methods
greatest<-max(acc3,accrs,acc5)
if(greatest==acc3)
{
  cat("Holdout method does best classification")
}
else if(greatest==accrs)
{
  cat("Random subsampling method does best classification")
}
else
{
  cat("cross validation method does best classification")
}






#5.3 Data is scaled to standard format.
data6 <- function(x) 
{ 
  (x-mean(x))/sd(x) 
}

sapply(iris[,-5],data6)
s<-sample(150,90)
iris_trained6<-iris[s,]
iris_testset6<-iris[-s,]
model<-naiveBayes(Species~.,data=iris_trained6)
pred6<-predict(model,iris_testset6)
confmat6<-confusionMatrix(pred6,iris_testset6$Species)$table
acc6<-((sum(diag(confmat6)))/sum(confmat6))*100
cat("Accuracy of this is ",acc6,"%")