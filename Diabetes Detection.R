setwd("D:\\CodeWithAbhishek\\R Programming\\data")
mydata<-read.csv("Daibetes Medical Reports.csv") #importing dataset
print(mydata)

is.na(mydata) #checking null values


#dealing with null values

newdata<-data.frame(mydata)     
newdata[newdata==0]<-NaN
newdata$Pregnancies[is.na(newdata$Pregnancies)]<-0
newdata$Outcome[is.na(newdata$Outcome)]<-0
print(newdata)

newdata$Glucose[is.na(newdata$Glucose)]<-mean(newdata$Glucose,na.rm = TRUE)
newdata$BloodPressure[is.na(newdata$BloodPressure)]<-mean(newdata$BloodPressure,na.rm = TRUE)
newdata$SkinThickness[is.na(newdata$SkinThickness)]<-mean(newdata$SkinThickness,na.rm = TRUE)
newdata$Insulin[is.na(newdata$Insulin)]<-mean(newdata$Insulin,na.rm = TRUE)
newdata$BMI[is.na(newdata$BMI)]<-mean(newdata$BMI,na.rm = TRUE)
newdata$DiabetesPedigreeFunction[is.na(newdata$DiabetesPedigreeFunction)]<-mean(newdata$DiabetesPedigreeFunction,na.rm = TRUE)
newdata$Age[is.na(newdata$Age)]<-mean(newdata$Age,na.rm = TRUE)
print(newdata)


#normalizing the dataset

normalize <- function(x)  #normalize function
  { 
     return ((x - min(x)) / (max(x) - min(x))) 
  }
newdata.n<-as.data.frame(lapply(newdata[,1:8], normalize))
print(newdata.n)


#spliting the dataset into training and testing dataset

set.seed(123)
split_data <- sample(1:nrow(newdata.n),size=nrow(newdata.n)*0.7,replace = FALSE) #random selection of 70% data.
train_set <- newdata[split_data,] # 70% training data
test_set <- newdata[-split_data,] # remaining 30% test data
#
##Creating seperate dataframe for 'Outcome'  which is our target attribute.
#
train_class <- newdata[split_data,9]
test_class <-newdata[-split_data,9]


#using KNN algorithm function

library(class)
NROW(train_class) #Find the number of observation

knn.23 <- knn(train=train_set, test=test_set, cl=train_class, k=23)
knn.24 <- knn(train=train_set, test=test_set, cl=train_class, k=24)
table(knn.23,test_class)
table(knn.24,test_class)

library(ggplot2)
library(lattice)
library(caret)
#
#Calculate the proportion of correct classification for k = 23, 24
#
ACC.23 <- 100 * sum(test_class == knn.23)/NROW(test_class)
ACC.24 <- 100 * sum(test_class == knn.24)/NROW(test_class)
ACC.23
ACC.24

#calculating accuracy using confusion matrix function
confusionMatrix(table(knn.23 ,test_class))

#for optimizing the model calculating the k-value,loop created 

i=1
k.optm=1
for (i in 1:23){
  knn.mod <- knn(train=train_set, test=test_set, cl=train_class, k=i)
  k.optm[i] <- sum(test_class== knn.mod)/NROW(test_class)      
  k=i
  cat(k,'=',k.optm[i],'
')
}


#Plotting an Accuracy graph
plot(k.optm, main="Accuracy Plot - KNN Algorithm",type="b", xlab="K- Value",ylab="Accuracy level",col="blue")

 

 
 
 



