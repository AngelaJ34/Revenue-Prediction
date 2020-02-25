P5_Train <- read.csv("P5_train1.csv")

P5_Train <- P5_train1
P5_Test <- P5_test1_students

View(P5_Train)
colnames(P5_Train)
summary(P5_Train)

plot(P5_Train$Duration~P5_Train$Activity)
boxplot(P5_Train$Duration~P5_Train$Activity)
plot(P5_Train$Duration~P5_Train$Revenue, main="Earning Potiential Based On Duration", col=c("blue","green","orange","yellow","light green","red"))
boxplot(P5_Train$Duration~P5_Train$Source)
Activitymean<-tapply(P5_Train$Activity,P5_Train$Source, mean)
barplot(Activitymean, main = "Activity Based On Source", xlab="Source", ylab = "Activity Level", col=c("blue","green","orange","yellow","light green","red"))
SiteRevemean<-tapply(P5_Train$Revenue,P5_Train$Source, mean)
barplot(SiteRevemean, main="Revenue Based On Source", col=c("blue","green","orange","yellow","light green","red"))

P5_directlink<-subset(P5_Train,P5_Train$Source=='Directlink')
plot(P5_directlink$Duration~P5_directlink$Activity)
boxplot(P5_directlink$Duration~P5_directlink$Activity)
plot(P5_directlink$Duration~P5_directlink$Revenue, main="Earning Potiential Based On Duration", col=c("blue","green","orange","yellow","light green","red"))
boxplot(P5_directlink$Duration~P5_directlink$Source)

P5_GS<-subset(P5_Train,P5_Train$Source=='GoogleSearch')
plot(P5_GS$Duration~P5_GS$Activity)
boxplot(P5_GS$Duration~P5_GS$Activity)
plot(P5_GS$Duration~P5_GS$Revenue, main="Earning Potiential Based On Duration", col=c("blue","green","orange","yellow","light green","red"))
boxplot(P5_GS$Duration~P5_GS$Source)

P5_GA<-subset(P5_Train,P5_Train$Source=='GoogleAdWord')
plot(P5_GA$Duration~P5_GA$Activity)
boxplot(P5_GA$Duration~P5_GA$Activity)
plot(P5_GA$Duration~P5_GA$Revenue, main="Earning Potiential Based On Duration", col=c("blue","green","orange","yellow","light green","red"))
boxplot(P5_GA$Duration~P5_GA$Source)

install.packages("e1071")
library(e1071)
#Duration & activity
perf.svm1 = svm(Revenue~Activity, data = P5_Train)
perf.svm1

#Duration & Revenue
perf.svm2 = svm(Revenue~Duration+Source+Activity, data = P5_Train)
perf.svm2

perf.svm_directlink = svm(Revenue~Duration+Activity, data = P5_directlink)
perf.svm_directlink

perf.lm_directlink = lm(Revenue~Activity, data = P5_directlink)
perf.lm_directlink

perf.GS = svm(Revenue~Duration+Activity, data = P5_GS)
perf.GS

perf.GA = svm(Revenue~Duration+Activity, data = P5_GA)
perf.GA

#Duration & Source
perf.svm3 = svm(Duration~Source, data = P5_Train)
perf.svm3

pred1 = predict(perf.svm1, newdata = P5_Train)
pred2 = predict(perf.svm2, newdata = P5_Train)
pred3 = predict(perf.svm3, newdata = P5_Train)

pred_directlink = predict(perf.svm_directlink, newdata = P5_directlink)
pred_lm_directlink = predict(perf.lm_directlink, newdata = P5_directlink)
pred_GS = predict(perf.GS, newdata = P5_GS)
pred_GA = predict(perf.GA, newdata = P5_GA)

mean((pred1 - P5_Train$Duration)^2)
mean((pred2 - P5_Train$Revenue)^2)
mean((pred3 - P5_Train$Duration)^2)
mean((pred_directlink - P5_directlink$Revenue)^2)
mean((pred_lm_directlink - P5_directlink$Revenue)^2)
mean((pred_GS - P5_GS$Revenue)^2)
mean((pred_GA - P5_GA$Revenue)^2)

pred = predict(perf.svm2, newdata=P5_test1_students)
mytestprediction<-P5_test1_students
mytestprediction$Revenue <- pred

P5_Test$Revenue<-0
pred = predict(perf.svm2, newdata=P5_Test)
mytestprediction<-P5_test1_students
mytestprediction$Revenue <- pred

submission <- mytestprediction[,c('ID','Revenue')]
write.csv(submission,file = "mytestsubmission.csv", row.names = FALSE)

Directlink <-mean(P5_Train$Revenue!=myprediction$Revenue)
Nondirectlink <-mean(P5_Train$Activity!=myprediction$Activity)

#directlink() find which model works
directlink<-subset(P5_Train,P5_Train$Source=='Directlink')
#nondirectlink() find which model works
nondirectlink<-subset(P5_Train,P5_Train$Revenue=='nondirectlink')

library(rpart)
library(rpart.plot)
colnames(P5_Train)
summary(P5_Train)
data1 <- P5_test1_students

#All Variables
tree1 <-rpart(Revenue ~ Duration+Source+Activity, control = rpart.control(minsplit = 10, minbucket = 150), data = P5_Train)
rpart.plot(tree1)
prp(tree1, type = 1, box.col = "yellow")

pred1<-predict(tree1, newdata= P5_Train)
predictedRevenue<-predict(tree1, newdata=P5_Train)

tree2 <-rpart(Revenue~Duration+Activity, control = rpart.control(minsplit = 10, minbucket = 150), data = P5_Train)
rpart.plot(tree2)
prp(tree2, type = 1, box.col = "yellow")

pred2<-predict(tree2, newdata=P5_Train)
predictedEarning<-predict(tree2, newdata=P5_Train)

data<-P5_Train
# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
#Splitting the data into training and testing
train_ind <- sample(seq_len(nrow(data)), size = sample_size)
# Train data
train <- data[train_ind, ]
# Test data
test <- data[-train_ind, ]

perf.model = svm(Duration~Activity, data = P5_Train)
pred <- predict(perf.model,test)
  
# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Revenue)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))

# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
#Splitting the data into training and testing
train_ind <- sample(seq_len(nrow(data)), size = sample_size)
# Train data
train <- data[train_ind, ]
# Test data
test <- data[-train_ind, ]

perf.model = svm(Duration~Revenue, data = P5_Train)
pred <- predict(perf.model,test)
  
# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Duration)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))

# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
#Splitting the data into training and testing
train_ind <- sample(seq_len(nrow(data)), size = sample_size)
# Train data
train <- data[train_ind, ]
# Test data
test <- data[-train_ind, ]
  
perf.model = svm(Duration~Source, data = P5_Train)
pred <- predict(perf.model,test)
  
# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Duration)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))

P5_Test <- read.csv("P5_test1_students.csv")
mytestprediction <- P5_Test
Testpredict<-predict(P5_, newdata=P5_Test)
Testpredict
mytestprediction$Revenue <- Testpredict

submission <- mytestprediction[,c('ID','Revenue')]
write.csv(submission, file = "mytestsubmission5.csv",row.names=FALSE)
