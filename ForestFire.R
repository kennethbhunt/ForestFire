#Data set: forestfires.csv
#Predict whether a fire would take place (fire), based on the following predictors: temp,
#RH, wind and rain. Use the following techniques:
 # - k nearest neighbor
#- support vector machine
#- neural networks

Forest <- read.csv('forestfires.csv', stringsAsFactors = F)
str(Forest)

Forest$fire <-as.factor(Forest$fire)

#Check for missing values 
sapply(Forest, function(x) sum(is.na(x)))

Forest <-Forest[complete.cases(Forest),]

#standardize the predictors 
Forest2 <- data.frame(scale(Forest[1:4]))

Forest2 <-cbind(Forest2, fire=Forest$fire)

n <-sample(517, 250)
forest_train <- Forest2[n, ]
forest_test <-Forest2[-n, ]
library(class)

#define the target variable in the training set
fire <-forest_train$fire


pred <- knn(train=forest_train[,-5], test = forest_test[,-5], cl=fire, k=10)

#get the prediction accuracy
mean(pred==forest_test$fire)

###Find optimal number of neighbors
t_knn <-tune.knn(forest_train[,-5], factor(forest_train[,5]), k=1:100)
t_knn #best is 19
plot(t_knn)

#Run knn with optimal k 
pred <- knn(train=forest_train[,-5], test = forest_test[,-5], cl=fire, k=19)
#get the prediction accuracy
mean(pred==forest_test$fire)

###Suport Vector Machine

library(e1071)

n<-sample(517, 250)
forest_train <- Forest[n, ]
forest_test <-Forest[-n, ]



fit <- svm(fire~., data=forest_train, 
           type= "C-classification", kernel="linear", cost=4)
pred <-predict(fit, forest_test)
mean(pred==forest_test$fire)
##53%

###To find best cost (Improve accuracy)
###10 fold cv 
t_lin <- tune.svm(fire~., data=forest_train, cost = 2^(2:8), kernal="linear")
t_lin$best.parameters

##Run SVM with optimun cost

fit <- svm(fire~., data=forest_train, 
           type= "C-classification", kernel="linear", cost=64)
pred <-predict(fit, forest_test)
mean(pred==forest_test$fire)
##Best accuracy 53%


library(neuralnet)

net <- neuralnet(fire~temp+RH+wind+rain, 
                 data=forest_train, hidden=10, algorithm ="rprop+", 
                 err.fct = "sse", act.fct = "logistic", 
                 rep = 1, stepmax = 1e06, threshold = 0.01, linear.output = F)

plot(net)

##Plot neural network without weights
plot(net, show.weights = F)

#Generate the main results error 71.1094
net$result.matrix

#Generate weights list 
net$weights

## Predictions in the test set 
pred <-compute(net, forest_test[,-5])
pred2 <-pred$net.result
head(pred2,5)

## Create a categorical predicted value 
predcat <-ifelse(pred2<0.5, 0,1)

###Classification table 70%
table(predcat, forest_test$fire)
mean(predcat==forest_test$fire)



