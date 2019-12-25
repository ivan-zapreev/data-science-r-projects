#-----------------------------------------------------------
# Case Study: MNIST 

library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(123, sample.kind = "Rounding")
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
#note that the line above is the corrected code - code in video at 0:52 is incorrect
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

#-----------------------------------------------------------
#Preprocessing MNIST Data

library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

#-----------------------------------------------------------
#

colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
ggplot(train_knn)

n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
fit_knn <- knn3(x[ ,col_index], y,  k = 5)

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2]

library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}

#-----------------------------------------------------------
#Variable Importance

library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
imp <- importance(rf)
imp

image(matrix(imp, 28, 28))

p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

#-----------------------------------------------------------
#Ensembles

p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)

#-----------------------------------------------------------
#Comprehension Check: Ensembles

#For these exercises we are going to build several machine learning models for 
#the mnist_27 dataset and then build an ensemble. Each of the exercises in this
#comprehension check builds on the last.

#Q1: 

#Use the training set to build a model with several of the models available from
#the caret package. We will test out 10 of the most common machine learning models
#in this exercise:

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

#Apply all of these models using train with all the default parameters.
#You may need to install some packages. Keep in mind that you will probably
#get some warnings. Also, it will probably take a while to train all of
#the models - be patient!
  
#Run the following code to train the various models:

library(caret)
library(dslabs)
set.seed(1, sample.kind = "Rounding") # in R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

#Did you train all of the models?

#Yes

#Q2:

#Now that you have all the trained models in a list, use sapply or map
#to create a matrix of predictions for the test set. You should end up
#with a matrix with length(mnist_27$test$y) rows and length(models) columns.

predicts <- sapply(fits, function(fit_mdl) {
  y_hat <- predict(fit_mdl, mnist_27$test)
})

#What are the dimensions of the matrix of predictions?

str(predicts)
class(predicts)

#Number of rows:

nrow(predicts)
nrow(predicts) == length(mnist_27$test$y)

#Number of columns:

ncol(predicts)
ncol(predicts) == length(models)

#Q3:

#Now compute accuracy for each model on the test set.

acc_models <- apply(predicts, 2, function(y_hat_model){
  y_hat_model <- factor(y_hat_model)
  confusionMatrix(y_hat_model, mnist_27$test$y)$overall["Accuracy"]
})

names(acc_models) <- models

#Report the mean accuracy across all models.
mean(acc_models)

#Q4:

#Next, build an ensemble prediction by majority vote and compute the accuracy of the ensemble.

y_hat_ens <- factor(apply(predicts, 1, function(y_hat_models){
  if(mean( y_hat_models == "2") > 0.5) {
    "2"
  } else {
    "7"
  }
}))

#What is the accuracy of the ensemble?

acc_ens <- confusionMatrix(y_hat_ens, mnist_27$test$y)$overall["Accuracy"]

#Q5:

#In Q3, we computed the accuracy of each method on the test set and noticed that
#the individual accuracies varied.

#How many of the individual methods do better than the ensemble?

sum(acc_models > acc_ens)

models[which(acc_models > acc_ens)]

#Q6:

#It is tempting to remove the methods that do not perform well and re-do the ensemble.
#The problem with this approach is that we are using the test data to make a decision.
#However, we could use the minimum accuracy estimates obtained from cross validation
#with the training data for each model. Obtain these estimates and save them in an object.
#Report the mean of these training set accuracy estimates.

min_train_fit_acc <- sapply(fits, function(fit_mdl) {
  min(fit_mdl$results["Accuracy"])
})

min_train_fit_acc

#What is the mean of these training set accuracy estimates?
mean(min_train_fit_acc)

#Q7:

#Now let's only consider the methods with an estimated accuracy of greater than or equal
#to 0.8 when constructing the ensemble.

accurate_predicts <- predicts[,which(min_train_fit_acc >= 0.8)]

y_hat_ens <- factor(apply(accurate_predicts, 1, function(y_hat_models){
  if(mean( y_hat_models == "2") > 0.5) {
    "2"
  } else {
    "7"
  }
}))

#What is the accuracy of the ensemble now?

acc_predicts_ens <- confusionMatrix(y_hat_ens, mnist_27$test$y)$overall["Accuracy"]
acc_predicts_ens











