#-------------------------------------------------------
#Distance

library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
#set.seed(1995) # if using R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]
y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

#distances between feature vectors
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:4,1:4]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
xx <- x[order(y), ]
d <- dist(t(xx))
dim(as.matrix(d))
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))

for(idx in 1:784) {
  dddd <- as.matrix(d)[idx,]
  image(1:28, 1:28, matrix(dddd, 28, 28))
}

#-------------------------------------------------------

#Q1

#Load the following dataset:

library(dslabs)
data("tissue_gene_expression")

#This dataset includes a matrix x:
  
dim(tissue_gene_expression$x)

#This matrix has the gene expression levels of 500 genes from 189 
#biological samples representing seven different tissues. The tissue
#type is stored in y:
  
table(tissue_gene_expression$y)

#Which of the following lines of code computes the Euclidean
#distance between each observation and stores it in the object d?

d <- dist(tissue_gene_expression$x)
class(d)
dm <- as.matrix(d)
dim(dm)
dm[1:3,1:3]

#Q2

#Using the dataset from Q1, compare the distances between

#observations 1 and 2 (both cerebellum)
dm[1,2]

#observations 39 and 40 (both colon)
dm[39,40]

#observations 73 and 74 (both endometrium)
dm[73,74]

#Distance-wise, are samples from tissues of the same type closer to each other?
dm[1,39]
dm[1,40]

dm[1,73]
dm[1,74]

dm[39,73]
dm[39,74]

dm[c(1,2,39,40,73,74),c(1,2,39,40,73,74)]

#Yes, the samples from the same tissue type are closest to each other. 

#Q3

#Make a plot of all the distances using the image function to see
#if the pattern you observed in Q2 is general.

dim(dm)
image(dm)

#Which code would correctly make the desired plot?

image(as.matrix(d))

#-------------------------------------------------------
#KNN

library(caret)

#Use the Logistic regression as a baseline to compare with
fit_glm <- glm(y ~ x_1 + x_2, data = mnist_27$train, family = "binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(y_hat_logistic, mnist_27$test$y)$overall["Accuracy"]

#Uset the KNN3

#Known way to call
fit_knn <- knn3(y ~ x_1 + x_2, data = mnist_27$train)

#Use all the predictors at once
fit_knn <- knn3(y ~ ., data = mnist_27$train)

#A more efficient way for when there is a lot of data
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist$train$y
fit_knn <- knn(x, y)

#Do the prediction
fit_knn <- knn3(y ~ ., data = mnist_27$train, k=5)

predict_on_test <- function(fit_knn){
  p_hat_knn <- predict(fit_knn, mnist_27$test)
  y_hat_knn <- ifelse(p_hat_knn[,1] > 0.5 , 7, 2) %>% 
    factor(levels=levels(mnist_27$test$y))
  confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
}

predict_on_test(fit_knn)

#-------------------------------------------------------

#An indicator of overtraining is that on a training set we get a 
#significantly higher accuracy thagt on a test set, check the
#training set accuracy for the same example now:

predict_on_train <- function(fit_knn){
  p_hat_knn_train <- predict(fit_knn, mnist_27$train)
  y_hat_knn_train <- ifelse(p_hat_knn_train[,1] > 0.5, 7, 2) %>%
    factor(levels=levels(mnist_27$test$y))
  y_train <- ifelse(mnist_27$train$y == 1.0, 2, 7) %>%
    factor(levels=levels(mnist_27$test$y))
  confusionMatrix(data = y_hat_knn_train, reference = y_train)$overall["Accuracy"]
}

predict_on_train(fit_knn)

#Even worse overfitting will be with the k=1 as then each
#point is accurate but this is an overfitting on the train set
fit_knn <- knn3(y ~ ., data = mnist_27$train, k=1)

predict_on_train(fit_knn)
predict_on_test(fit_knn)

#See how the accuracy changes with different K values on both test and training set
ks <- seq(1,251,2)

accuracy <- map_df(ks, function(k){
  fit_knn <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  train_error <- predict_on_train(fit_knn)
  test_error <- predict_on_test(fit_knn)
  
  tibble(train = train_error, test = test_error)
})

accuracy %>% mutate(k_value = ks) %>%
  gather(type, accuracy, -k_value) %>%
  ggplot(aes(x=k_value, y=accuracy, color=type)) + geom_line() +geom_point()

#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)

#-------------------------------------------------------

#Q1


#-------------------------------------------------------
