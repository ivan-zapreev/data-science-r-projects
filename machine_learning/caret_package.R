#------------------------------------------------
#Caret Package

library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

#------------------------------------------------
#Tuning Parameters with Caret

getModelInfo("knn")
modelLookup("knn")

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)

train_knn$bestTune

train_knn$finalModel

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

install.packages("gam")
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1

#------------------------------------------------

#Q1 

#Use the rpart function to fit a classification tree to the 
#tissue_gene_expression dataset. Use the train function to 
#estimate the accuracy. Try out cp values of seq(0, 0.1, 0.01).

#Plot the accuracies to report the results of the best model.
#Set the seed to 1991.

#Which value of cp gives the highest accuracy?

data(tissue_gene_expression)

set.seed(1, sample.kind = "Rounding")

tg <- data.frame(cp = seq(0, 0.1, 0.01))

fit <- train(tissue_gene_expression$x, tissue_gene_expression$y,
             method="rpart", tuneGrid = tg)

ggplot(fit, highlight=TRUE)

fit$bestTune$cp

#Q2

#Note that there are only 6 placentas in the dataset. By default,
#rpart requires 20 observations before splitting a node. That means
#that it is difficult to have a node in which placentas are the
#majority. Rerun the analysis you did in the exercise in Q1, but
#this time, allow rpart to split any node by using the argument
#control = rpart.control(minsplit = 0). Look at the confusion 
#matrix again to determine whether the accuracy increases. Again,
#set the seed to 1991.

#What is the accuracy now?

set.seed(1991, sample.kind = "Rounding")

tg <- data.frame(cp = seq(0, 0.1, 0.01))

fit_rpart <- train(tissue_gene_expression$x,
             tissue_gene_expression$y,
             method = "rpart",
             tuneGrid = tg,
             control = rpart.control(minsplit = 0))

fit_rpart$results$Accuracy

ggplot(fit_rpart, highlight = TRUE)

confusionMatrix(predict(fit_rpart, tissue_gene_expression$x),
                tissue_gene_expression$y)$overall["Accuracy"]

#Q3

#Plot the tree from the best fitting model of the analysis you ran in Q2.

#Which gene is at the first split?

plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

#Q4

#We can see that with just seven genes, we are able to predict the tissue type.
#Now let's see if we can predict the tissue type with even fewer genes using a
#Random Forest. Use the train function and the rf method to train a Random
#Forest model and save it to an object called fit. Try out values of mtry
#ranging from seq(50, 200, 25) (you can also explore other values on your own).
#What mtry value maximizes accuracy? To permit small nodesize to grow as we did
#with the classification trees, use the following argument: nodesize = 1.

#Note: This exercise will take some time to run. If you want to test out your
#code first, try using smaller values with ntree. Set the seed to 1991 again.

library(randomForest)

set.seed(1991, sample.kind = "Rounding")

tg <- data.frame(mtry = seq(50, 200, 25))

fit_rf <- train(tissue_gene_expression$x,
               tissue_gene_expression$y,
               method = "rf",
               tuneGrid = tg,
               nodesize = 1)

#What value of mtry maximizes accuracy?

fit_rf$bestTune$mtry

#Q5

#Use the function varImp on the output of train and save it to an object called imp.

fit<-fit_rf

#What should replace #BLANK in the code above?

imp <- varImp(fit)
imp

#Q6

#The rpart model we ran above produced a tree that used just seven predictors.
#Extracting the predictor names is not straightforward, but can be done.
#If the output of the call to train was fit_rpart, we can extract the names like this:

tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

#Calculate the variable importance in the Random Forest call for these seven
#predictors and examine where they rank.

imp <- varImp(fit_rf)
imp

#What is the importance of the CFHR4 gene in the Random Forest call?

#The importance is the Overall value of the CFHR4 gene in the importance list
#35

#What is the rank of the CFHR4 gene in the Random Forest call?

#The rank is the position of the CFHR4 gene in the importance list
#7

#--------------------------------------------------------
#Titanic Exercises

#The Titanic was a British ocean liner that struck an iceberg and sunk on its
#maiden voyage in 1912 from the United Kingdom to New York. More than 1,500 of
#the estimated 2,224 passengers and crew died in the accident, making this one
#of the largest maritime disasters ever outside of war. The ship carried a wide
#range of passengers of all ages and both genders, from luxury travelers in
#first-class to immigrants in the lower classes. However, not all passengers
#were equally likely to survive the accident. You will use real data about a
#selection of 891 passengers to predict which passengers survived.

#Libraries and data

#Use the titanic_train data frame from the titanic library as the starting
#point for this project.

library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

#Q1

#Split titanic_clean into test and training sets - after running the setup code,
#it should have 891 rows and 9 variables.

#Set the seed to 42, then use the caret package to create a 20% data partition
#based on the Survived column. Assign the 20% partition to test_set and the
#remaining 80% partition to train_set.

#How many observations are in the training set?


#How many observations are in the test set?


#What proportion of individuals in the training set survived?


#Q2
