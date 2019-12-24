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

#Q1: Training and test sets 

#Split titanic_clean into test and training sets - after running the setup code,
#it should have 891 rows and 9 variables.

as_tibble(titanic_clean)

#Set the seed to 42, then use the caret package to create a 20% data partition
#based on the Survived column. Assign the 20% partition to test_set and the
#remaining 80% partition to train_set.

y <- titanic_clean$Survived

set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean %>% slice(-test_index)
test_set <- titanic_clean %>% slice(test_index)


#How many observations are in the training set?
nrow(train_set)

#How many observations are in the test set?
nrow(test_set)

#What proportion of individuals in the training set survived?
mean(train_set$Survived ==1)

#Q2: Baseline prediction by guessing the outcome 

#The simplest prediction method is randomly guessing the outcome without
#using additional predictors. These methods will help us determine whether
#our machine learning algorithm performs better than chance. How accurate
#are two methods of guessing Titanic passenger survival?

#Set the seed to 3. For each individual in the test set, randomly guess
#whether that person survived or not by sampling from the vector c(0,1).
#Assume that each person has an equal chance of surviving or not surviving.

set.seed(3, sample.kind = "Rounding")

y_hat <- sample(c(0,1), length(test_set$Survived), replace = TRUE) %>% 
  factor(levels=levels(train_set$Survived))

#What is the accuracy of this guessing method?
confusionMatrix(data=y_hat, reference=test_set$Survived)

#Q3a: Predicting survival by sex 

#Use the training set to determine whether members of a given sex were
#more likely to survive or die. Apply this insight to generate survival
#predictions on the test set.

#What proportion of training set females survived?
female_survival_rate <- train_set %>% filter(Sex == "female") %>%
  do(data.frame(mean = mean(.$Survived == 1))) %>% pull(mean)
female_survival_rate

#What proportion of training set males survived?
male_survival_rate <- train_set %>% filter(Sex == "male") %>%
  do(data.frame(mean = mean(.$Survived == 1))) %>% pull(mean)
male_survival_rate

#Q3b: Predicting survival by sex 

#Use the training set to determine whether members of a given sex were
#more likely to survive or die. Apply this insight to generate survival
#predictions on the test set.

#Predict survival using sex on the test set:
#    if the survival rate for a sex is over 0.5, predict survival for all individuals of that sex, 
#    and predict death if the survival rate for a sex is under 0.5.

y_hat_sex <- test_set %>%
  do(data.frame(res = ifelse(.$Sex == "female", 1, 0) )) %>% pull(res) %>%
  factor(levels = levels(test_set$Survived))

#What is the accuracy of this sex-based prediction method on the test set?
cfm_sex <- confusionMatrix(y_hat_sex, test_set$Survived)
cfm_sex$overall["Accuracy"]

#Q4a: Predicting survival by passenger class 

compute_death_rate <- function(data_set, p_class) {
  pclass_survival_rate <- data_set %>% filter(Pclass == p_class) %>%
    do(data.frame(mean = mean(.$Survived == 1))) %>% pull(mean)
  pclass_survival_rate
}

#In which class(es) (Pclass) were passengers more likely to survive than die?
#compute_death_rate(titanic_clean, 1)
#compute_death_rate(titanic_clean, 2)
#compute_death_rate(titanic_clean, 3)

compute_death_rate(train_set, 1)
compute_death_rate(train_set, 2) #Reports 0.5 which does not qualify as (over 0.5)
compute_death_rate(train_set, 3)

#Q4b: Predicting survival by passenger class 

#Predict survival using passenger class on the test set: predict survival if
#the survival rate for a class is over 0.5, otherwise predict death.

y_hat_pclass <- test_set %>%
  do(data.frame(res = ifelse(.$Pclass == 1, 1, 0) )) %>% pull(res) %>%
  factor(levels = levels(test_set$Survived))

#What is the accuracy of this class-based prediction method on the test set?
cfm_pclass <- confusionMatrix(y_hat_pclass, test_set$Survived)
cfm_pclass$overall["Accuracy"]

#Q4c: Predicting survival by passenger class 

train_set %>% group_by(Sex, Pclass) %>% summarise(p = mean(Survived == 1)) %>% filter(p > 0.5)

#Q4d: Predicting survival by passenger class 

#Predict survival using both sex and passenger class on the test set. 
#Predict survival if the survival rate for a sex/class combination is 
#over 0.5, otherwise predict death.

y_hat_sex_pclass <- test_set %>%
  do(data.frame(res = ifelse((.$Pclass == 1 | .$Pclass == 2) & .$Sex == "female", 1, 0) )) %>% pull(res) %>%
  factor(levels = levels(test_set$Survived))

#What is the accuracy of this sex- and class-based prediction method on the test set?
cfm_sex_pclass <- confusionMatrix(y_hat_sex_pclass, test_set$Survived)
cfm_sex_pclass$overall["Accuracy"]

#Q5a: Confusion matrix 

#Use the confusionMatrix function to create confusion matrices for the sex model, 
#class model, and combined sex and class model. You will need to convert predictions 
#and survival status to factors to use this function.

#What is the "positive" class used to calculate confusion matrix metrics?

cfm_sex
cfm_pclass
cfm_sex_pclass
  
#Which model has the highest sensitivity?
cfm_sex$byClass["Sensitivity"]
cfm_pclass$byClass["Sensitivity"]
cfm_sex_pclass$byClass["Sensitivity"]

#Which model has the highest specificity?
cfm_sex$byClass["Specificity"]
cfm_pclass$byClass["Specificity"]
cfm_sex_pclass$byClass["Specificity"]

#Which model has the highest balanced accuracy?
cfm_sex$byClass["Balanced Accuracy"]
cfm_pclass$byClass["Balanced Accuracy"]
cfm_sex_pclass$byClass["Balanced Accuracy"]

#Q5b: Confusion matrix 

#What is the maximum value of balanced accuracy?

cfm_sex$byClass["Balanced Accuracy"]

#Q6: F1 scores 

#Use the F_meas function to calculate 𝐹1 scores for the sex model
#class model, and combined sex and class model. You will need to
#convert predictions to factors to use this function.

#Which model has the highest 𝐹1 score?
  
F_meas(dat = y_hat_sex, test_set$Survived)
F_meas(dat = y_hat_pclass, test_set$Survived)
F_meas(dat = y_hat_sex_pclass, test_set$Survived)

#-----------------------------------------------------
#Titanic Exercises, part 2

library(caret)

#Q7: Survival by fare - LDA and QDA 

#Set the seed to 1. Train a model using linear discriminant analysis (LDA) 
#with the caret lda method using fare as the only predictor.

set.seed(1, sample.kind = "Rounding")
fit_lda <- train(Survived ~ Fare, method="lda", data=train_set)

#What is the accuracy on the test set for the LDA model?
y_hat_lda <- predict(fit_lda, test_set)

confusionMatrix(y_hat_lda, test_set$Survived)$overal["Accuracy"]

#Set the seed to 1. Train a model using quadratic discriminant analysis (QDA) 
#with the caret qda method using fare as the only predictor.

set.seed(1, sample.kind = "Rounding")
fit_qda <- train(Survived ~ Fare, method="qda", data=train_set)

#What is the accuracy on the test set for the QDA model?
y_hat_qda <- predict(fit_qda, test_set)

confusionMatrix(y_hat_qda, test_set$Survived)$overal["Accuracy"]

#Q8: Logistic regression models 

#Set the seed to 1. Train a logistic regression model with the caret glm
#method using age as the only predictor.

set.seed(1, sample.kind = "Rounding")
fit_glm_age <- train(Survived ~ Age, method="glm", data=train_set)

#What is the accuracy on the test set using age as the only predictor?

y_hat_gml_age <- predict(fit_glm_age, test_set, type = "raw")

confusionMatrix(y_hat_gml_age, test_set$Survived)$overall["Accuracy"]

#Set the seed to 1. Train a logistic regression model with the caret glm
#method using four predictors: sex, class, fare, and age.

set.seed(1, sample.kind = "Rounding")
fit_glm_spfa <- train(Survived ~ Sex + Pclass + Fare + Age, method="glm", data=train_set)

#What is the accuracy on the test set using these four predictors?

y_hat_gml_spfa <- predict(fit_glm_spfa, test_set, type = "raw")

confusionMatrix(y_hat_gml_spfa, test_set$Survived)$overall["Accuracy"]

#Set the seed to 1. Train a logistic regression model with the caret glm
#method using all predictors. Ignore warnings about rank-deficient fit.

set.seed(1, sample.kind = "Rounding")
fit_glm_all <- train(Survived ~ ., method="glm", data=train_set)

#What is the accuracy on the test set using all predictors?

y_hat_gml_all <- predict(fit_glm_all, test_set, type = "raw")

confusionMatrix(y_hat_gml_all, test_set$Survived)$overall["Accuracy"]

#Q9a: kNN model 

#Set the seed to 6. Train a kNN model on the training set using caret.
#Try tuning with k = seq(3, 51, 2).

set.seed(6, sample.kind = "Rounding")
fit_knn <- train(Survived ~ . , method="knn", data = train_set, tuneGrid = data.frame(k = seq(3, 51, 2)))

#What is the optimal value of the number of neighbors k?
fit_knn$bestTune

#Q9b: kNN model

#Plot the kNN model to investigate the relationship between the number of 
#neighbors and accuracy on the training set.

ggplot(fit_knn, highlight = TRUE)

#Of these values of 𝑘, which yields the highest accuracy?
  
fit_knn$bestTune

#Q9c: kNN model

#What is the accuracy of the kNN model on the test set?

y_hat_knn <- predict(fit_knn, test_set, type = "raw")

confusionMatrix(y_hat_knn, test_set$Survived)$overall["Accuracy"]

#Q10: Cross-validation

#Set the seed to 8 and train a new kNN model. Instead of the default 
#training control, use 10-fold cross-validation where each partition 
#consists of 10% of the total.

set.seed(8, sample.kind = "Rounding")

tg <- data.frame(k = seq(3, 51, 2))
tc <- trainControl(method = "cv", number = 10, p = 0.9)

fit_knn_cv <- train(Survived ~ . , method="knn", 
                 data = train_set, 
                 tuneGrid = tg,
                 trControl = tc)

#What is the optimal value of k using cross-validation?

ggplot(fit_knn_cv, highlight = TRUE)

fit_knn_cv$bestTune

#What is the accuracy on the test set using the cross-validated kNN model?

y_hat_knn_cv <- predict(fit_knn_cv, test_set, type = "raw")

confusionMatrix(y_hat_knn_cv, test_set$Survived)$overall["Accuracy"]

#Q11a: Classification tree model

#Set the seed to 10. Use caret to train a decision tree with the rpart method.
#Tune the complexity parameter with cp = seq(0, 0.05, 0.002).

set.seed(10, sample.kind = "Rounding")

tg <- data.frame(cp = seq(0, 0.05, 0.002))

fit_rpart <- train(Survived ~ . ,
             data = train_set,
             method="rpart",
             tuneGrid = tg)

ggplot(fit_rpart, highlight=TRUE)

fit_rpart$bestTune$cp

#What is the optimal value of the complexity parameter (cp)?

y_hat_rpart <- predict(fit_rpart, test_set, type = "raw")

confusionMatrix(y_hat_rpart, test_set$Survived)$overall["Accuracy"]

#Q11b: Classification tree model

#Inspect the final model and plot the decision tree.

plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

#Which variables are used in the decision tree?
  

#Q11c: Classification tree model 

#Using the decision rules generated by the final model, predict 
#whether the following individuals would survive.

#A 28-year-old male
x <- data.frame(Sex = "male", Pclass = 1, Age = 28, Fare = 0.0, SibSp = 0, Parch = 0, FamilySize = 0, Embarked = "C")
predict(fit_rpart, x, type = "raw")

#A female in the second passenger class
x <- data.frame(Sex = "female", Pclass = 2, Age = 0, Fare = 0.0, SibSp = 0, Parch = 0, FamilySize = 0, Embarked = "C")
predict(fit_rpart, x, type = "raw")

#A third-class female who paid a fare of $8
x <- data.frame(Sex = "female", Pclass = 3, Age = 0, Fare = 8.0, SibSp = 0, Parch = 0, FamilySize = 0, Embarked = "C")
predict(fit_rpart, x, type = "raw")

#A 5-year-old male with 4 siblings
x <- data.frame(Sex = "male", Pclass = 1, Age = 5, Fare = 0.0, SibSp = 4, Parch = 0, FamilySize = 0, Embarked = "C")
predict(fit_rpart, x, type = "raw")

#A third-class female who paid a fare of $25
x <- data.frame(Sex = "female", Pclass = 3, Age = 0, Fare = 25.0, SibSp = 0, Parch = 0, FamilySize = 0, Embarked = "C")
predict(fit_rpart, x, type = "raw")

#A first-class 17-year-old female with 2 siblings
x <- data.frame(Sex = "female", Pclass = 1, Age = 17, Fare = 0.0, SibSp = 2, Parch = 0, FamilySize = 0, Embarked = "C")
predict(fit_rpart, x, type = "raw")

#A first-class 17-year-old male with 2 siblings
x <- data.frame(Sex = "male", Pclass = 1, Age = 17, Fare = 0.0, SibSp = 2, Parch = 0, FamilySize = 0, Embarked = "C")
predict(fit_rpart, x, type = "raw")

#Q12: Random forest model 

#Set the seed to 14. Use the caret train function with the rf method to
#train a random forest. Test values of mtry ranging from 1 to 7.
#Set ntree to 100.

set.seed(14, sample.kind = "Rounding")

tg <- data.frame(mtry = seq(1, 7, 1))

fit_rf <- train(Survived ~ ., 
                data = train_set,
                method = "rf",
                tuneGrid = tg,
                ntree = 100)

#What mtry value maximizes accuracy?

fit_rf$bestTune$mtry

#What is the accuracy of the random forest model on the test set?

y_hat_rf <- predict(fit_rf, test_set, type = "raw")

confusionMatrix(y_hat_rf, test_set$Survived)$overall["Accuracy"]

#Use varImp on the random forest model object to determine the 
#importance of various predictors to the random forest model.

#What is the most important variable?

varImp(fit_rf)

