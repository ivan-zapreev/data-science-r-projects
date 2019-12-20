library(caret)
library(dslabs)
library(tidyverse)
library(broom)

#Get the data for predictin the person's sex from its height
y<-heights$sex
x<-heights$height

#Split the data into the training and test sets
#set.seed(2007)
set.seed(2007, sample.kind="Rounding")

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights[-test_index,]
nrow(train_set)
test_set <- heights[test_index,]
nrow(test_set)

#The trivial "Guessing the outcome" approach
y_hat <- sample(c("Male", "Female"), length(test_index), replace=TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace=TRUE) %>% 
  factor(levels = levels(test_set$sex))

#Compute the proportion of properly guessed values, the value is abou 0.5 as we are just guessing
mean(y_hat == test_set$sex)

#Check if there is a difference between males and females we could use.
#We will see that on average males are taller than females
stats <- heights %>% group_by(sex) %>%
  summarize(avg=mean(height), se=sd(height))

#Predict male if it is above two sd from the average male
#We include the highet heights also as males, increasing the success rate
cut_off <- round(stats$avg[2] - 2*stats$se[2])
cut_off
y_hat <- ifelse((cut_off <= test_set$height) , "Male", "Female") %>% 
  factor(levels=levels(test_set$sex))

#Compute the proportion of properly guessed values, and we see it is already higher tha
mean(y_hat == test_set$sex)

#Experiment with a bunch of different cut-offs, on the training set
cut_offs <- seq(cut_off-1, cut_off+8)
accuracy <- map_dbl(cut_offs, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
max(accuracy)
best_cutoff <- cut_offs[which.max(accuracy)]
best_cutoff

#plot the accuracy on the test data
data.frame(cut_offs = cut_offs, accuracy = accuracy) %>%
  ggplot(aes(x=cut_offs, y = accuracy)) + geom_line() + geom_point() + 
  geom_vline(xintercept = best_cutoff, color="blue") +
  scale_x_continuous(breaks=cut_offs)

#Evaluate the found best cut-off on the test set, and see that we got even better results
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#How many features are available to us for prediction in the mnist digits dataset?
?read_mnist

#Build the confusion matrix
conf_mtx <- table(predicted = y_hat, actual = test_set$sex)
conf_mtx
#??? stat <- tidy(chisq.test(conf_mtx))
#??? stat

#Compute the accuracy separately for each sex and observe that the 
#accuracy for males is high and for females it is low
test_set %>% mutate(y_hat = y_hat ) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(sex == y_hat))

# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")
prev

confusionMatrix(data = y_hat, reference = test_set$sex)

# maximize F-score instead of the overal accuracy
cut_offs
F_1 <- map_dbl(cut_offs, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
max(F_1)

best_cutoff <- cut_offs[which.max(F_1)]
best_cutoff

#plot the accuracy on the test data
data.frame(cut_offs = cut_offs, accuracy = F_1) %>%
  ggplot(aes(x=cut_offs, y = accuracy)) + geom_line() + geom_point() + 
  geom_vline(xintercept = best_cutoff, color="blue") +
  scale_x_continuous(breaks=cut_offs)

#Test the resulting best cut-off on the test data
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))

#Compute sensitivity and specificity of the result
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

#Compute the F_1 score of the result
F_meas(data = y_hat, reference = factor(train_set$sex))

#Get the confusion matrix of the result
confusionMatrix(data = y_hat, reference = test_set$sex)

#----------------------------------------------------------
#ROC and precision-recall curves

p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


#-----------------------ASSESSMENTS--------------------------
library(dslabs)
library(dplyr)
library(lubridate)
data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)
dat <- dat %>% mutate(sex = factor(sex, c("Female", "Male")),
                      type = factor(type))
str(dat)

#y <- factor(dat$sex, c("Female", "Male"))
#x <- dat$type

#The type column of dat indicates whether students took classes in
#person ("inclass") or online ("online"). What proportion of the
#inclass group is female? What proportion of the online group is female?

#Enter your answer as a percentage or decimal (eg "50%" or "0.50")
#to at least the hundredths place.
options(digits = 3)
stats <- dat %>% group_by(type) %>% summarize(females = sum(sex=="Female")/n())
female_inclass <- stats$females[1]
female_inclass
female_online <- stats$females[2]
female_online

#In the course videos, height cutoffs were used to predict sex.
#Instead of using height, use the type variable. Use what you
#learned about Q1 to make an informed guess about sex based on
#the most prevalent sex for each type. Report the accuracy of
#your prediction of sex based on type. You do not need to split
#the data into training and test sets.

#Enter your accuracy as a percentage or decimal (eg "50%" or "0.50")
#to at least the hundredths place.

y_hat <- ifelse(dat$type == "inclass",
                sample(c("Female","Male"), 1, prob=c(female_inclass, 1.0 - female_inclass)),
                sample(c("Female","Male"), 1, prob=c(female_online, 1.0 - female_online))) %>%
  factor(levels = levels(dat$sex))

mean(y_hat == dat$sex)

#Write a line of code using the table function to show the confusion 
#matrix between y_hat and y. Use the exact format function(a, b) for
#your answer and do not name the columns and rows.
table(predicted = y_hat, actual = dat$sex)

#What is the sensitivity of this prediction? You can use the sensitivity
#function from the caret package. Enter your answer as a percentage or
#decimal (eg "50%" or "0.50") to at least the hundredths place.
sensitivity(y_hat, dat$sex)

#What is the specificity of this prediction? You can use the specificity
#function from the caret package. Enter your answer as a percentage or
#decimal (eg "50%" or "0.50") to at least the hundredths place.
specificity(y_hat, dat$sex)

confusionMatrix(data = y_hat, reference = dat$sex)

#-------------------------------------------------------------

#We will practice building a machine learning algorithm using a new dataset,
#iris, that provides multiple predictors for us to use to train. To start,
#we will remove the setosa species and we will focus on the versicolor and
#virginica iris species using the following code:

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
iris <- iris %>% mutate(Species = droplevels(Species))
y <- iris$Species

#First let us create an even split of the data into train and test partitions
#using createDataPartition. The code with a missing line is given below:

#set.seed(2) # if using R 3.5 or earlier 
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later

test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)

test <- iris[test_index,]
train <- iris[-test_index,]

#Using only the train iris dataset, for each feature, perform a simple
#search to find the cutoff that produces the highest accuracy, predicting
#virginica if greater than the cutoff and versicolor otherwise.
#Use the seq function over the range of each feature by intervals of 0.1
#for this search.

max_accuracy <- function(y, x) {
  #Create a data frame for simplicity
  dat <- data.frame(s = y, p = x)

  #Use the seq function over the range of each feature by intervals of 0.1 for this search.
  cut_offs <- seq(min(dat$p), max(dat$p), 0.1)

  #Perform a simple search to find the cutoff that produces the highest accuracy.
  accuracy <- map_dbl(cut_offs, function(cut_off){
    #Predicting virginica if greater than the cutoff and versicolor otherwise.
    y_hat <- ifelse(dat$p > cut_off, "virginica", "versicolor") %>% 
      factor(levels = levels(dat$s))
    mean(y_hat == dat$s)
  })
  #Report the corresponding maximum accuracy for the cut-off
  data.frame(accuracy = max(accuracy), cut_off = cut_offs[which.max(accuracy)])
}

max_a_sl <- max_accuracy(train$Species, train$Sepal.Length)
max_a_sw <- max_accuracy(train$Species, train$Sepal.Width)
max_a_pl <- max_accuracy(train$Species, train$Petal.Length)
max_a_pw <- max_accuracy(train$Species, train$Petal.Width)

result <- data.frame(feature = factor(c("max_a_sl","max_a_sw","max_a_pl","max_a_pw")), 
                     max_acc = c(max_a_sl$accuracy,max_a_sw$accuracy,max_a_pl$accuracy,max_a_pw$accuracy),
                     cut_off = c(max_a_sl$cut_off,max_a_sw$cut_off,max_a_pl$cut_off,max_a_pw$cut_off)) %>% 
  arrange(desc(max_acc))

result

#Which feature produces the highest accuracy?

result$feature[which.max(result$max_acc)]
result$cut_off[which.max(result$max_acc)]

#For the feature selected in Q8, use the smart cutoff value from
#the training data to calculate overall accuracy in the test data.
#What is the overall accuracy?  
y_hat <- ifelse(test$Petal.Length > 4.7, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

#Notice that we had an overall accuracy greater than 96% in the training data,
#but the overall accuracy was lower in the test data. This can happen often if
#we overtrain. In fact, it could be the case that a single feature is not the
#best choice. For example, a combination of features might be optimal. Using
#a single feature and optimizing the cutoff as we did on our training data can
#lead to overfitting.

#Given that we know the test data, we can treat it like we did our training data
#to see if the same feature with a different cutoff will optimize our predictions.

#Which feature best optimizes our overall accuracy?

max_a_sl <- max_accuracy(test$Species, test$Sepal.Length)
max_a_sw <- max_accuracy(test$Species, test$Sepal.Width)
max_a_pl <- max_accuracy(test$Species, test$Petal.Length)
max_a_pw <- max_accuracy(test$Species, test$Petal.Width)

result <- data.frame(feature = factor(c("max_a_sl","max_a_sw","max_a_pl","max_a_pw")), 
                     max_acc = c(max_a_sl$accuracy,max_a_sw$accuracy,max_a_pl$accuracy,max_a_pw$accuracy),
                     cut_off = c(max_a_sl$cut_off,max_a_sw$cut_off,max_a_pl$cut_off,max_a_pw$cut_off)) %>% 
  arrange(desc(max_acc))

result

result$feature[which.max(result$max_acc)]
result$cut_off[which.max(result$max_acc)]

#Now we will perform some exploratory data analysis on the data. 
plot(iris,pch=21,bg=iris$Species)

#Notice that Petal.Length and Petal.Width in combination 
#could potentially be more information than either feature alone.

#Optimize the the cutoffs for Petal.Length and Petal.Width separately
#in the train dataset by using the seq function with increments of 0.1.
#Then, report the overall accuracy when applied to the test dataset by
#creating a rule that predicts virginica if Petal.Length is greater
#than the length cutoff OR Petal.Width is greater than the width cutoff,
#and versicolor otherwise.

#What is the overall accuracy for the test data now?
y_hat <- ifelse((test$Petal.Length > 4.7) | (test$Petal.Width > 1.5),
                "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

