library(HistData)
library(broom)
library(tidyverse)
library(lattice)
library(caret)
library(dslabs)
#library(ModelMetrics)

rmse <- function(x,y) {
  mean((x - y)^2)
}

set.seed(1983, sample.kind="Rounding")
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

m <- mean(train_set$son)
# squared loss
mean((m - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father

#Oberve that the linear regression model gives us a better y_hat estimate than the simple mean
mean((y_hat - test_set$son)^2)

#-----------------------------------------------------

#We can use a pre-cooked function to compute the predicions
#It is easier that writing out the linear regression formula,
#Using the model coefficients, by hand.
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

# read help files
?predict.lm
?predict.glm


#-------------------------------------------------------
#Comprehension Check: Linear Regression

#Q1:
#set.seed(1) #if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") #if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
str(dat)

#We will build 100 linear models using the data above and calculate the mean
#and standard deviation of the combined models. First, set the seed to 1 again
#(make sure to use sample.kind="Rounding" if your R is version 3.6 or later).
#Then, within a replicate loop:
# (1) partition the dataset into test and training sets with p=0.5 and using 
#     dat$y to generate your indices,
# (2) train a linear model predicting y from x,
# (3) generate predictions on the test set, and
# (4) calculate the RMSE of that model.
# Then, report the mean and standard deviation (SD) of the RMSEs from all 100 models.

#Report all answers to at least 3 significant digits.
set.seed(1, sample.kind="Rounding") #if using R 3.6 or later
options(digits = 3)

num_repl <- 100

compute_mean_sd <- function(dat, num_repl) {
  rmses <- replicate(num_repl, {
    #(1) Partition the data set into the test and train sets
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    
    #(2) train a linear model predicting y from x
    lin_model <- lm(y~x, data = train_set)
    
    #(3) generate predictions on the test set
    pred_y <- predict(lin_model, test_set)
    
    #(4) calculate the RMSE (Root Mean Square Error) of that model
    rmse(test_set$y, pred_y)
  })
  data.frame(mean = mean(rmses), sd = sd(rmses))
}

res <- compute_mean_sd(dat, num_repl)

cat("Mean RMSE: ", res$mean)
cat("SD RMSE: ", res$sd)

#Q2:

#Now we will repeat the exercise above but using larger datasets. Write a function
#that takes a size n, then
# (1) builds a dataset using the code provided in Q1 but with
#     n observations instead of 100 and without the set.seed(1),
# (2) runs the replicate loop that you wrote to answer Q1, which
#     builds 100 linear models and returns a vector of RMSEs
# (3) calculates the mean and standard deviation.

#Set the seed to 1 (if using R 3.6 or later, use the argument sample.kind="Rounding")
#and then use sapply or map to apply your new function to n <- c(100, 500, 1000, 5000, 10000).

#Hint: You only need to set the seed once before running your function;
#      do not set a seed within your function. Also be sure to use sapply
#      or map as you will get different answers running the simulations
#      individually due to setting the seed.

create_dat_set <- function(ds_size) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = ds_size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  dat
}

test_various_set_sizes <- function(ds_size){
  dat <- create_dat_set(ds_size)
  compute_mean_sd(dat, 100)
}

set.seed(1, sample.kind="Rounding") #if using R 3.6 or later

n <- c(100, 500, 1000, 5000, 10000)

sapply(n, test_various_set_sizes)

#Q4

#Now repeat the exercise from Q1, this time making the correlation between
#x and y larger, as in the following code:

set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#Note what happens to RMSE - set the seed to 1 as before.
num_repl <- 100
set.seed(1, sample.kind="Rounding") #if using R 3.6 or later
res <- compute_mean_sd(dat, num_repl)

cat("Mean RMSE: ", res$mean)
cat("SD RMSE: ", res$sd)

#Q5

#Which of the following best explains why the RMSE in question
#4 is so much lower than the RMSE in question 1?

#In Q1:
#Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)

#In Q4:
#Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)

#mvrnorm - Produces one or more samples from the specified multivariate normal distribution.

#Sigma - a positive-definite symmetric matrix specifying the covariance matrix of the variables.

#When we increase the correlation between x and y, x has more predictive power and thus provides
# a better estimate of y. 

#Q6

#Create a data set using the following code.

set.seed(1, sample.kind="Rounding") #if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

#Note that y is correlated with both x_1 and x_2 but the two predictors are
#independent of each other, as seen by cor(dat).
cor(dat)

#Set the seed to 1, then use the caret package to partition into a test and
#training set of equal size. Compare the RMSE when using just x_1, just x_2
#and both x_1 and x_2. Train a linear model for each.

set.seed(1, sample.kind="Rounding") #if using R 3.6 or later

test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

lin_model_y_x1 <- lm(y~x_1, data = train_set)
lin_model_y_x2 <- lm(y~x_1, data = train_set)
lin_model_y_x1_x2 <- lm(y~x_1+x_2, data = train_set)

y_hat_x1 <- predict(lin_model_y_x1, test_set)
y_hat_x2 <- predict(lin_model_y_x2, test_set)
y_hat_x1_x2 <- predict(lin_model_y_x1_x2, test_set)

rmses <- data.frame(
  model = c("lin_model_y_x1",
             "lin_model_y_x2",
             "lin_model_y_x1_x2"),
  rmse = c(rmse(test_set$y, y_hat_x1),
            rmse(test_set$y, y_hat_x2),
            rmse(test_set$y, y_hat_x1_x2))
)
rmses

#Which of the three models performs the best (has the lowest RMSE)?
rmses$mode[which.min(rmses$rmse)]

#Q7
min(rmses$rmse)

#Q8

#Repeat the exercise from Q6 but now create an example in which x_1 and x_2 are highly correlated.

set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

#Set the seed to 1, then use the caret package to partition into a test and
#training set of equal size. Compare the RMSE when using just x_1, just x_2,
#and both x_1 and x_2.

set.seed(1, sample.kind="Rounding") #if using R 3.6 or later

test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

lin_model_y_x1 <- lm(y~x_1, data = train_set)
lin_model_y_x2 <- lm(y~x_1, data = train_set)
lin_model_y_x1_x2 <- lm(y~x_1+x_2, data = train_set)

y_hat_x1 <- predict(lin_model_y_x1, test_set)
y_hat_x2 <- predict(lin_model_y_x2, test_set)
y_hat_x1_x2 <- predict(lin_model_y_x1_x2, test_set)

rmses <- data.frame(
  model = c("lin_model_y_x1",
            "lin_model_y_x2",
            "lin_model_y_x1_x2"),
  rmse = c(rmse(test_set$y, y_hat_x1),
           rmse(test_set$y, y_hat_x2),
           rmse(test_set$y, y_hat_x1_x2))
)
rmses

#Compare the results from Q6 and Q8. What can you conclude?

#Adding extra predictors can improve RMSE substantially, but not when
#the added predictors are highly correlated with other predictors.

#---------------------------------------------------------------------
#Regression for a Categorical Outcome

library(dslabs)
data("heights")
y <- heights$height

#set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)

y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()

confusionMatrix(y_hat, test_set$sex)$overall[["Accuracy"]]

#------------------------------------------------------------------
#Logistic Regression

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])
range(p_hat)

# fit logistic regression model
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]

#------------------------------------------------------------------
#Case Study: 2 or 7

mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1),        
                             which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2),        
                             which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()


mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)

#------------------------------------------------------------------------
#Comprehension Check: Logistic Regression

#Define a dataset using the following code:
  
#set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
str(dat)

#Note that we have defined a variable x that is predictive of a binary
#outcome y: dat$train %>% ggplot(aes(x, color = y)) + geom_density().

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

#Set the seed to 1, then use the make_data function defined above to
#generate 25 different datasets with mu_1 <- seq(0, 3, len=25). Perform
#logistic regression on each of the 25 different datasets (predict 1 if p>0.5)
#and plot accuracy (res in the figures) vs mu_1 (delta in the figures).”

set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
mu_1 <- seq(0, 3, len=25)
res <- lapply(mu_1, function(x){
  #Get the new data set
  cat("Working with mu_1: ", x," \n")
  my_dat<- make_data(mu_1 = x)
  
  #Train the logistics linear model
  glm_fit <- my_dat$train %>% mutate(y = as.numeric(y==1)) %>%
    glm(y ~ x, data=., family = "binomial")
  
  #Get the predictions for the test set from the data
  p_hat_logit <- predict(glm_fit, newdata = my_dat$test, type = "response")
  
  #Convert the predicted probability data into the outcome using the prediciton rule
  y_hat_logit <- ifelse(p_hat_logit > 0.5, 1, 0) %>% factor(levels = levels(my_dat$test$y))
  
  #Get the accuracy values
  accuracy <- confusionMatrix(y_hat_logit, my_dat$test$y)$overall[["Accuracy"]]
  cat("WIth mu_1: ", x," getting accuracy: ",accuracy, " \n")
  accuracy
})

#Which is the correct plot?
data.frame(delta = mu_1, res = as.numeric(res)) %>%
  ggplot(aes(delta, res)) + geom_point()

