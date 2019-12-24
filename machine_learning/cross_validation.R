library(lattice)
library(ggplot2)
library(caret)
library(tidyverse)

#Q1

#Generate a set of random predictors and outcomes using the following code:

#set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

#Because x and y are completely independent, you should not be able to predict
#y using x with accuracy greater than 0.5. Confirm this by running cross-
#validation using logistic regression to fit the model. Because we have so
#many predictors, we selected a random sample x_subset. Use the subset when
#training the model.

#Which code correctly performs this cross-validation?
  
fit <- train(x_subset, y, method = "glm")
fit$results

#Q2

#Now, instead of using a random selection of predictors, we are going to search
#for those that are most predictive of the outcome. We can do this by comparing
#the values for the 𝑦=1 group to those in the 𝑦=0 group, for each predicto
#, using a t-test. You can do perform this step like this:

install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

#Which of the following lines of code correctly creates a vector of the p-values called pvals?

pvals <- tt$p.value

#Q3

#Create an index ind with the column numbers of the predictors that were
#"statistically significantly" associated with y. Use a p-value cutoff
#of 0.01 to define "statistically significantly."

ind <- which(pvals <= 0.01)
ind

#How many predictors survive this cutoff?
length(ind)

#Q4

#Now re-run the cross-validation after redefinining x_subset to be the
#subset of x defined by the columns showing "statistically significant"
#association with y.

x_subset <- x[ , ind]

#What is the accuracy now?

fit <- train(x_subset, y, method = "glm")
fit$results

#Q5

#Re-run the cross-validation again, but this time using kNN. Try out the
#following grid k = seq(101, 301, 25) of tuning parameters.
#Make a plot of the resulting accuracies.

#Which code is correct?
  
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

#Q6

#In the previous exercises, we see that despite the fact that x and y are
#completely independent, we were able to predict y with accuracy higher than
#70%. We must be doing something wrong then.

#What is it?

#We used the entire dataset to select the columns used in the model. 

#Q7

#Use the train function with kNN to select the best k for predicting tissue
#from gene expression on the tissue_gene_expression dataset from dslabs.
#Try k = seq(1,7,2) for tuning parameters. For this question, do not split
#the data into test and train sets (understand this can lead to overfitting,
#but ignore this for now).

library(dslabs)
data(tissue_gene_expression)

fit <- train(tissue_gene_expression$x, tissue_gene_expression$y,
             method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fit)

#What value of k results in the highest accuracy?
fit

#------------------------------------------------------------------------
#Bootstrap

n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

#set.seed(1995)
set.seed(1995, sample.kind="Rounding") #instead if using R 3.6 or later
N <- 250
X <- sample(income, N)
M<- median(X)
M

library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)

B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()

quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

mean(M) + 1.96 * sd(M) * c(-1,1)

mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)

#------------------------------------------------------------------------

#Q1

#The createResample function can be used to create bootstrap samples.
#For example, we can create 10 bootstrap samples for the mnist_27 dataset like this:

set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)
str(indexes)

#How many times do 3, 4, and 7 appear in the first resampled index?
sum(indexes$Resample01==3)
sum(indexes$Resample01==4)
sum(indexes$Resample01==7)

#Q2

#We see that some numbers appear more than once and others appear no times.
#This has to be this way for each dataset to be independent.
#Repeat the exercise for all the resampled indexes.

dat <- as_tibble(indexes) %>% gather(row, value)
sum(dat$value == 3)

#What is the total number of times that 3 appears in all of the resampled indexes?

#Q3

#Generate a random dataset using the following code:
  
y <- rnorm(100, 0, 1)

#Estimate the 75th quantile, which we know is qnorm(0.75),
#with the sample quantile: quantile(y, 0.75).

qnorm(0.75)
quantile(y, 0.75)

#Set the seed to 1 and perform a Monte Carlo simulation with 10,000
#repetitions, generating the random dataset and estimating the 75th
#quantile each time. What is the expected value and standard error
#of the 75th quantile?

set.seed(1, sample.kind="Rounding")

B <- 10000
q75_sample <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(q75_sample)
sd(q75_sample)

#Q4

#In practice, we can't run a Monte Carlo simulation. Use the sample:
set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)

#Set the seed to 1 again after generating y and use 10 bootstrap
#samples to estimate the expected value and standard error of the 75th quantile.

run_exercise_q4 <- function(y, bsss){
  indexes <- createResample(y, bsss)
  index_mtx <- as.tibble(indexes) %>% as.matrix()
  
  from_idx_to_q_val <- function(row_val) {
    #cat("row_val = ", row_val, "\n")
    #cat("y[row_val] = ", y[row_val], "\n")
    #cat("\n")
    quantile(y[row_val], 0.75)
  }
  
  q75_sample <- apply(index_mtx, 2, from_idx_to_q_val)
  q75_sample <- q75_sample %>% as.vector()
  
  cat("Mean q75_sample = ", mean(q75_sample), "\n" )
  cat("SD q75_sample = ", sd(q75_sample), "\n" )
}

set.seed(1, sample.kind="Rounding")

run_exercise_q4(y, 10)

#Q5 

#Repeat the exercise from Q4 but with 10,000 bootstrap samples instead of 10. Set the seed to 1. 
set.seed(1, sample.kind="Rounding")

run_exercise_q4(y, 10000)

#Q6

#When doing bootstrap sampling, the simulated samples are drawn
#from the empirical distribution of the original data.

#True or False: The bootstrap is particularly useful in situations
#in which a tractable variance formula does exist.


