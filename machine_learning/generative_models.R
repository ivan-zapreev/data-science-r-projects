#-------------------------------------------------
#Naive Bayes

# Generating train and test set
library("caret")
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Estimating averages and standard deviations
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# Estimating the prevalence
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi

# Getting an actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))

str(p_hat_bayes)

#-------------------------------------------------
#Controlling prevalence

# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Changing the cutoff of the decision rule
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

# Draw plot
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)


#-------------------------------------------------
#qda and lda

##QDA

# Load data
data("mnist_27")

# Estimate parameters from the data
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))

# Contour plots
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5)

# Fit model
library(caret)
train_qda <- train(y ~., method = "qda", data = mnist_27$train)
# Obtain predictors and accuracy
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

# Draw separate plots for 2s and 7s
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)

##LDA

params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

#-------------------------------------------------
#Case Study: More than Three Classes

if(!exists("mnist"))mnist <- read_mnist()

set.seed(3456) #use set.seed(3456, sample.kind="Rounding") in R 3.6 or later
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# binarize the values. Above 200 is ink, below is no ink
x <- x > 200 

# cbind proportion of pixels in upper right quadrant and proportion
#of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])

test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()

train_qda <- train(y ~ ., method = "qda", data = train_set)
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]

train_set %>% mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm")

#-------------------------------------------------

#Q1 

#Create a dataset of samples from just cerebellum and hippocampus,
#two parts of the brain, and a predictor matrix with 10 randomly
#selected columns using the following code:

library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later

ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

sub_tge <- cbind(as.tibble(y), as.tibble(x))

#Use the train function to estimate the accuracy of LDA.
#For this question, use the entire tissue_gene_expression
#dataset: do not split it into training and test sets
#(understand this can lead to overfitting).

train_lda <- train(value ~ ., method = "lda", data = sub_tge)

#What is the accuracy?
train_lda$results["Accuracy"]

#Q2

#In this case, LDA fits two 10-dimensional normal distributions. 
#Look at the fitted model by looking at the finalModel component 
#of the result of train. Notice there is a component called means 
#that includes the estimated means of both distributions. Plot
#the mean vectors against each other and determine which predictors
#(genes) appear to be driving the algorithm.

#Which TWO genes appear to be driving the algorithm?
train_lda$finalModel$means

as.tibble(train_lda$finalModel$means) %>% 
  mutate(name = c("cerebellum","hippocampus")) %>%
  gather(gene, mean, -name) %>% group_by(name) %>%
  ggplot(aes(x=gene, y=mean, color=name)) +
  geom_point() + geom_text(aes(label=gene))

#Q3

#Repeat the exercise in Q1 with QDA.

#Create a dataset of samples from just cerebellum and hippocampus,
#two parts of the brain, and a predictor matrix with 10 randomly
#selected columns using the following code:

library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") #if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#Use the train function to estimate the accuracy of QDA. For this
#question, use the entire tissue_gene_expression dataset: do not
#split it into training and test sets (understand this can lead
#to overfitting).

train_qda <- train(value ~ ., method = "qda", data = sub_tge)

#What is the accuracy?
  
train_qda$results["Accuracy"]

#Q4

#Which TWO genes drive the algorithm when using QDA instead of LDA?

train_qda$finalModel$means

as.tibble(train_qda$finalModel$means) %>% 
  mutate(name = c("cerebellum","hippocampus")) %>%
  gather(gene, mean, -name) %>% group_by(name) %>%
  ggplot(aes(x=gene, y=mean, color=name)) +
  geom_point() + geom_text(aes(label=gene))

#Q5

#One thing we saw in the previous plots is that the values of the predictors 
#correlate in both groups: some predictors are low in both groups and others 
#high in both groups. The mean value of each predictor found in colMeans(x)
#is not informative or useful for prediction and often for purposes of
#interpretation, it is useful to center or scale each column. This can be
#achieved with the preProcess argument in train. Re-run LDA with
#preProcess = "center". Note that accuracy does not change, but it is now
#easier to identify the predictors that differ more between groups than
#based on the plot made in Q2.

train_lda_cent <- train(value ~ ., method = "lda", data = sub_tge, preProcess = "center")

train_lda_cent$results["Accuracy"]

#Which TWO genes drive the algorithm after performing the scaling?

train_lda_cent$finalModel$means

as.tibble(train_lda_cent$finalModel$means) %>% 
  mutate(name = c("cerebellum","hippocampus")) %>%
  gather(gene, mean, -name) %>% group_by(name) %>%
  ggplot(aes(x=gene, y=mean, color=name)) +
  geom_point() + geom_text(aes(label=gene))

#Q6

#Now we are going to increase the complexity of the challenge slightly.
#Repeat the LDA analysis from Q5 but using all tissue types. Use the
#following code to create your dataset:

library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") #if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

full_tge <- cbind(as.tibble(y), as.tibble(x))

train_lda_cent <- train(value ~ ., method = "lda", data = full_tge, preProcess = "center")

#What is the accuracy using LDA?

train_lda_cent$results["Accuracy"]
