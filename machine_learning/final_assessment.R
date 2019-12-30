#-----------------------------------------------------
#Breast Cancer Project Part 1

#The brca dataset contains information about breast cancer diagnosis biopsy samples for tumors
#that were determined to be either benign (not cancer) and malignant (cancer).
#The brca object is a list consisting of:
# --- brca$y: a vector of sample classifications ("B" = benign or "M" = malignant)
# --- brca$x: a matrix of numeric features describing properties of the shape and size
#             of cell nuclei extracted from biopsy microscope images

#For these exercises, load the data by setting your options and loading the libraries 
#and data as shown in the code here:

options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

#The exercises in this assessment are available to Verified Learners only and are split
#into four parts, all of which use the data described here.

#IMPORTANT: Some of these exercises use dslabs datasets that were added in a July 2019 update.
#Make sure your package is up to date with the command update.packages("dslabs"). You can also 
#update all packages on your system by running update.packages() with no arguments, and you
#should consider doing this routinely.

#Q1: Dimensions and properties 
class(brca)
length(brca)
dim(brca$x)
length(brca$y)

#How many samples are in the dataset?
length(brca$y)

#How many predictors are in the matrix?
length(brca$x[1,])

#What proportion of the samples are malignant?
mean(brca$y == "M")

#Which column number has the highest mean?
col_mean <- colMeans(brca$x)
which.max(col_mean)

#Which column number has the lowest standard deviation?
col_sd <- colSds(brca$x)
which.min(col_sd)


#Q2: Scaling the matrix 

#Use sweep two times to scale each column: subtract the column mean,
#then divide by the column standard deviation.

x_sc <- sweep(brca$x, 2, col_mean)
mean(x_sc[,1])
x_sc <- sweep(x_sc, 2, col_sd, FUN = "/" )
sd(x_sc[,1])


#After scaling, what is the standard deviation of the first column?
sd(x_sc[,1])

#After scaling, what is the median value of the first column?
median(x_sc[,1])

#Q3: Distance

#Calculate the distance between all samples using the scaled matrix.

d <- dist(x_sc)
plot(d)

#Turn the distance object into a matrix of distances
dm <- as.matrix(d)

#What is the average distance between the first sample, which is
#benign, and other benign samples?

b_samples_idx <- which(brca$y == "B")
mean(dm[1, b_samples_idx])

#What is the average distance between the first sample and malignant samples?

m_samples_idx <- which(brca$y == "M")
mean(dm[1, m_samples_idx])

#Q4: Heatmap of features 

#Make a heatmap of the relationship between features using the scaled matrix.

library(RColorBrewer)

#Which of these heatmaps is correct?
#To remove column and row labels like the images below,
#use labRow = NA and labCol = NA.

#Q4 however, is asking for a heatmap of the relationship between 
#features meaning a heatmap of the pairwise distances between
#predictors and thus the result of dist(t(x)) is a squared
#symmetric matrix of 30x30 pairwise predictors' distance.
feature_dist <- dist(t(x_sc))
heatmap(as.matrix(feature_dist), labRow = NA, labCol = NA)

#Q5: Hierarchical clustering 

#Perform hierarchical clustering on the 30 features. 
#Cut the tree into 5 groups.

feature_h <- hclust(feature_dist)
plot(feature_h)

feature_groups <- cutree(feature_h, k = 5)

#All but one of the answer options are in the same group.
answers <- c("smoothness_mean", 
             "smoothness_worst",
             "compactness_mean",
             "compactness_worst",
             "concavity_mean",
             "concavity_worst")

index <- names(feature_groups) %in% answers

feature_groups[index]

#Which is in a different group?
  
#concavity_mean

#-----------------------------------------------------
#Breast Cancer Project Part 2

#Q6: PCA: proportion of variance 

#Perform a principal component analysis of the scaled matrix.

pca_sc <- prcomp(x_sc)

sum_pca_sc <- summary(pca_sc)
pfv_sc <- sum_pca_sc$importance["Proportion of Variance",]
plot(1:length(pfv_sc), pfv_sc)

#What proportion of variance is explained by the first principal component?

pfv_sc[1]

#How many principal components are required to explain at least 90% of the variance?

cum_var <- 0
for(idx in 1:length(pfv_sc)) {
  cum_var <- cum_var + pfv_sc[[idx]]
  if(cum_var >= 0.9){
    cat("Required ", idx," PCs\n")
    break
  }
}

#Q7: PCA: plotting PCs 

#Plot the first two principal components with color representing 
#tumor type (benign/malignant).

ggplot() +
  geom_point(aes(x=pca_sc$x[,1], y=pca_sc$x[,2], color=brca$y))

#Which of the following is true?

#Malignant tumors tend to have larger values of PC1 than benign tumors. 

#Q8: PCA: PC boxplot 

#Make a boxplot of the first 10 PCs grouped by tumor type.

pca_selected <- pca_sc$x[,1:10]

as_tibble(pca_selected) %>% 
  mutate(type=brca$y) %>%
  gather(pc,value,-type) %>%
  group_by(type) %>%
  ggplot(aes(pc, value, color=type)) +
  geom_boxplot()

#Which PCs are significantly different enough by tumor type 
#that there is no overlap in the interquartile ranges (IQRs) 
#for benign and malignant samples?
#Select ALL that apply.

#PC1 only

#-----------------------------------------------------
#Breast Cancer Project Part 3

#Set the seed to 1, then create a data partition splitting brca$y 
#and the scaled version of the brca$x matrix into a 20% test set 
#and 80% train using the following code:

x_scaled <- x_sc

#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding")  # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

#You will be using these training and test sets throughout the
#exercises in Parts 3 and 4. Save your models as you go, because
#at the end, you'll be asked to make an ensemble prediction and
#to compare the accuracy of the various models!

#Q9: Training and test sets 

#Check that the training and test sets have similar proportions
#of benign and malignant tumors.

#What proportion of the training set is benign?

mean(train_y == "B")

#What proportion of the test set is benign?

mean(test_y == "B")

#Q10a: K-means Clustering 

#The predict_kmeans function defined here takes two arguments - 
#a matrix of observations x and a k-means object k - and assigns
#each row of x to a cluster from k.

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

#Set the seed to 3. Perform k-means clustering on the training set with
#2 centers and assign the output to k. Then use the predict_kmeans
#function to make predictions on the test set.

set.seed(3, sample.kind = "Rounding")  # if using R 3.6 or later

fit_kmeans <- kmeans(train_x, centers = 2)
fit_kmeans

y_hat_kmeans <- predict_kmeans(test_x, fit_kmeans)
y_hat_kmeans <- ifelse(y_hat_kmeans == 1, "B", "M") %>% factor(levels=levels(test_y))
y_hat_kmeans

#What is the overall accuracy?
acc_kmeans <- mean(y_hat_kmeans == test_y)
acc_kmeans

#Q10b: K-means Clustering

#What proportion of benign tumors are correctly identified?
b_index = which(test_y=="B")
mean(y_hat_kmeans[b_index] == test_y[b_index])

#What proportion of malignant tumors are correctly identified?
m_index = which(test_y=="M")
mean(y_hat_kmeans[m_index] == test_y[m_index])

#Q11: Logistic regression model

#Fit a logistic regression model on the training set using all predictors.
#Ignore warnings about the algorithm not converging.

fit_glm <-train(train_x, train_y, method = "glm")

#Make predictions on the test set.

y_hat_glm <- predict(fit_glm, test_x)

#What is the accuracy of the logistic regression model on the test set?
acc_glm <- mean(y_hat_glm == test_y)
acc_glm

#Q12: LDA and QDA models 

#Train an LDA model and a QDA model on the training set.

fit_lda <-train(train_x, train_y, method = "lda")
fit_qda <-train(train_x, train_y, method = "qda")

#Make predictions on the test set using each model.

y_hat_lda <- predict(fit_lda, test_x)
y_hat_qda <- predict(fit_qda, test_x)

#What is the accuracy of the LDA model on the test set?
acc_lda <- mean(y_hat_lda == test_y)
acc_lda

#What is the accuracy of the QDA model on the test set?
acc_qda <- mean(y_hat_qda == test_y)
acc_qda

#Q13: Loess model 

#Set the seed to 5, then fit a loess model on the training set with the caret package.
#You will need to install the gam package if you have not yet done so.
#Use the default tuning grid. This may take several minutes; ignore warnings.

set.seed(5, sample.kind = "Rounding")  # if using R 3.6 or later

fit_loess <-train(train_x, train_y, method = "gamLoess")

#Generate predictions on the test set.
y_hat_loess <- predict(fit_loess, test_x)

#What is the accuracy of the loess model on the test set?
acc_loess <- confusionMatrix(y_hat_loess, test_y)$overall["Accuracy"]
acc_loess

#-----------------------------------------------------
#Breast Cancer Project Part 4

#Q14: K-nearest neighbors model 

#Set the seed to 7, 

set.seed(7, sample.kind = "Rounding")  # if using R 3.6 or later

#Train a k-nearest neighbors model on the training set using the caret package.
#Try odd values of 𝑘 from 3 to 21.
fit_knn <- train(train_x, train_y,
                   method = "knn", 
                   tuneGrid = data.frame(k = seq(3, 21, 2)))
ggplot(fit_knn)

# Use the final model to generate predictions on the test set.

y_hat_knn <- predict(fit_knn$finalModel, test_x, type = "class")
y_hat_knn

#What is the final value of 𝑘 used in the model?

fit_knn$bestTune

#What is the accuracy of the kNN model on the test set?

acc_knn <- confusionMatrix(y_hat_knn, test_y)$overall["Accuracy"]
acc_knn

#Q15a: Random forest model 

#Set the seed to 9

set.seed(9, sample.kind = "Rounding")  # if using R 3.6 or later

#Train a random forest model on the training set using the caret package.
#Test mtry values of 3, 5, 7 and 9.
#Use the argument importance=TRUE so that feature importance can be extracted.

tg <- data.frame(mtry = c(3, 5, 7, 9))

fit_rf <- train(train_x,
                train_y,
                method = "rf",
                tuneGrid = tg,
                importance=TRUE)

#Generate predictions on the test set.

y_hat_rf <- predict(fit_rf, test_x)

#What value of mtry gives the highest accuracy?
fit_rf$bestTune

#What is the accuracy of the random forest model on the test set?
acc_rf <- confusionMatrix(y_hat_rf, test_y)$overall["Accuracy"]
acc_rf

#What is the most important variable in the random forest model?
imp_rf <- varImp(fit_rf)
imp_rf

#Q15b: Random forest model 

#Consider the top 10 most important variables in the random forest model.
imp_rf_mtx <- as.matrix(imp_rf$importance)
as_tibble(imp_rf_mtx) %>% mutate(name = rownames(imp_rf_mtx)) %>% arrange(desc(B)) %>% slice(1:10)

#Which set of features is most important for determining tumor type?

#worst values

#Q16a: Creating an ensemble 

#Create an ensemble using the predictions from the 7 models created in the
#previous exercises: k-means, logistic regression, LDA, QDA, loess, k-nearest
#neighbors, and random forest. Use the ensemble to generate a majority 
#prediction of the tumor type (if most models suggest the tumor is malignant, predict malignant).

y_hat_ensemble <- 
  (as.numeric(y_hat_kmeans) +
     as.numeric(y_hat_glm) +
     as.numeric(y_hat_lda) +
     as.numeric(y_hat_qda) +
     as.numeric(y_hat_loess) +
     as.numeric(y_hat_knn) +
     as.numeric(y_hat_rf)) / 7
y_hat_ensemble <- ifelse(y_hat_ensemble > 1.5, "M", "B") %>% factor(levels = levels(test_y))
y_hat_ensemble

#What is the accuracy of the ensemble prediction?

acc_ensemble <- confusionMatrix(y_hat_ensemble, test_y)$overall["Accuracy"]
acc_ensemble

#16b: Creating an ensemble 

#Make a table of the accuracies of the 7 models and the accuracy of the ensemble model.
acc <- tibble(method = c("kmeans", "glm", "lda", "qda", "loess", "knn", "rf"),
       acc = c(acc_kmeans, acc_glm, acc_lda, acc_qda,acc_loess,acc_knn,acc_rf) ) %>%
  arrange(desc(acc))

#Which of these models has the highest accuracy?
acc %>% slice(1) %>% pull(method)
