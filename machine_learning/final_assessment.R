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

heatmap(x_sc, labRow = NA, labCol = NA)


#Q5: 

#-----------------------------------------------------
#Breast Cancer Project Part 2



#-----------------------------------------------------
#Breast Cancer Project Part 3



#-----------------------------------------------------
#Breast Cancer Project Part 4















