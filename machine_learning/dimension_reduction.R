#-----------------------------------------------------------
#Dimension Reduction

#We consider an example with twin heights. Some pairs are adults, 
#the others are children. Here we simulate 100 two-dimensional
#points that represent the number of standard deviations each
#individual is from the mean height. Each point is a pair of twins.
#We use the mvrnorm function from the MASS package to simulate
#bivariate normal data.

set.seed(1988, sample.kind = "Rounding")
library(MASS)
n <- 100
Sigma <- matrix(c(9, 9 * 0.9, 9 * 0.92, 9 * 1), 2, 2)
x <- rbind(mvrnorm(n / 2, c(69, 69), Sigma),
           mvrnorm(n / 2, c(55, 55), Sigma))

#A scatterplot quickly reveals that the correlation is high and
#that there are two groups of twins, the adults (upper right points)
#and the children (lower left points):
plot(x[,1],x[,2])
#Corelation is high
cor(x[,1],x[,2])

#We can compute these distances between observations using dist:
d <- dist(x)

#Observation 1 and 2 (blue), and observation 1 and 51 (red)
as.matrix(d)[1, 2]
#> [1] 1.98
as.matrix(d)[2, 51]
#> [1] 18.7

#Let’s start with the naive approach of simply removing one of the two dimensions

z <- x[,1]
d1 <- dist(z)

plot(dist(x), dist(z), col="green")
lines(1:50, 1:50, col="red")

#If instead we use a mean squared error (MSE) then the underestimation goes away.

#Note that z is just the first dimension of x so the MSE is trivial
d1 <- d1*sqrt(2)

plot(d, d1, col="green")
lines(1:50, 1:50, col="red")

#Now check on the standard deviation of the original distance and the approximated one 
sd(d-d1)

#Notice that if we instead plot the difference versus the average:
x_lin_trans  <- cbind((x[,2] + x[,1])/2,  x[,2] - x[,1])

#we can see how the distance between points is mostly explained by the first dimension: the average.

plot(x_lin_trans[,1], x_lin_trans[,2], xlab="average", ylab = "distance", ylim=c(-15,15))

#This means that we can ignore the second dimension and not lose too much information. 
# Using the first dimension of this transformed matrix we obtain an even better approximation:

d2 <-dist(x_lin_trans[,1])*sqrt(2)

plot(d, d2, col="green")
lines(1:50, 1:50, col="red")

#With the typical difference (standard deviation) improved by about 35%, compare:
sd(d-d1)
sd(d-d2)

#Dimension reduction can often be described as applying a transformation
#A to a matrix X with many columns that moves the information contained
#in X to the first few columns of Z=AX, then keeping just these few
#informative columns, thus reducing the dimension of the vectors
#contained in the rows.

#-----------------------------------------------------------------
#Orthogonal transformations (advanced)

#In our example, to achieve orthogonality, we multiply the first set of
#coefficients (first column of A) by √2 and the second by 1/√2, then we
#get the same exact distance if we use both dimensions:

z1 <- matrix(1,nrow=length(x[,1]), ncol=2)
z1[,1] <- (x[,1] + x[,2]) / sqrt(2)
z1[,2] <- (x[,2] - x[,1]) / sqrt(2)

d3 <- dist(z1)

#This gives us a transformation that preserves the distance between any two points:
max(d - d3)

#and an improved approximation if we use just the first dimension:
sd(d-dist(z1[,1]))

#In this case Z is called an orthogonal rotation of X: 
#     It preserves the distances between rows.

#Note that by using the transformation above we can summarize
#the distance between any two pairs of twins with just one dimension.
#For example, one-dimensional data exploration of the first dimension of Z
#clearly shows that there are two groups, adults and children:
  
library(tidyverse)
qplot(z1[,1], bins = 20, color = I("black"))

#We successfully reduced the number of dimensions from two to one with very little loss of information.

#The reason we were able to do this is because the columns of X were very correlated:
 
cor(x[,1], x[,2])

#and the transformation produced uncorrelated columns with “independent”
#information in each column:

cor(z1[,1], z1[,2])

#-----------------------------------------------------------
#Principal component analysis

#In the computation above, the total variability in our data can be
#defined as the sum of the sum of squares of the columns. We assume
#the columns are centered, so this sum is equivalent to the sum of
#the variances of each column:

#We can compute v1 and v2 using:
cmx <- colMeans(x^2)

#We can show mathematically that if we apply an orthogonal transformation
#as above, then the total variation remains the same:

sum(cmx)
cmz1 <- colMeans(z1^2)
sum(cmz1)

#However, while the variability in the two columns of X is about the
#same, in the transformed version Z 99% of the variability is included
#in just the first dimension:

cmz1/sum(cmz1)

#The first principal component (PC) of a matrix X is the linear
#orthogonal transformation of X that maximizes this variability.
#The function prcomp provides this info:

pca <- prcomp(x)

#Stores the needed rotation
pca$rotation

#Stores the rotated matrix
pca$x

#Using the matrix multiplication shown above, we have that the following are the same 

a <- sweep(x, 2, colMeans(x)) #Here we center the columns
b <- pca$x %*% t(pca$rotation) #The rotation is orthogonal which means that the inverse is its transpose.
max(abs(a - b))

#For a multidimensional matrix with X with p columns, we can find a transformation
#that creates Z that preserves distance between rows, but with the variance of the
#columns in decreasing order. The second column is the second principal component,
#the third column is the third principal component, and so on. As in our example,
#if after a certain number of columns, say k, the variances of the columns of Zj,
#j>k are very small, it means these dimensions have little to contribute to the
#distance and we can approximate distance between any two points with just k dimensions.
#If k is much smaller than p, then we can achieve a very efficient summary of our data.


#-----------------------------------------------------------
#Comprehension Check: Dimension Reduction

#Q1:

#We want to explore the tissue_gene_expression predictors by plotting them.

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#We want to get an idea of which observations are close to each other, but, as you
#can see from the dimensions, the predictors are 500-dimensional, making plotting
#difficult. Plot the first two principal components with color representing tissue type.

pca <- prcomp(tissue_gene_expression$x)

x_1_x_2 <- pca$x[,1:2]

dat <- as_tibble(x_1_x_2) %>% 
  mutate(ttype = tissue_gene_expression$y) # %>% 
#gather(gene, gep, -ttype)

dat %>% ggplot(aes(x=PC1, y=PC2, color=ttype)) + geom_point()

#Which tissue is in a cluster by itself?

#liver

#Q2:

#The predictors for each observation are measured using the same device and experimental
#procedure. This introduces biases that can affect all the predictors from one observation.
#For each observation, compute the average across all predictors, and then plot this
#against the first PC with color representing tissue. Report the correlation.

obs_avg <- rowMeans(tissue_gene_expression$x)

dat <- dat %>% mutate(obs_avg = obs_avg)

dat %>% ggplot(aes( x=PC1, y=obs_avg, color=ttype)) + geom_point()

#What is the correlation?
cor(dat$PC1, dat$obs_avg)

#Q3:

#We see an association with the first PC and the observation averages.
#Redo the PCA but only after removing the center. Part of the code is
#provided for you.

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Which line of code should be used to replace #BLANK in the code block above?

#Q4:

#For the first 10 PCs, make a boxplot showing the values for each tissue.

as.tibble(pc$x[,1:10]) %>% 
  mutate(type = tissue_gene_expression$y) %>%
  gather(pc, value, -type) %>%
  ggplot(aes(x=type,y=value)) + geom_boxplot() +
  facet_wrap(~pc, scales = "free")

#For the 7th PC, which two tissues have the greatest median difference?
data.frame(pc = pc$x[,7], tissue = tissue_gene_expression$y) %>%
  ggplot(aes(x = tissue, y = pc)) +
  geom_boxplot()

#Q5:

#Plot the percent variance explained by PC number.
#Hint: use the summary function.

sum_pc <- summary(pc)
pfv <- sum_pc$importance["Proportion of Variance",]
plot(1:length(pfv), pfv)

#How many PCs are required to reach a cumulative percent variance
#explained greater than 50%?

cum_var <- 0
for(idx in 1:length(pfv)) {
  cum_var <- cum_var + pfv[[idx]]
  if(cum_var > 0.5){
    cat("Required ", idx," PCs\n")
    break
  }
}


