#-------------------------------------------------------------
#Matrix Factorization

library(dslabs)
library(tidyverse)
library(dplyr)
data("movielens")

head(movielens)

train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #3252 is Scent of a Woman used in example
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

rownames(y)<- y[,1]
y <- y[,-1]
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
  knitr::kable()

set.seed(1)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)

X <- jitter(P%*%t(Q))
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

set.seed(1)
options(digits = 2)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

#-------------------------------------------------------------
#SVD and PCA

y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)

#-------------------------------------------------------------
#Comprehension Check: Matrix Factorization

#In this exercise set, we will be covering a topic useful for understanding matrix
#factorization: the singular value decomposition (SVD).

#SVD is a mathematical result that is widely used in machine learning, both in
#practice and to understand the mathematical properties of some algorithms.
#This is a rather advanced topic and to complete this exercise set you will have
#to be familiar with linear algebra concepts such as matrix multiplication,
#orthogonal matrices, and diagonal matrices.

#The SVD tells us that we can decompose an 𝑁×𝑝 matrix 𝑌 with 𝑝<𝑁 as 
#𝑌=𝑈𝐷𝑉⊤
#with 𝑈 and 𝑉 orthogonal of dimensions 𝑁×𝑝 and 𝑝×𝑝 respectively and 𝐷 a 𝑝×𝑝
#diagonal matrix with the values of the diagonal decreasing: 
#  𝑑1,1≥𝑑2,2≥…𝑑𝑝,𝑝

#In this exercise, we will see one of the ways that this decomposition can be useful.
#To do this, we will construct a dataset that represents grade scores for 100 students
#in 24 different subjects. The overall average has been removed so this data represents
#the percentage point each student received above or below the average test score.
#So a 0 represents an average grade (C), a 25 is a high grade (A+), and a -25 represents
#a low grade (F). You can simulate the data like this:

set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

#Our goal is to describe the student performances as succinctly as possible.
#For example, we want to know if these test results are all just a random
#independent numbers. Are all students just about as good? Does being good
#in one subject  imply you will be good in another? How does the SVD help
#with all this? We will go step by step to show that with just three
#relatively small pairs of vectors we can explain much of the variability
#in this 100×24 dataset.

#Q1:

#You can visualize the 24 test scores for the 100 students by plotting an image:

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#How would you describe the data based on this figure?

#The students that test well are at the top of the image and there
#seem to be three groupings by subject. 

#Q2:

#You can examine the correlation between the test scores directly like this:

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Which of the following best describes what you see?

#There is correlation among all tests, but higher if the tests are in
#science and math and even higher within each subject. 

#Q3:

#Remember that orthogonality means that 𝑈⊤𝑈 and 𝑉⊤𝑉 are equal to
#the identity matrix. This implies that we can also rewrite the
#decomposition as

#𝑌𝑉=𝑈𝐷 or 𝑈⊤𝑌=𝐷𝑉⊤

#We can think of 𝑌𝑉 and 𝑈⊤𝑉 as two transformations of 𝑌 that pre
#erve the total variability of 𝑌 since 𝑈 and 𝑉 are orthogonal.

#Use the function svd to compute the SVD of y. This function will
#return 𝑈, 𝑉, and the diagonal entries of 𝐷.

s <- svd(y)
names(s)

#You can check that the SVD works by typing:

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

#Compute the sum of squares of the columns of 𝑌 and store them in ss_y
#Then compute the sum of squares of columns of the transformed 𝑌𝑉 
#and store them in ss_yv. Confirm that sum(ss_y) is equal to sum(ss_yv).

sos <- function(column) {
  sum(column^2)
}
ss_y <- apply(y,2, sos)
ss_yv <- apply(y %*% s$v,2, sos)

options(digits=3)

round(sum(ss_y),0)== round(sum(ss_yv))

#What is the value of sum(ss_y) (and also the value of sum(ss_yv))?
sum(ss_y)

#Q4:

#We see that the total sum of squares is preserved. This is because 𝑉 is orthogonal
#Now to start understanding how 𝑌𝑉 is useful, plot ss_y against the column number
# and then do the same for ss_yv.

p <- length(ss_y)
p

ggplot() +
  geom_point(aes(x = 1:p, y = ss_y, color = "ss_y")) + 
  geom_point(aes(x = 1:p, y = ss_yv, color = "ss_yv"))
  
#What do you observe?

#ss_yv is decreasing and close to 0 for the 4th column and beyond. 

#Q5:

#Now notice that we didn't have to compute ss_yv because we already
#have the answer. How? Remember that 𝑌𝑉=𝑈𝐷 and because 𝑈 is orthogonal,
#we know that the sum of squares of the columns of 𝑈𝐷 are the diagonal
#entries of 𝐷 squared. Confirm this by plotting the square root of ss_yv
#versus the diagonal entries of 𝐷.

ggplot(aes(x = sqrt(ss_yv), y = s$d)) + geom_point()

#Which of these plots is correct?

#The diagonal one

#Q6:

#So from the above we know that the sum of squares of the columns of 𝑌
#(the total sum of squares) adds up to the sum of s$d^2 and that the 
#transformation 𝑌𝑉 gives us columns with sums of squares equal to s$d^2
#Now compute the percent of the total variability that is explained by
#just the first three columns of 𝑌𝑉.

library(tidyverse)
library(dslabs)
library(matrixStats)

yv_col_variability <- ss_yv
total_variability <- sum(yv_col_variability)
proportion <- yv_col_variability/total_variability

ggplot() +
  geom_point(aes(x = 1:p, y = proportion, color = "column variability"))


#What proportion of the total variability is explained by the first three
#columns of 𝑌𝑉?
sum(proportion[1:3])

#Q7:

#Before we continue, let's show a useful computational trick to avoid creating
#the matrix diag(s$d). To motivate this, we note that if we write 𝑈 out in it
#columns [𝑈1,𝑈2,…,𝑈𝑝] then 𝑈𝐷 is equal to

#𝑈𝐷=[𝑈1𝑑1,1,𝑈2𝑑2,2,…,𝑈𝑝𝑑𝑝,𝑝]

#Use the sweep function to compute 𝑈𝐷 without constructing diag(s$d) or using
#matrix multiplication.

ud <- s$u %*% diag(s$d)

ud_sweep <- sweep(s$u, 2, s$d, FUN = "*")

identical(ud, ud_sweep)

#Which code is correct?

identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

#Q8:

#We know that 𝑈1𝑑1,1, the first column of 𝑈𝐷, has the most variability 
#of all the columns of 𝑈𝐷. Earlier we looked at an image of 𝑌 using my_image(y),
#in which we saw that the student to student variability is quite large and 
#that students that are good in one subject tend to be good in all. This implies 
#that the average (across all subjects) for each student should explain a lot of 
#the variability. Compute the average score for each student, plot it against
#𝑈1𝑑1,1, and describe what you find.

stud_avg <- rowMeans(y)

ggplot() +
  geom_point(aes(x=stud_avg, y=-ud[,1]))

#Or alternatively

ggplot() +
  geom_point(aes(x=stud_avg, y=-s$u[,1] * s$d[1]))

#What do you observe?
  
#Q9:

#We note that the signs in SVD are arbitrary because:
  
#  𝑈𝐷𝑉⊤=(−𝑈)𝐷(−𝑉)⊤

#With this in mind we see that the first column of 𝑈𝐷 is almost identical to the
#average score for each student except for the sign.

#This implies that multiplying 𝑌 by the first column of 𝑉 must be performing a
#similar operation to taking the average. Make an image plot of 𝑉 and describe 
#the first column relative to others and how this relates to taking an average.

my_image(s$v)

s$v[,1]

s$v[,1]/sum(s$v[,1])

#How does the first column relate to the others, and how does this relate to 
#taking an average?
  
#The first column is very close to being a constant, which implies that the first 
#column of YV is the sum of the rows of Y multiplied by some constant, and is thus 
#proportional to an average. 

#-------------------------------------------------------------
#Comprehension Check: Clustering

#Q1:

#Load the tissue_gene_expression dataset. Remove the row means and compute the 
#distance between each observation. Store the result in d.

data(tissue_gene_expression)

#Which of the following lines of code correctly does this computation?

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x)) 

#Q2:

#Make a hierarchical clustering plot and add the tissue types as labels.

h <- hclust(d)

#You will observe multiple branches.

plot(h, cex = 0.65, main = "", xlab = "")

#Which tissue type is in the branch farthest to the left?
  
#liever

#Q3:

#Run a k-means clustering on the data with 𝐾=7. Make a table comparing the 
#identified clusters to the actual tissue types. Run the algorithm several
#times to see how the answer changes.

library(stringr)

x_0 <- tissue_gene_expression$x
x_0[is.na(x_0)] <- 0
k <- kmeans(x_0, centers = 7)

groups <- k$cluster

dat <- data.frame( name=rownames(as.matrix(groups)), group=groups) %>%
  filter(str_detect(name, "liver_.*"))

#What do you observe for the clustering of the liver tissue?
ggplot(dat, aes(name, group)) + 
  geom_point()

run_kmeans <- function(idx) {
  k <- kmeans(x_0, centers = 7)
  groups <- k$cluster
  dat <- data.frame( name=rownames(as.matrix(groups)), group=groups) %>%
    filter(str_detect(name, "liver_.*"))
  min(dat$group) == max(dat$group)
}

mean(sapply(1:10000, run_kmeans))

#Q4:

#Select the 50 most variable genes. Make sure the observations show 
#up in the columns, that the predictor are centered, and add a color 
#bar to show the different tissue types. Hint: use the ColSideColors 
#argument to assign colors. Also, use col = RColorBrewer::brewer.pal(11, "RdBu") 
#for a better use of colors.

#Part of the code is provided for you here:
  
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
#BLANK

#Which line of code should replace #BLANK in the code above?
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = color
#The first one as others mess up the order of the colors that is to correspond with the tissue_gene_expression$y
s)
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = rev(c#heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = rev(colors))
#heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))
#e(colors))
