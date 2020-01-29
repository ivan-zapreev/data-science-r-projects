#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
# Exploratory Data Analysis
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# Histogram

if(!require(UsingR)) install.packages("UsingR", repos = "http://cran.us.r-project.org")
library(UsingR)

x <- father.son$fheight

round(sample(x,20),1)

#Pure histogram
hist(x, breaks = seq(floor(min(x)), ceiling(max(x))), main="Height histogram", xlab="Height in inches")

#ECDF
xs <- seq(floor(min(x)), ceiling(max(x)), 0.1)
plot(xs, ecdf(x)(xs), type="l", xlab="Height in inches", ylab="ECDF(x)(.)")

#----------------------------------------------------------------------------------
# Histogram Exercises

#--------------------------------------
# Histogram Exercises #1 

# Given the above histogram, how many people are between the ages of 35 and 45?

# 6

#----------------------------------------------------------------------------------
# QQ-plot


# If the data is approximately normal then can estimate the following mean
mean(x > 70)
# Just using the normal distribution
1-pnorm(70, mean(x), sd(x))

# The same is for
mean(x < 59)
# and
pnorm(59, mean(x), sd(x))

# If we can do that for all percentiles and compare consistently then we can say the distributions are close
ps <- seq(0.01, 0.99, 0.01)
qs <- quantile(x, ps)
nqs <- qnorm(ps, mean(x), sd(x))
plot(nqs, qs, xlab="Theoretical distribution", ylab="Data distribution")
abline(0, 1)

# If the probability mass per pecentile agree with two distributions, 
# i.e. fall on the diagonal line then they are likely to be the same.

# The same can be achieved by build-in funcitons, for the normal distributions
qqnorm(x)
qqline(x)

#----------------------------------------------------------------------------------
# QQ-plot Exercises

load(file.path("data","skew.RData"))

class(dat)
dim(dat)
ncol(dat)

# Below "mfrow" means we want a multifigure grid filled in row-by-row. Another choice is mfcol.
par(mfrow = c(sqrt(ncol(dat)),sqrt(ncol(dat))))

# Using QQ-plots, compare the distribution of each column of the matrix to a normal. 
for(idx in 1:ncol(dat)) {
  qqnorm(dat[,idx], main=paste("Column: ",idx))
  qqline(dat[,idx])
}

# Identify the two columns which are skewed: 4, 9

# Examine each of these two columns using a histogram. Note which column has
# "positive skew", in other words the histogram shows a long tail to the right
# (toward larger values). Note which column has "negative skew", that is, a
# long tail to the left (toward smaller values). Note that positive skew looks
# like an up-shaping curve in a qqnorm() plot, while negative skew looks like
# a down-shaping curve.
par(mfrow = c(1,2))

idx <- 4
hist(dat[,idx], main=paste("Positive Skew - column ",idx))
idx <- 9
hist(dat[,idx], main=paste("Negative Skew - column ",idx))

#Re-set plotting to one plot
par(mfrow = c(1,1))

#--------------------------------------
# QQ-plot Exercises #1 

# Which column has positive skew (a long tail to the right)?

# 4

#--------------------------------------
# QQ-plot Exercises #2 

# Which column has negative skew (a long tail to the left)?

# 9

#----------------------------------------------------------------------------------
# Boxplot

# The executive salaries are not normally distributed
hist(exec.pay)
qqnorm(exec.pay)
qqline(exec.pay)

# Then using mean and deviation is not enough to describe the data.

# We can use the box plot to visualize this data
boxplot(exec.pay, ylab="10,000s of US dollars", ylim=c(1,400), main="Executive salaries")

# The middle line of the plot is the median - the 50% of the data is left, the 50% is right
mean(exec.pay)
median(exec.pay)

# The box is from 25% to 75% so the Q2 and Q3 quantiles showing where the middle 50% of the data is
# The distance betwen the min(Q2) and max(Q3) is IQR - interquartile range.
# The antennas show the range of the data, normally it is the
#    "minimum": Q1 - 1.5 * IQR
# and the
#    "maximum": Q3 + 1.5 * IQR
# The rest of the data are considered outliers

#----------------------------------------------------------------------------------
# Boxplot Exercises

#--------------------------------------
# Boxplot Exercises #1 

head(InsectSprays)

boxplot(split(InsectSprays$count, InsectSprays$spray))

boxplot(InsectSprays$count ~ InsectSprays$spray)

# Which spray seems the most effective (has the lowest median)?

# C

#--------------------------------------
# Boxplot Exercises #2 
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

# Consider a random sample of finishers from the New York City Marathon in 2002. 
# This dataset can be found in the UsingR package. Load the library and then
# load the nym.2002 dataset.

data(nym.2002, package="UsingR")

# Use boxplots and histograms to compare the finishing times of males and females.
head(nym.2002)

# It seems like men are faster
boxplot(split(nym.2002$time, nym.2002$gender))

#The distributions are right skewed but look similar
par(mfrow = c(1,2))
mdat <- nym.2002 %>% filter(gender=="Male")
hist(mdat$time, 
     breaks = seq(floor(min(mdat$time)), ceiling(max(mdat$time)),2),
     main = "Male finishing times")
fdat <- nym.2002 %>% filter(gender=="Female")
hist(fdat$time,
     breaks = seq(floor(min(fdat$time)), ceiling(max(fdat$time)),2),
     main = "Female finishing times")
par(mfrow = c(1,1))

# The distributions are similar but there is some shift

qqplot(mdat$time, fdat$time, xlab="Male times", ylab="Female times" )
abline(0,1)


#The sift is about 21 minues
median(mdat$time) - median(fdat$time)


# Which of the following best describes the difference?

# Male and females have similar right skewed distributions with
# the former, 20 minutes shifted to the left. 

#----------------------------------------------------------------------------------
# Scatterplot

data("father.son")

x <- father.son$fheight
y <- father.son$sheight

#Ploting the data shows some coreelation between the son and father heights
plot(x, y, xlab="Father's height in inches", ylab="Son's height in inches")
mean(x*y)

# This trend can not be summarized by giving mean or standard deviations of 
# the height distributions, but we can compute it as correlation, which gives
# the angle of the line of a regression line
cor(x, y)

# This can be made more vivid by the boxplots, if we stratify by father's heights
# we see that there is a linear trend in that the higher fathers tend to have higher sons
boxplot(split(y, round(x)))

mean(y[round(x) == 70])

# Now let us transform the data into standard units, i.e.
# center around zero and divide by the standard deviation.
x <- (x - mean(x))/sd(x)
y <- (y - mean(y))/sd(y)

#The next is about the same value as the cor(x,y)
mean(x*y)

# Now compute the means of the son's heights stratified by father's 
# heights. Here round(x*4)/4 subdivided by 0.25 of the unit
means <- tapply(y, round(x*4)/4, mean)
fheights <- as.numeric(names(means))
plot(fheights, means, ylab="Average stratified son's heights")
# Corelation gives the slope for the regression line
abline(0, cor(x,y))

# WARNING: This holds only for the nornally distributed data sets


# If the normal distribution is not approximately followed then we can
# get uncorrelated sets that show trends! 

set.seed(1, sample.kind = "Rounding")

a <- rnorm(100);
b <- rnorm(100);

# No trending
plot(a,b)
abline(0, cor(a,b))

# An almost zero correlation
cor(a,b)
# -0.0009943199

# Make a small mistake that makes the data not normal any more
a[25] <- 25
b[25] <- 26

# This shows a trend
plot(a,b)
abline(0, cor(a,b))

# A significant corelation
cor(a,b)
#0.8829068

#----------------------------------------------------------------------------------
# Scatterplot Exercises

# Consider a random sample of finishers from the New York City Marathon in 2002. 
# This dataset can be found in the UsingR package. Load the library and then load 
# the nym.2002 dataset.
data(nym.2002, package="UsingR")

# Use dplyr to create two new data frames: males and females, with the data for each gender.
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

males <- nym.2002 %>% filter(gender == "Male")
females <- nym.2002 %>% filter(gender == "Female")

#--------------------------------------
# Scatterplot Exercises #1

# For males, what is the Pearson correlation between age and time to finish? 
cor(males$age, males$time)

#--------------------------------------
# Scatterplot Exercises #2

# For females, what is the Pearson correlation between age and time to finish?

cor(females$age, females$time)

#--------------------------------------
# Scatterplot Exercises #3

# If we interpret these correlations without visualizing the data, we would conclude 
# that the older we get, the slower we run marathons, regardless of gender.

# Look at scatterplots and boxplots of times stratified by age groups (20-25, 25-30, etc..).

#Scatter plot
plot((males$age-mean(males$age))/sd(males$age), 
     (males$time-mean(males$time))/sd(males$time))
abline(0, cor(males$age, males$time))

#Boxplot, adding the actual values, to see what it is based upon
mdat <- split(males$time, round(males$age/5)*5)
boxplot(mdat)
stripchart(mdat, vertical=TRUE, method="jitter", pch=16, add=TRUE, col=1)

# Plot the mean times
mx <- round(males$age/5)*5
mmeans <- tapply(males$time, mx, mean)
mheights <- as.numeric(names(mmeans))
plot(mheights, mmeans)

#Scatter plot
plot((females$age-mean(females$age))/sd(females$age), 
     (females$time-mean(females$time))/sd(females$time))
abline(0, cor(females$age, females$time))

#Boxplot, adding the actual values, to see what it is based upon
fdat <- split(females$time, round(females$age/5)*5)
boxplot(fdat)
stripchart(fdat, vertical=TRUE, method="jitter", pch=16, add=TRUE, col=1)

# Plot the mean times
fx <- round(females$age/5)*5
fmeans <- tapply(females$time, fx, mean)
fheights <- as.numeric(names(fmeans))
plot(fheights, fmeans)

# After examining the data, what is a more reasonable conclusion?

# Finish times are constant up through around 50-60, then we get slower.

#----------------------------------------------------------------------------------
# Symmetry of Log Ratios Exercises

# Create a vector time of the sorted times:
time = sort(nym.2002$time)

#--------------------------------------
# Symmetry of Log Ratios Exercises #1

# What is the fastest time divided by the median time?
min(time)/median(time)
# 0.5605402

#--------------------------------------
# Symmetry of Log Ratios Exercises #3

# What is the slowest time divided by the median time?

max(time)/median(time)
# 2.156368

#--------------------------------------
# Symmetry of Log Ratios Exercises #3

# Compare the following two plots.

# 1. A plot of the ratio of times to the median time, with horizontal lines
# at twice as fast as the median time, and twice as slow as the median time.

plot(time/median(time), ylim = c(1/4, 4))
abline(h = c(1/2, 1, 2))

# 2. A plot of the log2 ratio of times to the median time. The horizontal
# lines indicate the same as above: twice as fast and twice as slow.

plot(log2(time/median(time)), ylim = c(-2, 2))
abline(h = -1:1)

#----------------------------------------------------------------------------------
# Plots to Avoid Exercises

#--------------------------------------
# Plots to Avoid Exercises #1 

# When is it appropriate to use pie charts or donut charts? 

# Never. 

#--------------------------------------
# Plots to Avoid Exercises #2 

# The use of pseudo-3D plots in the literature mostly adds: 

# Confusion. 

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
# Robust Summaries
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# Median, MAD, and Spearman Correlation

# We may have situations that are approximated by, for example, 99 data
# points from a standard normal distribution and one large number.
set.seed(1, sample.kind = "Rounding")
x <- c(rnorm(100, 0, 1)) ##real distribution
boxplot(x)

x_b <- x
x_b[23] <- 100 ##mistake made in 23th measurement
boxplot(x_b)

# In statistics we refer to these type of points as outliers.

# A small number of outliers can throw off an entire analysis.
# The initial data was samples from the N(0, 1) distribution
cat("Initial average is", mean(x),"and the SD is", sd(x))
cat("Broken average is",mean(x_b),"and the SD is",sd(x_b))

# The median, defined as the point having half the data larger and half
# the data smaller, is a summary statistic that is robust to outliers.
median(x)
# 0.1139092
median(x_b)
# 0.1684483

# The Median Absolute Seviation (MAD) is a robust summary for the standard deviation
sd(x)
# 0.8981994
mad(x_b)
# 0.8857141

# Spearman correlation - 
# The Spearman correlation follows the general idea of median and MAD,
# that of using quantiles.

# Construct a independent list of numbers, but for which a
# similar mistake was made for the same entry:
set.seed(1, sample.kind = "Rounding")
x <- c(rnorm(100, 0, 1)) ##real distribution
x[23] <- 100             ##mistake made in 23th measurement
y=c(rnorm(100,0,1))      ##real distribution
y[23] <- 84              ##similar mistake made in 23th measurement

library(rafalib)
mypar()

# The correlation is also sensitive to outliers
plot(x, y, main = paste("correlation=", round(cor(x, y), 3)),
     pch = 21, bg = 1, xlim = c(-3, 100), ylim = c(-3, 100))
abline(0, 1)

# We can avoid this if we convert each dataset to ranks and then compute correlation
plot(rank(x), rank(y), main = paste ("correlation=", round(cor(x, y, method="spearman"), 3)),
     pch = 21, bg = 1, xlim = c(-3, 100), ylim =c (-3, 100))
abline(0, 1)

#Ranks split the data regardless of the value, just put values in order with a unit increment

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# So if these statistics are robust to outliers, why would we ever use the non-robust version? 
#
# 1. In general, if we know there are outliers, then median and MAD are recommended over the 
#    mean and standard deviation counterparts.
# 2. However, there are examples in which robust statistics are less powerful than the 
#    non-robust versions.

# Symmetry of log ratios

# Reporting ratios or fold changes are common in the life science.
# Suppose you are studying ratio data showing, say, gene expression before and after treatment.
# You are given ratio data so values larger than 1 imply gene expression was higher after the
# treatment. If the treatment has no effect, we should see as many values below 1 as above 1.

# Ratios are not symmetric, simulatr the ratio of two positive random numbers:
x <- 2^(rnorm(100))
y <- 2^(rnorm(100)) 
ratios <- x / y 

# A histogram seems to suggest that the treatment does in fact have an effect:
mypar(1,2)
hist(ratios)

# The problem here is that ratios are not symmetrical around 1. 
# For example, 1/32 is much closer to 1 than 32/1.

# Taking logs takes care of this problem. 
# The log of ratios are of course symmetric around 0
logratios <- log2(ratios)
hist(logratios)

#----------------------------------------------------------------------------------
# Median, MAD, and Spearman Correlation Exercises



#--------------------------------------
# Median, MAD, and Spearman Correlation Exercises #1



#--------------------------------------
# Median, MAD, and Spearman Correlation Exercises #2



#--------------------------------------
# Median, MAD, and Spearman Correlation Exercises #3



#--------------------------------------
# Median, MAD, and Spearman Correlation Exercises #4



#--------------------------------------
# Median, MAD, and Spearman Correlation Exercises #5

#----------------------------------------------------------------------------------




#----------------------------------------------------------------------------------




#----------------------------------------------------------------------------------




