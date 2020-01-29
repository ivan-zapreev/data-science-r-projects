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
mypar(1,1)

#----------------------------------------------------------------------------------
# Median, MAD, and Spearman Correlation Exercises

# We will use one of the datasets included in R, which contains weight of chicks
# in grams as they grow from day 0 to day 21. This dataset also splits up the
# chicks by different protein diets, which are coded from 1 to 4. 
data(ChickWeight)

# The weights of all observations over time and color the points to represent the Diet:
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

# Reshape the data so that each row is a chick
chick <- reshape(ChickWeight, 
                 idvar = c("Chick","Diet"), 
                 timevar = "Time",
                 direction = "wide")
head(chick)

# Remove any chicks that have missing observations at any time points
chick <- na.omit(chick)

#--------------------------------------
# Median, MAD, and Spearman Correlation Exercises #1

# Focus on the chick weights on day 4 (check the column names of 'chick' and note the numbers). 
# How much does the average of chick weights at day 4 increase if we add an outlier measurement 
# of 3000 grams? Specifically, what is the average weight of the day 4 chicks, including the 
# outlier chick, divided by the average of the weight of the day 4 chicks without the outlier.
# Hint: use c to add a number to a vector. 

cwd4 <- chick[, "weight.4"]
boxplot(cwd4)

cwd4_b <- c(cwd4, 3000)
boxplot(cwd4_b)

abs(mean(cwd4) - mean(cwd4_b))

mean(cwd4_b)/mean(cwd4)
# 2.062407

#--------------------------------------
# Median, MAD, and Spearman Correlation Exercises #2

# In exercise 1, we saw how sensitive the mean is to outliers. Now let's see what 
# happens when we use the median instead of the mean. Compute the same ratio, but
# now using median instead of mean. Specifically, what is the median weight of the
# day 4 chicks, including the outlier chick, divided by the median of the weight
# of the day 4 chicks without the outlier. 

median(cwd4_b)/median(cwd4)
# 1

#--------------------------------------
# Median, MAD, and Spearman Correlation Exercises #3

# Now try the same thing with the sample standard deviation (the sd function in R).
# Add a chick with weight 3000 grams to the chick weights from day 4. How much does
# the standard deviation change? What's the standard deviation with the outlier 
# chick divided by the standard deviation without the outlier chick? 

sd(cwd4_b)/sd(cwd4)
# 101.2859

#--------------------------------------
# Median, MAD, and Spearman Correlation Exercises #4

# Compare the result above to the median absolute deviation in R, which is calculated 
# with the mad function. Note that the mad is unaffected by the addition of a single 
# outlier. The mad function in R includes the scaling factor 1.4826, such that mad and 
# sd are very similar for a sample from a normal distribution. What's the MAD with the 
# outlier chick divided by the MAD without the outlier chick?

mad(cwd4_b)/mad(cwd4)
# 1

#--------------------------------------
# Median, MAD, and Spearman Correlation Exercises #5

# Our last question relates to how the Pearson correlation is affected by an outlier as 
# compared to the Spearman correlation. The Pearson correlation between x and y is given
# in R by cor(x,y). The Spearman correlation is given by cor(x,y,method="spearman").

# Plot the weights of chicks from day 4 and day 21. We can see that there is some general 
# trend, with the lower weight chicks on day 4 having low weight again on day 21, and
# likewise for the high weight chicks.

cwd21 <- chick[, "weight.21"]

#plot(cwd4, cwd21)
plot((cwd4-mean(cwd4))/sd(cwd4), (cwd21-mean(cwd21))/sd(cwd21))
abline(0, cor(cwd4, cwd21))

# Calculate the Pearson correlation of the weights of chicks from day 4 and day 21. Now 
# calculate how much the Pearson correlation changes if we add a chick that weighs 3000
# on day4 and 3000 on day 21. Again, divide the Pearson correlation with the outlier chick
# over the Pearson correlation computed without the outliers.

cwd21_b <- c(cwd21, 3000)

cor(cwd4_b, cwd21_b)/cor(cwd4, cwd21)
# 2.370719

cor(cwd4_b, cwd21_b, method="spearman")/cor(cwd4, cwd21, method="spearman")
# 1.084826

cor(rank(cwd4_b), rank(cwd21_b))/cor(rank(cwd4), rank(cwd21))
# 1.084826

#----------------------------------------------------------------------------------
# Wilcoxon Rank Sum Test

# Since the t-test is based on mean and standard deviation measures it is susceptible
# to outliers. We perform a t-test on data for which the null is true.

set.seed(779, sample.kind = "Rounding") ##779 picked for illustration purposes
N <- 25
x <- rnorm(N, 0, 1)
y <- rnorm(N, 0, 1)

# Compute the t-statistics and the p value thereof is:
cat("t-test pval:", t.test(x, y)$p.value)
# 0.1589672
# The p-value is larger than 0.05 so we accept the NULL hypothesis
# Which we know is true by construction

# Create outliers:
x[1] <- 5
x[2] <- 7

# Compute the t-statistics and the p value thereof is:
cat("t-test pval:", t.test(x, y)$p.value)
# 0.04439948
# The p-value is smaller than 0.05 so we can negate the NULL hypothesis
# Which we known was true by construction before introfucting the two outliers

# An alternative to the t-test can be the Wilcox test, which is based on ranks
cat("Wilcox test pval:", wilcox.test(x, y)$p.value)
# 0.1310212
# The p-value is larger than 0.05 so we accept the NULL hypothesis
# Which we known was true by construction before introfucting the two outliers

library(rafalib)
mypar(1,2)

# Plot the data as is, as notice that the data seems pretty similar except for the two outliers in x
stripchart(list(x, y), vertical = TRUE,
           ylim = c(-7, 7), ylab = "Observations",
           pch = 21, bg = 1)
abline(h=0)

# The idea behind this test is:
# 1) combine all the data,
# 2) turn the values into ranks,
# 3) separate them back into their groups,
# 4) compute the sum or average rank and perform a test.

xrank <- rank(c(x, y))[seq(along = x)]
yrank <- rank(c(x, y))[-seq(along = y)]

stripchart(list(xrank, yrank), vertical = TRUE,
           ylab = "Ranks", pch = 21, bg = 1, cex = 1.25)
abline(h=median(c(xrank, yrank)))

ws <- sapply(x, function(z){
  rank(c(z,y))[1]-1 
})

text( rep(1.05, length(ws)), xrank, ws, cex = 0.8)

# W is the sum of the ranks for the first group relative to the second group
W <-sum(ws) 

# We can compute an exact p-value for W based on combinatorics.
# We can also use the CLT since statistical theory tells us that this W is approximated by the normal distribution.

# Why not to just use???????
cat("t-test pval:", t.test(rank(x), rank(y))$p.value)
# The p-valie is 1 so the NULL hypothesis is definitely good

#----------------------------------------------------------------------------------
# Mann-Whitney-Wilcoxon Test Exercises

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

# We will use one of the datasets included in R, which contains weight of chicks
# in grams as they grow from day 0 to day 21. This dataset also splits up the
# chicks by different protein diets, which are coded from 1 to 4. 
data(ChickWeight)

# The weights of all observations over time and color the points to represent the Diet:
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

# Reshape the data so that each row is a chick
chick <- reshape(ChickWeight, 
                 idvar = c("Chick","Diet"), 
                 timevar = "Time",
                 direction = "wide")
head(chick)

# Remove any chicks that have missing observations at any time points
chick <- na.omit(chick)

#--------------------------------------
# Mann-Whitney-Wilcoxon Test Exercises #1

# Save the weights of the chicks on day 4 from diet 1 as a vector x.
x <- chick %>% filter(Diet == 1) %>% pull("weight.4") %>% unlist()

# Save the weights of the chicks on day 4 from diet 4 as a vector y.
y <- chick %>% filter(Diet == 4) %>% pull("weight.4") %>% unlist()

# Perform a t-test comparing x and y (in R the function t.test(x,y) will perform the test). 
ttval <- t.test(x, y)
ttval$p.value
# 7.320259e-06

# Then perform a Wilcoxon test of x and y (in R the function wilcox.test(x,y) will perform the test). 
wtval <- wilcox.test(x, y)
wtval$p.value
# 0.0002011939

# A warning will appear that an exact p-value cannot be calculated with ties,
# so an approximation is used, which is fine for our purposes.

# Perform a t-test of x and y, after adding a single chick of weight 200 grams to x (the diet 1 chicks).
x_b <- c(x, 200)
ttval_b <- t.test(x_b, y)

# What is the p-value from this test?
ttval_b$p.value
# 0.9380347

#--------------------------------------
# Mann-Whitney-Wilcoxon Test Exercises #2

# Do the same for the Wilcoxon test. The Wilcoxon test is robust to the outlier.
# In addition, it has less assumptions that the t-test on the distribution of
# the underlying data.
wtval_b <- wilcox.test(x_b, y)
wtval_b$p.value
# 0.0009840921

#--------------------------------------
# Mann-Whitney-Wilcoxon Test Exercises #3

# We will now investigate a possible downside to the Wilcoxon-Mann-Whitney test statistic.
# Using the following code to make three boxplots, showing the true Diet 1 vs 4 weights,
# and then two altered versions: one with an additional difference of 10 grams and one with
# an additional difference of 100 grams. Use the x and y as defined above, NOT the ones with
# the added outlier. 

ibrary(rafalib)
mypar(1,3)
boxplot(x,y, main = "Original")
boxplot(x,y+10, main = "y + 10")
boxplot(x,y+100, main = "y + 100")

# What is the difference in t-test statistic (obtained by t.test(x,y)$statistic) between
# adding 10 and adding 100 to all the values in the group 'y'? Take the the t-test 
# statistic with x and y+10 and subtract the t-test statistic with x and y+100. 
# The value should be positive.

t.test(x, y + 10)$statistic - t.test(x, y + 100)$statistic
# 67.75097

#--------------------------------------
# Mann-Whitney-Wilcoxon Test Exercises #4

# Examine the Wilcoxon test statistic for x and y+10 and for x and y+100. 

wilcox.test(x, y + 10)$statistic - wilcox.test(x, y + 100)$statistic
# 0
wilcox.test(x, y + 10)$statistic
# 0

wilcox.test(x, y + 10)$p.value - wilcox.test(x, y + 100)$p.value
# 0
wilcox.test(x, y + 10)$p.value
# 5.032073e-05

# Because the Wilcoxon works on ranks, once the two groups show complete separation,
# that is all points from group 'y' are above all points from group 'x', the statistic
# will not change, regardless of how large the difference grows. Likewise, the p-value
# has a minimum value, regardless of how far apart the groups are. This means that the
# Wilcoxon test can be considered less powerful than the t-test in certain contexts.
# In fact, for small sample sizes, the p-value can't be very small, even when the
# difference is very large.

# What is the p-value if we compare c(1,2,3) to c(4,5,6) using a Wilcoxon test?
wilcox.test(c(1, 2, 3), c(4, 5, 6))$p.value
# 0.1

#--------------------------------------
# Mann-Whitney-Wilcoxon Test Exercises #5

# What is the p-value if we compare c(1,2,3) to c(400,500,600) using a Wilcoxon test?
wilcox.test(c(1, 2, 3), c(400, 500, 600))$p.value
# 0.1

#----------------------------------------------------------------------------------




