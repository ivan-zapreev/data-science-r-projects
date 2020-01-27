if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

library(dplyr)

fmw_data <- read.csv(file.path("data","femaleMiceWeights.csv"))

# Check on the mean weight for the control mice
control <- fmw_data %>% filter(Diet == "chow") %>% select(Bodyweight) %>% unlist
mean(control)

# Check on the mean weight for the treated mice
treatment <- fmw_data %>% filter(Diet == "hf") %>% select(Bodyweight) %>% unlist
mean(treatment)

obs <- mean(treatment) - mean(control)
obs

# Load the entire control population data
population <- read.csv(file.path("data", "femaleControlsPopulation.csv"))
nrow(population)
names(population)

#Convert into a list from the data frame
population <- population %>% unlist
names(population) <- 1:length(population)

#Do random sampling of the 12 mice and compute the mean
#If we run it multiple times we always get different mean
#values as the sample will choose the mice randomly
mean(sample(population, length(treatment)))

#The question is: Can we see the difference in the mean
#value of the control and treatment purely by chance?

#----------------------------------------------------------------------------------
# Random Variables Exercises
if(!require(downloader)) install.packages(downloader, repos = "http://cran.us.r-project.org")
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- file.path("data", basename(url))
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

#--------------------------------------------------
# Random Variables Exercises #1 
# What is the average of these weights? 
mean(x)

#--------------------------------------------------
# Random Variables Exercises #2 
# Take a random sample of size 5. What is the absolute value (use abs) of the 
# difference between the average of the sample and the average of all the values?
set.seed(1, sample.kind="Rounding")

abs(mean(sample(x, 5)) - mean(x))

#--------------------------------------------------
# Random Variables Exercises #3 
# After setting the seed at 5, set.seed(5) take a random sample of size 5.
# What is the absolute value of the difference between the average of the
# sample and the average of all the values? 
set.seed(5, sample.kind="Rounding")

abs(mean(sample(x, 5)) - mean(x))

#--------------------------------------------------
# Random Variables Exercises #4 
# Why are the answers from 2 and 3 different? 
  
# Because the average of the samples is a random variable. 

#----------------------------------------------------------------------------------
# The NULL distribution and the P values
set.seed(1, sample.kind="Rounding")

N <- 10000
nulls <- vector("numeric", N)
for(i in 1:N) {
  control_fake <- sample(population, 12)
  treatment_fake <- sample(population, 12)
  nulls[i] <- mean(treatment_fake) - mean(control_fake)
}

#The maximum will be more than "obs"
max(nulls)

#The NULL hypothesis is that there is no difference between the control and the treatment

#Plot the histogram to see the distribution of the differences
#This is the NULL distribution
hist(nulls)

#Count the number of times we observed the difference larger than the "obs"
sum(abs(nulls) > obs)

#The proportion of times the null is larger than the observation is:
mean(abs(nulls) > obs)

# This mean is the P value for the NULL hypothesis

#----------------------------------------------------------------------------------
# Null Distributions Exercises
if(!require(downloader)) install.packages(downloader, repos = "http://cran.us.r-project.org")
library(downloader) 

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- file.path("data", basename(url))
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

sample_mice_delta <- function(weights,
                              num_sample = 1000,
                              sample_size = 5,
                              weight_bound = 1) {
  set.seed(1, sample.kind="Rounding")
  
  N <- num_sample
  delta_vals <- vector("numeric", N)
  mean_x <- mean(weights)
  for(i in 1:N) {
    delta_vals[i] <- ( mean(sample(weights, sample_size)) - mean_x )
  }
  
  mean(abs(delta_vals) > weight_bound)
}

#--------------------------------------------------
# Null Distributions Exercises #1 
# Set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times.
# Save these averages. What proportion of these 1,000 averages are more than 1 gram 
# away from the average of x ? 
sample_mice_delta(x)

#--------------------------------------------------
# Null Distributions Exercises #2
# We are now going to increase the number of times we redo the sample from 1,000 to 10,000. 
# Set the seed at 1, then using a for-loop take a random sample of 5 mice 10,000 times. 
# Save these averages. What proportion of these 10,000 averages are more than 1 gram 
# away from the average of x ? 
sample_mice_delta(x, 10000)

#--------------------------------------------------
# Null Distributions Exercises #3
# Note that the answers to 1 and 2 barely changed. This is expected. The way we think about 
# the random value distributions is as the distribution of the list of values obtained if we 
# repeated the experiment an infinite number of times. On a computer, we can't perform an
# infinite number of iterations so instead, for our examples, we consider 1,000 to be large
# enough, thus 10,000 is as well. Now if instead we change the sample size, then we change 
# the random variable and thus its distribution.
# 
# Set the seed at 1, then using a for-loop take a random sample of 50 mice 1,000 times. 
# Save these averages. What proportion of these 1,000 averages are more than 1 gram away
# from the average of x ? 
sample_mice_delta(x, sample_size = 50)

#----------------------------------------------------------------------------------
# Probability Distributions Exercises

if(!require(gapminder)) install.packages("gapminder", repos = "http://cran.us.r-project.org")
library(gapminder)

data(gapminder)
head(gapminder)

# Create a vector x of the life expectancies of each country for the year 1952. 

x <- gapminder %>% filter(year == 1952) %>% select(lifeExp) %>% unlist

# Plot a histogram of these life expectancies to see the spread of the different countries.
hist(x)

#--------------------------------------------------
# Probability Distributions Exercises #1 
# What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?

mean(x <= 40)

# The second way, which is a bit more complex for beginners, is to use the ecdf() function.
Fn <- ecdf(x)

Fn(40)

#--------------------------------------------------
# Probability Distributions Exercises #2 
# What is the proportion of countries in 1952 that have a life expectancy between 40 and 60 years? 
# Hint: this is the proportion that have a life expectancy less than or equal to 60 years, minus 
# the proportion that have a life expectancy less than or equal to 40 years.

mean( x <= 60 ) - mean( x <= 40 )

Fn(60) - Fn(40)

#----------------------------------------------------------------------------------

# Suppose we want to plot the proportions of countries with life expectancy 'q' for a
# range of different years. R has a built in function for this, plot(ecdf(x)), but 
# suppose we didn't know this. The function is quite easy to build, by turning the code
# from question 1.1 into a custom function, and then using sapply(). Our custom function
# will take an input variable 'q', and return the proportion of countries in 'x' less
# than or equal to q. The curly brackets { and }, allow us to write an R function
# which spans multiple lines:

prop <- function(q) {
  mean(x <= q)
}

# Now let's build a range of q's that we can apply the function to:
qs <- seq(from=min(x), to=max(x), length=20)

# Print 'qs' to the R console to see what the seq() function gave us. 
# Now we can use sapply() to apply the 'prop' function to each element of 'qs':
props <- sapply(qs, prop)

# Take a look at 'props', either by printing to the console, or by plotting it over qs:
plot(qs, props)

# Note that we could also have written this in one line, by defining
# the 'prop' function but without naming it:
props <- sapply(qs, function(q) mean(x <= q))

# This last style is called using an "inline" function or an "anonymous" function.
# Let's compare our homemade plot with the pre-built one in R:
plot(ecdf(x))

#----------------------------------------------------------------------------------
# Central Limit Theorem: The Normal Distribution
if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")
library(downloader) 

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- file.path("data", basename(url))
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

# Using the same process as before (in Null Distribution Exercises), set the seed at 1,
# then using a for-loop take a random sample of 5 mice 1,000 times. Save these averages.
set.seed(1, sample.kind="Rounding")

mean_5_vals <- sapply(1:1000, function(idx) {
  mean(sample(x, 5))
})

# After that, set the seed at 1, then using a for-loop
# take a random sample of 50 mice 1,000 times. Save these averages.
set.seed(1, sample.kind="Rounding")

mean_50_vals <- sapply(1:1000, function(idx) {
  mean(sample(x, 50))
})

#--------------------------------------------------
# Normal Distribution Exercises #1 
# Use a histogram to "look" at the distribution of averages we get with 
# a sample size of 5 and a sample size of 50. How would you say they differ? 

hist(mean_5_vals)
hist(mean_50_vals)

# They both look roughly normal, but with a sample size of 50 the spread is smaller. 

#--------------------------------------------------
# Normal Distribution Exercises #2
# For the last set of averages, the ones obtained from a sample size of 50, 
# what proportion are between 23 and 25? 
Fn <- ecdf(mean_50_vals)

Fn(25) - Fn(23)

#--------------------------------------------------
# Normal Distribution Exercises #3
# Now ask the same question of a normal distribution with
# average 23.9 and standard deviation 0.43.

pnorm(25, mean = 23.9, sd = 0.43) - pnorm(23, mean = 23.9, sd = 0.43)

#----------------------------------------------------------------------------------
# Population, Samples, and Estimates Exercises
if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(downloader) 
library(dplyr)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- file.path("data", basename(url))
download(url, destfile=filename)
dat <- read.csv(filename) 

# We will remove the lines that contain missing values:
dat <- na.omit( dat )

#--------------------------------------------------
# Population, Samples, and Estimates Exercises #1 
# Use dplyr to create a vector x with the body weight of all males on the control
# (chow) diet. What is this population's average? 

head(dat)

male_chow_diet <- dat %>% filter(Sex == "M", Diet == "chow") %>% select(Bodyweight) %>% unlist()
mean_chow <- mean(male_chow_diet)
mean_chow

#--------------------------------------------------
#  Population, Samples, and Estimates Exercises #2 
if(!require(rafalib)) install.packages("rafalib", repos = "http://cran.us.r-project.org")
library(rafalib)

# Now use the rafalib package and use the popsd function to compute the population standard deviation. 
popsd(male_chow_diet)

# Note: popsd simply multiplies the result of var by (n-1) / n with n the populaton size and takes the square root.
sqrt(var(male_chow_diet)*(length(male_chow_diet)-1)/length(male_chow_diet))

# Note that: sd returns the unbiased sample estimate of the population varaince.
sd(male_chow_diet)

#--------------------------------------------------
# Population, Samples, and Estimates Exercises #3 
# Set the seed at 1. Take a random sample 𝑋 of size
# 25 from x. What is the sample average? 
set.seed(1, sample.kind="Rounding")

mean_chow_s <- mean(sample(male_chow_diet, 25))
mean_chow_s

#--------------------------------------------------
# Population, Samples, and Estimates Exercises #4 
# Use dplyr to create a vector y with the body weight of all males on 
# the high fat hf) diet. What is this population's average? 
male_hf_diet <- dat %>% filter(Sex == "M", Diet == "hf") %>% select(Bodyweight) %>% unlist()
mean_hf <- mean(male_hf_diet)
mean_hf

#--------------------------------------------------
# Population, Samples, and Estimates Exercises #5 
# Now use the rafalib package and use the popsd function
# to compute the population standard deviation. 
popsd(male_hf_diet)

#--------------------------------------------------
# Population, Samples, and Estimates Exercises #6 
# Set the seed at 1. Take a random sample 𝑌 of size
# 25 from y. What is the sample average?
set.seed(1, sample.kind="Rounding")

mean_hf_s <- mean(sample(male_hf_diet, 25))
mean_hf_s

#--------------------------------------------------
# Population, Samples, and Estimates Exercises #7 
# What is the difference in absolute value between 𝑦¯−𝑥¯ and 𝑌¯−𝑋¯?
abs((mean_hf - mean_chow) - (mean_hf_s - mean_chow_s))


#--------------------------------------------------
# Population, Samples, and Estimates Exercises #8
# Repeat the above for females. Make sure to set the seed to 1 before
# each sample call. What is the difference in absolute value between
# 𝑦¯−𝑥¯ and 𝑋¯−𝑌¯? 
female_chow_diet <- dat %>% filter(Sex == "F", Diet == "chow") %>% select(Bodyweight) %>% unlist()
femean_chow <- mean(female_chow_diet)
femean_chow

set.seed(1, sample.kind="Rounding")
femean_chow_s <- mean(sample(female_chow_diet, 25))
femean_chow_s

female_hf_diet <- dat %>% filter(Sex == "F", Diet == "hf") %>% select(Bodyweight) %>% unlist()
femean_hf <- mean(female_hf_diet)
femean_hf

set.seed(1, sample.kind="Rounding")
femean_hf_s <- mean(sample(female_hf_diet, 25))
femean_hf_s

abs((femean_hf - femean_chow) - (femean_hf_s - femean_chow_s))

#--------------------------------------------------
# Population, Samples, and Estimates Exercises #9
# For the females, our sample estimates were closer to the population difference
# than with males. What is a possible explanation for this? 

dat %>% filter(Sex == "F") %>% select(Bodyweight) %>% unlist() %>% popvar()
dat %>% filter(Sex == "M") %>% select(Bodyweight) %>% unlist() %>% popvar()

# The population variance of the females is smaller than that of the males;
# thus, the sample variable has less variability. 

#----------------------------------------------------------------------------------
# Central Limit Theorem Exercises

if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")
library(downloader) 
if(!require(rafalib)) install.packages("rafalib", repos = "http://cran.us.r-project.org")
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- file.path("data", basename(url))
download(url, destfile=filename)
dat_mp <- na.omit( read.csv(filename) )

# The Empirical Rulestates that for variable X that has a normal distribution with mean μ and standard
# deviation σ, approximately:
# * 68% of the values of X fall within 1 standard deviation of the mean in either direction (μ±σ)
# * 95% of the values of X fall within 2 standard deviations of the mean in either direction (μ±2σ)
# * 99.7%  (almost  all)  of  the  values  of X fall  within  3  standard  deviations  of  the  mean  in  eitherdirection (μ±3σ).

#--------------------------------------------------
# Central Limit Theorem Exercises #1
# If a list of numbers has a distribution that is well approximated by the normal distribution,
# what proportion of these numbers are within one standard deviation away from the list's average? 

#0.68

#--------------------------------------------------
# Central Limit Theorem Exercises #2
# What proportion of these numbers are within two standard deviations away from the list's average? 

#0.95

#--------------------------------------------------
# Central Limit Theorem Exercises #3
# What proportion of these numbers are within three standard deviations away from the list's average? 

#0.997

#--------------------------------------------------
# Central Limit Theorem Exercises #4
# Define y to be the weights of males on the control diet. 

male_dat_mp <- dat_mp %>% filter(Sex == "M" & Diet=="chow")

# y <- filter(dat_mp, Sex == "M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
# z <- ( y - mean(y) ) / popsd(y)
# mean( abs(z) <= 2 )

# What proportion of the mice are within one standard deviation away from 
# the average weight(remember to use popsd for the population sd)? 

male_weight_avg <- mean(male_dat_mp$Bodyweight, na.rm = T)
male_weight_sd <- sd(male_dat_mp$Bodyweight, na.rm = T)

mean(abs(male_dat_mp$Bodyweight - male_weight_avg) <= male_weight_sd)
# 0.6950673

#--------------------------------------------------
# Central Limit Theorem Exercises #5
# What proportion of these numbers are within two standard deviations away from the list's average? 

mean(abs(male_dat_mp$Bodyweight - male_weight_avg) <=  2*male_weight_sd)
# 0.9461883

#--------------------------------------------------
# Central Limit Theorem Exercises #6
# What proportion of these numbers are within three standard deviations away from the list's average? 

mean(abs(male_dat_mp$Bodyweight - male_weight_avg) <=  3*male_weight_sd)
# 0.9910314

#--------------------------------------------------
# Central Limit Theorem Exercises #7
# Note that the numbers for the normal distribution and our weights are relatively close.
# Also, notice that we are indirectly comparing quantiles of the normal distribution to 
# quantiles of the mouse weight distribution. We can actually compare all quantiles using
# a qqplot. Which of the following best describes the qq-plot comparing mouse weights to
# the normal distribution? 

y <- filter(dat_mp, Sex == "M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z)
abline(0,1)

# The mouse weights are well approximated by the normal distribution, although the larger 
# values (right tail) are larger than predicted by the normal. This is consistent with the
# differences seen between question 3 and 6. 

#--------------------------------------------------
# Central Limit Theorem Exercises #8
# Create the above qq-plot for the four populations: male/females on each of the two diets.
# What is the most likely explanation for the mouse weights being well approximated? 
# What is the best explanation for all these being well approximated by the normal distribution? 

mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z); abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z); abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z); abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z); abline(0,1)

# This just happens to be how nature behaves in this particular case.
# Perhaps the result of many biological factors averaging out. 

#--------------------------------------------------
# Central Limit Theorem Exercises #9
# Here we are going to use the function replicate to learn about the distribution
# of random variables. All the above exercises relate to the normal distribution 
# as an approximation of the distribution of a fixed list of numbers or a population.
# We have not yet discussed probability in these exercises. If the distribution of 
# a list of numbers is approximately normal, then if we pick a number at random from
# this distribution, it will follow a normal distribution. However, it is important
# to remember that stating that some quantity has a distribution does not necessarily
# imply this quantity is random. Also, keep in mind that this is not related to the
# central limit theorem. The central limit applies to averages of random variables.
# Let's explore this concept.

# We will now take a sample of size 25 from the population of males on the chow diet. 
# The average of this sample is our random variable. We will use the replicate to
# observe 10,000 realizations of this random variable. Set the seed at 1, generate
# these 10,000 averages. Make a histogram and qq-plot of these 10,000 numbers against
# the normal distribution.

# We can see that, as predicted by the CLT, the distribution of the random variable is 
# very well approximated by the normal distribution.

set.seed(1, sample.kind="Rounding")
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

# What is the average of the distribution of the sample average? 
mean(avgs)

#--------------------------------------------------
# Central Limit Theorem Exercises #10

# What is the standard deviation of the distribution of sample averages? 
sd(avgs)

#----------------------------------------------------------------------------------
# CLT in Practice

#The data looks normally distributed
hist(nulls)

#Center beforehand
qqnorm((nulls-mean(nulls))/sd(nulls))
abline(0, 1)

#Without centering beforehand
qqnorm(nulls)
qqline(nulls)

# Now make the sample size smaller, just take 3, was 12, and
# see how well the N(0,1) approximation is
set.seed(1, sample.kind="Rounding")

N <- 10000
nulls <- vector("numeric", N)
for(i in 1:N) {
  control_fake <- sample(population, 3)
  treatment_fake <- sample(population, 3)
  nulls[i] <- mean(treatment_fake) - mean(control_fake)
}

#Without centering beforehand
qqnorm(nulls)
qqline(nulls)

# The approximation seems to be a bit worse now with a shorter sample but still very close

#----------------------------------------------------------------------------------
# t-tests in Practice

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

data_fm <- read.csv(file.path("data", "femaleMiceWeights.csv"))

control <- filter(data_fm, Diet == "chow") %>% select(Bodyweight) %>% unlist()

treatment <- filter(data_fm, Diet == "hf") %>% select(Bodyweight) %>% unlist()

N <- length(treatment)

obs <- mean(treatment) - mean(control)

# Compute the t-statistics, first compute the standard error for the NULL hypothesis
# Standard error is a measure of the statistical accuracy of an estimate, equal to the 
# standard deviation of the theoretical distribution of a large population of such estimates. 
# E.g. standard error is the standard deviation computed from the sample.
se <- sqrt(var(treatment)/N + var(control)/N)

# This is the scaled value, and also should be centered 
# if we around 0, it it is the NULL hypothesis.
tstat <- obs / se

# By CLT the NULL distribution shall be distributed as N(0, 1), therefore
# the p-value can be computed without knowing the entire population as:
p_value <- 1 - pnorm(tstat)
p_value

#This is however the half of the tail, so assuming the symmetry the absolute value tails are:
2*p_value

# This p value is computed without using the entire population but from N(0, 1) and the sample

# Now let us check this on the entire population

qqnorm_tr_ct <- function(S) {
  set.seed(1, sample.kind="Rounding")
  N <- 10000
  nulls <- vector("numeric", N)
  for(i in 1:N) {
    control_fake <- sample(population, S)
    treatment_fake <- sample(population, S)
    # We immediately normalize based on the given sample so that we
    # should be getting the overall N(0, 1) for the NULL to hold
    se <- sqrt(var(treatment_fake)/S + var(control_fake)/S)
    nulls[i] <- (mean(treatment_fake) - mean(control_fake)) / se
  }
  
  qqnorm(nulls)
  abline(0, 1)
}

# For the sample size S = 12 we get a nice approximation
qqnorm_tr_ct(12)

# For the smaller sample size S = 3 the approximation is much worse
qqnorm_tr_ct(3)

# This is why we need larger sample sizes

#----------------------------------------------------------------------------------
# t-tests in Practice II

# Use the build-in t-test function, it does not use the CLT
# and the N(0,1) distribution but rather the T-distribution
# This is why the p-values will be different from what we
# computed ourselves earlier in the script.
ttest <- t.test(treatment, control)
ttest

# To have some idea if the T-distribution could be used, i.e.
# the samples are normall distributed, we can use qqplots
qqnorm(control)
qqline(control)

qqnorm(treatment)
qqline(treatment)

# Here we can see that we are not totally off, we seem to be close
# This also means that using the T distributions should be fine

#----------------------------------------------------------------------------------
# CLT and t-distribution in Practice Exercises
if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")
library(downloader)
library(dplyr)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- file.path("data", "femaleMiceWeights.csv")
if(!file.exists(filename)) {
  download(url,destfile = filename) 
}
dat_fmw <- read.csv(filename)

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #1

# The CLT is a result from probability theory. Much of probability theory was
# originally inspired by gambling. This theory is still used in practice by casinos.
# For example, they can estimate how many people need to play slots for there to be
# a 99.9999% probability of earning enough money to cover expenses. Let's try a simple 
# example related to gambling.

# Suppose we are interested in the proportion of times we see a 6 when rolling n=100 die.
# This is a random variable which we can simulate with x = sample(1:6, n, replace=TRUE)
# and the proportion we are interested in can be expressed as an average: mean(x==6).
# Because the die rolls are independent, the CLT applies.

# We want to roll n dice 10,000 times and keep these proportions. This random variable
# (proportion of 6s) has mean p=1/6 and variance p*(1-p)/n. So according to CLT 
# z = (mean(x==6) - p) / sqrt(p*(1-p)/n) should be normal with mean 0 and SD 1.
# Set the seed to 1, then use replicate to perform the simulation, and report what
# proportion of times z was larger than 2 in absolute value (CLT says it should be about 0.05).
# This is because for N(0,1), we should have about 95% of values within the 2 standard deviations from the mean.
set.seed(1, sample.kind="Rounding")

S <- 100                        # Sample size
N <- 10000                      # Number of samples
p_6 <- 1/6                      # Mean of proportion of 6s
sd_6_100 <- sqrt(p_6*(1-p_6)/S) # Standard deviation pf proportion of 6s

compute_z <- function() {
  x <- sample(1:6, S, replace=TRUE)
  z <- (mean(x == 6) - p_6) / sd_6_100
  z
}

compute_z_vs_2 <- function() {
  z <- compute_z()
  abs(z) > 2
}

sample <- replicate(N, compute_z_vs_2())
mean(sample)
# 0.0424

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #2

# For the last simulation you can make a qqplot to confirm the normal approximation. 
# Now, the CLT is an asympototic result, meaning it is closer and closer to being a
# perfect approximation as the sample size increases. In practice, however, we need
# to decide if it is appropriate for actual sample sizes. Is 10 enough? 15? 30?
set.seed(1, sample.kind="Rounding")
sample_z <- replicate(N, compute_z())
qqnorm(sample_z)
abline(0, 1)

# In the example used in exercise 1, the original data is binary (either 6 or not). 
# In this case, the success probability also affects the appropriateness of the CLT.
# With very low probabilities, we need larger sample sizes for the CLT to "kick in".

# Run the simulation from exercise 1, but for different values of p and n. For which of 
# the following is the normal approximation best?

#Compute the 0 centered and deviation scaled values for getting 6 mean sample of size S
compute_z <- function(p_6, S) {
  p <- (1 - p_6) / 5
  x <- sample(1:6, S, TRUE, c(p, p, p, p, p, p_6))
  sd_6_S <- sqrt(p_6*(1 - p_6) / S)
  z <- (mean(x == 6) - p_6) / sd_6_S
  z
}

N <- 10000

set.seed(1, sample.kind="Rounding")
sample_z <- replicate(N, compute_z(0.5, 5))
qqnorm(sample_z)
abline(0, 1)

set.seed(1, sample.kind="Rounding")
sample_z <- replicate(N, compute_z(0.5, 30))
qqnorm(sample_z)
abline(0, 1)

set.seed(1, sample.kind="Rounding")
sample_z <- replicate(N, compute_z(0.01, 30))
qqnorm(sample_z)
abline(0, 1)

set.seed(1, sample.kind="Rounding")
sample_z <- replicate(N, compute_z(0.01, 100))
qqnorm(sample_z)
abline(0, 1)

#  p=0.5 and n=30 gives the best result

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #3

# As we have already seen, the CLT also applies to averages of quantitative data.
# A major difference with binary data, for which we know the variance is 𝑝(1−𝑝),
# is that with quantitative data we need to estimate the population standard deviation.

# In several previous exercises we have illustrated statistical concepts with the 
# unrealistic situation of having access to the entire population. In practice,
# we do *not* have access to entire populations. Instead, we obtain one random
# sample and need to reach conclusions analyzing that data. dat is an example of
# a typical simple dataset representing just one sample. We have 12 measurements 
# for each of two populations: 

X <- filter(dat_fmw, Diet=="chow") %>% select(Bodyweight) %>% unlist()
Y <- filter(dat_fmw, Diet=="hf") %>% select(Bodyweight) %>% unlist()

# We think of 𝑋 as a random sample from the population of all mice in the control
# diet and 𝑌 as a random sample from the population of all mice in the high fat diet.

# Define the parameter 𝜇𝑋 as the average of the control population. We estimate th
# is parameter with the sample average 𝑋¯. What is the sample average?

m_x <- mean(X)
m_x

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #4

# We don't know 𝜇𝑋 , but want to use 𝑋¯ to understand 𝜇𝑋.
# Which of the following uses CLT to understand how well 𝑋¯ approximates 𝜇𝑋 ?

# 𝑋¯ follows a normal distribution with mean 𝜇𝑋 and standard deviation 𝜎𝑋12√ where 𝜎𝑋 is the population standard deviation. 

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #5

# The result above tells us the distribution of the following random variable:
# 𝑍=12‾‾‾√𝑋¯−𝜇𝑋𝜎𝑋. What does the CLT tell us is the mean of 𝑍 (you don't need code)? 

# 0

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #6

# The result of 4 and 5 tell us that we know the distribution of the difference
# between our estimate and what we want to estimate, but don't know. However,
# the equation involves the population standard deviation 𝜎𝑋, which we don't know.
# Given what we discussed, what is your estimate of 𝜎𝑋? 
sd_x <- sd(X)
sd_x

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #7

# Use the CLT to approximate the probability that our estimate 𝑋¯ is off by more than 2 grams from 𝜇𝑋. 

#Take into account the sample size for the distribution from the CTL
clt_sd_x <- sd_x/sqrt(length(X))

#Then we can compute the confidence intervals either
# on N(0, clt_sd_x), or N(0, 1) both assume centered
# random sample X-m_x which does not affect the variance
pnorm(-2, 0, clt_sd_x) + (1 - pnorm(2, 0, clt_sd_x))
pnorm(-2/clt_sd_x) + (1 - pnorm(2/clt_sd_x))
# 0.02189533

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #8

# Now we introduce the concept of a null hypothesis. We don't know 𝜇𝑋 nor 𝜇𝑌.
# We want to quantify what the data say about the possibility that the diet has 
# no effect: 𝜇𝑋=𝜇𝑌. If we use CLT, then we approximate the distribution of 𝑋¯
# as normal with mean 𝜇𝑋 and standard deviation 𝜎𝑋/𝑀‾‾‾√ and the distribution
# of 𝑌¯ as normal with mean 𝜇𝑌 and standard deviation 𝜎𝑌/𝑁‾‾√, with 𝑀 and 𝑁 
# the sample sizes for 𝑋 and 𝑌 respectively, in this case 12. This implies that
# the difference 𝑌¯−𝑋¯ has mean 0. We described that the standard deviation of
# this statistic (the standard error) is SE(𝑋¯−𝑌¯)=𝜎2𝑌/12+𝜎2𝑋/12‾‾‾‾‾‾‾‾‾‾‾‾‾‾√
# and that we estimate the population standard deviations 𝜎𝑋 and 𝜎𝑌 with the
# sample estimates. What is the estimate of SE(𝑋¯−𝑌¯)=𝜎2𝑌/12+𝜎2𝑋/12‾‾‾‾‾‾‾‾‾‾‾‾‾‾√ ? 

se_x_y <- sqrt(var(X)/length(X) + var(Y)/length(Y))
se_x_y
# 1.469867

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #9

# So now we can compute 𝑌¯−𝑋¯ as well as an estimate of this standard error and 
# construct a t-statistic. What number is this t-statistic? 
(mean(Y)-mean(X))/se_x_y

##or

t.test(Y,X)$stat

#--------------------------------------------------

# For some of the following exercises you need to review the t-distribution that was
# discussed in the lecture. If you have not done so already, you should review the
# related book chapters from our textbook which can also be found here External link
# and here External link.

# In particular, you will need to remember that the t-distribution is centered at 0
# and has one parameter: the degrees of freedom, that control the size of the tails.
# You will notice that if X follows a t-distribution the probability that X is smaller
# than an extreme value such as 3 SDs away from the mean grows with the degrees of
# freedom. For example, notice the difference between:

1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)

# As we explained, under certain assumptions, the t-statistic follows a t-distribution.
# Determining the degrees of freedom can sometimes be cumbersome, but the t.test function
# calculates it for you. One important fact to keep in mind is that the degrees of freedom
# are directly related to the sample size. There are various resources for learning more
# about degrees of freedom on the internet as well as statistics books.

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #10

# If we apply the CLT, what is the distribution of this t-statistic? 

# Normal with mean 0 and standard deviation 1. 

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #11

# Now we are ready to compute a p-value using the CLT. What is the probability of observing
# a quantity as large as what we computed in 9, when the null distribution is true? 

tstat <- (mean(Y)-mean(X))/se_x_y

ctl_p <- 1-pnorm(abs(tstat)) + pnorm(-abs(tstat))
ctl_p
# 0.0398622

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #12

# CLT provides an approximation for cases in which the sample size is large. 
# In practice, we can't check the assumption because we only get to see 1 outcome
# (which you computed above). As a result, if this approximation is off, so is
# our p-value. As described earlier, there is another approach that does not
# require a large sample size, but rather that the distribution of the population
# is approximately normal. We don't get to see this distribution so it is again
# an assumption, although we can look at the distribution of the sample with
# qqnorm(X) and qqnorm(Y). If we are willing to assume this, then it follows
# that the t-statistic follows t-distribution. What is the p-value under the
# t-distribution approximation? Hint: use the t.test function. 

qqnorm(X)
qqline(X)
qqnorm(Y)
qqline(Y)

ttest_p <- t.test(X,Y)$p.value
ttest_p
# 0.05299888

#--------------------------------------------------
# CLT and t-distribution in Practice Exercises #13

# With the CLT distribution, we obtained a p-value smaller than 0.05 and with the t-distribution,
# one that is larger. They can't both be right. What best describes the difference? 

(ctl_p < 0.05) && (ttest_p > 0.05)

# These are two different assumptions. The t-distribution accounts for the variability introduced by the estimation of the standard error and thus, under the null, large values are more p
#robable under the null distribution. 

#---------------------------------------------------------
#-------------------------


