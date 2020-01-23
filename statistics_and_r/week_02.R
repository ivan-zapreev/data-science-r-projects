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



#----------------------------------------------------------------------------------



#----------------------------------------------------------------------------------



#----------------------------------------------------------------------------------



#----------------------------------------------------------------------------------



#----------------------------------------------------------------------------------


