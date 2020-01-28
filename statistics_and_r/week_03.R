#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
# Inference I: P-values, Confidence Intervals and Power Calculations
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# T-test Exercises
if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")
library(downloader)

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

if(!require(rafalib)) install.packages("rafalib", repos = "http://cran.us.r-project.org")
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- file.path("data", basename(url))
download(url, destfile=filename)
babies <- read.table(filename, header=TRUE)

str(babies)

# This is a large dataset (1,236 cases), and we will pretend that it contains the entire population
# in which we are interested. We will study the differences in birth weight between babies born to
# smoking and non-smoking mothers.

# First, let's split this into two birth weight datasets:
#  * one of birth weights to non-smoking mothers and
#  * the other of birth weights to smoking mothers.

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

# Now, we can look for the true population difference in means between smoking and non-smoking birth weights.

mean(bwt.nonsmoke) - mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

# The population difference of mean birth weights is about 8.9 ounces. The standard deviations 
# of the nonsmoking and smoking groups are about 17.4 and 18.1 ounces, respectively.

# As we did with the mouse weight data, this assessment interactively reviews inference concepts
# using simulations in R. We will treat the babies dataset as the full population and draw samples
# from it to simulate individual experiments. We will then ask whether somebody who only received
# the random samples would be able to draw correct conclusions about the population.

# We are interested in testing whether the birth weights of babies born to non-smoking mothers
# are significantly different from the birth weights of babies born to smoking mothers.

#--------------------------------------------------
# T-test Exercises #1 

# Set the seed at 1 and obtain a samples from the non-smoking mothers (dat.ns) of size 𝑁=25.
# Then, without resetting the seed, take a sample of the same size from and smoking mothers (dat.s).
# Compute the t-statistic (call it tval). Please make sure you input the absolute value of the t-statistic. 

set.seed(1, sample.kind="Rounding")

N <- 25
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
tval <- t.test(dat.ns, dat.s)
tval$stat
# 2.120904 

#--------------------------------------------------
# T-test Exercises #2 

# Recall that we summarize our data using a t-statistics because we know that in situations
# where the NULL hypothesis is true (what we mean when we say "under the NULL") and the sample
# size is relatively large, this t-value will have an approximate standard normal distribution.
# Because we know the distribution of the t-value under the NULL, we can quantitatively determine
# how unusual the observed t-value would be if the NULL hypothesis were true.

# The standard procedure is to examine the probability a t-statistic that actually does follow
# the NULL hypothesis would have larger absolute value than the absolute value of the t-value we
# just observed -- this is called a two-sided test.

# We have computed these by taking one minus the area under the standard normal curve between
# -abs(tval) and abs(tval). In R, we can do this by using the pnorm function, which computes the
# area under a normal curve from negative infinity up to the value given as its first argument: 

1 - pnorm(abs(tval$stat)) + pnorm(-abs(tval$stat))
# 0.03392985

#--------------------------------------------------
# T-test Exercises #3 

# Because of the symmetry of the standard normal distribution, there is a simpler way to calculate
# the probability that a t-value under the NULL could have a larger absolute value than tval.
# Choose the simplified calculation from the following: 

2*pnorm(-abs(tval$stat)) 
# 0.03392985

# 2*pnorm(-abs(tval))

#--------------------------------------------------
# T-test Exercises #4 

# By reporting only p-values, many scientific publications provide an incomplete story of their findings.
# As we have mentioned, with very large sample sizes, scientifically insignificant differences between
# two groups can lead to small p-values. Confidence intervals are more informative as they include the
# estimate itself. Our estimate of the difference between babies of smoker and non-smokers:
#     mean(dat.s) - mean( dat.ns).
# If we use the CLT, what quantity would we add and subtract to this estimate to obtain a 99% confidence interval? 

# Solution:
# x = (mean(dat.s) - mean( dat.ns)) / sqrt(var(dat.s)/length(dat.s) + var(dat.ns)/length(dat.ns))
# Is approximately normally distributed N(0, 1). Therefore, Pr(qnorm(0.005) <= x <= qnorm(0.995)) = 0.99
# Therefore we need qnorm(0.005) * sqrt(var(dat.s)/length(dat.s) + var(dat.ns)/length(dat.ns)) which is
# equal to qnorm(0.995) * sqrt(var(dat.s)/length(dat.s) + var(dat.ns)/length(dat.ns)) up to the sign

abs_ci_diff <- qnorm(0.995) * sqrt(var(dat.s)/length(dat.s) + var(dat.ns)/length(dat.ns))
abs_ci_diff
# 12.0478

#----------------------------------------------------------------------------------
# Confidence Intervals

set.seed(1, sample.kind="Rounding")

chowPopulation <- read.csv(file.path("data", "femaleControlsPopulation.csv"))
chowPopulation <- unlist(chowPopulation)

mu_chow <- mean(chowPopulation)
print(mu_chow)

N <- 30
chow <- sample(chowPopulation, N)
print(mean(chow))

se <- sd(chow)/sqrt(N)
print(se)

conf <- pnorm(2) - pnorm(-2)
conf

# Normally speaking we do not know  mean(chowPopulation) 
# but it is also seen as a constant, so no influene on se
(mean(chow) - mean(chowPopulation))/se

# Due to the random variable above begin distributed as N(0, 1) we have
# that the following holds:
# Pr( -2 < (mean(chow) - mean(chowPopulation))/se < 2 ) = conf (about 95%)

# If we want to go from probability and want to have a 0.95 confidence interval
# then using the fact that the probability density function is symmetric for N(0, 1)
Q <- (1 - 0.95)/2
# Pr( qnorm(Q) < (mean(chow) - mean(chowPopulation))/se < -qnorm(Q) ) = Q 
# Note that qnorm(Q) is negative and abs(qnorm(Q)) == qnorm(1 - Q)
qnorm(Q)
abs(qnorm(Q))
qnorm(1 - Q)

# Thus the UNKNOWN (typically unknown) value of mean(chowPopulation) is located in
interval <- c( mean(chow) - abs(qnorm(Q))*se, mean(chow) + abs(qnorm(Q))*se )
interval

# Clearly the interval is centered around the mean(chow)
interval[1] < mean(chow) && mean(chow) < interval[2]

# Now let us show how the confidence interval works by re-sampling data
mypar() 

plot_conf_int <- function(B = 250, N = 30, aqn_Q) {
  true_mu <- mean(chowPopulation)
  plot(true_mu + c(-7, +7), c(1, 1), type="n",
       xlab="weight", ylab="interval", ylim=c(1, B))
  abline(v=true_mu)
  
  sq_N <- sqrt(N)
  for(i in 1:B) { 
    chow <- sample(chowPopulation, N)
    se <- sd(chow)/sq_N
    interval <- c( mean(chow) - aqn_Q*se, mean(chow) + aqn_Q*se )
    covered <- interval[1] <= true_mu && true_mu <= interval[2]
    color <- ifelse(covered, 1, 2)
    lines(interval, c(i, i), col = color)
  }
}

# Use the CLT and the N(0, 1) distribution
aqn_Q_n <- abs(qnorm(Q))

#Looking at the plot the CLT works pretty well for the sample size 30
plot_conf_int(B=250, N=30, aqn_Q_n)

# In case of the sample size equal to 5 the results are worse,
# the intervals are wider and the number of off-ones is more 
plot_conf_int(B=250, N=5, aqn_Q_n)

# For the smaller sample sizes the Student distribution is more appropriate
# This is also known as the T distribution approximation, here we choose 4 
# digrees of freedom and get larger intervals which still cover the true mean
# more often, see the next example:

aqn_Q_t <- abs(qt(Q, df = 4))

plot_conf_int(B=250, N=5, aqn_Q_t)

#----------------------------------------------------------------------------------
# Confidence Intervals Exercises

if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")
library(downloader)

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

if(!require(rafalib)) install.packages("rafalib", repos = "http://cran.us.r-project.org")
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- file.path("data", basename(url))
if(! file.exists(filename)) download(url, destfile=filename)
babies <- read.table(filename, header=TRUE)

str(babies)

# This is a large dataset (1,236 cases), and we will pretend that it contains the entire population
# in which we are interested. We will study the differences in birth weight between babies born to
# smoking and non-smoking mothers.

# First, let's split this into two birth weight datasets:
#  * one of birth weights to non-smoking mothers and
#  * the other of birth weights to smoking mothers.

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

# Now, we can look for the true population difference in means between smoking and non-smoking birth weights.

mean(bwt.nonsmoke) - mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

# The population difference of mean birth weights is about 8.9 ounces. The standard deviations 
# of the nonsmoking and smoking groups are about 17.4 and 18.1 ounces, respectively.

# As we did with the mouse weight data, this assessment interactively reviews inference concepts
# using simulations in R. We will treat the babies dataset as the full population and draw samples
# from it to simulate individual experiments. We will then ask whether somebody who only received
# the random samples would be able to draw correct conclusions about the population.

# We are interested in testing whether the birth weights of babies born to non-smoking mothers
# are significantly different from the birth weights of babies born to smoking mothers.

#--------------------------------------------------
# Confidence Intervals Exercises #1

# Set the seed at 1 and obtain two samples, each of size N = 25, from non-smoking mothers (dat.ns)
# and smoking mothers (dat.s). If instead of CLT, we use the t-distribution approximation, what
# do we add and subtract to obtain a 99% confidence interval (use 2*N-2 degrees of freedom)? 

set.seed(1, sample.kind="Rounding")

N <- 25
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)

aqn_Q_t <- abs(qt(0.995, df = 2*N-2))

abs_ci_diff <- aqn_Q_t * sqrt(var(dat.s)/length(dat.s) + var(dat.ns)/length(dat.ns))
abs_ci_diff
# 12.54534

#--------------------------------------------------
# Confidence Intervals Exercises #2

# Why are the values from T-test Exercises #3 and Confidence Intervals Exercises #1 so similar?

tval <- t.test(dat.ns, dat.s)
tval$conf.int
# [1]  0.5141953 19.3258047
# attr(,"conf.level")
# [1] 0.95

mu_diff <- mean(dat.ns) - mean(dat.s)
mu_diff
# 9.92

ci_diff <- c(mu_diff - abs_ci_diff, mu_diff + abs_ci_diff)
ci_diff
# -2.625339 22.465339

tval$p.value
# 0.03915615

# N and thus the degrees of freedom is large enough to make the normal and t-distributions very similar. 

#--------------------------------------------------
# Confidence Intervals Exercises #3

# No matter which way you compute it, the p-value pval is the probability that the NULL
# hypothesis could have generated a t-statistic more extreme than than what we observed:
# tval. If the p-value is very small, this means that observing a value more extreme than
# tval would be very rare if the NULL hypothesis were true, and would give strong evidence
# that we should reject the NULL hypothesis. We determine how small the p-value needs to
# be to reject the NULL by deciding how often we would be willing to mistakenly reject
# the NULL hypothesis.
# 
# The standard decision rule is the following: choose some small value 𝛼 (in most
# disciplines the conventional choice is 𝛼=0.05) and reject the NULL hypothesis
# if the p-value is less than 𝛼. We call 𝛼 the significance level of the test.
# 
# It turns out that if we follow this decision rule, the probability that we will
# reject the NULL hypothesis by mistake is equal to 𝛼. (This fact is not immediatel
# obvious and requires some probability theory to show.) We call the event of 
# rejecting the NULL hypothesis, when it is in fact true, a Type I error, we call 
# the probability of making a Type I error, the Type I error rate, and we say that 
# rejecting the NULL hypothesis when the p-value is less than 𝛼, controls the Type
# I error rate so that it is equal to 𝛼. We will see a number of decision rules that
# we use in order to control the probabilities of other types of errors. Often, we 
# will guarantee that the probability of an error is less than some level, but, in 
# this case, we can guarantee that the probability of a Type I error is exactly equal to 𝛼.
# 
# Which of the following sentences about a Type I error is not true?

# From the original data alone, you can tell whether you have made a Type I error.   

#--------------------------------------------------
# Confidence Intervals Exercises #4

# In the simulation we have set up here, we know the NULL hypothesis is false -- the true 
# value of difference in means is actually around 8.9. Thus, we are concerned with how often
# the decision rule outlined in the last section allows us to conclude that the NULL hypothesis
# is actually false. In other words, we would like to quantify the Type II error rate of the
# test, or the probability that we fail to reject the NULL hypothesis when the alternative
# hypothesis is true.
# 
# Unlike the Type I error rate, which we can characterize by assuming that the NULL hypothesis
# of "no difference" is true, the Type II error rate cannot be computed by assuming the 
# alternative hypothesis alone because the alternative hypothesis alone does not specify a 
# particular value for the difference. It thus does not nail down a specific distribution for 
# the t-value under the alternative.
# 
# For this reason, when we study the Type II error rate of a hypothesis testing procedure, we 
# need to assume a particular effect size, or hypothetical size of the difference between 
# population means, that we wish to target. We ask questions such as "what is the smallest 
# difference I could reliably distinguish from 0 given my sample size 𝑁?" or, more commonly,
# "How big does 𝑁 have to be in order to detect that the absolute value of the difference is
# greater than zero?" Type II error control plays a major role in designing data collection 
# procedures before you actually see the data, so that you know the test you will run has 
# enough sensitivity or power. Power is one minus the Type II error rate, or the probability 
# that you will reject the NULL hypothesis when the alternative hypothesis is true.
# 
# There are several aspects of a hypothesis test that affect its power for a particular effect
# size. Intuitively, setting a lower 𝛼 decreases the power of the test for a given effect siz
# e because the NULL hypothesis will be more difficult to reject. This means that for an 
# experiment with fixed parameters (i.e., with a predetermined sample size, recording mechanism, 
# etc), the power of the hypothesis test trades off with its Type I error rate, no matter what 
# effect size you target.
# 
# We can explore the trade off of power and Type I error concretely using the babies data. 
# Since we have the full population, we know what the true effect size is (about 8.93) and 
# we can compute the power of the test for true difference between populations.
# 
# Set the seed at 1 and take a random sample of 𝑁=5 measurements from each of the smoking
# and nonsmoking datasets. What is the p-value (use the t-test function)? 

set.seed(1, sample.kind="Rounding")
N <- 5
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)

tval <- t.test(dat.ns, dat.s)
tval$p.value
# 0.1366428

#----------------------------------------------------------------------------------
# Power Calculations

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

dat <- read.csv(file.path("data","mice_pheno.csv")) #Previously downloaded 

controlPopulation <- filter(dat,Sex == "F" & Diet == "chow") %>%  
  select(Bodyweight) %>% unlist

hfPopulation <- filter(dat,Sex == "F" & Diet == "hf") %>%  
  select(Bodyweight) %>% unlist

mu_hf <- mean(hfPopulation)
mu_control <- mean(controlPopulation)
print(mu_hf - mu_control)
# 2.375517

print((mu_hf - mu_control)/mu_control * 100) # percent increase
# 9.942157

#We do not always get p-value below 0.05
set.seed(1, sample.kind="Rounding")
N <- 5
hf <- sample(hfPopulation,N)
control <- sample(controlPopulation,N)
t.test(hf,control)$p.value
# 0.1410204

# A type I error -- is defined as rejecting the NULL when NULL is true -- a false positive
#
# The reason we don’t use infinitesimal cut-offs, i.e. go for very small p values, to avoid
# type I errors at all cost is that there is another error we can commit:
#
# A type II error -- is defined as not rejecting the NULL when NULL is false -- false negative
# 
# Most journals and regulatory agencies frequently insist that results be significant at the 0.01 or 0.05 levels. 
# 
# Power -- is the probability of rejecting the NULL when the NULL is false.
# Statistical theory gives us formulas to calculate power. The pwr package performs these calculations for you.

N <- 12
alpha <- 0.05
B <- 2000

# We will compute this probability by re-running the exercise many times and
# calculating the proportion of times the NULL hypothesis is rejected.

reject_null <- function(N, alpha=0.05){
  hf <- sample(hfPopulation, N) 
  control <- sample(controlPopulation, N)
  pval <- t.test(hf, control)$p.value
  pval < alpha
}

reject(N)
# FALSE

# Now we can use the replicate function to do this B times.
rejections <- replicate(B, reject_null(N))

# Our power is only:
mean(rejections)
# 0.2145

# This explains why the t-test was not rejecting when we knew the NULL was false.
# With a sample size of just 12, our power is about 23%. 
# This means that the probability of a Type - II error is about 77%
# I.e. we are likely not to reject NULL when NULL is false!

# Let’s see how power improves with N.

Ns <- seq(5, 100, 5)

power <- sapply(Ns,function(N){
  rejections <- replicate(B, reject_null(N))
  mean(rejections)
})

# Not surprisingly power increases with N, it is also assymptotic
plot(Ns, power, type="b")

# Similarly, if we change the level alpha at which we reject, power changes. 
# The smaller I want the chance of type I error to be, the less power I will have.

N <- 30
alphas <- c(0.5, 0.25, 0.125, 0.0625, 0.03125, 0.0150625)
power <- sapply(alphas,function(alpha){
  rejections <- replicate(B,reject(N, alpha=alpha))
  mean(rejections)
})
plot(alphas, power, xlab="alpha", type="b", log="x")

# There is no “right” power or “right” alpha level, but it is important
# that you understand what each means.

# NOTE: The p-values are somewhat arbitrary when the NULL hypothesis is not true
#       and therefore the alternative hypothesis is true!
#
# NOTE: When the alternative hypothesis is true, we can make a p-value as small
#       as we want simply by increasing the sample size.

# A function that returns a p-value for a given sample size N:
calculatePvalue <- function(N) {
  hf <- sample(hfPopulation, N) 
  control <- sample(controlPopulation, N)
  t.test(hf,control)$p.value
}

# We increase the sample size from 10 to 200
# For each sample size, we will calculate a few p-values. 
# We can do this by repeating each value of N a few (10) times.
Ns <- seq(10, 200, by = 10)
Ns_rep <- rep(Ns, each = 10)

# Run the simulations
pvalues <- sapply(Ns_rep, calculatePvalue)

# Plot the 10 p-values we generated for each sample size:
# The y-axis is log-scale
plot(Ns_rep, pvalues, log="y", xlab="sample size",
     ylab="p-values")

# The standard cutoffs of 0.01 and 0.05 are indicated with horizontal red lines.
abline(h=c(.01, .05), col="red", lwd=2)

# Here, having a larger sample size does help to increase the precision of
# our estimate of the difference Δ

# The p-values get smaller and smaller with increasing sample size because the 
# numerator of the t-statistic has N‾‾√ (for equal sized groups, and a similar
# effect occurs when M≠N). Therefore, if Δ is non-zero, the t-statistic will
# increase with N.

# A better statistic to report is the effect size with a confidence interval 
# or some statistic which gives the reader a sense of the change in a meaningful scale.

# We can report the effect size as a percent by dividing the difference and the 
# confidence interval by the control population mean:

N <- 12
hf <- sample(hfPopulation, N)
control <- sample(controlPopulation, N)
diff <- mean(hf) - mean(control)
diff / mean(control) * 100

t.test(hf, control)$conf.int / mean(control) * 100

# We can also report a statistic called Cohen’s d, which is the difference 
# between the groups divided by the pooled standard deviation of the two groups.

# This tells us how many standard deviations of the data the mean
# of the high-fat diet group is from the control group.
sd_pool <- sqrt(((N-1)*var(hf) + (N-1)*var(control))/(2*N - 2))
diff / sd_pool

# Under the alternative hypothesis, unlike the t-statistic which is guaranteed 
# to increase, the effect size and Cohen’s d will become more precise.

#----------------------------------------------------------------------------------
# Power Calculations Exercises

if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")
library(downloader)

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

if(!require(rafalib)) install.packages("rafalib", repos = "http://cran.us.r-project.org")
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- file.path("data", basename(url))
if(! file.exists(filename)) download(url, destfile=filename)
babies <- read.table(filename, header=TRUE)

str(babies)

# This is a large dataset (1,236 cases), and we will pretend that it contains the entire population
# in which we are interested. We will study the differences in birth weight between babies born to
# smoking and non-smoking mothers.

# First, let's split this into two birth weight datasets:
#  * one of birth weights to non-smoking mothers and
#  * the other of birth weights to smoking mothers.

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

# Now, we can look for the true population difference in means between smoking and non-smoking birth weights.

mean(bwt.nonsmoke) - mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

# The population difference of mean birth weights is about 8.9 ounces. The standard deviations 
# of the nonsmoking and smoking groups are about 17.4 and 18.1 ounces, respectively.

# As we did with the mouse weight data, this assessment interactively reviews inference concepts
# using simulations in R. We will treat the babies dataset as the full population and draw samples
# from it to simulate individual experiments. We will then ask whether somebody who only received
# the random samples would be able to draw correct conclusions about the population.

# We are interested in testing whether the birth weights of babies born to non-smoking mothers
# are significantly different from the birth weights of babies born to smoking mothers.

#--------------------------------------------------
# Power Calculations Exercises #1

# We can explore the trade off of power and Type I error concretely using the babies
# data. Since we have the full population, we know what the true effect size, i.e.
#     mean(bwt.nonsmoke) - mean(bwt.smoke)
# is (about 8.93) and we can compute the power of the test for true difference
# between populations.

# Set the seed at 1 and take a random sample of N = 5 measurements from each of the
# smoking and nonsmoking datasets. You used the t-test function to find the p-value.

set.seed(1, sample.kind = "Rounding")

N <- 5

dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
t.test(dat.ns, dat.s)$p.value

# The p-value is larger than 0.05 so using the typical cut-off, we would not reject.
# This is a type II error. Which of the following is *not* a way to decrease this
# type of error? 

# Find a population for which the null is not true. 

#--------------------------------------------------
# Power Calculations Exercises #2

# Set the seed at 1, then use the replicate function to repeat the code used in the
# exercise above 10,000 times. What proportion of the time do we reject at the 0.05 level? 

set.seed(1, sample.kind = "Rounding")

reject_null <- function(N, B) {
  dat.ns <- sample(bwt.nonsmoke, N)
  dat.s <- sample(bwt.smoke, N)
  tval <- t.test(dat.ns, dat.s)
  tval$p.value < B
}

B <- 0.05
R <- 10000
rejects <- replicate(R, reject_null(N, B))
mean(rejects)
# 0.0984

#--------------------------------------------------
# Power Calculations Exercises #3

# Note that, not surprisingly, the power is lower than 10%. Repeat the exercise above for 
# samples sizes of 30, 60, 90 and 120. Which of those four gives you power of about 80%? 

plot_N_powers <- function(R, B) {
  Ns_rep <- seq(30, 120, 30)
  power_values <- sapply(Ns_rep,
                         function(N_size) {
                           rejects <- replicate(R, reject_null(N_size, B))
                           mean(rejects)
                         }
  )
  plot(Ns_rep, power_values, xlab="Sample sizes", ylab="Power", type="b")
  abline(h = 0.8, col="red", lwd=3, lty=2)
  for(val in Ns_rep) {
    abline(v = val, col="green")
  }
}

plot_N_powers(R, B)
# 60

#--------------------------------------------------
# Power Calculations Exercises #4

# Repeat the problem above, but now require an 𝛼 level of 0.01.
# Which of those four gives you power of about 80%?
plot_N_powers(R, 0.01)
# 90

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
# Inference II: Monte Carlo Simulation, Permutation Tests and Association tests
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# Monte Carlo Simulation
set.seed(1, sample.kind = "Rounding")

controlPopulation <- read.csv(file.path("data", "femaleControlsPopulation.csv"))
controlPopulation <- unlist(controlPopulation)

ttest_generator <- function(N) {
  cases <- sample(controlPopulation, N)
  controls <- sample(controlPopulation, N)
  tstat <- (mean(cases) - mean(controls))/sqrt(var(cases)/N + var(controls)/N)
  return(tstat)
}

# Good N(0,1) approimation results for the sample size of 10
N <- 10
ttests <- replicate(1000, ttest_generator(N))

hist(ttests)

qqnorm(ttests)
abline(0,1)

# Worse N(0,1) approximation results for a smaller sample size of size 3
N <- 3
ttests <- replicate(1000, ttest_generator(N))
qqnorm(ttests)
abline(0,1)

# How well could a T distrubution handle the smaller sample size?
ps <- (seq(0, 999) + 0.5)/1000 #Compute quantiles
#Compute probabilities for the T distribution with the specified degrees of freedom
qqplot(qt(ps, df = 2*N-2), ttests, xlim=c(-6, 6), ylim=c(-6, 6))
abline(0, 1)

# The T distribution approximation seems to be better than the CLT (N(0, 1)) one.

#The entire population looks approximately normal
qqnorm(controlPopulation)
qqline(controlPopulation)

# If we did not have access to the entire population we could generae one, 
# knowing the mean and the standard devliation.
controls <- rnorm(5000, mean=24, sd=3.5)

# Let us consider the t-test generator function which generates the population
# Instead of sampling from a population. This way we do not need the data.
ttest_generator <- function(N, mean=24, sd=3.5) {
  cases <- rnorm(N, mean = mean, sd=sd)
  controls <- rnorm(N, mean = mean, sd=sd)
  tstat <- (mean(cases) - mean(controls))/sqrt(var(cases)/N + var(controls)/N)
  return(tstat)
}

# Let us see the results of what we generate
N <- 10
ttests <- replicate(1000, ttest_generator(N))

hist(ttests)

qqnorm(ttests)
abline(0,1)

# The result seem to be similar to that we've obtained when sampling from the population
# E.g. we gain a fairly good approximtion of the N(0, 1) distribution

# How well does the T distrubution approximates the data?
ps <- (seq(0, 999) + 0.5)/1000 #Compute quantiles
#Compute probabilities for the T distribution with the specified degrees of freedom
qqplot(qt(ps, df = 2*N-2), ttests, xlim=c(-6, 6), ylim=c(-6, 6))
abline(0, 1)

# Again the T-distribution approximation is better than that of CTL

#----------------------------------------------------------------------------------
# Monte Carlo Exercises


#--------------------------------------------------
# Monte Carlo Exercises #1

# Imagine you are William_Sealy_Gosset and have just mathematically derived the distribution
# of the t-statistic when the sample comes from a normal distribution. Unlike Gosset you have
# access to computers and can use them to check the results.

# Let's start by creating an outcome.

# Set the seed at 1, use rnorm to generate a random sample of size 5, 𝑋1,…,𝑋5 from a standard
# normal distribution, then compute the t-statistic 𝑡=5‾√𝑋¯/𝑠 with 𝑠 the sample standard deviation.
# What value do you observe?
set.seed(1, sample.kind = "Rounding")

N <- 5
X <- rnorm(N)
t <- mean(X)*sqrt(N)/sd(X)
t
# 0.3007746

#--------------------------------------------------
# Monte Carlo Exercises #2

# You have just performed a Monte Carlo simulation using rnorm , a random number generator for
# normally distributed data. Gosset's mathematical calculation tells us that the t-statistic
# defined in the previous exercises, a random variable, follows a t-distribution with 𝑁−
# 1 degrees of freedom. Monte Carlo simulations can be used to check the theory: we generate
# many outcomes and compare them to the theoretical result. Set the seed to 1, generate 𝐵=1000
# t-statistics as done in exercise 1. What proportion is larger than 2?
set.seed(1, sample.kind = "Rounding")

generate_tstat <- function (N) {
  X <- rnorm(N)
  t <- mean(X)*sqrt(N)/sd(X)
  return(t)
}

B <- 1000
tstats <- replicate(B, generate_tstat(N))

mean(tstats > 2)
# 0.068

# If we use the T ddistribution with the N-1 degrees of freedom
# directly and compute the same proportion exactly, we get:
1 - pt(2, df = N-1)
# 0.05805826

#--------------------------------------------------
# Monte Carlo Exercises #3

# The answer to exercise 2 is very similar to the theoretical prediction: 1-pt(2,df=4). 
# We can check several such quantiles using the qqplot function.

# To obtain quantiles for the t-distribution we can generate percentiles from just 
# above 0 to just below 1: B=100; ps = seq(1/(B+1), 1-1/(B+1),len=B) and compute 
# the quantiles with qt(ps,df=4). Now we can use qqplot to compare these theoretical 
# quantiles to those obtained in the Monte Carlo simulation. Use Monte Carlo simulation
# developed for exercise 2 to corroborate that the t-statistic 𝑡=𝑁‾‾√𝑋¯/𝑠 follows
# a t-distribution for several values of 𝑁.

# For which sample sizes does the approximation best work? 

B <- 100
ps <- seq(1/(B+1), 1-1/(B+1), len = B)
tprobs <- qt(ps, df = N-1)

qqplot(tprobs, tstats, xlim=c(-4,4), ylim=c(-4,4))
abline(0,1)

plot_qq_results <- function(B = 1000, N) {
  tstats <- replicate(B, generate_tstat(N))
  
  tprobs <- qt(ps, df = N-1)
  
  qqplot(tprobs, tstats, xlim=c(-4,4), ylim=c(-4,4))
  abline(0,1)

  qqnorm(tstats, xlim=c(-4,4), ylim=c(-4,4))
  abline(0,1)
}

plot_qq_results(N=10)
plot_qq_results(N=20)
plot_qq_results(N=40)
plot_qq_results(N=80)
plot_qq_results(N=160)
plot_qq_results(N=320)
plot_qq_results(N=640)

# The approximations are spot on for all sample sizes. 

#--------------------------------------------------
# Monte Carlo Exercises #4

# Use Monte Carlo simulation to corroborate that the t-statistic comparing
# two means and obtained with normally distributed (mean 0 and sd) data follows
# a t-distribution. In this case we will use the t.test function with var.equal=TRUE. 
# With this argument the degrees of freedom will be df=2*N-2 with N the sample size.
# For which sample sizes does the approximation best work? 
set.seed(1, sample.kind = "Rounding")

ttest_generator <- function(N, mean=0, sd=1) {
  cases <- rnorm(N, mean = mean, sd=sd)
  controls <- rnorm(N, mean = mean, sd=sd)
  tval <- t.test(cases, controls, var.equal=TRUE)
  return(tval$statistic)
}

# Let us see the results of what we generate
plot_qq_results <- function(B = 1000, N) {
  tstats <- replicate(B, generate_tstat(N))
  
  tprobs <- qt(ps, df = 2*N-2)
  
  qqplot(tprobs, tstats, xlim=c(-4,4), ylim=c(-4,4))
  abline(0,1)
  
  qqnorm(tstats, xlim=c(-4,4), ylim=c(-4,4))
  abline(0,1)
}

plot_qq_results(N=10)
plot_qq_results(N=20)
plot_qq_results(N=40)
plot_qq_results(N=80)
plot_qq_results(N=160)
plot_qq_results(N=320)
plot_qq_results(N=640)

# The approximations are spot on for all sample sizes. 

#--------------------------------------------------
# Monte Carlo Exercises #5

# Is the following statement true or false? If instead of generating the
# sample with X = rnorm(15) we generate it with binary data (either positive
# or negative 1 with probability 0.5) X = sample(c(-1,1), 15, replace=TRUE)
# then the t-statistic

tstat <- sqrt(15) * mean(X) / sd(X)

# is approximated by a t-distribution with 14 degrees of freedom. 

# false - X is not amples from normally distributed random variables!

set.seed(1, sample.kind = "Rounding")
N <- 15
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)
#The population data is not normal thus the theory does not apply.
#We check with a Monte Carlo simulation. The qqplot shows a large tail. 
#Note that there is a small but positive chance that all the X are the same.
##In this case the denominator is 0 and the t-statistics is not defined

#--------------------------------------------------
# Monte Carlo Exercises #6

# Is the following statement true or false ? If instead of generating the 
# sample with X=rnorm(N) with N=1000, we generate the data with binary 
# data X= sample(c(-1,1), N, replace=TRUE), then the t-statistic
# sqrt(N)*mean(X)/sd(X) is approximated by a t-distribution with 999 
# degrees of freedom. 

set.seed(1, sample.kind = "Rounding")
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X <-  sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
qqnorm(tstats)
abline(0,1)
#With N=1000, CLT kicks in and the t-statistic is approximated with normal 0,1
##Furthermore, t-distribution with df=999 and normal are practically the same.

# WTF !?!?!?!? The binary data is not normally distributed :|

#--------------------------------------------------
# Monte Carlo Exercises #7

# We can derive approximation of the distribution of the sample average or the
# t-statistic theoretically. However, suppose we are interested in the distribution
# of a statistic for which a theoretical approximation is not immediately obvious.

# Consider the sample median as an example. Use a Monte Carlo to determine which 
# of the following best approximates the median of a sample taken from normally 
# distributed population with mean 0 and standard deviation 1.

N <- 30
medians <- replicate(1000, {
  sample <- rnorm(N)
  median(sample)
})

qqnorm(medians)
qqline(medians)

mean(medians)
sd(medians)
1/sqrt(N)

ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qvals <- qt(ps,N-1)
qqplot(qvals, medians, xlim=c(-5,5), ylim=c(-5,5))
abline(0,1)

set.seed(1, sample.kind = "Rounding")
Ns <- seq(5,45,5)
library(rafalib)
mypar(3,3)
for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}
##there is an asymptotic result that says SD is sqrt(N*4*dnorm(0)^2)

#----------------------------------------------------------------------------------
# Permutations Exercises

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- file.path("data", basename(url))
if(!file.exists(filename)) {
  download(url, destfile=filename)
}
babies <- read.table(filename, header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

#--------------------------------------------------
# Permutations Exercises #1 

# We will generate the following random variable based on a sample
# size of 10 and observe the following difference:

N <- 10
set.seed(1,  sample.kind = "Rounding")
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obsdiff_mean <- mean(smokers) - mean(nonsmokers)
obsdiff_mean
obsdiff_median <- median(smokers) - median(nonsmokers)
obsdiff_median

# The question is whether this observed difference is statistically significant.
# We do not want to rely on the assumptions needed for the normal or t-distribution
# approximations to hold, so instead we will use permutations. We will reshuffle 
# the data and recompute the mean. We can create one permuted sample with the
# following code:

dat <- c(smokers, nonsmokers)
shuffle <- sample( dat )
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
mean(smokersstar) - mean(nonsmokersstar)

# The last value is one observation from the NULL distribution we will construct.
# Set the seed at 1, and then repeat the permutation 1,000 times to create a NULL
# distribution. What is the permutation derived p-value for our observation?

compute_null_distr <- function(fnct) {
  dat <- c(smokers, nonsmokers)
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  return(fnct(smokersstar) - fnct(nonsmokersstar))
}

set.seed(1,  sample.kind = "Rounding")
avgdiff_mean <- replicate(1000, compute_null_distr(mean))

hist(avgdiff_mean)
abline(v=obsdiff_mean, col="red", lwd=2)

# How many of the null means are bigger than the observed value? 
# That proportion would be the p-value for the null.
# 
# We SHALL add a 1 to the numerator and denominator to account for misestimation
# of the p-value (for more details see Phipson and Smyth, Permutation P-values 
# should never be zero).

(sum(abs(avgdiff_mean) > abs(obsdiff_mean)) + 1) / (length(avgdiff_mean) + 1)
# 0.05294705

#--------------------------------------------------
# Permutations Exercises #2

# Repeat the above exercise, but instead of the differences in mean, consider 
# the differences in median obs <- median(smokers) - median(nonsmokers). 
# What is the permutation based p-value? 

set.seed(1,  sample.kind = "Rounding")
avgdiff_median <- replicate(1000, compute_null_distr(median))

hist(avgdiff_median)
abline(v=obsdiff_median, col="red", lwd=2)

(sum(abs(avgdiff_median) > abs(obsdiff_median)) + 1) / (length(avgdiff_median) + 1)
# 0.01798202

#----------------------------------------------------------------------------------
# Association Tests

# An acquaintance of Fisher’s claimed that she could tell if milk was added before or after tea was poured. 
# Fisher gave her four pairs of cups of tea: one with milk poured first, the other after. 
# The order was randomized. Say she picked 3 out 4 correctly, do we believe she has a special ability?

# The data from the experiment above can be summarized by a 2 by 2 table:
tab <- matrix(c(3, 1, 1, 3), 2, 2)
rownames(tab)<-c("Poured Before", "Poured After")
colnames(tab)<-c("Guessed before", "Guessed after")
tab

# The function fisher.test performs the calculations above and can be obtained like this:
fisher.test(tab,alternative="greater")

# Imagine we have 250 individuals, where some of them have a given disease and the rest do not.
# We observe that 20% of the individuals that are homozygous for the minor allele (aa) have the
# disease compared to 10% of the rest. Would we see this again if we picked another 250 individuals?

#Let’s create a dataset with these percentages:
  
disease = factor( c(rep(0,180), rep(1,20), rep(0,40), rep(1,10) ),
                  labels=c("control","cases") )
genotype = factor( c(rep("AA/Aa",200), rep("aa",50)),
                   levels=c("AA/Aa","aa"))
dat <- data.frame(disease, genotype)
dat <- dat[sample(nrow(dat)),] #shuffle them up
head(dat)

# The table function tabulates the frequency of each level in a factor.
table(genotype)

table(disease)

# If you you feed the function two factors, it will tabulate all possible
# pairs and thus create the two by two table:
tab <- table(genotype, disease)
tab

# The odds ratio (OR). We compute the odds of having the disease if you are 
# an “aa”: 10/40, the odds of having the disease if you are an “AA/Aa”: 20/180,
# and take the ratio: (10/40)/(20/180):

(tab[2,2]/tab[2,1]) / (tab[1,2]/tab[1,1])

# Under the null hypothesis, the group with 200 individuals and the group with 50 individuals 
# were each randomly assigned the disease with the same probability. If this is the case, then 
# the probability of disease is:

p=mean(disease=="cases")
p

# The expected table is therefore:
expected <- rbind(c(1-p, p) * sum(genotype == "AA/Aa"),
                  c(1-p, p) * sum(genotype == "aa"))
dimnames(expected) <- dimnames(tab)
expected

# The Chi-square test uses an asymptotic result (similar to the CLT) related to 
# the sums of independent binary outcomes. Using this approximation, we can compute
# the probability of seeing a deviation from the expected table as big as the one 
# we saw. The p-value for this table is:

chisq.test(tab)$p.value


# Reporting only p-values is not an appropriate way to report the results of your experiment
# They have large sample sizes and report impressively small p-values. Yet when one looks
# closely at the results, we realize odds ratios are quite modest: barely bigger than 1.
# In this case the difference of having genotype AA/Aa or aa might not change an individual’s
# risk for a disease in an amount which is practically significant, in that one might not
# change one’s behavior based on the small increase in risk.

# There is not a one-to-one relationship between the odds ratio and the p-value.
# To demonstrate, we recalculate the p-value keeping all the proportions identical, 
# but increasing the sample size by 10, which reduces the p-value substantially 

tab<-tab * 10
chisq.test(tab)$p.value


#-------------------------# Generalized linear models provide estimates of the log odds ratio
fit <- glm(disease~genotype,family="binomial",data=dat)
coeftab<- summary(fit)$coef
coeftab

# The second row of the table shown above gives you the estimate and SE of the 
# log odds ratio. Mathematical theory tells us the this estimate is approximately
# normally distributed. We can therefore form a confidence interval and then
# exponentiate to provide a confidence interval for the OR.
ci <- coeftab[2,1] + c(-2,2)*coeftab[2,2]
exp(ci)

--------------------------------------------------------
# Association Tests Exercises

dat_asct <- read.csv(file.path("data","assoctest.csv"))

str(dat_asct)

#--------------------------------------------------
# Association Tests Exercises #1

# Compute the Chi-square test for the association of genotype with case/control status
# (using the table() function and the chisq.test() function). Examine the table to see
# if it looks enriched for association by eye. What is the X-squared statistic?

asct_tbl <- table(dat_asct)
asct_tbl
ctval <- chisq.test(asct_tbl)
ctval$statistic
#3.343653

#--------------------------------------------------
# Association Tests Exercises #2

# Compute the Fisher's exact test ( fisher.test() ) for the same table. What is the p-value?
ptval <- fisher.test(asct_tbl)
ptval$p.value
#0.05193834


