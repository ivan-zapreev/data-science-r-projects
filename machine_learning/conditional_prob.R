#In a previous module, we covered Bayes' theorem and the Bayesian paradigm.
#Conditional probabilities are a fundamental part of this previous covered rule.
#𝑃(𝐴|𝐵)=𝑃(𝐵|𝐴)𝑃(𝐴)𝑃(𝐵)
#We first review a simple example to go over conditional probabilities.

#Assume a patient comes into the doctor’s office to test whether they have a particular disease.
# 1. The test is positive 85% of the time when tested on a patient with the disease (high sensitivity): 𝑃(test+|disease)=0.85
# 2. The test is negative 90% of the time when tested on a healthy patient (high specificity): 𝑃(test−|heathy)=0.90
# 3. The disease is prevalent in about 2% of the community: 𝑃(disease)=0.02
#Using Bayes' theorem, calculate the probability that you have the disease if the test is positive.
#The following 4 questions (Q2-Q5) all relate to implementing this calculation using R.

#We have a hypothetical population of 1 million individuals with the following conditional probabilities as described below:
0.85*0.02/(0.85*0.02+0.1*0.98)

compute_D_P <- function(p_P_D, p_D, p_N_H) {
  p_H <- 1.0 - p_D
  p_P_H <- 1.0 - p_N_H
  p_P <- p_P_D*p_D + p_P_H*p_H
  p_D_P <- p_P_D*p_D/p_P
  p_D_P
}

compute_D_P(0.85,0.02,0.9)

#We have a hypothetical population of 1 million individuals with the following conditional probabilities as described below:
# 1. The test is positive 85% of the time when tested on a patient with the disease (high sensitivity): 𝑃(test+|disease)=0.85
# 1. The test is negative 90% of the time when tested on a healthy patient (high specificity): 𝑃(test−|heathy)=0.90
# 1. The disease is prevalent in about 2% of the community: 𝑃(disease)=0.02

#Here is some sample code to get you started:
  
set.seed(1, sample.kind="Rounding")

#Initialize the population with the deseased and healthy people
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)

#For the healtry people assign the test results according to the p_N_H, p_P_H probabilities
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))

#For the deseased people assign the test results according to the p_N_D, p_P_D probabilities
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

p_P_D <- 0.85
p_D <- 0.02
p_N_H <- 0.9
p_H <- 1.0 - p_D
p_P_H <- 1.0 - p_N_H
p_P <- p_P_D*p_D + p_P_H*p_H
p_D_P <- p_P_D*p_D/p_P

#What is the probability that a test is positive?
p_P <- p_P_D*p_D + p_P_H*p_H
p_P

#What is the probability that an individual has the disease if the test is negative?
p_N <- 1.0 - p_P
p_N_D <- 1.0 - p_P_D
p_D_N <- p_N_D*p_D/p_N
p_D_N

#What is the probability that you have the disease if the test is positive?
p_D_P

#Compare the prevalence of disease in people who test positive to the overall prevalence of disease.
#If a patient's test is positive, how much does that increase their risk of having the disease?

#First calculate the probability of having the disease given a positive test,
#then divide by the probability of having the disease.
p_D_P/p_D

#------------------------------------------------------------

#We are now going to write code to compute conditional probabilities for
#being male in the heights dataset. Round the heights to the closest inch.
#Plot the estimated conditional probability 𝑃(𝑥)=Pr(Male|height=𝑥) for each 𝑥.

#Part of the code is provided here, which of the following blocks of code
#can be used to replace MISSING CODE to make the correct plot?

library(dslabs)
data("heights")

heights %>%
  mutate(height = round(height)) %>% 
  group_by(height) %>%
  summarise(p = mean(sex=="Male")) %>%
  qplot(height, p, data =.)

#In the plot we just made in Q1 we see high variability for low values of height. 
#This is because we have few data points. This time use the quantile 0.1,0.2,…,0.9 
#and the cut function to assure each group has the same number of points. Note that
#for any numeric vector x, you can create groups based on quantiles like this:
#cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).

#Part of the code is provided here, which of the following lines of code can 
#be used to replace MISSING CODE to make the correct plot?

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, probs = ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#You can generate data from a bivariate normal distrubution using the
#MASS package using the following code: 
library(MASS)
library(tidyverse)

Sigma <- 9*matrix(c(1, 0.5, 0.5, 1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
dat
plot(dat)

#Using an approach similar to that used in the previous exercise, 
#let's estimate the conditional expectations and make a plot.
#Part of the code has again been provided for you: 

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, probs = ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(x = mean(x), y = mean(y)) %>%
  qplot(x, y, data =.)








