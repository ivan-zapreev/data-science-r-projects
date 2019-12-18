# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# inspect a grouped tibble
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

#------------------------------------------------------

# inspect data frame and tibble
Teams
as.tibble(Teams)

# subsetting a data frame sometimes generates vectors
class(Teams[,20])

# subsetting a tibble always generates tibbles
class(as.tibble(Teams[,20]))

# pulling a vector out of a tibble
class(as.tibble(Teams)$HR)

# access a non-existing column in a data frame or a tibble
Teams$hr
as.tibble(Teams)$hr

# create a tibble with complex objects
tibble(id = c(1, 2, 3), func = c(mean, median, sd))

#------------------------------------------------------

# use do to fit a regression line to each HR stratum
dat %>%  
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

# using do without a column name gives an error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))

# define a function to extract slope from lm
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
dat %>%  
  group_by(HR) %>%
  do(get_slope(.))

# not the desired output: a column containing data frames
dat %>%  
  group_by(HR) %>%
  do(slope = get_slope(.))

# data frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
    data.frame(term = names(fit$coefficients),
             slope = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}
dat %>%  
  group_by(HR) %>%
  do(get_lse(.))

#------------------------------------------------------

# use tidy to return lm estimates and related information as a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

# add confidence intervals with tidy
tidy(fit, conf.int = TRUE)

# pipeline with lm, do, tidy
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# make ggplots
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# inspect with glance
glance(fit)

#------------------------------------------------------------
# You want to take the tibble dat, which we used in the video 
# on the do function, and run the linear model R ~ BB for each 
# strata of HR. Then you want to add three new columns to your 
# grouped tibble: the coefficient, standard error, and p-value 
# for the BB term in the model.
# 
# You’ve already written the function get_slope, shown below.
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

#What additional code could you write to accomplish your goal?
dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))

#You want to know whether the relationship between home runs
#and runs per game varies by baseball league. You create the 
#following dataset:

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

#What code would help you quickly answer this question?
dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R~HR, data=.),conf.int=TRUE)) %>% 
  filter(term == "HR") 

#-------------------------------------------------------------

# We have investigated the relationship between fathers' heights 
# and sons' heights. But what about other parent-child relationships? 
# Does one parent's height have a stronger association with child
# height? How does the child's gender affect this relationship in
# heights? Are any differences that we observe statistically significant?
#   
# The galton dataset is a sample of one male and one female child 
# from each family in the GaltonFamilies dataset. The pair column
# denotes whether the pair is father and daughter, father and son,
# mother and daughter, or mother and son.
# 
# Create the galton dataset using the code below:
  
library(tidyverse)
library(HistData)
data("GaltonFamilies")
#set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

# Group by pair and summarize the number of observations in each group.
galton_summary <- galton %>% group_by(pair) %>% summarize(cnt = n())

# How many father-daughter pairs are in the dataset?
galton_summary %>% filter(pair=="father_daughter") %>% pull(cnt)

# How many mother-son pairs are in the dataset?
galton_summary %>% filter(pair=="mother_son") %>% pull(cnt)

#Calculate the correlation coefficients for fathers
#and daughters, fathers and sons, mothers and 
#daughters and mothers and sons.

correlation <- function(data){
  data.frame(cor = cor(data$parentHeight, data$childHeight))
}

res <- galton %>% group_by(pair) %>% do(correlation(.)) %>% arrange(desc(cor))

#Which pair has the strongest correlation in heights?
res$pair[which.max(res$cor)]

#Which pair has the strongest correlation in heights?
res$pair[which.min(res$cor)]

#Question 10 has two parts. The information here applies to both parts.
# 
#Use lm and the broom package to fit regression lines for each parent-
#child pair type. Compute the least squares estimates, standard errors,
#confidence intervals and p-values for the parentHeight coefficient
#for each pair.
data <- galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight") %>%
  select(pair, estimate, std.error, p.value, conf.low, conf.high, )

data %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point() + geom_hline(yintercept = 0)

data %>% mutate(conf_width = conf.high - conf.low) %>% arrange(desc(conf_width))
