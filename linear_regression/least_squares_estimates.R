# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

#--------------------------------------------------------------

# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit

# summary statistics
summary(fit)

#--------------------------------------------------------------

# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))
lse %>% summarize(cor(beta_0, beta_1))

#Advanced Note on LSE

#The LSE can be strongly correlated
lse %>% summarize(cor(beta_0, beta_1))
lse %>% ggplot(aes(x=beta_0, y=beta_1)) + geom_point(alpha=0.5)

#Standardize the father heights, which changes x_i to x_i - mean(x)
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})

#Observe what happens to the correlation in this case:
lse1 <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])
cor(lse1$beta_0,lse1$beta_1) 
lse1 %>% ggplot(aes(
  x=beta_0-mean(beta_0),
  y=beta_1-mean(beta_1))) + 
  geom_point(alpha=0.5)

#--------------------------------------------------------

# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()

#--------------------------------------------------------

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

#Load the Lahman library and filter the Teams data frame
#to the years 1961-2001. Run a linear model in R predicting
#the number of runs per game based on both the number of
#bases on balls per game and the number of home runs per game.

library(Lahman)

#Prepare data
data <- Teams %>% filter(yearID %in% 1961:2001 ) %>%
mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G)
  
#Run the linear model
fit <- lm(R_per_game~BB_per_game+HR_per_game, data=data)
fit

#We run a Monte Carlo simulation where we repeatedly take samples 
#of N = 100 from the Galton heights data and compute the regression
#slope coefficients for each sample. What does the central limit
#theorem tell us about the variables beta_0 and beta_1?

B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
lse

#In an earlier video, we ran the following linear model and
#looked at a summary of the results. What null hypothesis is
#the second p-value (the one in the father row) testing?
mod <- lm(son ~ father, data = galton_heights)
summary(mod)

#Define female_heights, a set of mother and daughter heights sampled from GaltonFamilies, as follows:
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight) 

#Fit a linear regression model predicting the mothers' heights using daughters' heights. 
fit <- lm(mother~daughter,data=female_heights)
fit

#What is the predicted height of the first mother in the dataset?
0.31*female_heights$daughter[1]+44.18

#What is the actual height of the first mother in the dataset?
female_heights$mother[1]

#-------------------------------------------------------------

# We have shown how BB and singles have similar predictive power
# for scoring runs. Another way to compare the usefulness of
# these baseball metrics is by assessing how stable they are
# across the years. Because we have to pick players based on
# their previous performances, we will prefer metrics that are
# more stable. In these exercises, we will compare the stability
# of singles and BBs.
# 
# Before we get started, we want to generate two tables: one for
# 2002 and another for the average of 1999-2001 seasons. We want
# to define per plate appearance statistics, keeping only players
# with more than 100 plate appearances. Here is how we create the 
# 2002 table:

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

#Now compute a similar table but with rates computed over 1999-2001. 
#Keep only rows from 1999-2001 where players have 100 or more plate 
#appearances, then calculate the average single rate (mean_singles)
#and average BB rate (mean_bb) per player over those three seasons.
bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) %>% 
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles),
            mean_bb = mean(bb))

#How many players had a single rate mean_singles of greater
#than 0.2 per plate appearance over 1999-2001?
sum(bat_01$mean_singles > 0.2)

#How many players had a BB rate mean_bb of greater than 0.2 
#per plate appearance over 1999-2001?
sum(bat_01$mean_bb > 0.2)

#Use inner_join to combine the bat_02 table with the table of
#1999-2001 rate averages you created in the previous question. 

ijr <- inner_join(bat_02, bat_01, by="playerID")

#What is the correlation between 2002 singles rates and 1999-2001 average singles rates?
cor(ijr$singles, ijr$mean_singles)

#What is the correlation between 2002 BB rates and 1999-2001 average BB rates?
cor(ijr$bb, ijr$mean_bb)


#Make scatterplots of mean_singles versus singles and mean_bb versus bb.
ijr %>% ggplot(aes(singles, mean_singles)) + geom_point(alpha=0.5)
ijr %>% ggplot(aes(bb, mean_bb)) + geom_point(alpha=0.5)

#Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
#What is the coefficient of mean_singles, the slope of the fit?
fit <- lm(singles~mean_singles,data=ijr)
fit

#Fit a linear model to predict 2002 bb given 1999-2001 mean_bb.
#What is the coefficient of mean_bb, the slope of the fit?
fit <- lm(bb~mean_bb,data=ijr)
fit


