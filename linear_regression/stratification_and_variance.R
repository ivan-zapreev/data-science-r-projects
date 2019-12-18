# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

#Mean son height
galton_heights %>% summarize(avg = mean(son)) %>% pull(avg)
  
# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# calculate values to plot regression line on original data
#(x -mu_x)/s_x = r * (y -mu_y)/s_y
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

#Check that the son's distribution in each segment 
#for father heights is approximately normal, which
#then means that the (y,x) distribution is a
#Bivariate Normal Distribution, so that we can rely
#on correlation and regression to analyze it
#I.e. the conditional expectation for y E(Y| X = x)
#follows the regression line
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)

# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

#The line for son's height dependency on the father's height
cat("E(Y | X = x) = ", m_1, "*x + ", b_1)

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y

#The line for father's height dependency on the son's height
cat("E(X | Y = y) = ", m_2, "*y + ", b_2)


#Analyze a set of mother and daughter heights, also from GaltonFamilies.
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%    
  filter(gender == "female") %>%    
  group_by(family) %>%    
  sample_n(1) %>%    
  ungroup() %>%    
  select(mother, childHeight) %>%    
  rename(daughter = childHeight) 

#Below y==daughter, x=mother
mu_y <- mean(female_heights$daughter)
s_y <- sd(female_heights$daughter)
mu_x <- mean(female_heights$mother)
s_x <- sd(female_heights$mother)
r <- cor(female_heights$mother, female_heights$daughter)

slope_y_x <- r * s_y / s_x
intercept_y_x <- mu_y - slope_y_x * mu_x

#What percent of the variability in daughter heights is explained by the mother's height?
r^2*100

#A mother has a height of 60 inches. What is the conditional expected value of her daughter's height given the mother's height?
slope_y_x * 60 + intercept_y_x
