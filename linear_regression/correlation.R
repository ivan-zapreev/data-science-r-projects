# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(childNum==1, gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

#Scale the distributions manually
galton_heights<- galton_heights %>% 
  mutate( scaled_father= (father-mean(father))/sd(father),
          scaled_son= (son-mean(son))/sd(son))
#Compute the regression
mean(galton_heights$scaled_father*galton_heights$scaled_son)
#A more optimal way to compute
mean((galton_heights$father-mean(galton_heights$father))*(galton_heights$son-mean(galton_heights$son)))/(sd(galton_heights$son)*sd(galton_heights$father))
#Compute the regression with automatic scaling
mean(scale(galton_heights$father)*scale(galton_heights$son))

#Compute the correlation
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
  })
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

# expected value and standard error
mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

