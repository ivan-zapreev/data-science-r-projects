#---------------------------------------------------------
#Regularization

library(dslabs)
library(tidyverse)
library(caret)
library(stringr)
data("movielens")
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))

test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()


#--------------------------------------------------------
#Comprehension Check: Regularization

options(digits=3)

#The exercises in Q1-Q8 work with a simulated dataset for 1000 schools.
#This pre-exercise setup walks you through the code needed to simulate
#the dataset.

#An education expert is advocating for smaller schools. The expert bases
#this recommendation on the fact that among the best performing schools,
#many are small schools. Let's simulate a dataset for 1000 schools.
#First, let's simulate the number of students in each school, using the
#following code:

set.seed(1986, sample.kind="Rounding")
n <- round(2^rnorm(1000, 8, 1))

#Now let's assign a true quality for each school that is completely
#independent from size. This is the parameter we want to estimate in
#our analysis. The true quality can be assigned using the following code:

set.seed(1, sample.kind="Rounding")

mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

#We can see the top 10 schools using this code: 

schools %>% top_n(10, quality) %>% arrange(desc(quality))

#Now let's have the students in the school take a test.
#There is random variability in test taking, so we will
#simulate the test scores as normally distributed with
#the average determined by the school quality with a
#standard deviation of 30 percentage points. This code
#will simulate the test scores:

set.seed(1, sample.kind="Rounding")

mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#Q1:

#What are the top schools based on the average score? Show just the ID,
#size, and the average score.

options(digits = 3)

ordered_schools_scores <- schools %>%
  arrange(desc(score)) %>% 
  select(id, size, score)
top_10_schools <- ordered_schools_scores %>% slice(1:10)
top_10_schools

#Report the ID of the top school and average score of the 10th school.

#What is the ID of the top school?
#Note that the school IDs are given in the form "PS x" - where x is a number.
#Report the number only.
str_extract(ordered_schools_scores$id[1], "\\d+")

#What is the average score of the 10th school?
ordered_schools_scores$score[10]

#Q2:

#Compare the median school size to the median school size of the top 10 schools based on the score.

options(digits = 4)

#What is the median school size overall?

median(ordered_schools_scores$size)

#What is the median school size of the of the top 10 schools based on the score?

median(top_10_schools$size)

#Q3:

#According to this analysis, it appears that small schools produce better test 
#scores than large schools. Four out of the top 10 schools have 100 or fewer 
#students. But how can this be? We constructed the simulation so that quality 
#and size were independent. Repeat the exercise for the worst 10 schools.

bottom_10_schools <- ordered_schools_scores %>% arrange(score) %>% slice(1:10)
bottom_10_schools

#What is the median school size of the bottom 10 schools based on the score?

median(bottom_10_schools$size)

#Q4:

#From this analysis, we see that the worst schools are also small. 
#Plot the average score versus school size to see what's going on. 
#Highlight the top 10 schools based on the true quality.

school_stats <- ordered_schools_scores %>%
  group_by(size) %>%
  summarize(avg = mean(score),
            se=sd(score),
            cnt=n())
school_stats

true_top_10 <- schools %>%
  arrange(desc(quality)) %>% 
  select(id, size, quality, score) %>%
  slice(1:10)
true_top_10

true_bottom_10 <- schools %>%
  arrange(quality) %>% 
  select(id, size, quality, score) %>%
  slice(1:10)
true_bottom_10

ggplot() + 
  geom_point(data = ordered_schools_scores, aes(size, score, alpha=0.2)) + 
  geom_smooth(data = ordered_schools_scores, aes(size, score), method="lm") +
  geom_point(data = true_top_10, aes(size, score), color="blue") +
  geom_point(data = true_bottom_10, aes(size, score), color="red")

#What do you observe?
  
#The standard error of the score has larger variability when the school is smaller,
#which is why both the best and the worst schools are more likely to be small. 

#Q5:

#Let's use regularization to pick the best schools. Remember regularization shrinks
#deviations from the average towards 0. To apply regularization here, we first need
#to define the overall average for all schools, using the following code:

overall_score_mean <- mean(sapply(scores, mean))
overall_score_mean

#Then, we need to define, for each school, how it deviates from that average.

#Write code that estimates the score above the average for each school but dividing
#by 𝑛+𝛼 instead of 𝑛, with 𝑛 the school size and 𝛼 a regularization parameter. Try 𝛼=25.

alpha <- 25

reg_score <- function(school_scores) {
  b_i <- sum(school_scores - overall_score_mean)/(length(school_scores) + alpha)
  overall_score_mean + b_i
}

schools <- schools %>% mutate(reg_score = sapply(scores, reg_score))

top_10_reg <- schools %>%
  arrange(desc(reg_score)) %>% 
  select(id, size, quality, score, reg_score) %>%
  slice(1:10)
top_10_reg

#What is the ID of the top school with regularization?
#Note that the school IDs are given in the form "PS x" - where x is a number.
#Report the number only.
str_extract(top_10_reg$id[1], "\\d+")

#What is the regularized score of the 10th school?
top_10_reg$reg_score[10]


true_top_10 <- schools %>%
  arrange(desc(quality)) %>% 
  select(id, size, quality, reg_score) %>%
  slice(1:10)
true_top_10

true_bottom_10 <- schools %>%
  arrange(quality) %>% 
  select(id, size, quality, reg_score) %>%
  slice(1:10)
true_bottom_10

ggplot() + 
  geom_point(data = schools, aes(size, reg_score, alpha=0.2)) + 
  geom_smooth(data = schools, aes(size, reg_score), method="lm") +
  geom_point(data = true_top_10, aes(size, reg_score), color="blue") +
  geom_point(data = true_bottom_10, aes(size, reg_score), color="red")

#Q6:

#Notice that this improves things a bit. The number of small schools that
#are not highly ranked is now lower. Is there a better 𝛼? Using value of 
# 𝛼 from 10 to 250, find the 𝛼 that minimizes the RMSE.

alpha_seq <- 10:250

schools <- schools %>% select(-reg_score)

delta_values_score <- function(school_scores) {
  d_i = sum(school_scores - overall_score_mean)
  n_i <- length(school_scores)
  data.frame(d_i=d_i, n_i=n_i)
}

dat <- lapply(scores, delta_values_score)
schools <- bind_cols(schools, bind_rows(dat))

compute_rmse <-function(alpha) {
  predicted_ratings <- schools %>% 
    mutate(b_i = d_i/(n_i + alpha)) %>%
    mutate(pred = overall_score_mean + b_i) %>%
    pull(pred)
  RMSE(schools$quality, predicted_ratings)
}

dat <- data.frame(alpha_seq = alpha_seq, rmse = sapply(alpha_seq, compute_rmse))

dat %>%
  ggplot(aes(alpha_seq, rmse)) + 
  geom_point()

#What value of 𝛼 gives the minimum RMSE?
alpha_seq[which.min(dat$rmse)]

#Q7

#Rank the schools based on the average obtained with the best 𝛼.
#Note that no small school is incorrectly included.

alpha <- alpha_seq[which.min(dat$rmse)]
alpha

comp_reg_score <- function(school_scores) {
  b_i <- sum(school_scores - overall_score_mean)/(length(school_scores) + alpha)
  overall_score_mean + b_i
}

schools <- schools %>% mutate(reg_score = sapply(scores, comp_reg_score))

top_10_reg <- schools %>%
  arrange(desc(reg_score)) %>% 
  select(id, size, quality, score, reg_score) %>%
  slice(1:10)
top_10_reg

#What is the ID of the top school now?
str_extract(top_10_reg$id[1], "\\d+")

#What is the regularized average score of the 10th school now?
top_10_reg$reg_score[10]

#Q8:

#A common mistake made when using regularization is shrinking values
#towards 0 that are not centered around 0. For example, if we don't
#subtract the overall average before shrinking, we actually obtain a
#very similar result. Confirm this by re-running the code from the
#exercise in Q6 but without removing the overall mean.

alpha_seq <- 10:250

#By setting the mean to zero we assume that the values are centered arround zero
overall_score_mean <- 0

schools <- schools %>% select(-d_i, -n_i)
schools <- schools %>% select(-d_i1, -n_i1)
schools <- schools %>% select(-d_i2, -n_i2)
schools

delta_values_score <- function(school_scores) {
  d_i = sum(school_scores-overall_score_mean)
  n_i <- length(school_scores)
  data.frame(d_i=d_i, n_i=n_i)
}

dat <- lapply(scores, delta_values_score)
schools <- bind_cols(schools, bind_rows(dat))

compute_rmse <-function(alpha) {
  predicted_ratings <- schools %>% 
    mutate(b_i = d_i/(n_i + alpha)) %>%
    mutate(pred = overall_score_mean + b_i) %>%
    pull(pred)
  RMSE(schools$quality, predicted_ratings)
}

dat <- data.frame(alpha_seq = alpha_seq, rmse = sapply(alpha_seq, compute_rmse))

dat %>%
  ggplot(aes(alpha_seq, rmse)) + 
  geom_point()

#What value of 𝛼 gives the minimum RMSE her?
alpha_seq[which.min(dat$rmse)]




















