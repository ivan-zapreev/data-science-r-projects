#------------------------------------------------------------
#Recommendation Systems

library(dslabs)
library(tidyverse)
library(dplyr)
data("movielens")

head(movielens)

movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable()

users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

movielens %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

library(caret)
set.seed(755,sample.kind = "Rounding")
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

#------------------------------------------------------------
#Building the Recommendation System

mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# fit <- lm(rating ~ as.factor(userId), data = movielens)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

rmse_results %>% knitr::kable()

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# lm(rating ~ as.factor(movieId) + as.factor(userId))
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
rmse_results %>% knitr::kable()

#------------------------------------------------------------
#Comprehension Check: Recommendation Systems

#The following exercises all work with the movielens data, which can be loaded using the following code:
  
library(dslabs)
data("movielens")

#Q1:

#Compute the number of ratings for each movie and then plot it against
#the year the movie came out. Use the square root transformation on the counts.

dat <- movielens %>% group_by(movieId) %>% 
  summarize(num_ratings = n(), year=min(year))

dat %>% ggplot(aes(x=year, y=num_ratings, color=movieId)) + 
  geom_point() + scale_y_sqrt()

#What year has the highest median number of ratings?

dat %>% ggplot(aes(x=factor(year), y=num_ratings)) + 
  geom_boxplot() + scale_y_sqrt()

dat %>% group_by(year) %>%
  summarize(med = median(num_ratings)) %>% 
  arrange(desc(med))
min(dat$year, na.rm=TRUE)

#Q2:

#We see that, on average, movies that came out after 1993 get more ratings.
#We also see that with newer movies, starting in 1993, the number of ratings
#decreases with year: the more recent a movie is, the less time users have
#had to rate it.

#Among movies that came out in 1993 or later, select the top 25 movies with
#the highest average number of ratings per year (n/year), and caculate the
#average rating of each of them. To calculate number of ratings per year,
#use 2018 as the end year.

dat_1993 <- movielens %>% filter(year >= "1993")
movie_names_1993 <- dat_1993 %>% 
  select(movieId, title, rating) %>%
  group_by(movieId) %>% 
  summarize(title= max(title), avg_rating=mean(rating)) #%>%
  #distinct()

num_years <- length(levels(factor(dat_1993$year)))
ratings_per_year<- dat_1993 %>% group_by(movieId) %>% 
  summarize(rpy = n()/num_years) %>%
  left_join(movie_names_1993, by="movieId")
ratings_per_year

#What is the average rating for the movie The Shawshank Redemption?

library(stringr)
options(digits = 3)
movielens %>% filter(str_detect(title,".*Shawshank.*"))
movielens %>% filter(title=="Shawshank Redemption, The") %>% summarize(mr = mean(rating))

#What is the average number of ratings per year for the movie Forrest Gump?
movielens %>% filter(title=="Forrest Gump") %>% summarize(avg_num_ratings = n()/num_years)

ratings_per_year %>% filter(title=="Forrest Gump") %>% pull(rpy)

#Q3:

#From the table constructed in Q2, we can see that the most frequently
#rated movies tend to have above average ratings. This is not surprising: 
#     more people watch popular movies.
#To confirm this, stratify the post-1993 movies by ratings per year and
#compute their average ratings. To calculate number of ratings per year,
#use 2018 as the end year. Make a plot of average rating versus ratings
#per year and show an estimate of the trend.

plot(ratings_per_year$rpy, ratings_per_year$avg_rating)

ratings_per_year %>% ggplot(aes(rpy, avg_rating)) + 
  geom_point(aes(alpha=0.5)) + geom_smooth(method = "lm")

#What type of trend do you observe?
  
#The more often a movie is rated, the higher its average rating. 

#Q4:

#Suppose you are doing a predictive analysis in which you need to 
#fill in the missing ratings with some value.

#Given your observations in the exercise in Q3, which of the
#following strategies would be most appropriate?

#Ass there are no ratings then from the plot we can assume a lower average 
#score for those movies that the oveall average, so the answer is:
#   Fill in the missing values with a lower value than the average rating across all movies. 

#Q5:

#The movielens dataset also includes a time stamp. This variable represents
#the time and date in which the rating was provided. The units are seconds
#since January 1, 1970. Create a new column date with the date.

#Which code correctly creates this new column?

library(lubridate)
movielens <- mutate(movielens, date = as_datetime(timestamp))

#Q6:

#Compute the average rating for each week and plot this average against day.
#Hint: use the round_date function before you group_by.

movielens %>% mutate(week = round_date(date, "week")) %>%
  group_by(week) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(week, avg_rating)) +
  geom_line() +
  geom_smooth()

#What type of trend do you observe?

#There is some evidence of a time effect on average rating. 

#Q7:

#Consider again the plot you generated in Q6.

#If we define 𝑑𝑢,𝑖 as the day for user's 𝑢 rating of mov
#ie 𝑖, which of the following models is most appropriate?

#𝑌𝑢,𝑖=𝜇+𝑏𝑖+𝑏𝑢+𝑓(𝑑𝑢,𝑖)+𝜀𝑢,𝑖, with 𝑓 a smooth function of 𝑑𝑢,𝑖

#Q8:

#The movielens data also has a genres column. This column includes
#every genre that applies to the movie. Some movies fall under several
#genres. Define a category as whatever combination appears in this
#column. Keep only categories with more than 1,000 ratings.
#Then compute the average and standard error for each category.
#Plot these as error bar plots.

library(dslabs)
data("movielens")

good_categories <- movielens %>% 
  mutate(category = genres) %>%
  group_by(category) %>% 
  summarise(avg=mean(rating), 
            se=sd(rating),
            tr_count = n()) %>%
  filter(tr_count > 1000) %>% 
  arrange(avg) %>%
  ungroup()

good_categories %>% slice(1:5)

good_categories %>%
  ggplot(aes(category, avg, group=1)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(x = category, 
                    ymin = avg - qnorm(0.975)*se,
                    ymax = avg + qnorm(0.975)*se))+
  theme(axis.text.x = element_text(angle = 90))

good_categories$category[which.min(good_categories$avg)] 

#Which genre has the lowest average rating?

library(tidyr)

#Enter the name of the genre exactly as reported in the plot,
#including capitalization and punctuation.

dat1 <- movielens %>% 
  filter(genres %in% good_categories$category) %>%
  separate_rows(genres, sep="\\|") %>%
  mutate(genres = factor(genres))

dat2 <- dat1 %>%
  group_by(genres) %>%
  summarise(avg=mean(rating), 
            se=sd(rating),
            num_ratings = n()) %>% arrange(avg)

dat2

dat2 %>%
  ggplot(aes(x=genres, y=avg)) +
  geom_point() +
  theme(axis.text.x = elemgeom_errorbar(aes(x = genres, 
                    ymin = avg - qnorm(0.975)*se,
                    ymax = avg + qnorm(0.975)*se))+
  ent_text(angle = 90))

#---------------

#Splitdat1 %>%
  group_by(genres) %>%
  ggplot(aes(x=genres, y=rating)) +
  geom_point() +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

 the genres into multiple rows
dat_genres <- movielens %>% 
  separate_rows(genres, sep="\\|") %>%
  mutate(genres = factor(genres)) %>%
  group_by(genres)

dat_genres

#Take the genres with more than 1000 ratings
good_genres <- dat_genres %>% 
  group_by(genres) %>%
  summarize(counts=n()) %>%
  filter(counts > 1000)

good_genres

#Filter out the ratings for the selected genres
#and compute their averages and then order
dat_genres %>% 
  filter(genres %in% good_genres$genres) %>%
  group_by(genres) %>%
  summarise(avg=mean(rating), num = n()) %>%
  arrange(avg)

#Q9:

#The plot you generated in Q8 shows strong evidence of a genre effect.
#Consider this plot as you answer the following question.

#If we define 𝑔𝑢,𝑖 as the genre for user 𝑢's rating of movie 𝑖,
#which of the following models is most appropriate?
#𝑌𝑢,𝑖=𝜇+𝑏𝑖+𝑏𝑢+𝑔𝑢,𝑖+𝜀𝑢,𝑖

#The answer is:
#𝑌𝑢,𝑖=𝜇+𝑏𝑖+𝑏𝑢+∑𝐾𝑘=1𝑥𝑘𝑢,𝑖𝛽𝑘+𝜀𝑢,𝑖, with 𝑥𝑘𝑢,𝑖=1 if 𝑔𝑢,𝑖 is genre 𝑘


