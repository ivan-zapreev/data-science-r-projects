#You will use the following code to generate your datasets.
#Develop your algorithm using the edx set. For a final test
#of your algorithm, predict movie ratings in the validation
#set as if they were unknown. RMSE will be used to evaluate
#how close your predictions are to the true values in the
#validation set.

################################
# Create edx set, validation set
################################

library(dslabs)
library(dplyr)
library(tidyr)
library(stringr)

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#----------------------------------------------------------------
#Quiz: MovieLens Dataset

#Q1:

#How many rows and columns are there in the edx dataset?

class(edx)
str(edx)

#Number of rows:

nrow(edx)

#Number of columns:

ncol(edx)

#Q2:

#How many zeros were given as ratings in the edx dataset?

sum(edx$rating == 0)

sum(is.na(edx$rating))

#How many threes were given as ratings in the edx dataset?

sum(edx$rating == 3)

#Q3:

#How many different movies are in the edx dataset?

edx$movieId %>% unique() %>% length()

#Q4:

#How many different users are in the edx dataset?

edx$userId %>% unique() %>% length()

#Q5:

#How many movie ratings are in each of the following genres in the edx dataset?

sum(str_detect(edx$genres,"Drama"))
sum(str_detect(edx$genres,"Comedy"))
sum(str_detect(edx$genres,"Thriller"))
sum(str_detect(edx$genres,"Romance"))

#The following code shall also work but separating rows just takes way to long even with pre-filtering
# edx_sep <- edx %>% 
#   filter(str_detect(genres,"Drama|Comedy|Thriller|Romance")) %>%
#   separate_rows(genres, sep = "|")
# 
# edx_sep %>%
#   filter(genres %in% c("Drama", "Comedy", "Thriller", "Romance")) %>%
#   group_by(genres) %>%
#   summarize(cnt = n()) %>% arrange(desc(cnt))

#Q6:

#Which movie has the greatest number of ratings?

movies <- c("Forrest Gump", 
            "Jurassic Park", 
            "Pulp Fiction", 
            "Shawshank Redemption, The", 
            "Speed 2: Cruise Control")

count_ratings <- function(movieTitle) {
  pattern <- paste(paste("^", movieTitle, sep=""), "\\s\\(\\d{4}+\\)$", sep="")
  index <- str_detect(edx$title, pattern)
  foundTitles <- edx$title[index] %>% unique()
  cnt <- length(foundTitles)
  if(cnt == 0) {
    cat("ERROR: The movie with the title: '", movieTitle, 
        "' with pattern: '", pattern,"' was not found!\n")
  }else{
    if(cnt > 1) {
      cat("ERROR: The movie with the title: '", movieTitle, 
          "' is not a single movie!\n")
      cat("Found: ", foundTitles, "\n")
    }
  }
  sum(index)
}

rating_cnts <- sapply(movies, count_ratings)
rating_cnts

movies[which.max(rating_cnts)]

#Q7:

#What are the five most given ratings in order from most to least?

edx %>% 
  mutate(rating = factor(rating)) %>%
  group_by(rating) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt)) %>% 
  slice(1:5) %>% pull(rating)

#Q8:

#True or False: In general, half star ratings are less common than whole
#star ratings (e.g., there are fewer ratings of 3.5 than there are ratings
#of 3 or 4, etc.).

mean(edx$rating == round(edx$rating))












