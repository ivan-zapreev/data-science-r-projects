####################################################################
# 
#
####################################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

#--------------------------------------------------------------------
# The function derived from the project initialization instructions
# to create edx set, validation set. It returns a data frame with the
# two corresponding entries:
#   edx - storing the edx set, meant for training
#   validation - storing the validation set, meant for final RMSE evaluation
#
# Note: Running this function could take a couple of minutes
#--------------------------------------------------------------------
create_exd_and_vaidation_sets <- function() {
  # Download the MovieLens 10M dataset:
  # https://grouplens.org/datasets/movielens/10m/
  # http://files.grouplens.org/datasets/movielens/ml-10m.zip
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  #Read the movie ratings from the downloaded file
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  #Read the movies from the downloaded file
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  
  #Convert the movies into a data frame
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  #Join the movies with the corresponding ratings data
  movielens <- left_join(ratings, movies, by = "movieId")

  #Before splitting the data set into the training and testing parts set the random seed  
  set.seed(1, sample.kind="Rounding")
  # if using R 3.5 or earlier, use `set.seed(1)` instead

  #Validation set will be about 10% of MovieLens data
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  
  #Make the edx (training) data to be the 90% of the data set
  edx <- movielens[-test_index,]
  #Make the validation (testing) data to be the 10% of the data set
  temp <- movielens[test_index,]
  
  #Make sure userId and movieId in validation set are also in edx set.
  validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)
  
  #Removed the non-needed temporary data
  rm(dl, ratings, movies, test_index, temp, movielens, removed)
  
  data.frame(edx=edx, validation=validation)
}

#----------------------------------------------------------------
# This is the main part of the cript that will be
#calling the utility functions from above

#Initialize the data
dat <- create_exd_and_vaidation_sets()

#
