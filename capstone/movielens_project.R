####################################################################
# Section to define and load needed packages
####################################################################

if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

####################################################################
# Section to define global variables
####################################################################

MOVIELENS_DATA_SET_NAME <- "MovieLens 10M dataset"
MOVIELENS_DATA_SET_SITE_URL <- "https://grouplens.org/datasets/movielens/10m/"
MOVIELENS_DATA_SET_FILE_URL <- "http://files.grouplens.org/datasets/movielens/ml-10m.zip"
RATINGS_DAT_FILE_NAME <- "ml-10M100K/ratings.dat"
MOVIES_DAT_FILE_NAME <- "ml-10M100K/movies.dat"
VALIDATION_SET_PROPORTION <- 0.1
MOVIELENS_DATA_FILE_NAME <- "movielens_data.rda"
MOVIELENS_REPORT_DATA_FILE_NAME <- "movielens_report.rda"

####################################################################
# Section to define helper functions
####################################################################

#--------------------------------------------------------------------
# This is a helper function which allows to remove the objects from the
# environment in case they are present. This function is obtained from
# the internet: 
#     https://stackoverflow.com/questions/7172568/write-a-function-to-remove-object-if-it-exists
#--------------------------------------------------------------------
ifrm <- function(obj, env = globalenv()) {
  obj <- deparse(substitute(obj))
  if(exists(obj, envir = env)) {
    rm(list = obj, envir = env)
  }
}

#--------------------------------------------------------------------
# This function derived from the project initialization instructions
# to create edx set, validation set. It returns a list with the
# two corresponding entries:
#   edx - storing the edx set, meant for training
#   validation - storing the validation set, meant for final RMSE evaluation
#
# Note: Running this function could take a couple of minutes
#--------------------------------------------------------------------
create_movielens_sets <- function() {
  # Download the MovieLens 10M dataset, if not already:
  # https://grouplens.org/datasets/movielens/10m/
  # http://files.grouplens.org/datasets/movielens/ml-10m.zip
  
  if(!file.exists(RATINGS_DAT_FILE_NAME) || !file.exists(MOVIES_DAT_FILE_NAME)){
    dl <- tempfile()
    cat("Downloading data file:", MOVIELENS_DATA_SET_FILE_URL,
        "into a temporary file:", dl, "\n")
    download.file(MOVIELENS_DATA_SET_FILE_URL, dl)
    
    cat("Unzipping the files:", RATINGS_DAT_FILE_NAME, "and", MOVIES_DAT_FILE_NAME, "\n")
    unzip(dl, RATINGS_DAT_FILE_NAME)
    unzip(dl, MOVIES_DAT_FILE_NAME)
  } else {
    cat("The", RATINGS_DAT_FILE_NAME, "and", MOVIES_DAT_FILE_NAME, "are locally present\n")
    cat("No need to re-download them from", MOVIELENS_DATA_SET_FILE_URL, "\n")
  }
  
  #Read the movie ratings from the downloaded file
  cat("Reading data from", RATINGS_DAT_FILE_NAME, "\n")
  ratings <- fread(text = gsub("::", "\t", readLines(RATINGS_DAT_FILE_NAME)),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  #Read the movies from the downloaded file
  cat("Reading data from", MOVIES_DAT_FILE_NAME, "\n")
  movies <- str_split_fixed(readLines(MOVIES_DAT_FILE_NAME), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  
  #Convert the movies into a data frame
  cat("Pre-processing and joining data from", MOVIES_DAT_FILE_NAME, " and ", RATINGS_DAT_FILE_NAME, "\n")
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  #Join the movies with the corresponding ratings data
  movielens <- left_join(ratings, movies, by = "movieId")

  #Before splitting the data set into the training and testing parts set the random seed  
  set.seed(1, sample.kind="Rounding")
  # if using R 3.5 or earlier, use `set.seed(1)` instead

  #Validation set will be about 10% of MovieLens data
  cat("Splitting data with", (1-VALIDATION_SET_PROPORTION)*100, 
      "% for the edx (train) set, and", VALIDATION_SET_PROPORTION*100,
      "% for the validation (test) set\n")
  test_index <- createDataPartition(y = movielens$rating, times = 1, 
                                    p = VALIDATION_SET_PROPORTION, list = FALSE)
  
  #Make the edx (training) data to be the 90% of the data set
  edx <- movielens[-test_index,]
  #Make the validation (testing) data to be the 10% of the data set
  temp <- movielens[test_index,]
  
  #Make sure userId and movieId in validation set are also in edx set.
  cat("Make sure that the validation set only contains users and movies from the edx set\n");
  validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  cat("Move the excluded events back to the edx set\n");
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)
  
  #Removed the non-needed temporary data
  ifrm(dl)
  rm(ratings, movies, test_index, temp, movielens, removed)
  
  cat("Finished preparing the data, creating the final data frame\n");
  return(list(edx=edx, validation=validation))
}

#--------------------------------------------------------------------
# This function is reponsible for getting the movielens train and test data
# If the data is present in the local MOVIELENS_DATA_FILE_NAME file then
# the data is simplu loaded from there, if not then it is re-generated and
# is then also stored into the MOVIELENS_DATA_FILE_NAME for re-use.
# It returns a list with the two corresponding entries:
#   edx - storing the edx set, meant for training
#   validation - storing the validation set, meant for final RMSE evaluation
#
# Note: Running this function could take a couple of minutes
#--------------------------------------------------------------------
get_movielens_data <- function() {
  #Check if the file exists then load the data from the file, 
  #otherwise re-create the data and also save it to the file
  cat("Checkin if the Movielens data is stored in:", MOVIELENS_DATA_FILE_NAME,"\n")
  if(!file.exists(MOVIELENS_DATA_FILE_NAME)){
    cat("The data is not stored in", MOVIELENS_DATA_FILE_NAME, "start re-generating\n")
    movielens_data <- create_movielens_sets()
    
    cat("The data is re-generated, storing it into:", MOVIELENS_DATA_FILE_NAME, "\n")
    save(movielens_data, file = MOVIELENS_DATA_FILE_NAME)
  } else {
    cat("The data is stored in", MOVIELENS_DATA_FILE_NAME, "and will be loaded\n")
    load(MOVIELENS_DATA_FILE_NAME)
  }
  return(movielens_data)
}

#--------------------------------------------------------------------
# This function creates the initial report to be filled
#--------------------------------------------------------------------
init_report_data <- function(movielens_data) {
  return(list(
    data_set_name = MOVIELENS_DATA_SET_NAME,
    data_set_site_url = MOVIELENS_DATA_SET_SITE_URL,
    data_set_file_url = MOVIELENS_DATA_SET_FILE_URL,
    valid_set_prop = VALIDATION_SET_PROPORTION,
    edx = data.frame(
      num_observations = nrow(movielens_data$edx),
      num_movies = length(unique(movielens_data$edx$movieId)),
      num_users = length(unique(movielens_data$edx$userId))
    ),
    validation = data.frame(
      num_observations = nrow(movielens_data$validation),
      num_movies = length(unique(movielens_data$validation$movieId)),
      num_users = length(unique(movielens_data$v$userId))
    ) 
  ))
}

#--------------------------------------------------------------------
# This function stores the final report into the 
#     MOVIELENS_REPORT_DATA_FILE_NAME
# file to be used later from the movielens_report.Rmd script
#--------------------------------------------------------------------
store_report_data <- function(movielens_report) {
  save(movielens_report, file = MOVIELENS_REPORT_DATA_FILE_NAME)
}

#--------------------------------------------------------------------
# This function computes the Root Mean Square Error (RMSE) that is
# the standard deviation of the residuals (prediction errors). 
# The two arguments are the predicted and the actual values to
# be used in RMSE computations. The order of the arguments is not
# imprortant.
#--------------------------------------------------------------------
RMSE <- function(x, y) {
  return(sqrt(mean((x - y)^2)))
}

####################################################################
# Section to define the main part of the cript that will be
# calling the utility functions from above and performing the
# main sequence
####################################################################

#01 - Load the movielens data
movielens_data <- get_movielens_data()

#02 - Initialize the report data frame thay will be storing all
#     the required information for the report to be generated
movielens_report <- init_report_data(movielens_data)

#00 - Evaluate the model on the validation set and compute the RMSE

#00 - Store the report into the file to be used from the movielens_report.Rmd
store_report_data(movielens_report)

