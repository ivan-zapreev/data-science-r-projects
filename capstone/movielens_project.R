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

#--------------------------------------------------------------------
# Define some tunable parameters
#--------------------------------------------------------------------

#Define the validation versus edx set ratio
VALIDATION_TO_EDX_SET_RATIO <- 0.1
#Define the test versus train set ratio
TEST_TO_TRAIN_SET_RATIO <- 0.2
#Define a sequene of lambdas for regularization
REGULARIZATION_LAMBDAS <- seq(0, 10, 0.25)

#--------------------------------------------------------------------
# Define some constant parameters
#--------------------------------------------------------------------

MOVIELENS_DATA_SET_NAME <- "MovieLens 10M dataset"
MOVIELENS_DATA_SET_SITE_URL <- "https://grouplens.org/datasets/movielens/10m/"
MOVIELENS_DATA_SET_FILE_URL <- "http://files.grouplens.org/datasets/movielens/ml-10m.zip"
RATINGS_DAT_FILE_NAME <- "ml-10M100K/ratings.dat"
MOVIES_DAT_FILE_NAME <- "ml-10M100K/movies.dat"
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
  cat("Splitting data with", (1-VALIDATION_TO_EDX_SET_RATIO)*100, 
      "% for the edx (train) set, and", VALIDATION_TO_EDX_SET_RATIO*100,
      "% for the validation (test) set\n")
  test_index <- createDataPartition(y = movielens$rating, times = 1, 
                                    p = VALIDATION_TO_EDX_SET_RATIO, list = FALSE)
  
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
    MOVIELENS_DATA_SET_NAME = MOVIELENS_DATA_SET_NAME,
    MOVIELENS_DATA_SET_SITE_URL = MOVIELENS_DATA_SET_SITE_URL,
    MOVIELENS_DATA_SET_FILE_URL = MOVIELENS_DATA_SET_FILE_URL,
    VALIDATION_TO_EDX_SET_RATIO = VALIDATION_TO_EDX_SET_RATIO,
    TEST_TO_TRAIN_SET_RATIO = TEST_TO_TRAIN_SET_RATIO,
    edx_set_info = data.frame(
      num_observations = nrow(movielens_data$edx),
      num_movies = length(unique(movielens_data$edx$movieId)),
      num_users = length(unique(movielens_data$edx$userId))
    ),
    validation_set_info = data.frame(
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
RMSE <- function(true_ratings, predicted_ratings) {
  return(sqrt(mean((true_ratings - predicted_ratings)^2)))
}

#--------------------------------------------------------------------
# This function trains the prediction model for the given training set
# and the regularization lambda coefficient. It accepts the arguments:
#    train_set - the training set
#    lambda - the model regularization parameter
# The model is based on  movie mean rating, movie effects accounted for
# by the mean movie rating, and the user effects accounted for by the
# mean user rating. In order to penalize for extreme average ratings
# caused by the movies  that were not rated much or user that were not
# rating much we use regulariation with the common lambda parameter for
# both penalties.
#
# For a movie m and the user u, the prediction model looks like:
#    pred = mu_hat + b_m + b_u
# where:
#    mu_hat - is the estimated average movie rating
#    b_m - the penalized via regularization with parameter lambda movie effect
#    b_u - the penalized via regularization with parameter lambda user effect
#
# The result of the function is a list with:
#     mu_hat - the mean movie rating
#     b_m - the data frame with the movieIds and the corresponding b_m values
#     b_u - the data frame with the userIds and the corresponding b_u values
#--------------------------------------------------------------------
prepare_model <- function(train_set, lambda) {
  #Copmute the overal movie average
  mu_hat <- mean(train_set$rating)
  
  cat("The mean movie rating for lambda", lambda, "is", mu_hat, "\n")
  
  #Take the penalized movie effects into account:
  b_ms <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu_hat) /(n() + lambda)) %>%
    select(movieId, b_m)
  cat("The first 10 b_m values are:", b_ms$b_m[1:10], "\n")
  
  #Take the penalized movie effects into account:
  b_us <- train_set %>% 
    left_join(b_ms, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu_hat)/(n() + lambda)) %>%
    select(userId, b_u)
  cat("The first 10 b_u values are:", b_us$b_u[1:10], "\n")
  
  #Return the result
  return(list(mu_hat = mu_hat, b_ms = b_ms, b_us = b_us))
}

#--------------------------------------------------------------------
# This function is responsible for computing the RMSE score of a given
# model on a given test set. The function arguments are:
#    model - the prediction model as returned by the prepare_model function
#    data_set - a test set defined by a data frame containing movieId and
#               userId pairs along with the true rating values
# The function first computes the ratings prediction for the test model
# and then computes and returns the RMSE score, based on the actal scores.
#--------------------------------------------------------------------
compute_model_rmse <- function(model, data_set) {
  cat("Predicting ratings for the given model and data set\n")
  
  #Make the prediction according to the model
  raw_pred_ratings <- 
    data_set %>% 
    left_join(model$b_ms, by = "movieId") %>%
    left_join(model$b_us, by = "userId") %>%
    mutate(pred = model$mu_hat + b_m + b_u) %>%
    pull(pred)

  cat("The 1:10 predicted ratings:", raw_pred_ratings[1:10],
      "N/A ratings count =", sum(is.na(raw_pred_ratings)), "\n")
  
  cat("The 1:10 true ratings:", data_set$rating[1:10],
      "N/A ratings count =", sum(is.na(data_set$rating)), "\n")
  
  #Compute and return the RMSE score
  return(RMSE(data_set$rating, raw_pred_ratings))
}

#--------------------------------------------------------------------
# This function trains the rating prediction model on the provided
#    data_set - the set to train the model on
# The training is done using the penalized regulariation with the
# lambda parameters in the range defined by REGULARIZATION_LAMBDAS.
# The parameter selection is based on the test set that consists of
# TEST_TO_TRAIN_SET_RATIO percent of the provied data set.
# The result of the function is the trained model storing:
#    trained_model - as provided by the prepare_model function
#                    with the optimal lambda value
#    lambdas - the range of lambdas that were considered
#    rmses - the corresponding rmses computed for the
#            lambdas on the testing set
#--------------------------------------------------------------------
train_model <- function(data_set) {
  cat("Splitting the data set into the testing and training set with the",
      TEST_TO_TRAIN_SET_RATIO, "ratio\n")
  #Split the data set into a training and testing parts
  #The testing set will be about 10% of original data set
  test_index <- createDataPartition(y = data_set$rating, times = 1, 
                                    p = TEST_TO_TRAIN_SET_RATIO, list = FALSE)
  
  #Make the training data to be the 90% of the data set
  train_set <- data_set[-test_index,]
  #Make the testing data to be the 10% of the data set
  temp_set <- data_set[test_index,]
  
  #Make sure we don’t include users and movies in the test set
  test_set <- temp_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  #Move the movies that did not make it into 
  #the test set back into the training set
  removed <- anti_join(temp_set, test_set)
  train_set <- rbind(train_set, removed)
  
  cat("Training set size is", nrow(train_set), "testing set size is", nrow(test_set), "\n")

  cat("Tuning the model with lambda parameters from the range:", REGULARIZATION_LAMBDAS, "\n")
  
  #Tune for the different values of the lambda parameter
  rmses <- sapply(REGULARIZATION_LAMBDAS, function(lambda){
    cat("Training the model with lambda = ", lambda, "\n")
    
    #Make the model for the given lambda, on the training set
    lambda_model <- prepare_model(train_set, lambda)
    
    cat("Computing the RMSE score for the model\n")
    #Compute the RSME score, on the test set
    rmse <- compute_model_rmse(lambda_model, test_set)
    cat("The RMSE score for lambda = ", lambda, " is", rmse,"\n")
    
    return(rmse)
  })

  #Obtain the optimal value of lambda
  opt_lambda <- REGULARIZATION_LAMBDAS[which.min(rmses)]
  
  #Re-create the model on the complete set with the optimal lambda
  trained_model <- prepare_model(data_set, opt_lambda)
  
  cat("For lambdas =", REGULARIZATION_LAMBDAS, "got rmse scores =", rmses, "\n")
  
  #Update the trained model with
  training_data = tibble(lambdas = REGULARIZATION_LAMBDAS, rmses = rmses)
  trained_model <- append(trained_model, training_data)

  return(trained_model)
}

#--------------------------------------------------------------------
# 
#--------------------------------------------------------------------
evaluate_model <- function(movielens_model, data_set, movielens_report) {
  cat("Compute the RMSE for the final model on the validation set")
  
  #Compute the RMSE for the model and the data set
  rmse <- compute_model_rmse(movielens_model, data_set)
  
  cat("The computed RMSE for the final model on the validation set is ", rmse, "\n")
  
  #Extend the report with the model and the RMSE score
  movielens_results <- tibble(movielens_model = movielens_model)
  movielens_report <- append(movielens_report, movielens_results)
  movielens_report <- append(movielens_report, list(validation_set_rmse = rmse))

  return(movielens_report)
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

#03 - Incrementally build and train the model
movielens_model <- train_model(movielens_data$edx)

#04 - Evaluate the model on the validation set and compute the RMSE
movielens_report <- evaluate_model(movielens_model,
                                   movielens_data$validation,
                                   movielens_report)

#05 - Store the report into the file to be used from the movielens_report.Rmd
store_report_data(movielens_report)

