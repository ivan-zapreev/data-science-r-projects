####################################################################
# Section to define and load needed packages
####################################################################
options(digits=10)

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
REGULARIZATION_LAMBDAS <- seq(0, 7, 0.25)

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
FIRST_ELEM_SEQ <- 1:5 #This sequence is used for debug printing purposes only

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
# This function splits the data set into the training and testing parts:
#    data_set - the dat set to be split
#    ratio - the ratio to be used, the % to be used for testing
# Returns a list with the following attributes:
#    tratin_set - the training set
#    test_set - the testing set
# The function makes sure that the test set does not contain movies 
# and users not present in the training set
#--------------------------------------------------------------------
split_train_test_sets <- function(data_set, ratio) {
  #Before splitting the data set into the training and testing parts set the random seed  
  set.seed(1, sample.kind="Rounding")
  # if using R 3.5 or earlier, use `set.seed(1)` instead
  
  cat("Splitting the data set into the testing and training set with the", ratio, "ratio\n")
  #Split the data set into a training and testing parts
  #The testing set will be about 10% of original data set
  test_index <- createDataPartition(y = data_set$rating, times = 1, 
                                    p = ratio, list = FALSE)
  
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
  
  rm(test_index, temp_set, removed)
  
  return(list(train_set = train_set, test_set = test_set))
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
  
  #Extend the data set with the week
  movielens <- movielens %>% 
    mutate(weekId = round_date(as_datetime(timestamp), "week"))
  
  #Before splitting the data set into the training and testing parts set the random seed  
  set.seed(1, sample.kind="Rounding")
  # if using R 3.5 or earlier, use `set.seed(1)` instead
  
  #Split the sets into training and testing
  split_sets <- split_train_test_sets(movielens, VALIDATION_TO_EDX_SET_RATIO)

  #Removed the non-needed temporary data
  ifrm(dl)
  rm(ratings, movies, movielens)
  
  cat("Finished preparing the data, creating the final data frame\n");
  return(list(edx=split_sets$train_set, validation=split_sets$test_set))
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
# This is the initialization function that allows to initialize the 
# model structure with the training set average rating and also, if
# requested, the LOESS model fit for the average movie rating per week
# The function arguments are:
#    train_set - the training set data
#    is_time - the timing effects flag, if true then the LOESS fit
#              for the average movie rating per week is computed
# The function returns a list with two attribuites
# one of which is optional:
#    mu_hat - the average movie rating
#    b_t_fit - the fitted LOES model for the average 
#              movie rating per weeek
#--------------------------------------------------------------------
init_model <- function(train_set, is_time) {
  #Copmute the overal movie average
  mu_hat <- mean(train_set$rating)
  
  #Initialize the model with the mean rating
  model <- list(mu_hat = mu_hat)
  
  #Check if we need to take the timing effects into account
  if(is_time) {
    #Compute the smooth Local Regression (LOESS)
    #fit for the average movie rating per week
    amrpw_fit <- train_set %>% 
      group_by(weekId) %>%
      summarise(avg_rating = mean(rating)) %>%
      mutate(weekId = as.numeric(weekId)) %>%
      loess(avg_rating ~ weekId, degree = 2, data = .)
    
    #Compute the movie rating timing effects per week:
    b_ts <- train_set %>% 
      mutate(amrpw = predict(amrpw_fit, as.numeric(weekId))) %>%
      group_by(weekId) %>%
      summarize(b_t = min(amrpw) - mu_hat) #Movies with equal weekId have equal amrpw values
    
    cat("The first b_t values are:", b_ts$b_t[FIRST_ELEM_SEQ],
        ", N/A count:", sum(is.na(b_ts$b_t)), "\n")
    
    #Extend the model with the LOESS fit model
    model <- append(model, list(b_ts = b_ts))
  }
  
  #Return the result
  return(model)
}

#--------------------------------------------------------------------
# This helper function accepts a model list and checks
# if it has the "b_ts" key. If it does then it returns 
# TRUE and otherwise FALSE.
#--------------------------------------------------------------------
is_timing_effects <- function(model) {
  return(sum(str_detect(names(model),"b_ts")) != 0)
}

#--------------------------------------------------------------------
# This function trains the prediction model for the given training set
# and the regularization lambda coefficient. It accepts the arguments:
#    base_model - the basic model, initialized with:
#        mu_hat - the average movie rating
#        b_t_fit - the fitted model of the average movie rating per week
#                  this parameter is optional and is only present if the
#                  timing effects are to be taken into accoubt
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
# without timing effects taken into account and
#    pred = mu_hat + b_t + b_m + b_u
# with the timing effects
# where:
#    mu_hat - is the estimated average movie rating
#    b_t - the penalized via regularization with parameter lambda timing effect
#    b_m - the penalized via regularization with parameter lambda movie effect
#    b_u - the penalized via regularization with parameter lambda user effect
#
# The result of the function is a list with:
#     mu_hat - the mean movie rating
#     lambda - the regularization parameter value used
#     b_t - the data frame with the weekIds and the corresponding b_t values
#     b_m - the data frame with the movieIds and the corresponding b_m values
#     b_u - the data frame with the userIds and the corresponding b_u values
#--------------------------------------------------------------------
prepare_model <- function(base_model, train_set, lambda) {
  #Copmute the overal movie average
  mu_hat <- base_model$mu_hat
  
  cat("The mean movie rating for lambda", lambda, "is", mu_hat, "\n")
  
  #Check if the timing effects are needed
  is_time = is_timing_effects(base_model)
  if(is_time) {
    #Compute the movie rating effects:
    b_ms <- train_set %>% 
      left_join(base_model$b_ts, by = "weekId") %>%
      group_by(movieId) %>%
      summarize(b_m = sum(rating - b_t - mu_hat) / (n() + lambda)) %>%
      select(movieId, b_m)
    
    #Compute the user rating effects:
    b_us <- train_set %>% 
      left_join(b_ms, by = "movieId") %>%
      left_join(base_model$b_ts, by = "weekId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_t - b_m - mu_hat) / (n() + lambda)) %>%
      select(userId, b_u)
  } else {
    #Compute the movie rating effects:
    b_ms <- train_set %>% 
      group_by(movieId) %>%
      summarize(b_m = sum(rating - mu_hat) / (n() + lambda)) %>%
      select(movieId, b_m)

    #Compute the user rating effects:
    b_us <- train_set %>% 
      left_join(b_ms, by = "movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_m - mu_hat) / (n() + lambda)) %>%
      select(userId, b_u)
  }

  #Return the result
  ext_model <- list(lambda = lambda, 
                    b_ms = b_ms,
                    b_us = b_us)

  cat("The first b_m values are:", b_ms$b_m[FIRST_ELEM_SEQ], "\n")
  cat("The first b_u values are:", b_us$b_u[FIRST_ELEM_SEQ], "\n")
  
  return(append(base_model, ext_model))
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

  #Check if the timing effects are needed
  is_time = is_timing_effects(model)
  if(is_time) {
    #Make the prediction according to the model
    raw_pred_ratings <- 
      data_set %>% 
      left_join(model$b_ts, by = "weekId") %>%
      left_join(model$b_ms, by = "movieId") %>%
      left_join(model$b_us, by = "userId") %>%
      mutate(pred = model$mu_hat + b_t + b_m + b_u) %>%
      pull(pred)
  } else {
    #Make the prediction according to the model
    raw_pred_ratings <- 
      data_set %>%
      left_join(model$b_ms, by = "movieId") %>%
      left_join(model$b_us, by = "userId") %>%
      mutate(pred = model$mu_hat + b_m + b_u) %>%
      pull(pred)
  }

  cat("The first predicted ratings:", raw_pred_ratings[FIRST_ELEM_SEQ],
      ", N/A count =", sum(is.na(raw_pred_ratings)), "\n")
  cat("The first true ratings:", data_set$rating[FIRST_ELEM_SEQ],
      ", N/A count =", sum(is.na(data_set$rating)), "\n")
  
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
train_model <- function(data_set, is_time) {
  #Split the data set into training and testing parts
  split_sets <- split_train_test_sets(data_set, TEST_TO_TRAIN_SET_RATIO)

  cat("Tuning the model with lambda parameters from the range:", REGULARIZATION_LAMBDAS, "\n")
  
  #Initialize the base model
  base_model <- init_model(split_sets$train_set, is_time)
  
  #Tune for the different values of the lambda parameter
  rmses <- sapply(REGULARIZATION_LAMBDAS, function(lambda){
    cat("Training the model with lambda = ", lambda, "\n")
    
    #Make the model for the given lambda, on the training set
    prepared_model <- prepare_model(base_model, split_sets$train_set, lambda)
    
    cat("Computing the RMSE score for the model\n")
    #Compute the RSME score, on the test set
    rmse <- compute_model_rmse(prepared_model, split_sets$test_set)
    cat("The RMSE score for lambda = ", lambda, " is", rmse,"\n")
    
    return(rmse)
  })

  #Obtain the optimal value of lambda
  opt_lambda <- REGULARIZATION_LAMBDAS[which.min(rmses)]
  
  #Re-create the model on the complete set with the optimal lambda
  trained_model <- prepare_model(base_model, data_set, opt_lambda)
  
  cat("For lambdas =", REGULARIZATION_LAMBDAS, "got rmse scores =", rmses, "\n")
  
  #Update the trained model with
  training_data = tibble(lambdas = REGULARIZATION_LAMBDAS, rmses = rmses)
  trained_model <- append(trained_model, training_data)

  return(trained_model)
}

#--------------------------------------------------------------------
# This function is responsible for evaluation of the model on the 
# given data set and updating the report, it accepts arguments:
#    model - the trained statistical movielens model
#    data_set - the data set to evaluate the model on (validation set)
#    report - the movielens report to be extended with the results
# It retuns the update report list, extended with:
#    timing_model_res - the results list, if the Timing model was used
#    basic_mode_res - the results list, if the Basic model was used
# The results object contains the following:
#    model - the evaludated model
#    rmse - the computed RMSE score
#    is_time - the flag indicating whether the model is the Timing model
#--------------------------------------------------------------------
evaluate_model <- function(model, data_set, report) {
  cat("Compute the model's RMSE on the validation set\n")
  
  #Compute the RMSE for the model and the data set
  rmse <- compute_model_rmse(model, data_set)
  
  #Check if the model takes the timing effects into account
  is_time = is_timing_effects(model)
  
  cat("The validation set RMSE ( lambda =", model$lambda, ", timing =", is_time, ") is ", rmse, "\n")
  
  #Extend the report with the model and the RMSE score
  model_result <- list(model = model, rmse = rmse, is_time = is_time)
  if(is_time) {
    report <- append(report, list(timing_model_res = model_result))
  } else {
    report <- append(report, list(basic_model_res = model_result))
  }

  return(report)
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

#03 - Incrementally build and train the model WITHOUT the timing effects
basic_model <- train_model(movielens_data$edx, FALSE)

#04 - Evaluate the model on the validation set and compute the RMSE
movielens_report <- evaluate_model(basic_model,
                                   movielens_data$validation,
                                   movielens_report)

#05 - Incrementally build and train the model WITH the timing effects
timing_model <- train_model(movielens_data$edx, TRUE)

#06 - Evaluate the model on the validation set and compute the RMSE
movielens_report <- evaluate_model(timing_model,
                                   movielens_data$validation,
                                   movielens_report)

#05 - Store the report into the file to be used from the movielens_report.Rmd
store_report_data(movielens_report)

