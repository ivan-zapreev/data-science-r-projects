####################################################################
# Section to define and load needed packages
####################################################################
options(digits=10)

if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
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

#Define the validation versus modeling set ratio
VALIDATION_TO_MODELING_SET_RATIO <- 0.1
#Define the test versus train set ratio
TEST_TO_TRAIN_SET_RATIO <- 0.2
#Define a sequene of lambdas for regularization
REGULARIZATION_LAMBDAS <- seq(0, 10, 0.25)

#--------------------------------------------------------------------
# Define some constant parameters
#--------------------------------------------------------------------

AROG_AROG_DATA_FILE_NAME <- "\'Apartment rental offers in Germany\' dataset"
AROG_DATA_SET_SITE_URL <- "https://www.kaggle.com/corrieaar/apartment-rental-offers-in-germany"
AROG_DATA_DIR_NAME <- "data"
AROG_ZIP_FILE_NAME <- "apartment-rental-offers-in-germany.zip"
AROG_DATA_SET_FILE_URL <-  paste("https://raw.githubusercontent.com/ivan-zapreev/data-science-r-projects/master/capstone/",
                              paste(AROG_DATA_DIR_NAME,
                                    paste("/",AROG_ZIP_FILE_NAME, sep=""),
                                    sep=""),
                              sep="")
AROG_DATA_DIR_PATH <- file.path(".", AROG_DATA_DIR_NAME)
AROG_DATA_FILE_REL_NAME <- file.path(AROG_DATA_DIR_PATH, AROG_ZIP_FILE_NAME)
AROG_CSV_FILE_NAME <- "immo_data.csv"
AROG_CSV_FILE_REL_NAME <- file.path(AROG_DATA_DIR_PATH, AROG_CSV_FILE_NAME)
AROG_DATA_FILE_NAME <- "apartment_rental_data.rda"
AROG_REPORT_FILE_NAME <- "apartment_rental_report.rda"
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
# Creates the AROG_DATA_DIR_PATH folder if it is missing
#--------------------------------------------------------------------
ensure_arog_data_dir <- function() {
  if(!dir.exists(AROG_DATA_DIR_PATH)) {
    cat("The", AROG_DATA_DIR_PATH, "folder is missing, creating\n")
    dir.create(AROG_DATA_DIR_PATH)
  } else {
    cat("The", AROG_DATA_DIR_PATH, "folder is present\n")
  }
}

#--------------------------------------------------------------------
# Downloads the AROG_DATA_FILE_REL_NAME file from
# AROG_DATA_SET_FILE_URL if it is missing.
# Assumes that the AROG_DATA_DIR_PATH folder is present.
#--------------------------------------------------------------------
ensure_arog_zip_file <- function() {
  if(!file.exists(AROG_DATA_FILE_REL_NAME)){
    cat("The", AROG_DATA_FILE_REL_NAME, "file is missing, downloading\n")
    download.file(AROG_DATA_SET_FILE_URL, AROG_DATA_FILE_REL_NAME)
  } else {
    cat("The", AROG_DATA_FILE_REL_NAME, "file is present\n")
  }
}

#--------------------------------------------------------------------
# Assumes that the AROG_DATA_DIR_PATH folder is present.
# Assumes that the AROG_DATA_FILE_REL_NAME file is present.
#--------------------------------------------------------------------
ensure_arog_csv_file <- function() {
  if(!file.exists(AROG_CSV_FILE_REL_NAME)){
    cat("The", AROG_CSV_FILE_REL_NAME, "file is missing, extracting\n")
    unzip(AROG_DATA_FILE_REL_NAME, AROG_CSV_FILE_NAME, exdir = AROG_DATA_DIR_PATH)
  } else {
    cat("The", AROG_CSV_FILE_REL_NAME, "file is present\n")
  }
}

#--------------------------------------------------------------------
# Check ensures that the AROG_CSV_FILE_REL_NAME file is present 
#--------------------------------------------------------------------
ensure_arog_data <- function() {
  ensure_arog_data_dir()
  ensure_arog_zip_file()
  ensure_arog_csv_file()
}

#--------------------------------------------------------------------
# This function is responsible for downloading and pre-processing the 
# data set. This includes data cleaning and splitting.
#
# It returns a list with the following entries:
#   training set - to train the models on
#   testing set - to be used for regularization parameter tuning
#   validation set - to be used for the final evaluation of the trained models
#
# Note: Running this function could take a couple of minutes
#--------------------------------------------------------------------
create_arog_data <- function() {
  #Ensure that the AROG data is present
  ensure_arog_data()

  #Read the raw AROG data from the csv file
  cat("Reading data from", AROG_CSV_FILE_REL_NAME, "\n")
  raw_arog_data <- read_csv(AROG_CSV_FILE_REL_NAME)
  raw_arog_data <- as_tibble(raw_arog_data)

  #Select the columns of interest
  
  #Clean and pre-process the data
  
  #Split into modeling and validation sets
  
  #Split the modeling set into training and testing sets
}

#--------------------------------------------------------------------
# This function is reponsible for getting the apartment rental training,
# testing and valdiation data.
#
# It returns a list with the following entries:
#   training set - to train the models on
#   testing set - to be used for regularization parameter tuning
#   validation set - to be used for the final evaluation of the trained models
#
# Note: Running this function could take a couple of minutes
#--------------------------------------------------------------------
get_arog_data <- function() {
  #Check if the file exists then load the data from the file, 
  #otherwise re-create the data and also save it to the file
  cat("Checkin if the data is stored in:", AROG_DATA_FILE_NAME,"\n")
  if(!file.exists(AROG_DATA_FILE_NAME)){
    cat("The data is not stored in", AROG_DATA_FILE_NAME, "start re-generating\n")
    arog_data <- create_arog_data()
    
    cat("The data is re-generated, storing it into:", AROG_DATA_FILE_NAME, "\n")
    save(arog_data, file = AROG_DATA_FILE_NAME)
  } else {
    cat("The data is stored in", AROG_DATA_FILE_NAME, "and will be loaded\n")
    load(AROG_DATA_FILE_NAME)
  }
  return(arog_data)
}

####################################################################
# Section to define the main part of the cript that will be
# calling the utility functions from above and performing the
# main sequence
####################################################################

#01 - Dowbload/Pre-process/Load the dataset data
arog_data <- get_arog_data()

#02 - Initialize the report data frame thay will be storing all
#     the required information for the report to be generated
#arog_report <- init_report_data(arog_data)


