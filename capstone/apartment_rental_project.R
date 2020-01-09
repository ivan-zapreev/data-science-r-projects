####################################################################
# Section to define and load needed packages
####################################################################
options(digits=10)

#Data utility packages
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(R.utils)) install.packages("R.utils", repos = "http://cran.us.r-project.org")

#Modelling packages
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("class", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(mgcv)) install.packages("mgcv", repos = "http://cran.us.r-project.org")

#Libraries to be loaded
library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(caret)
library(dslabs)
library(data.table)
library(dplyr)
library(R.utils)

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
#The modeling method time out in seconds
TRAIN_CPU_TIME_OUT_SECONDS <- 3*60*60 #Is set to 3 CPU hours
#The number of principle components to consider
NUM_PC_TO_CONSIDER <- 2 #Is set to two which explains the 99.3% of data variability
#Setting it to 6 will explain the 99.99% of data variability
KNN_K_SEQUENCE <- seq(16, 24, 1) #The k sequence for the tuning grid of KNN

#--------------------------------------------------------------------
# Define some constant parameters
#--------------------------------------------------------------------

AROG_DATA_SET_NAME <- "\'Apartment rental offers in Germany\' dataset"
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
AROG_DATA_FILE_NAME <- "arog_data.rda"
AROG_REPORT_FILE_NAME <- "arog_report.rda"
FIRST_ELEM_SEQ <- 1:5 #This sequence is used for debug printing purposes only
MAX_NUM_FLOORS_TO_CONSIDER <- 10 #The limit on the number of floors for filtering
MAX_FLOOR_TO_CONSIDER <- 100 #The limit on the floor value for filtering

#The Min/Max numerical/integer field values based on the outliers analysis
MIN_MAX_OUTLIER_FILTERS <- list(
  "noRooms" = c(1, 20), 
  "noParkSpaces" = c(0, 200), 
  "livingSpace" = c(1, 1000), 
  "baseRent" = c(100, 30000), 
  "electricityBasePrice" = c(0, 100), 
  "heatingCosts" = c(0, 3000), 
  "serviceCharge" = c(10, 10000), 
  "totalRent" = c(10, 100000))

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
# This function allows to get a factor column and replace all of its
# N/A entries and any other entries names (defaults to c("")) in the
# provided levels_set with a new level (defaults to "unknown")
#--------------------------------------------------------------------
map_na_and_others_to_unknown <- function(data_col, levels_set = c(""),
                                         new_level_name = "unknown") {
  curr_levels <- levels(data_col)
  new_data_col <- ifelse(is.na(data_col) | (curr_levels[data_col] %in% levels_set),
                         new_level_name, curr_levels[data_col]) %>% factor()
  rm(curr_levels)
  return(new_data_col)
}

#--------------------------------------------------------------------
# Removes outlier rows for a given column from a given data set.
# The outliers are detected os those outside the limits in
# MIN_MAX_OUTLIER_FILTERS whuch has values based on the analysis
# of the boxplot.stats(.)$out data.
# Function arguments:
#    data_set - the data set to be filtered
#    data_set - the column name to be considered for outliers
# Result:
#    Outliers-free data set
# NOTE:
#    Previously we would remove all the outliers, but this
#    is too much as some flats may be outluers by themselves
#    and not occur due to broken input from the flat owners
#       outliers <- boxplot.stats(col_data)$out
#       cat("Detected",length(outliers),"outliers in column: \'",column,"\'\n")
#       index <- which(col_data %in% outliers)
#       clean_set <- data_set[-index,]
#--------------------------------------------------------------------
remove_outliers <- function(data_set, column) {
  cat("Initial data size for removing", column,
      " outliers:",nrow(data_set),"rows\n")
  
  #First detect the outliers
  col_data <- data_set[, column] %>% pull(column)
  
  #Obtain the min/max limits
  min_max_values <- MIN_MAX_OUTLIER_FILTERS[[column]]
  cat("Considering the allowed", column, "min/max values:",
      min_max_values[1], "/", min_max_values[2], "\n")
  
  #Identify outlier row indexes and remove 
  good_index <- which((min_max_values[1] <= col_data) & (col_data <= min_max_values[2]))
  clean_set <- data_set[good_index,]
  
  cat("Outliers-free data size for", column,
      " outliers:",nrow(clean_set),"rows\n")
  return(clean_set)
}

#--------------------------------------------------------------------
# This function wrangles the data from the original data set
#--------------------------------------------------------------------
wrangle_data <- function(selected_arog_data) {
  #----------------------------------------
  # totalRent - 15.0% N/A values
  #----------------------------------------
  #Remove data with no totalRent values as these
  #is what we are going to be predicting
  clean_arog_data <- selected_arog_data %>% 
    filter(!is.na(totalRent))
  
  #--------------------------------------------
  # 1 N/A entry in the data set per column
  #----------------------------------------
  #Remove the marginal NA entries
  clean_arog_data <- clean_arog_data %>% 
    filter(!is.na(hasKitchen) & 
             !is.na(balcony) &
             !is.na(lift) &
             !is.na(garden) &
             !is.na(cellar) &
             !is.na(livingSpace) &
             !is.na(noRooms) &
             !is.na(newlyConst) &
             !is.na(baseRent))
  
  #----------------------------------------
  # electricityBasePrice - 76.2% N/A values
  #----------------------------------------
  #Set the electricityBasePrice to zero, as there are
  #   x <- selected_arog_data %>% filter(!is.na(electricityBasePrice))
  #   sum(x$electricityBasePrice == 0)
  #no zero prices for electricity in the data and
  #the number of N/A values is about 80%
  clean_arog_data$electricityBasePrice <- 
    ifelse(is.na(clean_arog_data$electricityBasePrice),
           0, clean_arog_data$electricityBasePrice)  
  
  #----------------------------------------
  # energyEfficiencyClass - 72.3% N/A values
  #----------------------------------------
  #The enerty efficiency factor levels are: 
  #    levels(selected_arog_data$energyEfficiencyClass)
  # ""               "A"              "A_PLUS"         "B" 
  # "C"              "D"              "E"             
  # "F"              "G"              "H"              "NO_INFORMATION"
  #So set all the N/A and "" energy efficiency levens to "NO_INFORMATION"
  clean_arog_data$energyEfficiencyClass <- 
    map_na_and_others_to_unknown(clean_arog_data$energyEfficiencyClass,
                                 new_level_name = "NO_INFORMATION")
  
  #----------------------------------------
  # heatingCosts - 68.2% N/A values
  #----------------------------------------
  #Set the heating costs to zero as there are already
  #    x <- selected_arog_data %>% filter(!is.na(heatingCosts))
  #    sum(x$heatingCosts == 0)
  # 1989 zero-valued heating cost entries 
  clean_arog_data$heatingCosts <- ifelse(is.na(clean_arog_data$heatingCosts),
                                         0.0, clean_arog_data$heatingCosts)  
  
  #----------------------------------------
  # noParkSpaces - 65.8% N/A values
  #----------------------------------------
  #Set the number of parking spaces to zero as there are already
  #    x <- selected_arog_data %>% filter(!is.na(noParkSpaces))
  #    sum(x$noParkSpaces == 0)
  # 2850 zero-valued parking space entries
  clean_arog_data$noParkSpaces <- ifelse(is.na(clean_arog_data$noParkSpaces),
                                         0, clean_arog_data$noParkSpaces)
  
  
  #----------------------------------------
  # interiorQual - 38.8% N/A values
  #----------------------------------------
  # Introducing a new "unknown" value factor:
  #    levels(selected_arog_data$interiorQual)
  #
  # "" "luxury" "normal" "simple" "sophisticated"
  #
  # and setting the N/A and "" values to "unknown"
  #    x <- selected_arog_data %>% filter(!is.na(interiorQual))
  #    sum(x$interiorQual == "")
  clean_arog_data$interiorQual <- 
    map_na_and_others_to_unknown(clean_arog_data$interiorQual)
  
  #----------------------------------------
  # numberOfFloors - 36.2% N/A values
  #----------------------------------------
  # This column is too polluted, see the bar plots per
  # typeOfFlat type. There is too much noize and bias.
  clean_arog_data <- clean_arog_data %>% 
    select(-numberOfFloors)
  
  #----------------------------------------
  # condition - 25.4% N/A values
  #----------------------------------------
  # Introducing a new "unknown" value factor:
  #    levels(selected_arog_data$condition)
  #
  # ""                   "first_time_use" "first_time_use_after_refurbishment"
  # "fully_renovated"    "mint_condition" "modernized"                        
  # "need_of_renovation" "negotiable"     "refurbished"                       
  #
  # and setting the N/A and "" values to "unknown"
  #    x <- selected_arog_data %>% filter(!is.na(condition))
  #    sum(x$condition == "")
  clean_arog_data$condition <-
    map_na_and_others_to_unknown(clean_arog_data$condition)
  
  #----------------------------------------
  # yearConstructed - 21.3% N/A values
  #----------------------------------------
  # There is no good default to replace the N/A values here.
  # Yet, it is a significant amount of data which we do not
  # want to exclude. Therefore drop this column from the
  # analysis and just use the newlyConst flag
  clean_arog_data <- clean_arog_data %>%
    select(-yearConstructed)
  
  #----------------------------------------
  # typeOfFlat - 13.9% N/A values - 
  # ! Do it first as the floors depend on it !
  #----------------------------------------
  # Then we consider the factor levels:
  #    levels(selected_arog_data$typeOfFlat)
  #
  # ""             "apartment" "ground_floor" "half_basement"       "loft"               
  # "maisonette"    "other"     "penthouse"   "raised_ground_floor" "roof_storey"        
  # "terraced_flat"
  #
  # and set the N/A and "" values to "unknown" value, as "other" is known but just not in the list
  #    x <- selected_arog_data %>% filter(!is.na(typeOfFlat))
  #    sum(x$typeOfFlat == "")
  clean_arog_data$typeOfFlat <-
    map_na_and_others_to_unknown(clean_arog_data$typeOfFlat)
  
  #----------------------------------------
  # floor - 19.0% N/A values
  #----------------------------------------
  #Set the floor values for "ground_floor" and "raised_ground_floor"
  clean_arog_data$floor <- ifelse(is.na(clean_arog_data$floor) &
                                    !is.na(clean_arog_data$typeOfFlat) &
                                    (clean_arog_data$typeOfFlat %in%
                                       c("ground_floor", "raised_ground_floor")),
                                  0, clean_arog_data$floor)  
  #Set the floor values for "half_basement"
  clean_arog_data$floor <- ifelse(is.na(clean_arog_data$floor) &
                                    !is.na(clean_arog_data$typeOfFlat) &
                                    (clean_arog_data$typeOfFlat == "half_basement"),
                                  -1, clean_arog_data$floor)  
  
  #Assign the remaining N/A floors to the mean values in the category
  flat_types <- levels(clean_arog_data$typeOfFlat)
  flat_types <- setdiff(flat_types, c("ground_floor", "raised_ground_floor", "half_basement"))
  #Consider the outliers right away for mean floor computations
  floor_outliers <- boxplot.stats(clean_arog_data$floor)$out
  for(ft in flat_types) {
    #Compute the mean value
    mean_ft_val <- clean_arog_data %>% 
      filter(!is.na(floor) & (typeOfFlat == ft) & 
               !(floor %in% floor_outliers)) %>%
      pull(floor) %>% mean()
    #Set the mean value
    clean_arog_data$floor <- ifelse(is.na(clean_arog_data$floor) &
                                      (clean_arog_data$typeOfFlat == ft),
                                    mean_ft_val, clean_arog_data$floor)
  }
  rm(mean_ft_val, flat_types, floor_outliers)
  
  #----------------------------------------
  # heatingType - 16.4% N/A values
  #----------------------------------------
  # Introducing a new "unknown" value factor:
  #    levels(selected_arog_data$heatingType)
  #
  # ""                     "central_heating"   "combined_heat_and_power_plant" 
  # "district_heating"     "electric_heating"  "floor_heating"                 
  # "gas_heating"          "H"                 "heat_pump"                     
  # "night_storage_heater" "oil_heating"       "self_contained_central_heating"
  # "solar_heating"        "stove_heating"     "wood_pellet_heating"
  #
  # and setting the N/A, "", and "H" values to "unknown"
  #    x <- selected_arog_data %>% filter(!is.na(heatingType))
  #    sum(x$heatingType == "")
  #    sum(x$heatingType == "H")
  clean_arog_data$heatingType <-
    map_na_and_others_to_unknown(
      clean_arog_data$heatingType,
      c("","H"))
  
  #----------------------------------------
  # serviceCharge - 2.58% N/A values
  #----------------------------------------
  #Set the heating costs to zero as there are already
  #    x <- selected_arog_data %>% filter(!is.na(serviceCharge))
  #    sum(x$serviceCharge == 0)
  # 2496 zero-valued heating cost entries 
  clean_arog_data$serviceCharge <- ifelse(is.na(clean_arog_data$serviceCharge),
                                          0.0, clean_arog_data$serviceCharge)  
  
  #----------------------------------------
  # typeOfFlat is not consistent with the floors:
  #----------------------------------------
  # Making the floor values consistent for the aparent cases:
  # * Re-setting the number of floors:
  #   * `half_basement` --`floor = -1`
  # * `ground_floor` -- `floor = 0`
  # * `raised_ground_floor` --`floor = 0`
  clean_arog_data$floor <- ifelse(clean_arog_data$typeOfFlat %in% 
                                    c("ground_floor", "raised_ground_floor"),
                                  0, clean_arog_data$floor)
  clean_arog_data$floor <- ifelse(clean_arog_data$typeOfFlat == "half_basement",
                                  1, clean_arog_data$floor)
  
  #----------------------------------------
  # Flats with negative floor values
  #----------------------------------------
  # Here we see that there are 46 appartments with negative floors which are not
  # "half_basement", "other", or N/A ("unknown"). We shall just remove them from
  # our data as this is just 0.02% of the data.
  clean_arog_data <- clean_arog_data %>%
    filter(!((floor < 0) & ! (typeOfFlat %in% c("half_basement", "other", "unknown"))))
  
  #----------------------------------------
  # Exliding the outliers
  #----------------------------------------
  clean_arog_data <- remove_outliers(clean_arog_data, "noRooms")
  clean_arog_data <- remove_outliers(clean_arog_data, "noParkSpaces")
  clean_arog_data <- remove_outliers(clean_arog_data, "livingSpace")
  clean_arog_data <- remove_outliers(clean_arog_data, "baseRent")
  clean_arog_data <- remove_outliers(clean_arog_data, "electricityBasePrice")
  clean_arog_data <- remove_outliers(clean_arog_data, "heatingCosts")
  clean_arog_data <- remove_outliers(clean_arog_data, "serviceCharge")
  clean_arog_data <- remove_outliers(clean_arog_data, "totalRent")
  
  #----------------------------------------
  # Exliding the zero - valued totalRent
  #----------------------------------------
  clean_arog_data <- clean_arog_data %>%
    filter(totalRent > 0 )
  
  #----------------------------------------
  # Combining the regio columns
  #----------------------------------------
  #Because we want to be able to do predicitons per 
  #city and avoind cities with the same names within 
  #different lands and regions we shall combine the
  #regio columnss into a new single one
  clean_arog_data <- clean_arog_data %>% 
    unite("location", c("regio1", "regio2", "regio3"), remove=FALSE) %>%
    select(-regio1, -regio2, -regio3) %>% mutate(location = factor(location))
  
  #----------------------------------------
  # Make sure the integer-valued columns are rounded
  #----------------------------------------
  clean_arog_data <- clean_arog_data %>%
    mutate(floor = as.integer(round(floor)), 
           noParkSpaces = as.integer(round(noParkSpaces)),
           noRooms = as.integer(round(noRooms)))
  
  #----------------------------------------
  #Fix wrond and convert dates:
  #----------------------------------------
  # "Sep18" -> "2018-09-22"
  # "May19" -> "2019-05-10"
  # "Oct19" -> "2019-10-08"
  clean_arog_data$date <- ifelse(clean_arog_data$date == "Sep18",
                                 "2018-09-22", clean_arog_data$date)
  clean_arog_data$date <- ifelse(clean_arog_data$date == "May19",
                                 "2019-05-10", clean_arog_data$date)
  clean_arog_data$date <- ifelse(clean_arog_data$date == "Oct19",
                                 "2019-10-08", clean_arog_data$date)
  clean_arog_data <- clean_arog_data %>% 
    mutate(date = ymd(date))
  
  return(clean_arog_data)
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
  #Before splitting the data set into the training and
  #testing parts set the random seed to keep the behavior
  #stable during the development phase, may be removed later
  set.seed(1)
  
  cat("Splitting the data set into the testing and training set with the", ratio, "ratio\n")
  #Split the data set into a training and testing parts
  #The testing set will be about 10% of original data set
  test_index <- createDataPartition(y = data_set$totalRent, times = 1, 
                                    p = ratio, list = FALSE)
  
  #Make the training data to be the 90% of the data set
  train_set <- data_set[-test_index,]
  #Make the testing data to be the 10% of the data set
  temp_set <- data_set[test_index,]
  
  #Create the test-set candidate
  test_set <- temp_set %>% 
    semi_join(train_set, by = "heatingType") %>%
    semi_join(train_set, by = "typeOfFlat") %>%
    semi_join(train_set, by = "condition") %>%
    semi_join(train_set, by = "interiorQual") %>%
    semi_join(train_set, by = "energyEfficiencyClass") %>%
    semi_join(train_set, by = "location") %>%
    semi_join(train_set, by = "date")
  
  #Move the entries that did not make it into 
  #the test set back into the training set
  removed <- anti_join(temp_set, test_set, 
                       by = c("heatingType",
                              "typeOfFlat",
                              "condition",
                              "interiorQual",
                              "energyEfficiencyClass",
                              "location",
                              "date"))
  train_set <- rbind(train_set, removed)
  
  cat("Training set size is", nrow(train_set), 
      "testing set size is", nrow(test_set), "\n")
  
  rm(test_index, temp_set, removed)
  
  return(list(train_set = train_set, test_set = test_set))
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
  
  #Read the raw AROG data from the csv file, if it has not being done yet
  if(!exists("raw_arog_data")){
    cat("Reading data from", AROG_CSV_FILE_REL_NAME, "\n")
    suppressWarnings(raw_arog_data <- read_csv(AROG_CSV_FILE_REL_NAME))
    raw_arog_data <- as_tibble(raw_arog_data)
  }
  
  #Select the columns of interest
  selected_arog_data <- raw_arog_data %>%
    select(hasKitchen, heatingType, balcony, lift,
           garden, cellar, noParkSpaces, livingSpace,
           typeOfFlat, noRooms, floor, numberOfFloors,
           condition, newlyConst, interiorQual,
           yearConstructed, energyEfficiencyClass,
           regio1, regio2, regio3,
           baseRent, electricityBasePrice, 
           heatingCosts, serviceCharge, totalRent, date) %>%
    mutate(heatingType = factor(heatingType),
           typeOfFlat = factor(typeOfFlat),
           condition = factor(condition),
           interiorQual = factor(interiorQual),
           energyEfficiencyClass = factor(energyEfficiencyClass))
  
  #Clean and pre-process the data
  wrangled_arog_data <- wrangle_data(selected_arog_data)
  
  #Split into modeling and validation sets
  split_arog_data_one <- split_train_test_sets(wrangled_arog_data,
                                               VALIDATION_TO_MODELING_SET_RATIO)
  
  #Split the modeling into training and testing sets
  split_arog_data_two <- split_train_test_sets(split_arog_data_one$train_set,
                                               TEST_TO_TRAIN_SET_RATIO)
  
  #Return the resulting sets
  return(list(raw_data        = raw_arog_data,
              selected_data   = selected_arog_data, 
              wrangled_data   = wrangled_arog_data, 
              modeling_data   = split_arog_data_one$train_set,
              training_data   = split_arog_data_two$train_set,
              testing_data    = split_arog_data_two$test_set,
              validation_data = split_arog_data_one$test_set))
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
  if(!file.exists(AROG_DATA_FILE_NAME)){
    cat("The", AROG_DATA_FILE_NAME, "file is not present, re-generating\n")
    arog_data <- create_arog_data()
    
    cat("The", AROG_DATA_FILE_NAME, "file is re-generated, storing to disk\n")
    save(arog_data, file = AROG_DATA_FILE_NAME)
  } else {
    cat("The", AROG_DATA_FILE_NAME, "file is present, reading\n")
    load(AROG_DATA_FILE_NAME)
  }
  return(arog_data)
}


#--------------------------------------------------------------------
# This function creates the initial report to be filled
#--------------------------------------------------------------------
init_report_data <- function() {
  return(list(
    AROG_CSV_FILE_NAME = AROG_CSV_FILE_NAME,
    AROG_DATA_SET_NAME = AROG_DATA_SET_NAME,
    AROG_DATA_SET_SITE_URL = AROG_DATA_SET_SITE_URL,
    AROG_DATA_SET_FILE_URL = AROG_DATA_SET_FILE_URL,
    VALIDATION_TO_MODELING_SET_RATIO = VALIDATION_TO_MODELING_SET_RATIO,
    TEST_TO_TRAIN_SET_RATIO = TEST_TO_TRAIN_SET_RATIO,
    MAX_NUM_FLOORS_TO_CONSIDER = MAX_NUM_FLOORS_TO_CONSIDER,
    MAX_FLOOR_TO_CONSIDER = MAX_FLOOR_TO_CONSIDER,
    MIN_MAX_OUTLIER_FILTERS = MIN_MAX_OUTLIER_FILTERS,
    TRAIN_CPU_TIME_OUT_SECONDS = TRAIN_CPU_TIME_OUT_SECONDS,
    KNN_K_SEQUENCE = KNN_K_SEQUENCE
  ))
}

#--------------------------------------------------------------------
# This function stores the final report into the 
#     AROG_REPORT_DATA_FILE_NAME
# file to be used later from the movielens_report.Rmd script
#--------------------------------------------------------------------
store_report_data <- function(arog_report) {
  save(arog_report, file = AROG_REPORT_FILE_NAME)
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
# This function prepares predictors data matrix from a given data set.
#    data_set -- the data set to prepare the  data matrix from
# The performed steps are:
#    1. Remove the totalRent column
#    2. Converting to numeric (data) matrix
# The resulting matrix is returned "as-is"
#--------------------------------------------------------------------
prepare_data_matrix <- function(data_set) {
  #Compute the full matrix from the data set
  data_mtx <- data_set %>% select(-totalRent) %>% data.matrix(.)
  
  #Return the predictors matrix
  return(data_mtx)
}

#--------------------------------------------------------------------
# This function takes:
#    pca_result - the PCA analysis results
#    data_mtx - the data matrix
#    num_pc - the number of PC to consider, defaults to NUM_PC_TO_CONSIDER
# and transforms the data_mtx into the PC matrix by:
#    1. Zero-centering the data_mtx columns
#    2. Applying pca_result$rotation matrix
#    3. Selecting num_pc first columns
# The resulting matrix is returned "as-is"
#--------------------------------------------------------------------
prepare_pc_predictors <- function(pca_result, data_mtx, num_pc = NUM_PC_TO_CONSIDER) {
  #Zero-center the columns
  cent_pred_mtx <- sweep(data_mtx, 2, colMeans(data_mtx))
  
  #Rotate to move to the new basis
  rot_pred_mtx <- cent_pred_mtx %*% pca_result$rotation
  
  #Only return the required principle component columns
  return(rot_pred_mtx[,1:num_pc])
}

#--------------------------------------------------------------------
# This function takes the modeling and validation sets to prepare the
# data that will be used for model training and validation in the PC
# (principle component) space. The function arguments are:
#    model_set - the modeling set
#    valid_set - the validation set
# The result is a list with the following attributes:
#    model_pc_mtx - the matrix with R (totalRent), PC1, PC2
#                   columns for the modeling set
#    valid_pc_mtx - the matrix with R (totalRent), PC1, PC2
#                   columns for the modeling set
#--------------------------------------------------------------------
prepare_pc_space_data <- function(model_set, valid_set) {
  #Compute the data matrixes for the data sets
  model_mtx <- prepare_data_matrix(model_set)
  valid_mtx <- prepare_data_matrix(valid_set)
  
  #Perform the PCA analysis on the modeling matrix
  pca_result <- prcomp(model_mtx)
  
  #Prepare the PC space matrix for the modeling set
  pc_mtx_col_names <- c("R", "PC1", "PC2")
  model_pc_mtx <- prepare_pc_predictors(pca_result, model_mtx)
  model_pc_mtx <- cbind(model_set$totalRent, model_pc_mtx)
  colnames(model_pc_mtx) <- pc_mtx_col_names
  
  #Prepare the PC space matrix for the validation set
  valid_pc_mtx <- prepare_pc_predictors(pca_result, valid_mtx)
  valid_pc_mtx <- cbind(valid_set$totalRent, valid_pc_mtx)
  colnames(valid_pc_mtx) <- pc_mtx_col_names
  
  #Remove the temporary data
  rm(pca_result, model_mtx, valid_mtx)
  
  #Return the required data
  return(list(model_pc_mtx = model_pc_mtx,
              valid_pc_mtx = valid_pc_mtx))
}

#--------------------------------------------------------------------
# This function allows to train a model specified by the method
#    model_pc_mtx - the numeric-valued predictor space data matrix
#    method - the method to be used
# The training will be done with a time-out defined by the 
#   GLOBAL_METHOD_TIME_OUT_SECONDS
# The result is the list with the following elements:
#    method - the method used
#    start_time - the time the training started
#    success - the success indicating flag
#    end_time - the time the training finished, if success == TRUE
#    fit_model - the fit model, if success == TRUE
#--------------------------------------------------------------------
train_model <- function(model_pc_mtx, method, ...) {
  #Remove the fit model global if it exists
  ifrm(fit_model)
  
  #Initialize new empty training results list
  train_res <- list(method = method)
  
  #Train the model, with a time-out
  tryCatch({
    train_res <- withTimeout({
      #Record the start time
      train_res <- append(train_res, list(start_time = Sys.time()))
      
      #Fit the model from data
      fit_model <- train(model_pc_mtx[,2:ncol(model_pc_mtx)],
                         model_pc_mtx[,1], method = method, ...)
      
      #Record the end time and the result
      train_res <- append(train_res, list(fit_model = fit_model))
    }, timeout = TRAIN_CPU_TIME_OUT_SECONDS)
  }, TimeoutException = function(ex) {
    message("Timeout (", TRAIN_CPU_TIME_OUT_SECONDS, 
            " sec.) while training the '", method, "' model, skipping!")
  })
  
  #Mark the success flag
  train_res <- append(train_res, 
                      list(end_time = Sys.time(), 
                           success = !is.null(train_res$fit_model)))
  
  #Remove the fit model global if it exists
  ifrm(fit_model)
  
  #Return the result
  return(train_res)
}

#--------------------------------------------------------------------
# The model evaluation function takes the:
#     mdl_res - the modeling results with the fit model to make predictions
#     valid_pc_mtx - the validation set data matrix in PC space
# Once the model predicts the values are the RMSE score is computed.
# The result of the function is the list with the following elements:
#    mdl_res - the modeling results
#    exp_res - the expected result values, i.e. valid_pc_mtx[,1]
#    act_res - the actually predicted values
#    rmse    - the RMSE score between valid_pc_mtx[,1] and act_res
#--------------------------------------------------------------------
evaluate_model <- function(mdl_res, valid_pc_mtx) {
  if(mdl_res$success) {
    #Predict the raw data based on the fit model and predictors
    act_res <- predict(mdl_res$fit_model, 
                       valid_pc_mtx[,2:ncol(valid_pc_mtx)], 
                       type = "raw")
    
    #Compute the RMSE score
    rmse <- RMSE(act_res, valid_pc_mtx[,1])
  } else {
    #Training failed so return the NA results
    act_res <- NA 
    rmse    <- NA
  }
  
  #Create the resulting list and return
  return(list(mdl_res  = mdl_res, 
              exp_res  = valid_pc_mtx[, 1],
              act_res  = act_res,
              rmse     = rmse))
}

#--------------------------------------------------------------------
# This function accepts:
#    arog_report - the report to be extended with the
#                  training and validaiton results
#    pc_space_data - the modeling and validation set
#                    dat matrixes in PC space
#    method - the method name to be used for training
#    ... - any other arguments, as tuning grid parameters to be 
#          forwarded to the train(.) function of the caret package
# This function returns the updated report list, storing a method
# named element which is the list storing the model trainig and the
# model evaluation results. This function also always saves the 
# updated report version into the file. So that each time a new model
# is done there is an updated set of data ready to be reported upon.
#--------------------------------------------------------------------
train_model_and_report <- function(arog_report, pc_space_data, method, ...) {
  curr_date_time <- function() {return(strftime(Sys.time(),"%D %H:%M:%S"))}
  
  #Run training
  cat("Start trainig `", method,"` model (", curr_date_time(),")\n")
  train_res <- train_model(pc_space_data$model_pc_mtx, method, ...)
  
  #Run evaluation
  cat("Start evaluating `", method,"` model (", curr_date_time(),")\n")
  eval_res  <- evaluate_model(train_res, pc_space_data$valid_pc_mtx)
  
  cat("Finished evaluating `", method,"` model (", curr_date_time(),")\n")
  
  #Create the resulting list
  results <- list(train_res = train_res,
                  eval_res = eval_res)
  
  #Append the list as a named element to the report
  arog_report <- append(arog_report, list("xxxx" = results))
  names(arog_report)[which(names(arog_report)=="xxxx")] <- paste(method,"_results",sep="")
  
  cat("The '", method,"' model RMSE score is", eval_res$rmse,"\n")
  
  #Store the report into the file to be used from the arog_report.Rmd
  store_report_data(arog_report)
  
  #Return the updated report
  return(arog_report)
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
arog_report <- init_report_data()

#03 - Prepare PC space data
pc_space_data <- prepare_pc_space_data(
  arog_data$modeling_data, arog_data$validation_data)

#04 - Train and validate the LM model
arog_report <- train_model_and_report(arog_report, pc_space_data, "lm")

#05 - Train and validate the GLM model
arog_report <- train_model_and_report(arog_report, pc_space_data, "glm")

#06 - Train and validate the KNN model
arog_report <- train_model_and_report(arog_report, pc_space_data, "knn",
                                      tuneGrid = data.frame(k = KNN_K_SEQUENCE))

#07 - Train and validate the Rborist model
arog_report <- train_model_and_report(arog_report, pc_space_data, "Rborist")

#08 - Train and validate the svmLinear model
arog_report <- train_model_and_report(arog_report, pc_space_data, "svmLinear")

#09 - Train and validate the gamLoess model
arog_report <- train_model_and_report(arog_report, pc_space_data, "gamLoess")
