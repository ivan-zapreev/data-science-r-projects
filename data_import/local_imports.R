library(dslabs)
library(readr)
library(readxl)
library(tidyverse)

#Get the dslabs library data directory
dslabs_data_dir <- system.file("extdata", package = "dslabs")
#List the files from the directory
list.files(dslabs_data_dir)
#Set the local file folder
data_folder <- file.path(getwd(), "data")

#Create the file copying function
copy_file_to_local <- function(file_name){
  full_path <- file.path(dslabs_data_dir, file_name)
  cat("Copying ", full_path, " to ", data_folder)
  is_success <- file.copy(full_path, data_folder, overwrite=TRUE)
  #Check on the return status and dubble check with the file exists function
  if(is_success && file.exists(file_name)){
    cat(", Is successful!")
  } else {
    cat(", Has failed!")
  }
}

#Copy the murders.csv file to the local directory, read and print its head
murders_csv_file <- file.path(data_folder, "murders.csv")
copy_file_to_local(murders_csv_file)

#Reading the data using the READR library
read_lines(murders_csv_file, n_max = 3)
murders_data <- read_csv(murders_csv_file)
class(murders_data)
class(murders_data$abb)
head(murders_data)

#Read data with the tidyverse 
murders_data <- read.csv(murders_csv_file)
class(murders_data)
class(murders_data$abb)
head(murders_data)

#Read data with the tidyverse without to factor conversion
murders_data <- read.csv(murders_csv_file, stringsAsFactors = FALSE)
class(murders_data$abb)

#Copy the 2010_bigfive_regents.xls file to the local directory, read and print its head
bigfive_regents_xls_file <- file.path(data_folder, "2010_bigfive_regents.xls")
copy_file_to_local(bigfive_regents_xls_file)
sheets <- excel_sheets(bigfive_regents_xls_file)
sheet <- sheets[sample(1:length(sheets),1)] #Coose a random sheet name
bigfive_regents_data_part <- read_excel(bigfive_regents_xls_file, sheet)
head(bigfive_regents_data_part)
