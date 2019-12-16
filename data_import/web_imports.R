library(readr)

#Read the data directly from the internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
data <- read_csv(url)
head(data)
#Set the local file folder
data_folder <- file.path(getwd(), "data")

#Download the file to local use
web_murders_csv_file <- file.path(data_folder, "web_murders.csv")
download.file(url, web_murders_csv_file)