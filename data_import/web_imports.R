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

#Generate some temporary files and folders
tempdir()
tempfile()

#Download the temporary file read it and then remove
temp_murders_csv_file <- tempfile()
download.file(url, temp_murders_csv_file)
data <- read.csv(temp_murders_csv_file)
head(data)
file.remove(temp_murders_csv_file)

#Read some comma-separated csv file with no header from the url
url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_lines(url, n_max=5)
data <- read_csv(url, col_names = FALSE)
cat("Numer of rowns: ", nrow(data))
cat("Numer of columns: ", length(names(data)))

#Read the dable with some header comments, make sure there is no comments present in the data 
url <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_annmean_mlo.txt"
co2_mauna_loa <- read_table(url, skip=56) 
co2_mauna_loa <- co2_mauna_loa %>% select(-"#")
head(co2_mauna_loa)
cat("Numer of rowns: ", nrow(co2_mauna_loa))
