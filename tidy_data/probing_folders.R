library(tidyverse)
library(ggrepel)
library(dslabs)
library(rvest)

get_pdf_data <- function(url){
  temp_file <- tempfile()
  download.file(url, temp_file)
  txt <- pdf_text(temp_file)
  file.remove(temp_file)
  txt
}

lidl_folder_url<-"https://media.lidl-flyer.com/416709fa-1a7e-11ea-9fa3-005056ab0fb6/Delicieux-Feest-in-t-land-04.pdf"
lidl_folder_pdf <- get_pdf_data(lidl_folder_url)

str(lidl_folder_pdf)

first_page <- lidl_folder_pdf[7] %>% str_split("\n")
first_page <- first_page[[1]]
first_page



