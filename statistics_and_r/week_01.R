if(!require(swirl)) install.packages("swirl", repos = "http://cran.us.r-project.org")
library(swirl)

# | When you are at the R prompt (>):
# | -- Typing skip() allows you to skip the current question.
# | -- Typing play() lets you experiment with R on your own; swirl will ignore what you do...
# | -- UNTIL you type nxt() which will regain swirl's attention.
# | -- Typing bye() causes swirl to exit. Your progress will be saved.
# | -- Typing main() returns you to swirl's main menu.
# | -- Typing info() displays these options again.

#----------------------------------------------------------------------------------

#--------------------------------------------------
# Exercise #1 
# What version of R are you using (hint: make sure you download the
# latest version and then type version)? Please note that this question
# does not count toward your grade, but it is important to make sure 
# that you are using the latest version of R. If the answer is not the
# MOST updated, please just let us know
version

#--------------------------------------------------
# Exercise #2 
# Create a numeric vector containing the numbers 
# 2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23. 
# What is the average of these numbers? 

mean(c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23))

#--------------------------------------------------
# Exercise #3 
# Use a for loop to determine the value of ∑25𝑖=1𝑖2

sum <- 0
for(i in 1:25) {
  sum <- sum + i^2
}
sum

# E--------------------------------------------------
#xercise #4 
# The cars dataset is available in base R. You can type cars to see it. 
# Use the class function to determine what type of object is cars. 

class(cars)

# E--------------------------------------------------
#xercise #5
# How many rows does the cars object have? 

nrow(cars)

# E--------------------------------------------------
# Exercise #6
# What is the name of the second column of cars? 

names(cars)[2]

#--------------------------------------------------
# Exercise #7 
# The simplest way to extract the columns of a matrix or data.frame is using [.
# For example you can access the second column with cars[,2].
# What is the average distance traveled in this dataset? 
mean(cars[,2])

#-------------------------------------------------- Exercise #8 
# Familiarize yourself with the which function. 
# What row of cars has a a distance of 85? 
which(cars$dist == 85)

#----------------------------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)

#Loading the csv data 
data_tv <- read_csv("./data/femaleMiceWeights.csv")
class(data_tv)

data_df <- read.csv("./data/femaleMiceWeights.csv")
class(data_df)

#Not identical
identical(data_tv, data_df)

#But have the same values
which(data_tv$Diet != data_df$Diet)
which(data_tv$Bodyweight != data_df$Bodyweight)
which(data_tv != data_df)

#----------------------------------------------------------------------------------
#Getting Started Exercises
if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")
library(downloader) 

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "./data/femaleMiceWeights.csv" 
download(url, destfile=filename)

# G--------------------------------------------------
#etting Started Exercises #1 
# Read in the file femaleMiceWeights.csv and report the exact name of the column containing the weights. 

fmw_data <- read.csv(filename)
names(fmw_data)

# G--------------------------------------------------
#etting Started Exercises #2 
# The [ and ] symbols can be used to extract specific rows and specific columns of the table.
# What is the entry in the 12th row and second column? 
fmw_data[12,2]

# G--------------------------------------------------
#etting Started Exercises #3 
# You should have learned how to use the $ character to extract a column from a table and return it as a vector.
# Use $ to extract the weight column and report the weight of the mouse in the 11th row. 
fmw_data$Bodyweight[11]

# G--------------------------------------------------
# Getting Started Exercises #4 
# The length function returns the number of elements in a vector. How many mice are included in our dataset? 
length(fmw_data$Diet)

#-------------------------------------------------- Getting Started Exercises #5 
# To create a vector with the numbers 3 to 7, we can use seq(3,7) or, because they are consecutive, 3:7. 
# View the data and determine what rows are associated with the high fat or hf diet. 
# Then use the mean function to compute the average weight of these mice. 
which(fmw_data$Diet == "hf")
mean(fmw_data$Bodyweight[13:24])

mean(fmw_data$Bodyweight[fmw_data$Diet == "hf"])

# G--------------------------------------------------
#etting Started Exercises #6 
# One of the functions we will be using often is sample. Read the help file for sample using ?sample.
# Now take a random sample of size 1 from the numbers 13 to 24 and report back the weight of the
# mouse represented by that row. Make sure to type set.seed(1) to ensure that everybody gets the
# same answer.
# 
# (A note about set.seed: The default behavior of this function was changed in R 3.6. If you're using
# R 3.6 or later, then you need to add an argument to set.seed(x) to get the expected answer. Use 
# set.seed(x, sample.kind="Rounding") instead. We will remind you to add this argument when it is necessary.)

set.seed(1, sample.kind="Rounding")

fmw_data$Bodyweight[sample(13:24, 1)]

#----------------------------------------------------------------------------------

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

data <- read.csv(filename)

#View the data in RStudio table
View(data)

controls <- filter(data, Diet == "chow")

str(controls)

control_body_weights <- select(controls, Bodyweight)

#Turn the column into a simple vector
unlist(control_body_weights)

#The same but in a sinle line, using the pipeline:
data %>% filter(Diet == "chow") %>% select(Bodyweight) %>% unlist(.)

#----------------------------------------------------------------------------------
#dplyr Exercises
if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")
library(downloader)

url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- file.path("data", basename(url))
download(url,filename)

# d--------------------------------------------------
#plyr Exercises #1 
# Read in the msleep_ggplot2.csv file with the function read.csv and use the function class
# to determine what type of object is returned. 
msgg_data <- read.csv(filename)
class(msgg_data)

# d--------------------------------------------------
#plyr Exercises #2 
# Now use the filter function to select only the primates. How many animals in the table are 
# primates? Hint: the nrow function gives you the number of rows of a data frame or matrix. 
pm_msgg_data <- msgg_data %>% filter(order == "Primates")
pm_msgg_data %>% nrow

# d--------------------------------------------------
#plyr Exercises #3 
# What is the class of the object you obtain after subsetting the table to only include primates?
class(pm_msgg_data)

# d--------------------------------------------------
# dplyr Exercises #4 
# Now use the select function to extract the sleep (total) for the primates. What class is 
# this object? Hint: use %>% to pipe the results of the filter function to select. 
pm_msgg_data %>% select(sleep_total) %>% class

#-------------------------------------------------- dplyr Exercises #5 
# Now we want to calculate the average amount of sleep for primates (the average of the numbers 
# computed above). One challenge is that the mean function requires a vector so, if we simply 
# apply it to the output above, we get an error. Look at the help file for unlist and use it to 
# compute the desired average. 
pm_msgg_data %>% select(sleep_total) %>% unlist %>% mean

# d--------------------------------------------------
#plyr Exercises #6 
# For the last exercise, we could also use the dplyr summarize function. We have not introduced 
# this function, but you can read the help file and repeat exercise 5, this time using just filter 
# and summarize to get the answer. 
pm_msgg_data %>% summarize(avg = mean(sleep_total))


















