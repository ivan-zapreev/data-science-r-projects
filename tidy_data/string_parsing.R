library(tidyverse)
library(dslabs)
library(stringr)
library(rvest)

# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>%
  html_node("table") %>%
  html_table() %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
# head(murders_raw)
# class(murders_raw$population)
# class(murders_raw$total)
# class(murders_raw$murder_rate)
str(murders_raw)

murders_num <- murders_raw %>%
  mutate(state = factor(state), population = parse_number(population), total = parse_number(total))
str(murders_num)

#Function to detect commas
commas <- function(x) {any(str_detect(x, ','))}
commas_detected <- murders_raw %>% summarize_all(list(commas))
#The loop removing commas
for(column in names(murders_raw)) {
  if(commas_detected[1,column]){
    murders_raw[,column] <- as.numeric(str_replace_all(murders_raw[,column], ",",""))
  }
}
str(murders_raw)

#Assignment 4
data <- read_table("./data/s3.1.a1.q4.csv")
data %>% mutate_at(2:3, parse_number)
data %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
         mutate_at(2:3, as.numeric)


