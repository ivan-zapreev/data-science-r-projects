library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

data(brexit_polls)

str(brexit_polls)

sum(month(brexit_polls$startdate)==4)

weeks <- round_date(brexit_polls$enddate, unit="week") %>% week()
sum(weeks==week("2016-06-12"))

brexit_polls %>% mutate(endday=weekdays(enddate)) %>% group_by(endday) %>% summarize(cnt=n()) %>% arrange(cnt)
table(weekdays(brexit_polls$enddate))

data(movielens)

movielens %>%
  mutate(date=as_datetime(timestamp), year_review=year(date)) %>%
  group_by(year_review) %>% summarize(cnt=n()) %>% arrange(desc(cnt))

movielens %>%
  mutate(date=as_datetime(timestamp), hour_review=hour(date)) %>% 
  group_by(hour_review) %>% summarize(cnt=n()) %>% arrange(desc(cnt))

install.packages("gutenbergr")
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata

gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice")) %>%
  select(gutenberg_id, title)

gutenberg_works() %>% 
  filter(str_detect(title, "Pride and Prejudice")) %>%
  select(gutenberg_id, title)

p_and_p <- gutenberg_download(1342)

words <- p_and_p %>% unnest_tokens(word, text) %>% select(word)
nrow(words)

data(stop_words)
meaningfull_words <- words %>% filter(!word %in% stop_words$word)
nrow(meaningfull_words)

non_digit_words <- meaningfull_words %>% filter(!str_detect(word,"\\d"))
nrow(non_digit_words)

most_frequent_words <- non_digit_words %>% group_by(word) %>% summarize(cnt=n()) %>% filter(cnt > 100)
nrow(most_frequent_words)

most_frequent_words %>% arrange(desc(cnt)) %>% pull(word) %>% .[1]

afinn <- get_sentiments("afinn")

afinn_sentiments <- non_digit_words %>% inner_join(afinn)
sum(is.na(afinn_sentiments$value))
nrow(afinn_sentiments)

nrow(filter(afinn_sentiments, value>0 ))/nrow(afinn_sentiments)

nrow(filter(afinn_sentiments, value==4 ))


