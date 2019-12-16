library(tidyverse)
library(dslabs)
data(gapminder)

#Create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

#Plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

#Import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

new_tidy_data <- wide_data %>% gather(year, fertility, `1960`:`2015`)
new_tidy_data <- wide_data %>% gather(year, fertility, -country)

class(tidy_data$year)
class(new_tidy_data$year)

new_tidy_data <- wide_data %>% gather(year, fertility, -country, convert=TRUE)
class(new_tidy_data$year)

# ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))

# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 

# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")

# full code for tidying data
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

data <- read_table("data/s2.1.a1.q5.csv")
data %>% spread(year, time)

data <- read_table("data/s2.1.a1.q6.csv")
data %>% spread(var1, people)

data <- read_csv("data/s2.1.a1.q7.csv")
data %>% gather(key, value, -age_group) %>%
         separate(col = key, into = c("year", "variable_name"), sep = "_")  %>%
         spread(variable_name, value)

data <- read_csv("data/s2.1.a1.q8.csv")
data %>% separate(col=key, c("player", "variable_name"), extra="merge") %>%
         spread(variable_name, value)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_tidy <- co2_wide %>% gather(month, co2, -year)
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat %>% spread(gender, admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2 %>% spread(column_name, value)
