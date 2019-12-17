library(tidyverse)
library(pdftools)
library(stringr)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
#system2("open", args = fn)

txt <- pdf_text(fn)
class(txt)
length(txt)
str(txt)

x <- txt[9] %>% str_split('\n')
str(x)
class(x)
length(x)

s <- x[[1]]
str(s)
class(s)
length(s)

s %>% str_extract(".$")

s<-str_trim(s)
str_extract(s[1], ".$")

header_index <- s %>% str_which("2015") %>% .[1]
header_index

month <- s[header_index] %>% str_split("\\s+", simplify = TRUE) %>% .[1]
header <- s[header_index] %>% str_split("\\s+", simplify = TRUE) %>% .[2:length(.)]

total_index <- s %>% str_which("^Total")

n <- s %>% str_count("\\d+")
n
sum(n==1)
single_index <- which(n==1)

indexes_to_remove = c(-1:-header_index, -single_index, -total_index:-length(s))
s <- s[indexes_to_remove]
length(s)

s <- s %>% str_remove_all("[^\\d\\s]")

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

class(s)

tab <- cbind(s, matrix(data=9, nrow=nrow(s), ncol=1))
colnames(tab) <- c("day", header, "month")

class(tab) <- "numeric"
mean(tab[,2])
mean(tab[,3])
mean(tab[1:19,4])
mean(tab[20:30,4])

tab <- as.data.frame(tab) %>% select(-month)

tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

tab %>% filter(year != "2018") %>%
  ggplot(aes(x=day, y=deaths, color=year)) + geom_line() +
  geom_vline(xintercept=20)
