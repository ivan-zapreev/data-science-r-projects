# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&oldid=919733895"    # permalink
h <- read_html(url)
class(h)
h

html_tabs <- h %>% html_nodes("table")
html_tab <- html_tabs[[2]]
class(html_tab)

data_tab <- html_tab %>% html_table()
class(data_tab)
head(data_tab)

named_data_tab <- data_tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(named_data_tab)
str(named_data_tab)

h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".method-list") %>% html_text()
recipe
#The selector will not be found the page has changed
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".sticky-ingredients") %>% html_text()
ingredients

#ASSESSMENTS
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")

#Assesment 1-3
tab1 <- nodes[[1]] %>% html_table()
tab2 <- nodes[[2]] %>% html_table()
tab3 <- nodes[[3]] %>% html_table()
tab4 <- nodes[[4]] %>% html_table()
 
tab10 <- nodes[[10]] %>% html_table()

length(xml_length(nodes))

tab19 <- nodes[[19]] %>% html_table()
tab20 <- nodes[[20]] %>% html_table()
tab21 <- nodes[[21]] %>% html_table()

head(tab10)
tab10 <- tab10[-c(1), ]
head(tab10)
tab10 <- tab10[, -c(1)] %>% setNames(c("Team", "Payroll", "Average"))
head(tab10)

head(tab19)
tab19 <- tab19[-c(1), ]%>% setNames(c("Team", "Payroll", "Average"))
head(tab19)

library(tidyverse)
library(ggrepel)
library(dslabs)

combined <- full_join(tab10, tab19, by="Team")
nrow(combined)

#Assesment 4-5
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
nodes <- html_nodes(h, "table")
length(nodes)

check_table_node <- function(table_node, idx){
  data_table <- table_node %>% html_table(fill=TRUE)
  result <- FALSE
  if(ncol(data_table)==9){
    cat("Found table with 9 columns, index = ", idx, "\n")
    if(names(data_table)[1] == "Date(s) conducted") {
      cat("Found table with the Date(s) conducted column, index = ", idx, "\n")
      result <- TRUE
    }
  }
  result
}

for(i in 1:length(nodes)) {
  if(check_table_node(nodes[[i]],i)){
    cat(">>>>> Found required table with index = ", i, "\n")
    break;
  }
}



