# generate the Monte Carlo simulation
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))

# calculate correlation between X,Y for each group
res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))
res

# plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")

# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

# linear regression on group with maximum correlation
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(y ~ x, data = .)))

#----------------------------------------------------------------

# simulate independent X, Y and standardize all except entry 23
set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

# plot shows the outlier
qplot(x, y, alpha = 0.5)

# outlier makes it appear there is correlation
cor(x,y)
cor(x[-23], y[-23])

# use rank instead
qplot(rank(x), rank(y))
cor(rank(x), rank(y))

# Spearman correlation with cor function
cor(x, y, method = "spearman")

#----------------------------------------------------------------

# cause and effect reversal using son heights to predict father heights
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>% 
  do(tidy(lm(father ~ son, data = .)))

#----------------------------------------------------------------

# UC-Berkeley admission data
library(dslabs)
data(admissions)
admissions

# percent men and women accepted
admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

# test whether gender and admission are independent
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

# percent admissions by major
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)

# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot number of applicants admitted and not
admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)

admissions %>% 
  mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# condition on major and then look at differences
admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants)) + geom_point()
# average difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))


#------------------Assessment----------------------
library(dslabs)
data("research_funding_rates")
rfr <- as.tibble(research_funding_rates)

#Construct a two-by-two table of gender (men/women) by award status
#(awarded/not) using the total numbers across all disciplines.

awards <- data.frame(
    gender = c("male", "female"),
    awarded = c(sum(rfr$awards_men),
                sum(rfr$awards_women)),
    not_awarded = c(sum(rfr$applications_men - rfr$awards_men),
                    sum(rfr$applications_women - rfr$awards_women)))

#What is the number of men not awarded?
awards$not_awarded[1]

#What is the number of women not awarded?
awards$not_awarded[2]

#What is the percentage of men awarded?
awards$awarded[1]/(awards$awarded[1]+awards$not_awarded[1]) * 100

#What is the percentage of women awarded?
awards$awarded[2]/(awards$awarded[2]+awards$not_awarded[2]) * 100

#Run a chi-squared test on the two-by-two table to determine whether
#the difference in the two success rates is significant. (You can use
#tidy to turn the output of chisq.test into a data frame as well.)
chi <- awards %>% select(-gender) %>% do(tidy(chisq.test(.)))
chi

#There may be an association between gender and funding. But can we
#infer causation here? Is gender bias causing this observed difference?
#The response to the original paper claims that what we see here is
#similar to the UC Berkeley admissions example. Specifically they state
#that this "could be a prime example of Simpson’s paradox; if a higher
#percentage of women apply for grants in more competitive scientific
#disciplines, then an analysis across all disciplines could incorrectly
#show 'evidence' of gender inequality."

#To settle this dispute, use this dataset with number of applications,
#awards, and success rate for each gender:

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

#To check if this is a case of Simpson's paradox, plot the success
#rates versus disciplines, which have been ordered by overall success,
#with colors to denote the genders and size to denote the number of
#applications.

dat %>% ggplot(aes(x=discipline, y=success, color=gender, size = applications)) +
  geom_point() + scale_x_discrete(labels = abbreviate)

dat %>% group_by(discipline) %>%
  summarize(success = sum(awards)/sum(applications)*100) %>%
  arrange(desc(success))
