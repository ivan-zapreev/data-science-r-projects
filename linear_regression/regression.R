# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

#------------------------------------------------------------

#A way to actually pick the players for the team can be done using 
#what computer scientists call linear programming. Although we don't
#go into this topic in detail in this course, we include the code anyway:

library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

#This algorithm chooses these players:
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))

our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

#We note that these players generally have above average 
#BB and HR rates while the same is not true for singles.

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

#------------------------------------------------------------

#The code to create a table with player ID, their names, and their most played position:
library(Lahman)
playerInfo <- Fielding %>%
group_by(playerID) %>%
arrange(desc(G)) %>%
slice(1) %>%
ungroup %>%
left_join(Master, by="playerID") %>%
select(playerID, nameFirst, nameLast, POS)

#The code to create a table with only the ROY award winners and add their batting statistics:
ROY <- AwardsPlayers %>%
filter(awardID == "Rookie of the Year") %>%
left_join(playerInfo, by="playerID") %>%
rename(rookie_year = yearID) %>%
right_join(Batting, by="playerID") %>%
mutate(AVG = H/AB) %>%
filter(POS != "P")

#The code to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons:
ROY <- ROY %>%
filter(yearID == rookie_year | yearID == rookie_year+1) %>%
group_by(playerID) %>%
mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
filter(n() == 2) %>%
ungroup %>%
select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG) 

#The code to use the spread function to have one column for the rookie and sophomore years batting averages:
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
  
#The code to calculate the proportion of players who have a lower batting average their sophomore year:
mean(ROY$sophomore - ROY$rookie <= 0)
  
#The code to do the similar analysis on all players that played the 2013 and 2014 seasons and batted more than 130 times (minimum to win Rookie of the Year):
two_years <- Batting %>%
filter(yearID %in% 2013:2014) %>%
group_by(playerID, yearID) %>%
filter(sum(AB) >= 130) %>%
summarize(AVG = sum(H)/sum(AB)) %>%
ungroup %>%
spread(yearID, AVG) %>%
filter(!is.na(`2013`) & !is.na(`2014`)) %>%
left_join(playerInfo, by="playerID") %>%
filter(POS!="P") %>%
select(-POS) %>%
arrange(desc(`2013`)) %>%
select(nameFirst, nameLast, `2013`, `2014`)

two_years

#The code to see what happens to the worst performers of 2013:
arrange(two_years, `2013`)

#The code to see  the correlation for performance in two separate years:
qplot(`2013`, `2014`, data = two_years)

summarize(two_years, cor(`2013`,`2014`))

#-----------------------------------------------------------------

#The code to use dslabs function rfalling_object to generate simulations of dropping balls:
library(dslabs)
falling_object <- rfalling_object()

#The code to draw the trajectory of the ball:
falling_object %>%
ggplot(aes(time, observed_distance)) +
geom_point() +
ylab("Distance in meters") +
xlab("Time in seconds")

#The code to use the lm() function to estimate the coefficients:
fit <- falling_object %>%
mutate(time_sq = time^2) %>%
lm(observed_distance~time+time_sq, data=.)

tidy(fit)

#The code to check if the estimated parabola fits the data:
augment(fit) %>%
ggplot() +
geom_point(aes(time, observed_distance)) +
geom_line(aes(time, .fitted), col = "blue")

#The code to see the summary statistic of the regression:
tidy(fit, conf.int = TRUE)

#-------------------------------------------------------------

#Imagine you have two teams. Team A is comprised of batters who,
#on average, get two bases on balls, four singles, one double,
#and one home run. Team B is comprised of batters who, on average,
#get one base on balls, six singles, two doubles, and one triple.

A <- data.frame(
  BB = 2,       #two bases on balls
  singles = 4,  #four singles
  doubles = 1,  #one double
  triples = 0,
  HR = 1         #one home run
)

B <- data.frame(
  BB = 1,       #one bases on balls
  singles = 6,  #six singles
  doubles = 2,  #two doubles
  triples = 1,  #one triple
  HR = 0
)

#Which team scores more runs, as predicted by our model?

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

compute_R <- function(coefs, values) {
  coefs$estimate[1]+
    coefs$estimate[2]*values$BB+
    coefs$estimate[3]*values$singles+
    coefs$estimate[4]*values$doubles+
    coefs$estimate[5]*values$triples+
    coefs$estimate[6]*values$HR
}

cat("Prediction for team A: ", compute_R(coefs,A))
cat("Prediction for team B: ", compute_R(coefs,B))

#----------------------------------------------------

#Use the Teams data frame from the Lahman package.
#Fit a multivariate linear regression model to obtain 
#the effects of BB and HR on Runs (R) in 1971. Use 
#the tidy function in the broom package to obtain 
#the results in a data frame.

library(Lahman)

#Clarification: For questions 9-11, do not normalize
#by number of games. For example, use R rather than R/G.

fit <- Teams %>% 
  filter(yearID==1971) %>%
  lm(R~BB+HR,data=.)

fit <- tidy(fit)
fit

#Repeat the above exercise to find the effects of BB and HR on runs 
#(R) for every year from 1961 to 2018 using do and the broom package.

bb_fit <- Teams %>% 
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R~BB+HR,data=.),conf.int=TRUE)) %>%
  filter(term == "BB") %>%
  select(-term) %>%
  ungroup()

#Make a scatterplot of the estimate for the effect of BB on runs over
#time and add a trend line with confidence intervals. 

bb_fit %>% 
  ggplot(aes(x=yearID, y=estimate, ymin = conf.low, ymax=conf.high)) +
  geom_point() +
  geom_smooth(method="lm")

#Fit a linear model on the results from Question 10 to determine the 
#effect of year on the impact of BB.

year_bb_data <- bb_fit %>% select(yearID, estimate) %>% setNames(c("yearID","BB"))

#For each additional year, by what value does the impact of BB on runs change?
#Recall from the instructions at the top of the page that you should
#not normalize by the number of games (use R and BB instead of R/G and BB/G.
year_bb_fit <- year_bb_data %>% lm(BB~yearID, data=.)
year_bb_fit <- tidy(year_bb_fit)
year_bb_fit

#------------------Assessment: Linear Models-------------------

#This assessment has 6 multi-part questions that will all use the setup below;
#Game attendance in baseball varies partly as a function of how well a team is playing.
#Load the Lahman library. The Teams data frame contains an attendance column.
#This is the total attendance for the season. To calculate average attendance,
#divide by the number of games played, as follows:

library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G) %>% as.tibble()

#Use linear models to answer the following 3-part question about Teams_small.

#Use runs (R) per game to predict average attendance.
fit_R <- Teams_small %>% mutate(R_per_game = R/G) %>% lm(avg_attendance ~ R_per_game, data=.) %>% tidy()
fit_R

#Use home runs (HR) per game to predict average attendance.
fit_HR <- Teams_small %>% mutate(HR_per_game = HR/G) %>% lm(avg_attendance ~ HR_per_game, data=.) %>% tidy()
fit_HR

#Use number of wins to predict average attendance; do not normalize for number of games.
fit_W <- Teams_small %>% lm(avg_attendance ~ W, data=.) %>% tidy()
fit_W

#For every game won in a season, how much does average attendance increase?
fit_W$estimate[2]

#Suppose a team won zero games in a season. Predict the average attendance.
fit_W$estimate[1]

#Use year to predict average attendance.
fit_Y <- Teams_small %>% lm(avg_attendance ~ yearID, data=.) %>% tidy()
fit_Y

#Game wins, runs per game and home runs per game are positively correlated with attendance.
#We saw in the course material that runs per game and home runs per game are correlated
#with each other. Are wins and runs per game or wins and home runs per game correlated?

#What is the correlation coefficient for wins and runs per game?
Teams_small %>% 
  mutate(R_per_game = R/G) %>%
  do(cor = cor(.$W, .$R_per_game)) %>% pull(cor) %>% .[[1]]

#What is the correlation coefficient for wins and home runs per game?
Teams_small %>% 
  mutate(HR_per_game = HR/G) %>%
  do(cor = cor(.$W, .$HR_per_game)) %>% pull(cor) %>% .[[1]]

#Stratify Teams_small by wins: divide number of wins by 10 and then round
#to the nearest integer. Keep only strata 5 through 10, which have 20 or
#more data points.

filtered_data <- Teams_small %>% 
  mutate(W_stratas = round(W/10,0)) %>% 
  filter(W_stratas %in% 5:10) %>%
  group_by(W_stratas) %>%
  filter(n() >= 20)

hist(x=filtered_data$W_stratas)

#Use the stratified dataset to answer this three-part question.

#How many observations are in the 8 win strata?
sum(filtered_data$W_stratas == 8)

#Calculate the slope of the regression line predicting average attendance
#given runs per game for each of the win strata.
get_slope <- function(y, x){
  data.frame(slope = cor(x, y) * sd(y)/ sd(x))
} 

slopes_A_R <- filtered_data %>% 
  mutate(R = R/G) %>% 
  do(get_slope(.$avg_attendance, .$R)) %>% arrange(desc(slope))

#Which win stratum has the largest regression line slope?
slopes_A_R$W_stratas[which.max(slopes_A_R$slope)]

#Calculate the slope of the regression line predicting average attendance
#given HR per game for each of the win strata.
slopes_A_HR <- filtered_data %>% 
  mutate(HR = HR/G) %>% 
  do(get_slope(.$avg_attendance, .$HR)) %>% arrange(desc(slope))

#Which win stratum has the largest regression line slope?
slopes_A_HR$W_stratas[which.max(slopes_A_HR$slope)]

#Which of the followng are true about the effect of win strata on average attendance?

#----Across all win strata, runs per game are positively correlated with average attendance.?
filtered_data %>%
  mutate(R = R/G) %>% 
  ggplot(aes(x= R-mean(R), y=avg_attendance-mean(avg_attendance))) +
  geom_point(alpha=0.3) + 
  geom_smooth(method = 'lm')+
  facet_wrap(~W_stratas)
#<- YES

#----Runs per game have the strongest effect on attendance when a team wins many games. 
slopes_A_R
slopes_A_R$W_stratas[which.max(slopes_A_R$slope)]
#<- NO

#----After controlling for number of wins, home runs per game are not correlated with attendance. 
filtered_data %>%
  mutate(HR = HR/G) %>% 
  ggplot(aes(x= HR-mean(HR), y=avg_attendance-mean(avg_attendance))) +
  geom_point(alpha=0.3) + 
  geom_smooth(method = 'lm')+
  facet_wrap(~W_stratas)
#<- NO

#Home runs per game have the strongest effect on attendance when a team does not win many games. 
slopes_A_HR
slopes_A_HR$W_stratas[which.max(slopes_A_HR$slope)]
#<- YES

#Among teams with similar numbers of wins, teams with more
#home runs per game have larger average attendance. 
filtered_data %>%
  mutate(HR = HR/G) %>% 
  ggplot(aes(x= HR-mean(HR), y=avg_attendance-mean(avg_attendance))) +
  geom_point(alpha=0.3) + 
  geom_smooth(method = 'lm')+
  facet_wrap(~W_stratas)
#<- YES

#Fit a multivariate regression determining the effects of runs per game, home
#runs per game, wins, and year on average attendance. Use the original
#Teams_small wins column, not the win strata from question 3.

fit <- Teams_small %>% 
  mutate(R_per_game = R/G,
         HR_per_game = HR/G) %>% 
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data=.) %>% tidy()
fit

#What is the estimate of the effect of runs per game on average attendance?
fit$estimate[2]

#What is the estimate of the effect of home runs per game on average attendance?
fit$estimate[3]

#What is the estimate of the effect of number of wins in a season on average attendance?
fit$estimate[4]

#Use the multivariate regression model from Question 4. 
#Suppose a team averaged 5 runs per game, 1.2 home runs per game, 
#and won 80 games in a season.

compute_avg_attendance <- function(coef, R_per_game, HR_per_game, W, yearID) {
  coef[1]+coef[2]*R_per_game+coef[3]*HR_per_game+coef[4]*W+coef[5]*yearID
}

#What would this team's average attendance be in 2002?
compute_avg_attendance(fit$estimate, 5, 1.2, 80, 2002)

#What would this team's average attendance be in 1960?
compute_avg_attendance(fit$estimate, 5, 1.2, 80, 1960)

#Use your model from Question 4 to predict average attendance for 
#teams in 2002 in the original Teams data frame.

cor_est <- Teams %>%
  filter(yearID==2002) %>%
  mutate(R_per_game = R/G,
         HR_per_game = HR/G,
         avg_attendance = attendance/G,
         avg_attendance_est = 
           compute_avg_attendance(fit$estimate,
                                  R_per_game,
                                  HR_per_game,
                                  W,
                                  yearID))

#What is the correlation between the predicted attendance and actual attendance?
cor_est %>% 
  do(cor = cor(.$avg_attendance_est, .$avg_attendance)) %>%
  pull(cor) %>%
  .[[1]]

