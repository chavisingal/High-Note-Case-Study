library(dplyr)
library(psych)
library(ggplot2)
library(GGally)
library(MatchIt)
library(gridExtra)

high_note <- read.csv('~/Downloads/HighNote_Data_Midterm.csv')

head(high_note)
summary(high_note)
#43827 records, mean age 24 & median age 23 (so mostly young people in 20s), more male as mean is 0.63, median friend count is 7, avg friends age is same as the population age, most people's friends are in 2-3 countries, Most people have 0 subscriber friends, people listen to around 8k-10k songs, love around 18 tracks, most people dont post anything, most people dont have playlists, people have around 4 shouts, 8% people converted to preminum account, most people have been on the site for 44 months, and only 35% people are from US, UK, Germany

#-------------------------------------------------------------------------------------

describeBy(high_note,group=high_note$adopter)

#Age: adopters only around 1 year older on average than non-subscribers
#Male: Proportion of males very slightly higher in adopters -
#Friend Count: Substantially higher for adopters ---
#Average Friend Age: Not much differnece
#Average friend male: Not much difference
#Friend country count: Friends of subscribers are from more differeent countries than non-subscribers ---
#Subscriber Friend Count: Slightly higher for subscribers but not much different (median = 0 for both) -
#Songs Listened: Significantly higher for subscribers, thus more time on app ---
#Loved tracks: Much higher for subscribers ---
#posts: Average 3 times higher for subscribers but median = 0 for both ---
#Playlists: No playlists usually for non-subscribers, while more than half of subscribers have at least one ---
#shouts: significantly higher for subscribers ---
#tenure: not much difference
#good country: not much differnet 

#Taking log for the hihgly skewed terms
high_note$ln_friend_cnt <- log(high_note$friend_cnt)
high_note$ln_friend_country_cnt <- log(high_note$friend_country_cnt +1)
high_note$ln_subscriber_friend_cnt <- log(high_note$subscriber_friend_cnt +1)
high_note$ln_songsListened <- log(high_note$songsListened +1)
high_note$ln_lovedTracks <- log(high_note$lovedTracks +1)
high_note$ln_posts <- log(high_note$posts +1)
high_note$ln_playlists <- log(high_note$playlists +1)
high_note$ln_shouts <- log(high_note$shouts +1)


#Checking the differences now:
describeBy(high_note,group=high_note$adopter)

#-------------------------------------------------------------------------------------

#DATA VISUALIZATIONS

##1 Demographics -

#1 Age Distribution
ggplot(high_note, aes(x=age)) + geom_histogram(binwidth = 3) +
  ggtitle("Age Distribution") +
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

#2 Age for Adopters and Non-Adopters
ggplot(high_note, aes(x=as.factor(adopter),y=age)) + geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) +
  ggtitle("Age for Adopters and Non-Adopters") +
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)

#3 Tenure for Adopters and Non-Adopters
ggplot(high_note, aes(x=as.factor(adopter),y=tenure)) + geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) +
  ggtitle("Tenure for Adopters and Non-Adopters") +
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)

#4 Good Country for Adopters and Non-Adopters
ggplot(high_note, aes(x=as.factor(adopter),y=as.factor(good_country))) + geom_jitter()+
  ggtitle("Good Country for Adopters and Non-Adopters") + theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

#5 Gender for Adopters and Non-Adopters
ggplot(high_note, aes(x=as.factor(adopter),y=as.factor(male))) + geom_jitter()+
  ggtitle("Gender for Adopters and Non-Adopters") + theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

#6 Gender distribution by age
ggplot(high_note,aes(x = age,fill = as.factor(male))) + geom_bar(position = "fill") +
  ggtitle("Age Distribution by Gender") +
theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))


##2 - PEER INFLUENCE

#7 Avg_friend_age for Adopters and Non-Adopters
ggplot(high_note, aes(x=as.factor(adopter),y=avg_friend_age)) + geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) +
  ggtitle("Avg friend age for Adopters and Non-Adopters") +
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)

#8 Avg_friend_male for Adopters and Non-Adopters
ggplot(high_note, aes(x=as.factor(adopter),y=avg_friend_male)) + geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) +
  ggtitle("Avg friend male for Adopters and Non-Adopters") +
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)

#9 Friend_country_cnt for Adopters and Non-Adopters
ggplot(high_note, aes(x=as.factor(adopter),y=friend_country_cnt)) + geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4) +
  ggtitle("Friend country count for Adopters and Non-Adopters") +
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)

#10 subscriber_friend_cnt for Adopters and Non-Adopters
ggplot(high_note, aes(x=as.factor(adopter),y=ln_subscriber_friend_cnt)) + geom_boxplot(outlier.colour="red",outlier.shape=8,outlier.size=3) +
  ggtitle("Subscriber friend count for Adopters and Non-Adopters") +
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)


#-------------------------------------------------------------------------------------

#PROPENSITY SCORE MATCHING

high_note$subscriber_friend_treat <- ifelse(high_note$subscriber_friend_cnt >= 1,1,0)

# Difference-in-means: pre-treatment covariates

# Let's calculate the mean for each covariate by treatment status

high_note_cov <- c('ln_friend_cnt','ln_friend_country_cnt','ln_songsListened','ln_lovedTracks','ln_posts','ln_playlists','ln_shouts','tenure','age','good_country','male','avg_friend_age','avg_friend_male')
high_note %>%
  group_by(subscriber_friend_treat) %>%
  select(one_of(high_note_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

#Generating a propensity score matching model for subscriber_friend_treat
high_note_m_ps <- glm(subscriber_friend_treat ~ ln_friend_cnt + ln_friend_country_cnt + ln_songsListened + ln_lovedTracks + ln_posts + ln_playlists + ln_shouts + tenure + age + good_country + male + avg_friend_age + avg_friend_male,
                      family = binomial(), data = high_note)

summary(high_note_m_ps)

# Using this model, we can now calculate the propensity score for each record which is simply the user's predicted probability of being treated, given the estimates from the logit model.
prs_df <- data.frame(pr_score = predict(high_note_m_ps, type = "response"),
                     subscriber_friend_treat = high_note_m_ps$model$subscriber_friend_treat)

head(prs_df)

# Examining the region of common support
# After estimating the propensity score, it is useful to plot histograms of the estimated propensity scores by treatment status

labs <- paste("Subscriber friend count:", c("No Subscriber Friends", "One or more subscriber friends"))

prs_df %>%
  mutate(subscriber_friend_treat = ifelse(subscriber_friend_treat == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~subscriber_friend_treat) +
  xlab("Probability of having a subscriber friend") +
  theme_bw()

# The method we use below is to find pairs of observations that have very similar propensity scores, but that differ in their treatment status. We use the package MatchIt for this. 
#This package estimates the propensity score in the background and then matches observations based on the method of choice (“nearest” in this case).

high_note_nomiss <- high_note %>%  # MatchIt does not allow missing values
  select(adopter, subscriber_friend_treat, one_of(high_note_cov)) %>%
  na.omit()

high_note_mod_match <- matchit(subscriber_friend_treat ~ ln_friend_cnt + ln_friend_country_cnt + ln_songsListened + ln_lovedTracks + ln_posts + ln_playlists + ln_shouts + tenure + age + good_country + male + avg_friend_age + avg_friend_male,
                               method = "nearest",caliper=0.1,data = high_note_nomiss)

# We can get some information about how successful the matching was using summary(mod_match) and plot(mod_match)
summary(high_note_mod_match)
plot(high_note_mod_match)

# To create a dataframe containing only the matched observations, use the match.data() function
high_note_dta_m <- match.data(high_note_mod_match)
dim(high_note_dta_m)

# The final dataset contains a variable called distance, which is the propensity score.
# Examining covariate balance in the matched sample
# Visual Inspection
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$subscriber_friend_treat <- as.factor(dta$subscriber_friend_treat)
  ggplot(dta, aes(x = distance, y = variable, color = subscriber_friend_treat)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw()
}


grid.arrange(
  fn_bal(high_note_dta_m, "ln_friend_cnt"),
  fn_bal(high_note_dta_m, "ln_friend_country_cnt") + theme(legend.position = "none"),
  fn_bal(high_note_dta_m, "ln_songsListened"),
  fn_bal(high_note_dta_m, "ln_lovedTracks") + theme(legend.position = "none"),
  fn_bal(high_note_dta_m, "ln_posts"),
  fn_bal(high_note_dta_m, "ln_playlists") + theme(legend.position = "none"),
  fn_bal(high_note_dta_m, "ln_shouts"),
  fn_bal(high_note_dta_m, "tenure") + theme(legend.position = "none"),
  fn_bal(high_note_dta_m, "age"),
  fn_bal(high_note_dta_m, "good_country") + theme(legend.position = "none"),
  fn_bal(high_note_dta_m, "male"),
  fn_bal(high_note_dta_m, "avg_friend_age") + theme(legend.position = "none"),
  fn_bal(high_note_dta_m, "avg_friend_male"),
  nrow = 7, widths = c(0.7, 0.5)
)

#This shows that when matching on only on propensity scores, it matches pretty well on all the variables as well
#They may not exactly match, but the averages of those observations (green and red) will be same estimating treatment effects


# Difference of means
high_note_dta_m %>%
  group_by(subscriber_friend_treat) %>%
  select(one_of(high_note_cov)) %>%
  summarise_all(funs(mean))

lapply(high_note_cov, function(v) {
  t.test(high_note_dta_m[, v] ~ high_note_dta_m$subscriber_friend_treat)
})

# Estimating treatment effects
# Estimating the treatment effect is simple once we have a matched sample that we are happy with. We can use a t-test:

with(high_note_dta_m, t.test(adopter ~ subscriber_friend_treat))
#We see that the p-value is less than 0.05 and hence the difference is statistically significant. Thus, having friends who are subscribers, has an impact on a user being an adopter or non-adopter.

#Logistic regression to see the effect of treatment group on dependent variable using our matched data
high_note_model_1 <- glm(adopter ~ subscriber_friend_treat, data = high_note_dta_m,, family=binomial())
summary(high_note_model_1)

exp(0.60041)


#-------------------------------------------------------------------------------------

#Logistic regression of all the variables on the dependent variable - adopter
high_note_model <- glm(adopter ~ ln_friend_cnt + ln_friend_country_cnt + 
                         subscriber_friend_treat + ln_songsListened + ln_lovedTracks + ln_posts + ln_playlists + ln_shouts+ tenure + age + good_country + male + avg_friend_age + avg_friend_male, data=high_note_dta_m, family=binomial())
summary(high_note_model)

#ln_friend_count, ln_friend_country_cnt and avg_friend_male are insignificant so we remove them

high_note_model2 <- glm(adopter ~ subscriber_friend_treat + ln_songsListened + ln_lovedTracks + ln_posts + ln_playlists + ln_shouts+ tenure + age + good_country + male + avg_friend_age, data=high_note_dta_m, family=binomial())
summary(high_note_model2)

#Checking for correlation
cor_data <- subset(high_note_dta_m,select = c(subscriber_friend_treat,ln_songsListened,ln_lovedTracks,ln_posts,ln_playlists,ln_shouts,tenure,age,good_country,male,avg_friend_age))

ggcorr(cor_data,label=TRUE,label_round = 2)

#age and avg_friend_age highly correlated. So we drop avg_friend age from our model

#Final regression model
high_note_model3 <- glm(adopter ~ subscriber_friend_treat + ln_songsListened + ln_lovedTracks + ln_posts + ln_playlists + ln_shouts+ tenure + age + good_country + male, data=high_note_dta_m, family=binomial())
summary(high_note_model3)

#Calculating the odds ratio
odds_ratio <- data.frame(exp(high_note_model3$coefficients))
odds_ratio



