# Libraries
library(tidyverse)
library("dplyr")
library("lmtest")
library("MASS")
library("mfx")
library("BaylorEdPsych")
library("htmltools")
library("aod")
library("logistf")
library("car")
library(generalhoslem)
library("stargazer")


## Please set working directory to folder containing codes location before proceeding

## Uploading specification test for the model
source("linktest.R")
source("AllGOFTests.R")

options(scipen = 5, digits = 5)

data <- read_csv('test.csv')

data[!complete.cases(data),]

sum(is.na(data))

# Only 83 missing values so we will just simply remove them

data <- na.omit(data)

glimpse(data)

table(data$satisfaction)

# The distribution of dependent variable is pretty even

# Encoding dependent variable to 0 - 1 

data$satisfaction <- ifelse(data$satisfaction == 'satisfied', 1, 0)

# Initial variables encoding

data$Gender <- as.factor(data$Gender)
data$`Customer Type` <- factor(data$`Customer Type`, levels = c("disloyal Customer", "Loyal Customer"), ordered = F)
data$`Type of Travel` <- as.factor((data$`Type of Travel`))
data$Class <- factor(data$Class, levels = c("Eco", "Eco Plus", "Business"), ordered = F)



### Satisfaction variables encoding

variable_names <- c("Inflight wifi service", 
                    "Departure/Arrival time convenient", 
                    "Ease of Online booking", 
                    "Gate location", 
                    "Food and drink", 
                    "Online boarding", 
                    "Seat comfort", 
                    "Inflight entertainment", 
                    "On-board service", 
                    "Leg room service", 
                    "Baggage handling", 
                    "Checkin service", 
                    "Inflight service", 
                    "Cleanliness")


#### Function to automatically make factors out of satisfaction of service variables
#### The Function removes values 0 which are treated as NA = lack of answer in the survey

process_variable <- function(data, variable_name) {
  data <- data %>% filter(!(data[[variable_name]] %in% 0))
  
  data[[variable_name]] <- factor(data[[variable_name]], levels = c(1, 2, 3, 4, 5), ordered = F)
  
  print(variable_name)

  print(table(data[[variable_name]]))
  
  return(data)
}

for (variable in variable_names) {
  data <- process_variable(data, variable)
}

#### Own introduced variable - workable wifi means it is good enough to work with it (there is no such word, but we already made the tests and everything)

data$wifi_good_bad  = as.factor(ifelse(as.numeric(data$`Inflight wifi service`) >= 3, 'Workable', 'Bad'))


# -------------------------- EDA ----------------------------------------------

# 

plot_histograms <- function(data) {
  
  numeric_cols <- sapply(data, is.numeric)
  data_numeric <- data[, numeric_cols]
  for (col in colnames(data_numeric)) {
    hist(data_numeric[[col]], main = paste("Histogram of", col), xlab = col, ylab = "Frequency", col = "blue", border = "black")
  }
}


plot_histograms(data)

# barplots of categorical variables

plot_categorical <- function(data) {

  categorical_vars <- names(data)[sapply(data, is.factor)]
  

  for (var in categorical_vars) {
  
    barplot(table(data[[var]]),
            main = paste("Bar plot of", var),
            xlab = var,
            ylab = "Frequency",
            col = rainbow(length(unique(data[[var]]))))
  }
}


plot_categorical(data)

## distribution of arget variable across the gender levels

ggplot(data, aes(x = Gender, fill = as.factor(satisfaction))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "navy"), 
                    name = "Satisfaction",
                    labels = c("Not Satisfied", "Satisfied")) +
  labs(title = "Satisfaction by Gender",
       x = "Gender",
       y = "Count of Customers") +
  theme_minimal()

## histogram of age
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Age",
       x = "Age",
       y = "Count") +
  theme_minimal()

# Histogram for Flight Distance
ggplot(data, aes(x = `Flight Distance`)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Flight Distance",
       x = "Flight Distance",
       y = "Count") +
  theme_minimal()

## distirbution of target variable across the own introduced WiFi variable
ggplot(data, aes(x = wifi_good_bad, fill = as.factor(satisfaction))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "navy"), 
                    name = "Satisfaction",
                    labels = c("Not Satisfied", "Satisfied")) +
  labs(title = "Satisfaction by WiFi usefulness",
       x = "WiFi level",
       y = "Count of Customers") +
  theme_minimal()


# ------------------------- feature selection -------------------------------

table1 <- table(data$`Type of Travel`, data$`Departure/Arrival time convenient`)
chisq.test(table1)

table2 <- table(data$`Type of Travel`, data$wifi_good_bad)
chisq.test(table2)

table3 <- table(data$Class, data$`Seat comfort`)
chisq.test(table3)

table4<- table(data$Class, data$`Leg room service`)
chisq.test(table4)

### in all tests we reject the null hhypothesis that the variables are independent due to p-value < 0.05 significance level


# ---------------------- model choice ---------------------------------------

# Model choice: logit or probit?

logit_model <- glm(satisfaction ~ 
                     `Customer Type`
                   + `On-board service`
                   + `Baggage handling`
                   + `Inflight entertainment`
                   + `Arrival Delay in Minutes`
                   + `Departure Delay in Minutes`
                   + `Flight Distance`
                   + `Departure/Arrival time convenient`
                   + `Ease of Online booking`
                   + `Online boarding`
                   + `Seat comfort`
                   + `Leg room service`
                   + Gender
                   + I(Age^2)
                   + Age
                   + `Gate location`
                   + Age:`Gate location`
                   + Age:`Ease of Online booking`
                   + `Checkin service`
                   + `Food and drink`
                   + Cleanliness
                   +`Inflight service`
                   + wifi_good_bad
                   ,data = data, family = binomial(link = "logit"))

null <- glm(satisfaction~1,data = data,family=binomial(link="logit"))
lrtest(logit_model,null)


logitgof(data$satisfaction, fitted(logit_model), g = 10)

o.r.test(logit_model)
stukel.test(logit_model)

summary(linktest(logit_model))

summary(logit_model)


PseudoR2(logit_model)



probit_model <- glm(satisfaction ~ 
                      `Customer Type`
                    + `On-board service`
                    + `Baggage handling`
                    + `Inflight entertainment`
                    + `Arrival Delay in Minutes`
                    + `Departure Delay in Minutes`
                    + `Flight Distance`
                    + `Departure/Arrival time convenient`
                    + `Ease of Online booking`
                    + `Online boarding`
                    + `Seat comfort`
                    + `Leg room service`
                    + Gender
                    + I(Age^2)
                    + Age
                    + `Gate location`
                    + Age:`Gate location`
                    + Age:`Ease of Online booking`
                    + `Checkin service`
                    + `Food and drink`
                    + Cleanliness
                    +`Inflight service`
                    + wifi_good_bad
                    ,data = data, family = binomial(link = "probit"))

logitgof(data$satisfaction, fitted(probit_model), g = 10)

summary(linktest(probit_model))

summary(probit_model)


PseudoR2(probit_model)

stargazer(logit_model,probit_model, type = "text")

# Based on this analysis we should choose logit model as it passes all the tests and 
# have lower AIC value


# ------------------------General to specific approach --------------------------------

# testing for joint insignificance of the levels

linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0"))

# P-value > 5%, we fail to reject H0 that variables are jointly statistically insignificant, so we can remove `Departure Delay in Minutes`


# We cannot eliminate any of these variables as the levels are jointly significant 
logit_model1 <- glm(satisfaction ~ 
                      `Customer Type`
                    + `On-board service`
                    + `Baggage handling`
                    + `Inflight entertainment`
                    + `Arrival Delay in Minutes`
                    + `Flight Distance`
                    + `Departure/Arrival time convenient`
                    + `Ease of Online booking`
                    + `Online boarding`
                    + `Seat comfort`
                    + `Leg room service`
                    + Gender
                    + I(Age^2)
                    + Age
                    + `Gate location`
                    + Age:`Gate location`
                    + Age:`Ease of Online booking`
                    + `Checkin service`
                    + `Food and drink`
                    + Cleanliness
                    +`Inflight service`
                    + wifi_good_bad
                    ,data = data, family = binomial(link = "logit"))


summary(logit_model1)

linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                 "`Food and drink`2 = 0",
                 "`Food and drink`3 = 0",
                 "`Food and drink`4 = 0",
                 "`Food and drink`5 = 0"))

# P-value > 5%, we fail to reject H0 that variables are jointly statistically insignificant, so we can remove `Departure Delay in Minutes`
# jointly with 'Food and drink'

logit_model2 <- glm(satisfaction ~ 
                      `Customer Type`
                    + `On-board service`
                    + `Baggage handling`
                    + `Inflight entertainment`
                    + `Arrival Delay in Minutes`
                    + `Flight Distance`
                    + `Departure/Arrival time convenient`
                    + `Ease of Online booking`
                    + `Online boarding`
                    + `Seat comfort`
                    + `Leg room service`
                    + Gender
                    + I(Age^2)
                    + Age
                    + `Gate location`
                    + Age:`Gate location`
                    + Age:`Ease of Online booking`
                    + `Checkin service`
                    + Cleanliness
                    +`Inflight service`
                    + wifi_good_bad
                    ,data = data, family = binomial(link = "logit"))


summary(logit_model2)

linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                                "`Food and drink`2 = 0",
                                "`Food and drink`3 = 0",
                                "`Food and drink`4 = 0",
                                "`Food and drink`5 = 0",
                                "`Ease of Online booking`2:Age= 0",
                                "`Ease of Online booking`3:Age= 0",
                                "`Ease of Online booking`4:Age= 0",
                                "`Ease of Online booking`5:Age= 0"))




linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                                "Age:`Gate location`2 = 0",
                                "Age:`Gate location`3 = 0",
                                "Age:`Gate location`4 = 0",
                                "Age:`Gate location`5 = 0",
                                "`Food and drink`2 = 0",
                                "`Food and drink`3 = 0",
                                "`Food and drink`4 = 0",
                                "`Food and drink`5 = 0"
                                ))

linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                                "`Food and drink`2 = 0",
                                "`Food and drink`3 = 0",
                                "`Food and drink`4 = 0",
                                "`Food and drink`5 = 0",
                                "`Inflight service`2 = 0",
                                "`Inflight service`3 = 0",
                                "`Inflight service`4 = 0",
                                "`Inflight service`5 = 0"))

linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                                "`Food and drink`2 = 0",
                                "`Food and drink`3 = 0",
                                "`Food and drink`4 = 0",
                                "`Food and drink`5 = 0",
                                "`Checkin service`2 = 0",
                                "`Checkin service`3 = 0",
                                "`Checkin service`4 = 0",
                                "`Checkin service`5 = 0"))

linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                                "`Food and drink`2 = 0",
                                "`Food and drink`3 = 0",
                                "`Food and drink`4 = 0",
                                "`Food and drink`5 = 0",
                                "Cleanliness2 = 0",
                                "Cleanliness3 = 0",
                                "Cleanliness4 = 0",
                                "Cleanliness5 = 0"))

linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                                "`Gate location`2 = 0",
                                "`Gate location`3 = 0",
                                "`Gate location`4 = 0",
                                "`Gate location`5 = 0",
                                "`Food and drink`2 = 0",
                                "`Food and drink`3 = 0",
                                "`Food and drink`4 = 0",
                                "`Food and drink`5 = 0"
))

linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                                "`Food and drink`2 = 0",
                                "`Food and drink`3 = 0",
                                "`Food and drink`4 = 0",
                                "`Food and drink`5 = 0",
                                "`Leg room service`2  = 0",
                                "`Leg room service`3  = 0",
                                "`Leg room service`4  = 0",
                                "`Leg room service`5  = 0"))

linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                                "`Food and drink`2 = 0",
                                "`Food and drink`3 = 0",
                                "`Food and drink`4 = 0",
                                "`Food and drink`5 = 0",
                                "`Seat comfort`2 = 0",
                                "`Seat comfort`3 = 0",
                                "`Seat comfort`4 = 0",
                                "`Seat comfort`5 = 0"
                                ))

linearHypothesis(logit_model,c("`Departure Delay in Minutes` = 0",
                               "`Food and drink`2 = 0",
                               "`Food and drink`3 = 0",
                               "`Food and drink`4 = 0",
                               "`Food and drink`5 = 0",
                              "`Online boarding`2 = 0",
                               "`Online boarding`3 = 0",
                               "`Online boarding`4 = 0",
                               "`Online boarding`5 = 0"))


linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                                "`Food and drink`2 = 0",
                                "`Food and drink`3 = 0",
                                "`Food and drink`4 = 0",
                                "`Food and drink`5 = 0",
                                "`Ease of Online booking`2 = 0",
                                "`Ease of Online booking`3 = 0",
                                "`Ease of Online booking`4 = 0",
                                "`Ease of Online booking`5 = 0"))

linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                                "`Food and drink`2 = 0",
                                "`Food and drink`3 = 0",
                                "`Food and drink`4 = 0",
                                "`Food and drink`5 = 0",
                                "`Departure/Arrival time convenient`2 = 0",
                                "`Departure/Arrival time convenient`3 = 0",
                                "`Departure/Arrival time convenient`4 = 0",
                                "`Departure/Arrival time convenient`5 = 0"))

linearHypothesis(logit_model, c("`Departure Delay in Minutes`= 0",
                                "`Food and drink`2 = 0",
                                "`Food and drink`3 = 0",
                                "`Food and drink`4 = 0",
                                "`Food and drink`5 = 0",
                                "`Baggage handling`2 = 0",
                                "`Baggage handling`3 = 0",
                                "`Baggage handling`4 = 0",
                                "`Baggage handling`5 = 0"))


# Besides `Departure Delay in Minutes` jointly with 'Food and Drinks'
# P-value < 5%, we reject H0 that variables are jointly statistically insignificant, so we cannot remove anything else with GETS


final_model <- glm(satisfaction ~ 
                     `Customer Type`
                   + `On-board service`
                   + `Baggage handling`
                   + `Inflight entertainment`
                   + `Arrival Delay in Minutes`
                   + `Flight Distance`
                   + `Departure/Arrival time convenient`
                   + `Ease of Online booking`
                   + `Online boarding`
                   + `Seat comfort`
                   + `Leg room service`
                   + Gender
                   + I(Age^2)
                   + Age
                   + `Gate location`
                   + Age:`Gate location`
                   + Age:`Ease of Online booking`
                   + `Checkin service`
                   + Cleanliness
                   +`Inflight service`
                   + wifi_good_bad
                   ,data = data, family = binomial(link = "logit"))

summary(final_model)

stargazer(probit_model,logit_model,logit_model1,final_model,type = "text")

# tests for the new  model

null <- glm(satisfaction~1,data = data,family=binomial(link="logit"))
lrtest(final_model,null)

# we reject H0 that restricted model is better - the variables in model are jointly statistically significant
# due to pvalue < 0.05 significance level

logitgof(data$satisfaction, fitted(final_model), g = 10)

o.r.test(final_model)

stukel.test(final_model)

# all diagnostic test passed, specification is correct
# pvalue > 0.05 so we fail to reject the null that specification is correct

summary(linktest(final_model))

# yhat2 statistically insiginifcant, yhat significant - model is correct

PseudoR2(final_model)

# 0.88 Count, 0.72 Adj.Count, 0.75 McKelvey-Zavoina

# --------------------- marginal effects --------------------------------------

# for mean observation

options(scipen = 5, digits=5)

logitmfx(formula = satisfaction ~ 
           `Customer Type`
         + `On-board service`
         + `Baggage handling`
         + `Inflight entertainment`
         + `Arrival Delay in Minutes`
         + `Flight Distance`
         + `Departure/Arrival time convenient`
         + `Ease of Online booking`
         + `Online boarding`
         + `Seat comfort`
         + `Leg room service`
         + Gender
         + I(Age^2)
         + Age
         + `Gate location`
         + Age:`Gate location`
         + Age:`Ease of Online booking`
         + `Checkin service`
         + Cleanliness
         +`Inflight service`
         + wifi_good_bad,
         data = data,
         atmean = T)


# Average marginal effects 


logitmfx(formula = satisfaction ~ 
           `Customer Type`
         + `On-board service`
         + `Baggage handling`
         + `Inflight entertainment`
         + `Arrival Delay in Minutes`
         + `Flight Distance`
         + `Departure/Arrival time convenient`
         + `Ease of Online booking`
         + `Online boarding`
         + `Seat comfort`
         + `Leg room service`
         + Gender
         + I(Age^2)
         + Age
         + `Gate location`
         + Age:`Gate location`
         + Age:`Ease of Online booking`
         + `Checkin service`
         + Cleanliness
         +`Inflight service`
         + wifi_good_bad,
         data = data,
         atmean = F)

