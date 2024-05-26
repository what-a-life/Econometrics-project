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


options(scipen = 5)

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

data <- data %>%
  mutate(Log_Flight_Distance = log(`Flight Distance` + 1))

par(mfrow = c(1, 1))
source("linktest.R")
source("AllGOFTests.R")


# ---------------------- model choice ---------------------------------------

# Model choice: logit or probit?

logit_model <- glm(satisfaction ~ 
                    `Customer Type` 
                  + `Arrival Delay in Minutes`
                  + `Departure Delay in Minutes`
                  + `Flight Distance`
                  + `Departure/Arrival time convenient`
                  + `Ease of Online booking`
                  + `Online boarding`
                  + `Seat comfort`
                  + `Leg room service`
                  + Gender
                  + Age:`Gate location`
                  + Age:`Ease of Online booking`
                  + Age:`Checkin service`
                  + Age:`Food and drink`
                  + Cleanliness
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
                   + `Arrival Delay in Minutes`
                   + `Departure Delay in Minutes`
                   + `Flight Distance`
                   + `Departure/Arrival time convenient`
                   + `Ease of Online booking`
                   + `Online boarding`
                   + `Seat comfort`
                   + `Leg room service`
                   + Gender
                   + Age:`Gate location`
                   + Age:`Ease of Online booking`
                   + Age:`Checkin service`
                   + Age:`Food and drink`
                   + Cleanliness
                   ,data = data, family = binomial(link = "probit"))

logitgof(data$satisfaction, fitted(probit_model), g = 10)

summary(linktest(probit_model))

summary(probit_model)


PseudoR2(probit_model)

# Based on this analysis we should choose logit model as it passes all the tests and 
# have lower AIC value


# ------------------------General to specific approach --------------------------------

# testing for joint insignificance of the levels

linearHypothesis(logit_model, c("`Departure/Arrival time convenient`2 = 0",
                                "`Departure/Arrival time convenient`3 = 0",
                                "`Departure/Arrival time convenient`4 = 0",
                                "`Departure/Arrival time convenient`5 = 0"))

linearHypothesis(logit_model, c("`Ease of Online booking`2 = 0",
                                "`Ease of Online booking`3 = 0",
                                "`Ease of Online booking`4 = 0",
                                "`Ease of Online booking`5 = 0"))

linearHypothesis(logit_model,c("`Online boarding`2 = 0",
                               "`Online boarding`3 = 0",
                               "`Online boarding`4 = 0",
                               "`Online boarding`5 = 0"))

linearHypothesis(logit_model, c("`Seat comfort`2 = 0",
                                "`Seat comfort`3 = 0",
                                "`Seat comfort`4 = 0",
                                "`Seat comfort`5 = 0"))

linearHypothesis(logit_model, c("Cleanliness2 = 0",
                                "Cleanliness3 = 0",
                                "Cleanliness4 = 0",
                                "Cleanliness5 = 0"))

linearHypothesis(logit_model, c("Age:`Gate location`1 = 0",
                                "Age:`Gate location`2 = 0",
                                "Age:`Gate location`3 = 0",
                                "Age:`Gate location`4 = 0",
                                "Age:`Gate location`5 = 0"))

linearHypothesis(logit_model, c("`Ease of Online booking`2:Age= 0",
                                "`Ease of Online booking`3:Age= 0",
                                "`Ease of Online booking`4:Age= 0",
                                "`Ease of Online booking`5:Age= 0"))

linearHypothesis(logit_model, c("Age:`Checkin service`2 = 0",
                                "Age:`Checkin service`3 = 0",
                                "Age:`Checkin service`4 = 0",
                                "Age:`Checkin service`5 = 0"))


# We cannot eliminate any of these variables as the levels are jointly significant 

linearHypothesis(logit_model, "`Departure Delay in Minutes`= 0")

# P-value > 5%, we fail to reject H0

final_model <- glm(satisfaction ~ 
                      `Customer Type` 
                    + `Arrival Delay in Minutes`
                    + `Flight Distance`
                    + `Departure/Arrival time convenient`
                    + `Ease of Online booking`
                    + `Online boarding`
                    + `Seat comfort`
                    + `Leg room service`
                    + Gender
                    + Age:`Gate location`
                    + Age:`Ease of Online booking`
                    + Age:`Checkin service`
                    + Age:`Food and drink`
                    + Cleanliness
                    ,data = data, family = binomial(link = "logit"))

summary(final_model)


# tests for the new  model

null <- glm(satisfaction~1,data = data,family=binomial(link="logit"))
lrtest(final_model,null)

logitgof(data$satisfaction, fitted(final_model), g = 10)

o.r.test(final_model)

stukel.test(final_model)

summary(linktest(final_model))


PseudoR2(final_model)

# --------------------- marginal effects --------------------------------------

# for mean observation

logitmfx(formula = satisfaction ~ 
           `Customer Type` 
         + `Arrival Delay in Minutes`
         + `Flight Distance`
         + `Departure/Arrival time convenient`
         + `Ease of Online booking`
         + `Online boarding`
         + `Seat comfort`
         + `Leg room service`
         + Gender
         + Age:`Gate location`
         + Age:`Ease of Online booking`
         + Age:`Checkin service`
         + Age:`Food and drink`
         + Cleanliness,
         data = data,
         atmean = T)


# Average marginal effects 


logitmfx(formula = satisfaction ~ 
           `Customer Type` 
         + `Arrival Delay in Minutes`
         + `Flight Distance`
         + `Departure/Arrival time convenient`
         + `Ease of Online booking`
         + `Online boarding`
         + `Seat comfort`
         + `Leg room service`
         + Gender
         + Age:`Gate location`
         + Age:`Ease of Online booking`
         + Age:`Checkin service`
         + Age:`Food and drink`
         + Cleanliness,
         data = data,
         atmean = F)

