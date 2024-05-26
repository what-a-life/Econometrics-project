# previous code

#levels <- c(0, 1, 2, 3, 4, 5, 6)
#labels <- c("No WiFi", "WiFi Satisfaction 1", "WiFi Satisfaction 2", "WiFi Satisfaction 3", "WiFi Satisfaction 4", "WiFi Satisfaction 5")

#data$`Inflight wifi service` <- as.character(data$`Inflight wifi service`)

data <- data %>% filter(!(data$`Inflight wifi service` %in% 0))
data$`Inflight wifi service` <- factor(data$`Inflight wifi service`, levels = c(1, 2, 3, 4, 5), ordered = F)
table(data$`Inflight wifi service`)

data <- data %>% filter(!(data$`Departure/Arrival time convenient` %in% 0))
data$`Departure/Arrival time convenient` <- factor(data$`Departure/Arrival time convenient`, levels = c(1, 2, 3, 4, 5), ordered = F)
table(data$`Departure/Arrival time convenient`)

data <- data %>% filter(!(data$`Departure/Arrival time convenient` %in% 0))
data$`Departure/Arrival time convenient` <- factor(data$`Departure/Arrival time convenient`, levels = c(1, 2, 3, 4, 5), ordered = F)
table(data$`Departure/Arrival time convenient`)

#######################################################################################

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
#library(gofcat)

options(scipen = 5)

data <- read_csv('test.csv')

data[!complete.cases(data),]

data <- na.omit(data)

#glimpse(data)

#table(data$satisfaction)

data$satisfaction <- ifelse(data$satisfaction == 'satisfied', 1, 0)

#table(data$`Inflight wifi service`)

#hist(log(data$`Flight Distance`))

data <- subset(data, select = c(-...1,-id))

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

# Plot histograms for each variable
#par(mfrow=c(4, 4))  # Arrange plots in a 4x4 grid

#for (variable in variable_names) {
#  hist(data[[variable]], main = variable, xlab = "Satisfaction level")
#}


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

mylogit <- glm(satisfaction ~ . + I(Age^2) , data=data, family=binomial(link="logit"))

summary(mylogit)

# tests
null <- glm(satisfaction~1,data = data,family=binomial(link="logit"))
lrtest(mylogit,null)


logitgof(data$satisfaction, fitted(mylogit), g = 10)

o.r.test(mylogit)
#stukel.test(mylogit)

summary(linktest(mylogit))

mylogit <- glm(satisfaction ~ . + I(Age^2) + `Class`:`Type of Travel` +
                 
                 
                 , data=data, family=binomial(link="logit"))

summary(mylogit)
data$Cleanliness <- factor(ifelse(data$Cleanliness %in% c(1, 2), 'dissatisfied', 'satisfied'))

szpagat <- glm(satisfaction ~ Gender + `Customer Type` + `Type of Travel` + `Class`
               + I(log(`Arrival Delay in Minutes`+1))+I(log(`Departure Delay in Minutes`+1))
               + `Class`:`Type of Travel`
               + I(Age^2)
               + `Gate location`
               + Log_Flight_Distance

               , data=data, family=binomial(link="logit"))

logitgof(data$satisfaction, fitted(szpagat), g = 10)

o.r.test(szpagat)
stukel.test(szpagat)

summary(linktest(szpagat))

szpagetti2 <- glm(satisfaction ~ 
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

logitgof(data$satisfaction, fitted(szpagetti2), g = 10)

o.r.test(szpagetti2)
stukel.test(szpagetti2)

summary(linktest(szpagetti2))

summary(szpagetti2)


PseudoR2(szpagetti2)





model1 <- glm(satisfaction~
                `Customer Type`+
                
                `Arrival Delay in Minutes`+
                
                `Leg room service`+
                
                
                `Flight Distance`+
                
                `Departure/Arrival time convenient`+
                `Ease of Online booking`+
                
                
                `Online boarding`+
                `Seat comfort`
              ,
              data = data, family = binomial(link = "logit"))


logitgof(data$satisfaction, fitted(model1), g = 10)

o.r.test(model1)
stukel.test(model1)

summary(linktest(model1))

summary(model1)


# ---------------------- model choice ---------------------------------------

# Model choice: logit or ptobit?

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

o.r.test(probit_model)
stukel.test(probit_model)

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

logit_model1 <- glm(satisfaction ~ 
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

summary(logit_model1)


# tests for the new  model

logitgof(data$satisfaction, fitted(logit_model1), g = 10)

o.r.test(logit_model1)

stukel.test(logit_model1)

summary(linktest(logit_model1))

summary(logit_model1)

PseudoR2(logit_model1)

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

