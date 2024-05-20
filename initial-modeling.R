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

