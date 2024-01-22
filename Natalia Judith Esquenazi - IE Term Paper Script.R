

### Term Paper Natalia Judith Esquenazi 
## December 2023

# Import libraries 

library(tidyverse)
library(ggplot2)
library(dplyr)
library(summarytools)
library(moments)
library(stargazer)
library(MASS)
library(texreg)
library(Hmisc)
library(texreg)
library(tidyr)
library(readxl)
library(hrbrthemes)
library(fixest)
library(modelsummary)
library(estimatr)
library(texreg)
library(plm)


# Working Environment
setwd("C:/Users/usuario/Desktop/Masters Degree CEU/Impact Evaluation/Term Paper")
setwd("~/Impact Evaluation Term Paper")

# Import the data 
df <- read_excel("C:/Users/usuario/Desktop/Masters Degree CEU/Impact Evaluation/Term Paper/new_sample.xlsx", col_types = c("numeric", "text", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "text", "numeric", "numeric"))

# Data Visualization -----------------------------------

# School performance in public schools by year
new_df2 <- df %>%
  select(sector, performance, year) %>%
  group_by(sector, year) %>%
  summarise(avg_performance = mean(performance))

new_df2 %>%
  ggplot( aes(x=as.factor(year), y=avg_performance, color=as.factor(sector), 
              group=as.factor(sector))) +
  geom_line() +
  ylab("Average Performance") +
  xlab("Year") +
  ggtitle("Graph 1: School Performance in Public and Private Schools")+
  theme_ipsum() +
  geom_text(aes(label= round(avg_performance)),vjust=1)+
  scale_color_manual(values = c("1", "2"), labels = c("Public", "Private"), name="Sector")


# Socioeconomic level, performance and sector 
new_df3 <- df %>%
  select(ID, income_level, performance, sector, year) %>%
  group_by(sector, year, income_level) %>%
  summarise(avg_performance = mean(performance)) 

new_df3 %>%
  filter(sector == 1 & income_level == 2) %>%
  ggplot( aes(x=as.factor(year), y=avg_performance, color=as.factor(income_level), 
              group=as.factor(income_level))) +
  geom_line() +
  ylab("Average Performance") +
  xlab("Year") +
  ggtitle("Graph 2: School Performance by Income Level in Public Schools")+
  theme_ipsum() +
  geom_text(aes(label= round(avg_performance)),vjust=1)+
  scale_color_manual(values = c("1"), labels = c("Low"), name="Income Level")

## Regression Model ------------------------------------------------------------

new_df4 <- df %>%
  select(ID, treatment, performance, year) %>%
  group_by(ID, treatment, year) %>%
  summarise(avg_performance = mean(performance))

# Creating a unique school ID for years 2018, 2021 and 2022
# new_df4$school_id <- paste(new_df4$ID, new_df4$year, sep = "")

# Simple Regression 
model1 <- lm(avg_performance ~ treatment, data = new_df4)
summary(model1)

# Simple Regression including year
model2 <- lm(avg_performance ~ treatment + year, data = new_df4)
summary(model2)

stargazer(model1, model2 , title="Regression Results", 
          align=TRUE,
          column.labels = c("Model 1", "Model 2"),
          omit.stat=c("LL","f"),
          out= "table1.txt",
          no.space=TRUE)

## Panel Data Fixed Effects ------------------------------------------------------------

# Panel Data: Schools Fixed effects 
model3 <- feols(avg_performance ~ treatment | ID, data = new_df4)
summary(model3)

# Panel Data: Year Fixed effects 
model4 <- feols(avg_performance ~ treatment | year, data = new_df4)
summary(model4)

# Panel Data: School and Year Fixed effects 
model5 <- feols(avg_performance ~ treatment | ID + year, data = new_df4)
summary(model5)

table2 <- screenreg(list(model3, model4, model5),
                    model.names = c("School Fixed", "Year Fixed", "School and Year Fixed"))

capture.output(cat(table2), file = "table2.txt")

# Panel Data incorporating Income levels
new_df5 <- df %>%
  select(ID, treatment, performance, year, income_level) %>%
  group_by(ID, income_level, year, treatment) %>%
  summarise(avg_performance = mean(performance)) 

# Creating a unique school ID for income level
new_df5$school_id <- paste(new_df5$ID, new_df5$income_level, sep = "")

# Simple Regression
model6 <- lm(avg_performance ~ treatment + as.factor(income_level), data = new_df5)
summary(model6)

# Panel Data: School Fixed effects 
model7 <- feols(avg_performance ~ treatment | school_id, data = new_df5)
summary(model7)

# Panel Data: Year Fixed effects 
model8 <- feols(avg_performance ~ treatment | year, data = new_df5)
summary(model8)

# Panel Data: School and Year Fixed effects 
model9 <- feols(avg_performance ~ treatment | school_id + year, data = new_df5)
summary(model9)

table3 <- screenreg(list(model6, model7, model8, model9))

capture.output(cat(table3), file = "table3.txt")

## Difference in Differences --------------------------------------------------

new_df6 <- df %>%
  select(ID, treatment, performance, time, year) %>%
  group_by(ID, year, time, treatment) %>%
  summarise(avg_performance = mean(performance)) 

# Creating a unique school ID for income level
new_df6$school_id <- paste(new_df6$ID, new_df6$year, sep = "")

# Parallel trends 
new_df2 %>%
  ggplot( aes(x=as.factor(year), y=avg_performance, color=as.factor(sector), 
              group=as.factor(sector))) +
  geom_line() +
  ylab("Average Performance") +
  xlab("Year") +
  ggtitle("Graph 3: Paralell Trends")+
  theme_ipsum() +
  geom_text(aes(label= round(avg_performance)),vjust=1)+
  scale_color_manual(values = c("1", "2"), labels = c("Treatment", "Control"), name="Treatment")


# Difference in Differences
model10 <- lm(avg_performance ~ treatment + time + I(treatment * time), data = new_df6)
summary(model10)

stargazer(model10,title="Difference in Differences Results", 
          align=TRUE,
          column.labels = c("Model 1"),
          omit.stat=c("LL","f"),
          out= "table4.txt",
          no.space=TRUE)


  
  















