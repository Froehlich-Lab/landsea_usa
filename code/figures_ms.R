##HE Froehlich
##Nov 21 2023
###Figs for the paper


rm(list=ls())

library(ggplot2)
library(lattice)
library(tidyverse)
library(dplyr)
library(lme4)
library(multcomp)
library(naniar)
library(usmap)
library(viridis)
library(reshape) #transpose data
library(patchwork)
library(modelsummary)
library(see)
library(ggraph)
library(ROCR)
library(randomForest)
library(pdp)

#Read in all data
data<-read_csv("Combined_cleaned_data_forR.csv")
head(data)

##***********FIGURE 1: General Respondent Frequency*********
##Tabulate frequency of responses for Figure 1 (upper panel)
freq_data <- table(data$poltical2, data$age, data$gender)
freq_data <- as.data.frame(freq_data)
names(freq_data)<- c("pol", "age", "gender", "freq")
freq_data


p1<-ggplot(freq_data, aes(x = age, y = freq, fill=pol))+
  labs(x = 'age', y = "frequency")+ 
  geom_bar(stat = "identity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=11),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_brewer()+
  facet_wrap( ~  gender)

freq_data <- table(data$US_census_region, data$income_2, data$gender)
freq_data <- as.data.frame(freq_data)
names(freq_data)<- c("region", "income_2", "gender", "freq")
freq_data

levels(freq_data$income_2)
freq_data$income_2 <- ordered(freq_data$income_2,
                           levels = c("high", "middle", "low", "wont_say"))

##Tabulate frequency of responses for Figure 1 (lower panel)
p2<-ggplot(freq_data, aes(x = income_2, y = freq, fill=region))+
  labs(x = 'income', y = "frequency")+ 
  geom_bar(stat = "identity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=11),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_brewer()+
  facet_wrap( ~  gender)

p1/p2


##----FIGURE 2 Land v SEA Q6 & Q13--------------------------------
##read in specific land versus sea questions
data <- read_csv("LandvSEA.csv") #specific questions comparing land vs sea for total awareness & overall concern
data2<-read_csv("IMPACT_variables.csv") #conservation need per ecosystem
head(data2)

aware<-ggplot(data, aes(x = survey, y = Q6_total_awareness, fill =survey))+
  labs(x = '', y = "Total Awarness")+ 
  geom_violin() +
  geom_boxplot(width=0.1)+
  #geom_jitter()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=11),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_brewer()

concern <-ggplot(data, aes(x = survey, y = Q13_concern_fish, fill =survey))+
  labs(x = '', y = "Overall Concern")+ 
  #geom_jitter()+
  geom_violin() +
  geom_boxplot(width=0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=11),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_brewer()

freq_data <- table(data2$Q8Cons_need , data2$Q8Cons_need_score)
freq_data <- as.data.frame(freq_data)
names(freq_data)<- c("Environment", "Score", "Freq")
freq_data$Score <- as.numeric(freq_data$Score)/5
freq_data

levels(freq_data$Environment)
freq_data$Environment <- ordered(freq_data$Environment ,
                                 levels = c("coast", "open_ocean", "lakes_rivers"
                                            ,"plains_grasslands", "forests", "mountains"))

need<-ggplot(freq_data, aes(x = Environment, y = Freq, fill= as.factor(Score)))+
  labs(x = '', y = "Frequency of Responses")+ 
  geom_bar(stat = "identity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=11),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()+
  scale_fill_brewer()

(aware + concern)/need

##----FIGURE 3 Land v SEA--------------------------------
data <- read_csv("LandvSEA.csv")
head(data)

q9<-ggplot(data, aes(x = survey, y = Q9_percent, fill =survey))+
  labs(x = '', y = "% Funding Allocation")+ 
  #geom_jitter()+
  geom_violin() +
  geom_boxplot(width=0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=11),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))+
  scale_fill_brewer()

data21 <- read_csv("LandvSea_Q21.csv")
head(data21)

freq_data <- table(data21$survey, data21$Q21, data21$binary)
freq_data <- as.data.frame(freq_data)
names(freq_data)<- c("Survey", "Type", "Response", "Freq")
freq_data

levels(freq_data$Type)
freq_data$Type <- ordered(freq_data$Type ,
                                 levels = c("none", "market", "donate", "programs", "invest_govregs", "partnerships", "penalties"))

q21<-ggplot(freq_data, aes(x = Type, y = Freq, fill= Response))+
  labs(x = '', y = "Frequency of Responses")+ 
  geom_bar(stat = "identity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=11),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()+
  scale_fill_brewer()+
  facet_wrap(~Survey)

q9/q21

##******************FIGURE 4 RANDOM FOREST RESULTS***********************
full.rf <- read_csv("Full_model_results.csv")
head(full.rf)

ggplot(full.rf, aes(x = reorder(variable_detail, MSE), y = MSE, color=prune))+
  labs(x = '', y = "%MSE")+ 
  geom_point(aes(size=11))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=11),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))+
  coord_flip()+
  scale_fill_brewer()+
  facet_wrap(~model)


##************FIGURE 5 OUTDOOR RECREATION********************
data3<-read_csv("Combined_reduced_Rmodel.csv")
head(data3)
#remove Ids
data <- data3[,-2]
#omit NAs
data <- na.omit(data3)
#make dataframe
is.data.frame(data3)

set.seed(8)
rf.fit <- randomForest(Q7_impact_me ~ Q8_total_need + Q21_fund_source_penalties + Q11_habitat_health + Q14_voted + Q4_total + Q14_donated, data=data3, ntree=1000, importance=TRUE)
rf.fit

##Partial Plots of specific predictors:
rf.fit %>% # the %>% operator is read as "and then"
  partial(pred.var = "Q4_total") %>%
  plotPartial(smooth = FALSE, lwd = 2, xlab = "Total # of activities", ylab = "Impact to me and/or my community")

data4<-read_csv("Combined_cleaned_data_forR.csv")
head(data4)

p2<-ggplot(data4, aes(x = Q4_fish_hunt, y = Q7_impact_me))+
  labs(x = 'Total number of activities', y = "Impact to me or my community")+ 
  geom_boxplot() +
  geom_jitter(alpha = 1/3, aes(color = Q4_total))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=11),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))+
  scale_fill_brewer()


##---read in reponses on outdoor activity type-----
data5 <- read_csv("LandvSea_Q4.csv")
head(data5)


freq_data5 <- table(data5$activity, data5$binary)
freq_data5 <- as.data.frame(freq_data5)
names(freq_data5)<- c("act", "response", "freq")
freq_data5
levels(freq_data5$act)
freq_data$act <- ordered(freq_data5$act ,
                         levels = c("camping", "hiking", "fishing"
                                    ,"sailing", "snorkeling/diving", "hunting", "none"))

p3<-ggplot(freq_data5, aes(x = act, y = freq, fill= response))+
  labs(x = '', y = "Frequency of Responses")+ 
  geom_bar(stat = "identity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text.x  = element_text(size=11),axis.text.y  = element_text(size=11),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA))+
  scale_y_continuous(expand = c(0,0))+
  coord_flip()+
  scale_fill_brewer()

p2/p3