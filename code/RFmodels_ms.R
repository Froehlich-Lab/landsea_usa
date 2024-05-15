##HE Froehlich
##Nov 7 2023
##PollFish clean survey results (from August 2021)
##Random Forest Models

rm(list=ls())

library(ggplot2)
library(lattice)
library(tidyverse)
library(dplyr)
library(lme4)
library(multcomp)
library(reshape)
library(patchwork)
library(modelsummary)


data<-read_csv("Combined_reduced_Rmodel.csv")
head(data)
data <- na.omit(data)

#Random forests improve predictive accuracy by generating a large number of 
#bootstrapped trees (based on random samples of variables), classifying a case 
#using each tree in this new "forest", and deciding a final predicted outcome 
#by combining the results across all of the trees (an average in regression, 
#a majority vote in classification). 
library(ROCR)
library(randomForest)
library(pdp)

##----------------------------
##---Random Forest---------
##-------------------------------
#******Predict what variables influence perceived impact to me or my community*****
#FULL MODEL
set.seed(8)
rf.fit <- randomForest(Q7_impact_me ~ age +  gender + no_kids + education + income + political2 + US_census_region + community + survey +
                         Q4_fish_hunt +  Q4_total + Q14_voted + Q14_donated + Q15_voting_effective + Q16_participate + Q18_aware_org + Q21_fund_source_penalties+
                         Q8_total_need + Q10_animal_health + Q11_habitat_health + Q13_concern_both + Q21_fund_source_partnerships + Q21_fund_source_market, 
                    data=data, ntree=1000, importance=TRUE)

print(rf.fit) # view results 
importance(rf.fit) # importance of each predictor
varImpPlot(rf.fit, main="Impact to me or my community")


#Check prune fit (variance explained)
set.seed(8)
rf.fit <- randomForest(Q7_impact_me ~ Q8_total_need, data=data, ntree=1000, importance=TRUE)
rf.fit <- randomForest(Q7_impact_me ~ Q8_total_need + Q21_fund_source_penalties, data=data, ntree=1000, importance=TRUE)
rf.fit <- randomForest(Q7_impact_me ~ Q8_total_need + Q21_fund_source_penalties + Q11_habitat_health, data=data, ntree=1000, importance=TRUE)
rf.fit <- randomForest(Q7_impact_me ~ Q8_total_need + Q21_fund_source_penalties+ Q11_habitat_health + Q14_voted, data=data, ntree=1000, importance=TRUE)
rf.fit <- randomForest(Q7_impact_me ~ Q8_total_need + Q21_fund_source_penalties + Q11_habitat_health + Q14_voted + Q4_total, data=data, ntree=1000, importance=TRUE)
rf.fit <- randomForest(Q7_impact_me ~ Q8_total_need + Q21_fund_source_penalties + Q11_habitat_health + Q14_voted + Q4_total + Q14_donated, data=data, ntree=1000, importance=TRUE)

print(rf.fit) # view results 
importance(rf.fit) # importance of each predictor
varImpPlot(rf.fit, main="Impact to me or my community")

##Partial Plots of specific predictors:
rf.fit %>% # the %>% operator is read as "and then"
  partial(pred.var = "Q4_total") %>%
  plotPartial(smooth = TRUE, lwd = 2, ylab = expression(Q7_impact_me))


#******Predict what variables influence perception of need for conservation*****
##FULL MODEL
set.seed(8)
rf.fit2 <- randomForest(Q8_total_need ~ age +  gender + no_kids + education + income + political2 + US_census_region + community + survey +
                          Q4_fish_hunt +  Q4_total + Q14_voted + Q14_donated + Q15_voting_effective + Q16_participate + Q18_aware_org + Q21_fund_source_penalties+
                          Q7_impact_me + Q10_animal_health + Q11_habitat_health + Q13_concern_both + Q21_fund_source_partnerships + Q21_fund_source_market, 
                       data=data, ntree=1000, importance=TRUE)

print(rf.fit2) # view results 
importance(rf.fit2) # importance of each predictor
varImpPlot(rf.fit2, main="need for active conservation efforts")

#Check prune fit (variance explained)
set.seed(8)
rf.fit <- randomForest(Q8_total_need ~ Q7_impact_me 
                       ,data=data, ntree=1000, importance=TRUE)
rf.fit <- randomForest(Q8_total_need ~ Q7_impact_me + Q13_concern_both
                       ,data=data, ntree=1000, importance=TRUE)
rf.fit <- randomForest(Q8_total_need ~ Q7_impact_me + Q13_concern_both + Q15_voting_effective 
                       ,data=data, ntree=1000, importance=TRUE)
rf.fit <- randomForest(Q8_total_need ~ Q7_impact_me + Q13_concern_both + Q15_voting_effective + Q11_habitat_health 
                       ,data=data, ntree=1000, importance=TRUE)
print(rf.fit) # view results 
importance(rf.fit) # importance of each predictor
varImpPlot(rf.fit, main="need for active conservation efforts")
