##HE Froehlich
##Nov 15 2023
## Land vs Sea Qs

rm(list=ls())

library(readr)
library(ggplot2)
library(lattice)
library(tidyverse)
library(dplyr)
library(lme4)
library(multcomp)


data <- read_csv("LandvSEA.csv")
head(data)

##******Q6 Which of the following environmental issues are you aware of?*****
kruskal.test(Q6_overfish_deforest ~ survey, data = data) 
kruskal.test(Q6_melt_extremes ~ survey, data = data) 
kruskal.test(Q6_plastic_waste ~ survey, data = data) 
kruskal.test(Q6_oil_toxics ~ survey, data = data) 
kruskal.test(Q7_oa_soil ~ survey, data = data) 

##*****total awareness (combined Q6 scoring and nomalized)*******
kruskal.test(Q6_total_awareness ~ survey, data = data) 

##************Q13: Please rate how concerned you are personally about the current health of ocean fish & wildlife and ocean habitats in the United States?***********
kruskal.test(Q13_concern_fish ~ survey, data = data)

##**********Q9 The budget needs to be split between ocean & marine vs terrestrial (on-land) environments and wildlife. In your opinion, what percent of the budget would you put towards ocean & marine conservation?****
kruskal.test(Q9_percent ~ survey, data = data)

##******************Q10_animal_health <dbl>, Q11_habitat_health <dbl>********************
kruskal.test(Q10_animal_health ~ survey, data = data)

kruskal.test(Q11_habitat_health ~ survey, data = data)

##******************Q19_scenario_effective ********************
kruskal.test(Q19_scenario_effective ~ survey, data = data) 

##******************Q20_conservation intervention type ********************
kruskal.test(Q20_supplament ~ survey, data = data) 

##******************Q21_funding source ********************
kruskal.test(Q21_total_funds ~ survey, data = data) 


##***********Q8 is there a need for active conservation efforts in each of the following environments? If so, what level of active conservation effort does each need?**** 
data2<-read_csv("IMPACT_variables.csv") 
head(data2)

#scored and normalized total conservation need
kruskal.test(Q8Cons_need_score/5 ~ Q8Cons_need, data = data2) 


