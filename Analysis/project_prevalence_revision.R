rm(list=ls())
library(socialmixr)
library(epimixr)
library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(remotes)
library(readxl)

serologyn<-read_excel("data/Immunity_model_input_data.xlsx",sheet="JULY") #serology data
mcv<-read_excel("data/Immunity_model_input_data.xlsx",sheet="mcv") #coverage data
contact<-read_excel("data/Immunity_model_input_data.xlsx",sheet="matrixn") #coverage data
pop<-read_excel("data/Immunity_model_input_data.xlsx",sheet="population") #coverage data
#data_19=data_19 %>% filter(Year==2019) 

seroprevalence <- serologyn 

## determine lower bounds of age groups
lower_age_groups <- tibble(group=colnames(contact)) %>%
  separate(group, c("low", "high"), sep="-", fill="right") %>%
  mutate(low=parse_number(low)) %>%
  .$low

cvm <- mcv %>%
  spread(year, uptake) %>%   
  select(-vaccine) %>%
  as.matrix

## assume 2019 like 2018
cvm <- cbind(cvm, cvm[, ncol(cvm)])   ##no 2019 coverage so use 2018 to look like 2019 and label it
colnames(cvm)[ncol(cvm)] <- "2019"


cvm <- cbind(cvm, cvm[, ncol(cvm)])   ##2020 coverage same as 2019 ##scenario2
colnames(cvm)[ncol(cvm)] <- "2020"

#cvm <- cbind(cvm, c(0.40,0.23))        ##50% reduction      ##scenario3
#colnames(cvm)[ncol(cvm)] <- "2020"

#cvm <- cbind(cvm, c(0,0))  ##no 2020 coverage 1st scenario 0 coverage
#colnames(cvm)[ncol(cvm)] <- "2020"

maternal_immunity <- seroprevalence %>%  
  filter(lower.age.limit==0) %>%
  .$seropositive

baseline_immunity <- seroprevalence$seropositive
names(baseline_immunity) <- seroprevalence$lower.age.limit

## project immunity
shifted_immunity <-
  project_immunity(baseline.immunity = baseline_immunity,
                   baseline.year = 2019,
                   year = 2020,
                   coverage = cvm,
                   schedule = c(0, 2),
                   maternal.immunity = maternal_immunity,
                   efficacy = 0.95)

seroprevalence_shifted <- seroprevalence %>%
  mutate(seropositive=shifted_immunity,
         perspective="2020")



## group seroprevalence data in age groups of contact survey
seroprevalence_grouped <- seroprevalence %>%
  rbind(seroprevalence_shifted) %>%
  mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
  left_join(pop %>% mutate(perspective=as.character(Year)) %>% select(-Year),
            by=c("lower.age.limit", "perspective")) %>%
  group_by(perspective) %>%
  mutate(age_group=limits_to_agegroups(age_group)) %>%
  group_by(age_group, perspective) %>%
  summarise(prop.immune=sum(seropositive * Population) / sum(Population),
            Population=sum(Population)) %>%
  ungroup()

seroprevalencen=seroprevalence_grouped


#write.csv(seroprevalence_grouped,file="20_based_17.csv")






