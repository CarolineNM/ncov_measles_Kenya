library(socialmixr)
library(epimixr)
library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(remotes)
library(readxl)


contact<-read_excel("data/Immunity_model_input_data.xlsx",sheet="reduced_matrix") #coverage data
contact=as.matrix(contact) 

seroprevalence <- seroprevalencen

# ## determine number of seroprevalence age groups after adjusting
# nsero <- length(unique(seroprevalence$age_group))
# 
# ## adjust contact columns that don't exist in seroprevalence
# contacts[, nsero] <- rowSums(contacts[, nsero:ncol(contacts)])
# contacts <- contacts[1:nsero, 1:nsero]

adjusted_immunity <- seroprevalence %>%
  group_by(perspective) %>%
  summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
  mutate(type="Contact-adjusted")


plain_immunity <- seroprevalence %>%
  group_by(perspective) %>%
  summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
  mutate(type="Plain")


list(
  by_age=
    seroprevalence %>%
    select(-Population) %>%
    mutate(prop.immune=round(prop.immune, 3)) %>%
    spread(perspective, prop.immune) %>%
    rename(`Age group (years)`=age_group),
  aggregate=
    rbind(
      plain_immunity %>%
        mutate(immunity=round(immunity, 3)) %>%
        spread(perspective, immunity),
      adjusted_immunity %>%
        mutate(immunity=round(immunity, 3)) %>%
        spread(perspective, immunity)
    )
)



str(contact)