rm(list=ls())
options(dplyr.summarise.inform=F) 
library(socialmixr)
library(epimixr)
library(dplyr)
library(tidyverse)
library(readr)
library(tidyr)
library(tibble)
library(remotes)
library(readxl)
library(rriskDistributions)

serologyn<-read_excel("D:/MANUSCRIPT/ncov_measles_Kenya/data/monthly_modelinput.xlsx",sheet="newre-grouped")
immunity_adults<-read_excel("D:/MANUSCRIPT/ncov_measles_Kenya/data/monthly_modelinput.xlsx",sheet="adults")
contactn<-read_excel("D:/MANUSCRIPT/ncov_measles_Kenya/data/monthly_modelinput.xlsx",sheet="contact2") #contact data
pop<-read_excel("D:/MANUSCRIPT/ncov_measles_Kenya/data/monthly_modelinput.xlsx",sheet="pop") #population data
#seroprevalence<-read_excel("D:/MANUSCRIPT/ncov_measles_Kenya/data/monthly_modelinput.xlsx",sheet="year3")
#contact=as.matrix(contact)

lower_age_groups <- tibble(group=colnames(contactn)) %>%
  separate(group, c("low", "high"), sep="-", fill="right") %>%
  mutate(low=parse_number(low)) %>%
  .$low

df=data.frame(
  Age_groups=c("0","12_60","72_180","192_228","240_588",">588"),
  Population=c(9617,45170,68547,33289,72143,24214))


get_mixing_mat <- function(df_mixing_by_id_prop,df){
  # create proposed contact matrix
  pop=df_mixing_by_id_prop %>% group_by(p_agegp) %>% summarise(count=length(local_id))
  Contact_matrix_raw = df_mixing_by_id_prop %>% group_by(p_agegp) %>% 
    summarise_at(paste0("agegp",1:length(unique(df_mixing_by_id_prop$p_agegp))), funs(sum(.,na.rm=T)))
  Contact_matrix_raw = Contact_matrix_raw[,-1] %>% as.matrix() %>% t()
  M = t(t(Contact_matrix_raw)/pop$count) # col=participants; row=contacts
  names(dimnames(M))=list("contact","participant")
  
  M=t(M)*df$Population
  M=(M+t(M))/2   # adjust for symetry
  M=M/df$Population
  Mixing_mat=(t(M)/df$Population)*7 # per capita per week
  rownames(Mixing_mat)=df$Age_groups
  colnames(Mixing_mat)=rownames(Mixing_mat)
  return(Mixing_mat) 
}
mycontacts <- read.csv("D:/NewTCV/Lwak/contact_data.csv", header = T)
mycontacts$age_class_cont<-car::recode(mycontacts$age_class_cont, '"Infant"=1;"Pre-School"=2;"Primary"=3; "Secondary"=4; "Adult"=5;"Older"=6')
mycontacts<- mycontacts[,c("csid","age_class_part","age_class_cont")]
contacts2=as.data.frame(mycontacts)

contacts=vector(length(4000),mode="list")
pop_20=pop %>% mutate(Year=c(rep(19,72),rep(20,72)))
g=vector(length(4000),mode="list")
seroprevalence_grouped_20=vector(length(4000),mode="list")
seroprevalence_shifted_20=vector(length(4000),mode="list")
output_20= vector(length(4000),mode="list")
cvm_20=vector(length(4000),mode="list")
young_immunity=vector(length(4000),mode="list")
combinedimmunity=vector(length(4000),mode="list")
baseline_immunity=vector(length(4000),mode="list")
combined=vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
adults=vector(length(4000),mode="list")
datan=vector(length(4000),mode="list")


for (i in 1:4000){
  contacts[[i]]=contacts2 %>% sample_n(10042,replace=T)
  contacts[[i]] <- contacts[[i]] %>% group_by(csid,age_class_cont) %>% mutate(num=n())
  contacts[[i]] <- contacts[[i]] %>% distinct(.keep_all = T)
  contacts[[i]] <- data.frame(contacts[[i]])
  contacts[[i]] <- reshape(contacts[[i]],v.names="num", idvar = c("csid","age_class_part"), timevar="age_class_cont",direction = "wide")
  contacts[[i]] <- contacts[[i]][,c("csid","age_class_part","num.1","num.2","num.3","num.4","num.5","num.6")]
  names(contacts[[i]]) <- c("local_id","p_agegp",paste0("agegp",1:6))
  contacts[[i]]$p_agegp <- paste0("agegp",contacts[[i]]$p_agegp)
  contact = get_mixing_mat(contacts[[i]],df)
  
  ###otherdata
  combined[[i]]=serologyn %>% sample_n(497,replace=T)
  combined[[i]]$Test=factor(combined[[i]]$Test)
  agegroups=data.frame(unique(serologyn$lower.age.limit))
  colnames(agegroups)="lower.age.limit"
  young_immunity[[i]]=data.frame(combined[[i]]) %>% select(lower.age.limit,Test) %>% group_by(lower.age.limit,Test,.drop = FALSE) %>% 
    summarise(total=n()) %>% mutate(seropositive=total/sum(total)) %>% filter(Test=="Positive",.preserve = TRUE) %>% ungroup()
  if(!0%in%young_immunity[[i]]$lower.age.limit){(young_immunity[[i]]=young_immunity[[i]] %>% add_row(lower.age.limit = 0,Test = "Positive", total=0,seropositive=0.05,.before = 1))}else {(young_immunity[[i]]=young_immunity[[i]])}
  if(!9%in%young_immunity[[i]]$lower.age.limit){(young_immunity[[i]]=young_immunity[[i]] %>% add_row(lower.age.limit = 9,Test = "Positive", total=0,seropositive=0.5,.before = 2))}else {(young_immunity[[i]]=young_immunity[[i]])}
  if(!12%in%young_immunity[[i]]$lower.age.limit){(young_immunity[[i]]=young_immunity[[i]] %>% add_row(lower.age.limit = 12,Test = "Positive", total=0,seropositive=0.94,.before = 3))}else {(young_immunity[[i]]=young_immunity[[i]])}
  if(!24%in%young_immunity[[i]]$lower.age.limit){(young_immunity[[i]]=young_immunity[[i]] %>% add_row(lower.age.limit = 24,Test = "Positive", total=0,seropositive=0.89,.before = 4))}else {(young_immunity[[i]]=young_immunity[[i]])}
  if(!36%in%young_immunity[[i]]$lower.age.limit){(young_immunity[[i]]=young_immunity[[i]] %>% add_row(lower.age.limit = 36,Test = "Positive", total=0,seropositive=0.92,.before = 5))}else {(young_immunity[[i]]=young_immunity[[i]])}
  if(!48%in%young_immunity[[i]]$lower.age.limit){(young_immunity[[i]]=young_immunity[[i]] %>% add_row(lower.age.limit = 48,Test = "Positive", total=0,seropositive=0.96,.before = 6))}else {(young_immunity[[i]]=young_immunity[[i]])}
  if(!108%in%young_immunity[[i]]$lower.age.limit){(young_immunity[[i]]=young_immunity[[i]] %>% add_row(lower.age.limit = 108,Test = "Positive", total=0,seropositive=0.96,.before = 7))}else {(young_immunity[[i]]=young_immunity[[i]])}
  
  immunity_adults$seropositive=rbeta(1,71,3)
  combinedimmunity[[i]]=rbind(data.frame(young_immunity[[i]]),immunity_adults)
  baseline_immunity[[i]]=combinedimmunity[[i]]$seropositive
  names(baseline_immunity[[i]])=combinedimmunity[[i]]$lower.age.limit
  maternal_immunity=data.frame(combinedimmunity[[i]]) %>% filter(lower.age.limit==0)%>%
    .$seropositive
  perspective=data.frame(rep("19",63))
  colnames(perspective)="perspective"
  combinedimmunity[[i]]=cbind(data.frame(combinedimmunity[[i]][,c(1,4)]),perspective)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  g[[i]]=rbeta(1,153,3)
  cvm_20[[i]]=as.matrix(rbind(rep(rbeta(1,335,89),21),rep(rbeta(1,260,305),21)))
  colnames(cvm_20[[i]])=c(0:20)
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity[[i]],  ##August 2019 baseline year 19
                     baseline.year =19,
                     year =20 ,
                     coverage = cvm_20[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy= g[[i]])
  
  seroprevalence_shifted_20[[i]] <- combinedimmunity[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="20")
  
  seroprevalence_shifted_20[is.na(seroprevalence_shifted_20)] <- 0
  
  seroprevalence_grouped_20[[i]] <- combinedimmunity[[i]] %>%
    rbind(seroprevalence_shifted_20[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_20 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  
  adjusted_immunity <- seroprevalence_grouped_20[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_20[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_20[[i]]=list(
    by_age=
      seroprevalence_grouped_20[[i]] %>%
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
  
  out_20 <- list(R=R,RT=RT,contacts=contacts,pre_COVID_herd=pre_COVID_herd,combinedimmunity=combinedimmunity,COVID_herd=COVID_herd,seroprevalence_shifted_20=seroprevalence_shifted_20,output_20=output_20)
  save(out_20,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_20.Rdata")
  
}



data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_20.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_21= vector(length(4000),mode="list")
cvm_21=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_20
seroprevalence_shifted_21=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_21=vector(length(4000),mode="list")

for (i in 1:4000){ 
  contact = get_mixing_mat(contacts[[i]],df)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  g[[i]]=rbeta(1,153,3)
  
  cvm_21[[i]] <- cbind(cvm_20[[i]], cvm_20[[i]][, ncol(cvm_20[[i]])])   ##ScenarioA same coverage
  colnames(cvm_21[[i]])[22] <- "21"
  
  pop_21=pop %>% mutate(Year=c(rep(21,72),rep(20,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity<- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =20,
                     year =21 ,
                     coverage = cvm_21[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_21[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="21")
  
  seroprevalence_shifted_21[is.na(seroprevalence_shifted_21)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_21[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_21[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_21 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  
  adjusted_immunity <- seroprevalence_grouped_21[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_21[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_21[[i]]=list(
    by_age=
      seroprevalence_grouped_21[[i]] %>%
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
  
  
  out_21 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_21=seroprevalence_shifted_21,output_21=output_21)
  save(out_21,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_21.Rdata")
  
}
##year 22 is nov 2019
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_21.Rdata"))
output_22= vector(length(4000),mode="list")
cvm_22=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_21
seroprevalence_shifted_22=vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_22=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  g[[i]]=rbeta(1,153,3)
  
  cvm_22[[i]] <- cbind(cvm_21[[i]], cvm_21[[i]][, ncol(cvm_21[[i]])])   ##ScenarioA same coverage
  colnames(cvm_22[[i]])[23] <- "22"
  
  pop_22=pop %>% mutate(Year=c(rep(21,72),rep(22,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =21,
                     year =22 ,
                     coverage = cvm_22[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_22[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="22")
  
  seroprevalence_shifted_22[is.na(seroprevalence_shifted_22)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_22[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_22[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_22 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  
  adjusted_immunity <- seroprevalence_grouped_22[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_22[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_22[[i]]=list(
    by_age=
      seroprevalence_grouped_22[[i]] %>%
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
  
  out_22 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_22=seroprevalence_shifted_22,output_22=output_22)
  save(out_22,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_22.Rdata")
  
}


##year 23 is Dec 2019
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_22.Rdata"))
output_23= vector(length(4000),mode="list")
cvm_23=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_22
seroprevalence_shifted_23=vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_23=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_23[[i]] <- cbind(cvm_22[[i]], cvm_22[[i]][, ncol(cvm_22[[i]])])   ##ScenarioA same coverage
  colnames(cvm_23[[i]])[24] <- "23"
  
  pop_23=pop %>% mutate(Year=c(rep(23,72),rep(22,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =22,
                     year =23 ,
                     coverage = cvm_23[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_23[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="23")
  
  seroprevalence_shifted_23[is.na(seroprevalence_shifted_23)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_23[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_23[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_23 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_23[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_23[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_23[[i]]=list(
    by_age=
      seroprevalence_grouped_23[[i]] %>%
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
  
  
  out_23 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_23=seroprevalence_shifted_23,output_23=output_23)
  save(out_23,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_23.Rdata")
  
}


##year 24 is Jan 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_23.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_24= vector(length(4000),mode="list")
cvm_24=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_23
seroprevalence_shifted_24=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_24=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  g[[i]]=rbeta(1,153,3)
  
  cvm_24[[i]] <- cbind(cvm_23[[i]], cvm_23[[i]][, ncol(cvm_23[[i]])])   ##ScenarioA same coverage
  colnames(cvm_24[[i]])[25] <- "24"
  
  pop_24=pop %>% mutate(Year=c(rep(23,72),rep(24,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =23,
                     year =24 ,
                     coverage = cvm_24[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy =g[[i]])
  
  seroprevalence_shifted_24[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="24")
  
  seroprevalence_shifted_24[is.na(seroprevalence_shifted_24)] <- 0
  
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_24[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_24[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_24 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_24[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_24[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_24[[i]]=list(
    by_age=
      seroprevalence_grouped_24[[i]] %>%
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
  
  
  out_24 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_24=seroprevalence_shifted_24,output_24=output_24)
  save(out_24,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_24.Rdata")
  
}

##year 25 is Feb 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_24.Rdata"))
output_25= vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
cvm_25=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_24
seroprevalence_shifted_25=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_25=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_25[[i]] <- cbind(cvm_24[[i]], cvm_24[[i]][, ncol(cvm_24[[i]])])   ##ScenarioA same coverage
  colnames(cvm_25[[i]])[26] <- "25"
  
  pop_25=pop %>% mutate(Year=c(rep(25,72),rep(24,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =24,
                     year =25 ,
                     coverage = cvm_25[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_25[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="25")
  
  seroprevalence_shifted_25[is.na(seroprevalence_shifted_25)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_25[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_25[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_25 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  
  adjusted_immunity <- seroprevalence_grouped_25[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_25[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_25[[i]]=list(
    by_age=
      seroprevalence_grouped_25[[i]] %>%
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
  
  
  out_25 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_25=seroprevalence_shifted_25,output_25=output_25)
  save(out_25,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_25.Rdata")
  
}

##year 26 is Mar 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_25.Rdata"))
output_26= vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
cvm_26=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_25
seroprevalence_shifted_26=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_26=vector(length(4000),mode="list")
for (i in 1:4000){
  g[[i]]=rbeta(1,153,3)
  contact = get_mixing_mat(contacts[[i]],df)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_26[[i]] <- cbind(cvm_25[[i]], cvm_25[[i]][, ncol(cvm_25[[i]])])   ##ScenarioA same coverage
  colnames(cvm_26[[i]])[27] <- "26"
  
  
  pop_26=pop %>% mutate(Year=c(rep(25,72),rep(26,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =25,
                     year =26 ,
                     coverage = cvm_26[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_26[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="26")
  
  seroprevalence_shifted_26[is.na(seroprevalence_shifted_26)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_26[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_26[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_26 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  
  adjusted_immunity <- seroprevalence_grouped_26[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_26[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_26[[i]]=list(
    by_age=
      seroprevalence_grouped_26[[i]] %>%
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
  
  
  out_26 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_26=seroprevalence_shifted_26,output_26=output_26)
  save(out_26,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_26.Rdata")
  
}

##year 27 is Apr 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_26.Rdata"))
output_27= vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
cvm_27=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_26
seroprevalence_shifted_27=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_27=vector(length(4000),mode="list")

for (i in 1:4000){
  g[[i]]=rbeta(1,153,3)
  contact = get_mixing_mat(contacts[[i]],df)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_27[[i]] <- cbind(cvm_26[[i]], (cvm_26[[i]][, ncol(cvm_26[[i]])]))   ##ScenarioA same coverage
  colnames(cvm_27[[i]])[28] <- "27"
  
  pop_27=pop %>% mutate(Year=c(rep(27,72),rep(26,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =26,
                     year =27 ,
                     coverage = cvm_27[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_27[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="27")
  
  seroprevalence_shifted_27[is.na(seroprevalence_shifted_27)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_27[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_27[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_27 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  
  adjusted_immunity <- seroprevalence_grouped_27[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_27[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_27[[i]]=list(
    by_age=
      seroprevalence_grouped_27[[i]] %>%
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
  
  
  
  out_27 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_27=seroprevalence_shifted_27,output_27=output_27)
  save(out_27,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_27.Rdata")
  
}

##year 28 is May 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_27.Rdata"))
output_28= vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
cvm_28=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_27
seroprevalence_shifted_28=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_28=vector(length(4000),mode="list")

for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_28[[i]] <- cbind(cvm_27[[i]], cvm_27[[i]][, ncol(cvm_27[[i]])])   ##ScenarioA same coverage
  colnames(cvm_28[[i]])[29] <- "28"
  
  
  pop_28=pop %>% mutate(Year=c(rep(27,72),rep(28,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =27,
                     year =28 ,
                     coverage = cvm_28[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy =g[[i]])
  
  seroprevalence_shifted_28[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="28")
  
  seroprevalence_shifted_28[is.na(seroprevalence_shifted_28)] <- 0
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_28[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_28[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_28 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_28[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_28[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_28[[i]]=list(
    by_age=
      seroprevalence_grouped_28[[i]] %>%
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
  
  out_28 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_28=seroprevalence_shifted_28,output_28=output_28)
  save(out_28,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_28.Rdata")
  
}

##year 29 is June 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_28.Rdata"))
output_29= vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
cvm_29=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_28
seroprevalence_shifted_29=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_29=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_29[[i]] <- cbind(cvm_28[[i]], cvm_28[[i]][, ncol(cvm_28[[i]])])   ##ScenarioA same coverage
  colnames(cvm_29[[i]])[30] <- "29"
  
  pop_29=pop %>% mutate(Year=c(rep(29,72),rep(28,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =28,
                     year =29 ,
                     coverage = cvm_29[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = 0.95)
  
  seroprevalence_shifted_29[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="29")
  
  seroprevalence_shifted_29[is.na(seroprevalence_shifted_29)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_29[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_29[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_29 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  
  
  adjusted_immunity <- seroprevalence_grouped_29[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_29[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_29[[i]]=list(
    by_age=
      seroprevalence_grouped_29[[i]] %>%
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
  
  
  out_29 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_29=seroprevalence_shifted_29,output_29=output_29)
  save(out_29,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_29.Rdata")
}

##year 30 is July 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_29.Rdata"))
output_30= vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
cvm_30=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_29
seroprevalence_shifted_30=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_30=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_30[[i]] <- cbind(cvm_29[[i]], cvm_29[[i]][, ncol(cvm_29[[i]])])   ##ScenarioA same coverage
  colnames(cvm_30[[i]])[31] <- "30"
  
  pop_30=pop %>% mutate(Year=c(rep(29,72),rep(30,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =29,
                     year =30 ,
                     coverage = cvm_30[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_30[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="30")
  
  seroprevalence_shifted_30[is.na(seroprevalence_shifted_30)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_30[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_30[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_30 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_30[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_30[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_30[[i]]=list(
    by_age=
      seroprevalence_grouped_30[[i]] %>%
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
  
  out_30 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_30=seroprevalence_shifted_30,output_30=output_30)
  save(out_30,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_30.Rdata")
  
}

##year 31 is Aug 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_30.Rdata"))
cvm_31=vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_31= vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_30
seroprevalence_shifted_31=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_31=vector(length(4000),mode="list")
for (i in 1:4000){
  
  g[[i]]=rbeta(1,153,3)
  contact = get_mixing_mat(contacts[[i]],df)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_31[[i]] <- cbind(cvm_30[[i]],cvm_30[[i]][, ncol(cvm_30[[i]])])   ##ScenarioA same coverage
  colnames(cvm_31[[i]])[32] <- "31"
  
  pop_31=pop %>% mutate(Year=c(rep(31,72),rep(30,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =30,
                     year =31 ,
                     coverage = cvm_31[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_31[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="31")
  
  seroprevalence_shifted_31[is.na(seroprevalence_shifted_31)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_31[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_31[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_31 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_31[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_31[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_31[[i]]=list(
    by_age=
      seroprevalence_grouped_31[[i]] %>%
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
  
  out_31 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_31=seroprevalence_shifted_31,output_31=output_31)
  save(out_31,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_31.Rdata")
  
}

##year 32 is sep 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_31.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_32= vector(length(4000),mode="list")
cvm_32=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_31
seroprevalence_shifted_32=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_32=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_32[[i]] <- cbind(cvm_31[[i]], cvm_31[[i]][, ncol(cvm_31[[i]])])   ##ScenarioA same coverage
  colnames(cvm_32[[i]])[33] <- "32"
  
  
  pop_32=pop %>% mutate(Year=c(rep(31,72),rep(32,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =31,
                     year =32 ,
                     coverage = cvm_32[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_32[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="32")
  
  seroprevalence_shifted_32[is.na(seroprevalence_shifted_32)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_32[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_32[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_32 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_32[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_32[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_32[[i]]=list(
    by_age=
      seroprevalence_grouped_32[[i]] %>%
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
  
  out_32 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_32=seroprevalence_shifted_32,output_32=output_32)
  save(out_32,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_32.Rdata")
  
}

##year 33 is oct 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_32.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_33= vector(length(4000),mode="list")
cvm_33=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_32
seroprevalence_shifted_33=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_33=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_33[[i]] <- cbind(cvm_32[[i]], cvm_32[[i]][, ncol(cvm_32[[i]])])   ##ScenarioA same coverage
  colnames(cvm_33[[i]])[34] <- "33"
  
  
  pop_33=pop %>% mutate(Year=c(rep(33,72),rep(32,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =32,
                     year =33 ,
                     coverage = cvm_33[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy =g[[i]])
  
  seroprevalence_shifted_33[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="33")
  
  seroprevalence_shifted_33[is.na(seroprevalence_shifted_33)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_33[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_33[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_33 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_33[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_33[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_33[[i]]=list(
    by_age=
      seroprevalence_grouped_33[[i]] %>%
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
  
  out_33 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_33=seroprevalence_shifted_33,output_33=output_33)
  save(out_33,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_33.Rdata")
  
}

##year 34 is Nov 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_33.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_34= vector(length(4000),mode="list")
cvm_34=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_33
seroprevalence_shifted_34=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_34=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_34[[i]] <- cbind(cvm_33[[i]], cvm_33[[i]][, ncol(cvm_33[[i]])])   ##ScenarioA same coverage
  colnames(cvm_34[[i]])[35] <- "34"
  
  pop_34=pop %>% mutate(Year=c(rep(33,72),rep(34,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =33,
                     year =34 ,
                     coverage = cvm_34[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_34[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="34")
  
  seroprevalence_shifted_34[is.na(seroprevalence_shifted_34)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_34[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_34[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_34 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_34[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_34[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_34[[i]]=list(
    by_age=
      seroprevalence_grouped_34[[i]] %>%
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
  
  out_34 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_34=seroprevalence_shifted_34,output_34=output_34)
  save(out_34,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_34.Rdata")
  
}

##year 35 is Dec 2020
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_34.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_35= vector(length(4000),mode="list")
cvm_35=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_34
seroprevalence_shifted_35=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_35=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  
  cvm_35[[i]] <- cbind(cvm_34[[i]], cvm_34[[i]][, ncol(cvm_34[[i]])])   ##ScenarioA same coverage
  colnames(cvm_35[[i]])[36] <- "35"
  
  
  pop_35=pop %>% mutate(Year=c(rep(35,72),rep(34,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =34,
                     year =35 ,
                     coverage = cvm_35[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_35[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="35")
  
  seroprevalence_shifted_35[is.na(seroprevalence_shifted_35)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_35[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_35[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_35 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_35[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_35[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_35[[i]]=list(
    by_age=
      seroprevalence_grouped_35[[i]] %>%
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
  
  out_35 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_35=seroprevalence_shifted_35,output_35=output_35)
  save(out_35,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_35.Rdata")
  
}

##year 36 is Jan 2021
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_35.Rdata"))
output_36= vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
cvm_36=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_35
seroprevalence_shifted_36=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_36=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  
  cvm_36[[i]] <- cbind(cvm_35[[i]], cvm_35[[i]][, ncol(cvm_35[[i]])])   ##ScenarioA same coverage
  colnames(cvm_36[[i]])[37] <- "36"
  
  pop_36=pop %>% mutate(Year=c(rep(35,72),rep(36,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =35,
                     year =36 ,
                     coverage = cvm_36[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_36[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="36")
  
  seroprevalence_shifted_36[is.na(seroprevalence_shifted_36)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_36[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_36[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_36 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_36[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_36[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_36[[i]]=list(
    by_age=
      seroprevalence_grouped_36[[i]] %>%
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
  
  out_36 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_36=seroprevalence_shifted_36,output_36=output_36)
  save(out_36,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_36.Rdata")
  
}

##year 37 is feb 2021
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_36.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_37= vector(length(4000),mode="list")
cvm_37=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_36
seroprevalence_shifted_37=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_37=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  
  cvm_37[[i]] <- cbind(cvm_36[[i]], cvm_36[[i]][, ncol(cvm_36[[i]])])   ##ScenarioA same coverage
  colnames(cvm_37[[i]])[38] <- "37"
  
  pop_37=pop %>% mutate(Year=c(rep(37,72),rep(36,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =36,
                     year =37 ,
                     coverage = cvm_37[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_37[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="37")
  
  seroprevalence_shifted_37[is.na(seroprevalence_shifted_37)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_37[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_37[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_37 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_37[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_37[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_37[[i]]=list(
    by_age=
      seroprevalence_grouped_37[[i]] %>%
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
  
  out_37 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_37=seroprevalence_shifted_37,output_37=output_37)
  save(out_37,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_37.Rdata")
  
}

##year 38 is mar 2021
data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_37.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_38= vector(length(4000),mode="list")
cvm_38=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_37
seroprevalence_shifted_38=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_38=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  
  cvm_38[[i]] <- cbind(cvm_37[[i]], cvm_37[[i]][, ncol(cvm_37[[i]])])   ##ScenarioA same coverage
  colnames(cvm_38[[i]])[39] <- "38"
  
  pop_38=pop %>% mutate(Year=c(rep(37,72),rep(38,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =37,
                     year =38 ,
                     coverage = cvm_38[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_38[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="38")
  
  seroprevalence_shifted_38[is.na(seroprevalence_shifted_38)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_38[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_38[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_38 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_38[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_38[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_38[[i]]=list(
    by_age=
      seroprevalence_grouped_38[[i]] %>%
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
  
  out_38 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_38=seroprevalence_shifted_38,output_38=output_38)
  save(out_38,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_38.Rdata")
  
}

##year 39 is Apr 2021

data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_38.Rdata"))
output_39= vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
cvm_39=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_38
seroprevalence_shifted_39=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_39=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  
  cvm_39[[i]] <- cbind(cvm_38[[i]], cvm_38[[i]][, ncol(cvm_38[[i]])])   ##ScenarioA same coverage
  colnames(cvm_39[[i]])[40] <- "39"
  
  pop_39=pop %>% mutate(Year=c(rep(39,72),rep(38,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =38,
                     year =39 ,
                     coverage = cvm_39[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_39[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="39")
  
  seroprevalence_shifted_39[is.na(seroprevalence_shifted_39)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_39[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_39[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_39 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_39[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_39[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_39[[i]]=list(
    by_age=
      seroprevalence_grouped_39[[i]] %>%
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
  
  out_39 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_39=seroprevalence_shifted_39,output_39=output_39)
  save(out_39,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_39.Rdata")
  
}

##year 40 is May 2021

data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_39.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_40= vector(length(4000),mode="list")
cvm_40=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_39
seroprevalence_shifted_40=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_40=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  
  cvm_40[[i]] <- cbind(cvm_39[[i]], cvm_39[[i]][, ncol(cvm_39[[i]])])   ##ScenarioA same coverage
  colnames(cvm_40[[i]])[41] <- "40"
  
  pop_40=pop %>% mutate(Year=c(rep(39,72),rep(40,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =39,
                     year =40 ,
                     coverage = cvm_40[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_40[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="40")
  
  seroprevalence_shifted_40[is.na(seroprevalence_shifted_40)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_40[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_40[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_40 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_40[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_40[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_40[[i]]=list(
    by_age=
      seroprevalence_grouped_40[[i]] %>%
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
  
  out_40 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_40=seroprevalence_shifted_40,output_40=output_40)
  save(out_40,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_40.Rdata")
  
}

##year 41 is jun 2021

data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_40.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_41= vector(length(4000),mode="list")
cvm_41=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_40
seroprevalence_shifted_41=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_41=vector(length(4000),mode="list")
for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_41[[i]] <- cbind(cvm_40[[i]], cvm_40[[i]][, ncol(cvm_40[[i]])])   ##ScenarioA same coverage
  colnames(cvm_41[[i]])[42] <- "41"
  
  pop_41=pop %>% mutate(Year=c(rep(41,72),rep(40,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =40,
                     year =41 ,
                     coverage = cvm_41[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_41[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="41")
  
  seroprevalence_shifted_41[is.na(seroprevalence_shifted_41)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_41[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_41[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_41 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_41[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_41[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_41[[i]]=list(
    by_age=
      seroprevalence_grouped_41[[i]] %>%
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
  
  out_41 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_41=seroprevalence_shifted_41,output_41=output_41)
  save(out_41,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_41.Rdata")
  
}

##year 42 is Jul 2021

data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_41.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_42= vector(length(4000),mode="list")
cvm_42=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_41
seroprevalence_shifted_42=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_42=vector(length(4000),mode="list")

for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  
  cvm_42[[i]] <- cbind(cvm_41[[i]], cvm_41[[i]][, ncol(cvm_41[[i]])])   ##ScenarioA same coverage
  colnames(cvm_42[[i]])[43] <- "42"
  
  
  
  pop_42=pop %>% mutate(Year=c(rep(41,72),rep(42,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =41,
                     year =42 ,
                     coverage = cvm_42[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_42[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="42")
  
  seroprevalence_shifted_42[is.na(seroprevalence_shifted_42)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_42[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_42[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_42 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_42[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_42[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_42[[i]]=list(
    by_age=
      seroprevalence_grouped_42[[i]] %>%
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
  
  out_42 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_42=seroprevalence_shifted_42,output_42=output_42)
  save(out_42,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_42.Rdata")
  
}

##year 43 is Aug 2021

data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_42.Rdata"))
output_43= vector(length(4000),mode="list")
cvm_43=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_42
seroprevalence_shifted_43=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_43=vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
for (i in 1:4000){
  
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  contact = get_mixing_mat(contacts[[i]],df)
  
  cvm_43[[i]] <- cbind(cvm_42[[i]], cvm_42[[i]][, ncol(cvm_42[[i]])])   ##ScenarioA same coverage
  colnames(cvm_43[[i]])[44] <- "43"
  
  
  pop_43=pop %>% mutate(Year=c(rep(43,72),rep(42,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =42,
                     year =43 ,
                     coverage = cvm_43[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_43[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="43")
  
  seroprevalence_shifted_43[is.na(seroprevalence_shifted_43)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_43[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_43[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_43 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_43[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_43[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_43[[i]]=list(
    by_age=
      seroprevalence_grouped_43[[i]] %>%
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
  
  out_43 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_43=seroprevalence_shifted_43,output_43=output_43)
  save(out_43,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_43.Rdata")
  
}

##year 44 is sep 2021

data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_43.Rdata"))
output_44= vector(length(4000),mode="list")
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
cvm_44=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_43
seroprevalence_shifted_44=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_44=vector(length(4000),mode="list")

for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_44[[i]] <- cbind(cvm_43[[i]], cvm_43[[i]][, ncol(cvm_43[[i]])])   ##ScenarioA same coverage
  colnames(cvm_44[[i]])[45] <- "44"
  
  
  pop_44=pop %>% mutate(Year=c(rep(43,72),rep(44,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =43,
                     year =44 ,
                     coverage = cvm_44[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_44[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="44")
  
  seroprevalence_shifted_44[is.na(seroprevalence_shifted_44)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_44[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_44[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_44 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_44[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_44[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_44[[i]]=list(
    by_age=
      seroprevalence_grouped_44[[i]] %>%
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
  
  out_44 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_44=seroprevalence_shifted_44,output_44=output_44)
  save(out_44,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_44.Rdata")
  
}

##year 45 is oct 2021

data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_44.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_45= vector(length(4000),mode="list")
cvm_45=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_44
seroprevalence_shifted_45=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_45=vector(length(4000),mode="list")

for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_45[[i]] <- cbind(cvm_44[[i]], cvm_44[[i]][, ncol(cvm_44[[i]])])   ##ScenarioA same coverage
  colnames(cvm_45[[i]])[46] <- "45"
  
  pop_45=pop %>% mutate(Year=c(rep(45,72),rep(44,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =44,
                     year =45 ,
                     coverage = cvm_45[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_45[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="45")
  
  seroprevalence_shifted_45[is.na(seroprevalence_shifted_45)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_45[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_45[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_45 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_45[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_45[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_45[[i]]=list(
    by_age=
      seroprevalence_grouped_45[[i]] %>%
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
  
  out_45 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_45=seroprevalence_shifted_45,output_45=output_45)
  save(out_45,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_45.Rdata")
  
}

##year 46 is Nov 2021

data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_45.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_46= vector(length(4000),mode="list")
cvm_46=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_45
seroprevalence_shifted_46=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_46=vector(length(4000),mode="list")

for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  cvm_46[[i]] <- cbind(cvm_45[[i]], cvm_45[[i]][, ncol(cvm_45[[i]])])   ##ScenarioA same coverage
  colnames(cvm_46[[i]])[47] <- "46"
  
  pop_46=pop %>% mutate(Year=c(rep(45,72),rep(46,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =45,
                     year =46 ,
                     coverage = cvm_46[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_46[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="46")
  
  seroprevalence_shifted_46[is.na(seroprevalence_shifted_46)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_46[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_46[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_46 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_46[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_46[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_46[[i]]=list(
    by_age=
      seroprevalence_grouped_46[[i]] %>%
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
  
  out_46 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_46=seroprevalence_shifted_46,output_46=output_46)
  save(out_46,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_46.Rdata")
  
}

##year 47 is Dec 2021

data=get(load("D:/MANUSCRIPT/ncov_measles_Kenya/A/out_46.Rdata"))
R=vector(length(4000),mode="list")
RT=vector(length(4000),mode="list")
pre_COVID_herd=vector(length(4000),mode="list")
COVID_herd=vector(length(4000),mode="list")
output_47= vector(length(4000),mode="list")
cvm_47=vector(length(4000),mode="list")
seroprevalence=data$seroprevalence_shifted_46
seroprevalence_shifted_47=vector(length(4000),mode="list")
g=vector(length(4000),mode="list")
seroprevalence_grouped_47=vector(length(4000),mode="list")

for (i in 1:4000){
  contact = get_mixing_mat(contacts[[i]],df)
  g[[i]]=rbeta(1,153,3)
  R[[i]]=rlnorm(1,2.66,0.08)
  RT[[i]]=rbeta(1,6.92,6.92)
  pre_COVID_herd[[i]]=1-1/R[[i]]
  COVID_herd[[i]]=1-1/(RT[[i]]*R[[i]])
  
  
  cvm_47[[i]] <- cbind(cvm_46[[i]], cvm_46[[i]][, ncol(cvm_46[[i]])])   ##ScenarioA same coverage
  colnames(cvm_47[[i]])[48] <- "47"
  
  
  pop_47=pop %>% mutate(Year=c(rep(47,72),rep(46,72)))   ##name pop according to analysis years
  
  maternal_immunity <- seroprevalence[[i]] %>%  
    filter(lower.age.limit==0) %>%
    .$seropositive
  
  baseline_immunity <- seroprevalence[[i]]$seropositive
  names(baseline_immunity) <- seroprevalence[[i]]$lower.age.limit
  
  ## project immunity
  shifted_immunity <-
    project_immunity(baseline.immunity = baseline_immunity,  ##August 2019 baseline year 19
                     baseline.year =46,
                     year =47 ,
                     coverage = cvm_47[[i]],
                     schedule = c(9, 18),
                     maternal.immunity = maternal_immunity,
                     efficacy = g[[i]])
  
  seroprevalence_shifted_47[[i]] <- seroprevalence[[i]] %>%
    mutate(seropositive=shifted_immunity,
           perspective="47")
  
  seroprevalence_shifted_47[is.na(seroprevalence_shifted_47)] <- 0
  
  ## group seroprevalence data in age groups of contact survey
  seroprevalence_grouped_47[[i]] <- seroprevalence[[i]] %>%
    rbind(seroprevalence_shifted_47[[i]]) %>%
    mutate(age_group=reduce_agegroups(lower.age.limit, lower_age_groups)) %>%
    left_join(pop_47 %>% mutate(perspective=as.character(Year)) %>% select(-Year),
              by=c("lower.age.limit", "perspective")) %>%
    group_by(perspective) %>%
    mutate(age_group=limits_to_agegroups(age_group)) %>%
    group_by(age_group, perspective) %>%
    summarise(prop.immune=sum(seropositive * Population) / sum(Population),
              Population=sum(Population)) %>%
    ungroup()
  adjusted_immunity <- seroprevalence_grouped_47[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=adjust_immunity(contact, immunity=prop.immune)) %>%
    mutate(type="Contact-adjusted")
  
  plain_immunity <- seroprevalence_grouped_47[[i]] %>%
    group_by(perspective) %>%
    summarise(immunity=sum(prop.immune*Population)/sum(Population)) %>%
    mutate(type="Plain")
  
  output_47[[i]]=list(
    by_age=
      seroprevalence_grouped_47[[i]] %>%
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
  
  out_47 <- list(R=R,RT=RT,pre_COVID_herd=pre_COVID_herd,COVID_herd=COVID_herd,seroprevalence_shifted_47=seroprevalence_shifted_47,output_47=output_47)
  save(out_47,file = "D:/MANUSCRIPT/ncov_measles_Kenya/A/out_47.Rdata")
  
}
