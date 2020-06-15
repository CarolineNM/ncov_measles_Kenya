rm(list=ls())
library(readxl)
library(tidyverse)
library(binom)
library(Kendall)
library(haven)

data<-read_dta("data/Serovack_10may20.dta")  #converting stata file
#write.csv(data,file="serovack_10may20.csv")
serologyn<-read_excel("data/Immunity_model_input_data.xlsx",sheet="revised_serology") #serology data
serologyn$Age_years=factor(serologyn$Age_years,levels = unique(serologyn$Age_years))
serologyn$Year=factor(serologyn$Year,levels = unique(serologyn$Year))
serologyn %>% mutate(ymid=test$mean,ymin=test$lower,ymax=test$upper) %>% 
  ggplot(aes(x=Year,y=uptake2,group=vaccine))+
  geom_line(aes(col=vaccine),size=2)+
  theme_bw()+ylab("Vaccination coverage (%)")+xlab("Survey year")
  ###plot with confidence intervals
serologyn<-read_excel("data/Immunity_model_input_data.xlsx",sheet="JULY_serology") #coverage data
#serologyn=serologyn %>%filter(Year==2019) 
serologyn$Age_years=factor(serologyn$Age_years,levels = unique(serologyn$Age_years))
#serologyn$Year=factor(serologyn$Year,levels = unique(serologyn$Year))
test=binom.confint(serologyn$seropositive,serologyn$Total,conf.level=0.95,"wilson")
serologyn %>% mutate(ymid=test$mean,ymin=test$lower,ymax=test$upper) %>% 
  ggplot(aes(x=Age_years,y=ymid))+
  geom_bar(stat = "identity",position=position_dodge())+
  theme_bw()+ylab("Age immunity profiles")+xlab("Ages")+
  theme(axis.text.x=element_text(angle = 60,hjust = 1))+
  geom_pointrange(mapping=aes(x=Age_years, y=ymid,ymin=ymin,ymax=ymax), position=position_dodge(0.9),colour="red",alpha=0.9, size=0.2)

##########################3
serologyn<-read_excel("data/Immunity_model_input_data.xlsx",sheet="serology2") #coverage data
serologyn=serologyn %>%filter(Year==2019) 
serologyn$Age_years=factor(serologyn$Age_years,levels = unique(serologyn$Age_years))
serologyn$Year=factor(serologyn$Year,levels = unique(serologyn$Year))
test=binom.confint(serologyn$Seropositive,serologyn$Total,conf.level=0.95,"wilson")
serologyn %>% mutate(ymid=test$mean,ymin=test$lower,ymax=test$upper) %>% 
  ggplot(aes(x=Age_years,y=ymid))+
  geom_bar(stat = "identity",position=position_dodge())+
  theme_bw()+ylab("Age immunity profiles")+xlab("Age in Years")+
  theme(axis.text.x=element_text(angle = 60,hjust = 1))+
  geom_pointrange(mapping=aes(x=Age_years, y=ymid,ymin=ymin,ymax=ymax), position=position_dodge(0.9),colour="red",alpha=0.9, size=0.2)








data<-read_excel("data/New_immunity.xlsx",sheet="July2") #coverage data
data$Age_group=factor(data$Age_group,levels = unique(data$Age_group))
data$Year=factor(data$Year,levels = unique(data$Year))
data %>% 
  ggplot(aes(x=Age_group,y=prop,fill=Scenario))+
  geom_bar(stat = "identity",position=position_dodge())+
  theme_bw()+ylab("Age immunity profiles")+xlab("Immunity type")+
  theme(axis.text.x=element_text(angle = 60,hjust = 1))+
  geom_hline(yintercept=0.86,col="red")
  

data<-read_excel("data/Immunity_model_input_data.xlsx",sheet="scenarioA") #coverage data
data$Age_group=factor(data$Age_group,levels = unique(data$Age_group))
data$Year=factor(data$Year,levels = unique(data$Year))
data %>% 
  ggplot(aes(x=Age_group,y=prop,fill=Scenario))+
  facet_grid(~Year)+
  geom_bar(stat = "identity",position=position_dodge())+
  theme_bw()+ylab("Age immunity profiles")+xlab("Age_Category")+
  theme(axis.text.x=element_text(angle = 60,hjust = 1))




















data$Month=factor(data$Month,levels = unique(data$Month))
data %>% gather(Type,variables,c(4,5)) %>% 
  ggplot(aes(x=Month,y=variables,group=Type))+
  geom_line(aes(col=Type),size=1.3)+
  theme_bw()+ylab("MR1 vaccine counts")+xlab("Time in Months and Years")+
  theme(axis.text.x=element_text(angle = 60,hjust = 1))

MannKendall(data$KEIR)
acf(data$DHIS)
pacf(data$DHIS)


data$Age_Years=factor(data$Age_Years,levels = unique(data$Age_Years))
data %>% gather(Proportion,variables,c(6,7)) %>% mutate(ymid=test$mean,ymin=test$lower,ymax=test$upper) %>% 
  ggplot(aes(x=Age_Years,y=ymid,ymin=ymin,ymax=ymax,fill=Proportion))+
  geom_bar(stat = "identity",position=position_dodge())+
  facet_wrap(~Year)+theme_bw()+ylab("Seroprevalence")+
  theme(axis.text.x=element_text(angle = 60,hjust = 1))
  
  geom_pointrange(mapping=aes(x=Age_years, y=ymid,ymin=ymin,ymax=ymax), position=position_dodge(0.9),colour="black",alpha=0.9, size=0.2)

test=binom.confint(c(data$Seronegative,data$Seropositive),data$T,conf.level=0.95,"wilson")




data$Age_years=factor(data$Age_years,levels = unique(data$Age_years))
data %>% gather(Proportion,variables,c(5,6)) %>% 
  ggplot(aes(x=Year,y=variables,fill=Proportion))+
  geom_bar(stat = "identity",position=position_dodge())+
  theme_bw()+ylab("Seroprevalence (%)")+xlab("Survey year")+scale_y_continuous(breaks=c(0,25,50,75,100))

data%>% mutate(ymid=test$mean,ymin=test$lower,ymax=test$upper) %>% 
  ggplot(aes(x=Age_years,y=ymid,ymin=ymin,ymax=ymax))+
  geom_bar(stat = "identity",position=position_dodge())+
  theme_bw()+ylab("Seroprevalence")+xlab("Survey year")+
  facet_grid(~Year)+
  geom_pointrange(mapping=aes(x=Year, y=ymid,ymin=ymin,ymax=ymax), position=position_dodge(0.9),colour="black",alpha=0.9, size=0.2)

test=binom.confint(data$Seropositive,data$Total,conf.level=0.95,"wilson")



geom_text(aes(label=Samples), position=position_dodge(width=0.8), vjust=-0.1)

data$Age_Category=factor(data$Age_Category,levels = unique(data$Age_Category))
data %>% gather(Proportion,variables,c(6,7)) %>% mutate(ymid=test$mean,ymin=test$lower,ymax=test$upper) %>% 
  ggplot(aes(x=Age_Category,y=ymid,ymin=ymin,ymax=ymax,fill=Proportion))+
  geom_bar(stat = "identity",position=position_dodge())+
  facet_wrap(~Year)+theme_bw()+ylab("Seroprevalence")+
  geom_pointrange(mapping=aes(x=Age_Category, y=ymid,ymin=ymin,ymax=ymax), position=position_dodge(0.9),colour="black",alpha=0.9, size=0.2)
  
  
  


test=binom.confint(c(data$Seropositive,data$Seronegative),data$T,conf.level=0.95,"wilson")

x=age,y=Inc_med, ymin=Inc_low, ymax=Inc_up

# 
# write.csv(test,file="positives2.csv")
# 
# sd=test$lower+test$mean
# sd2=test$upper-test$mean
# 
# #formula=p_hat +/- 



upper=p_hat+z*sqrt(p_hat*(1-p_hat)/data2$Total)
lower=p_hat-z*sqrt(p_hat*(1-p_hat)/data2$Total)

p_hat + c(-1.96,1.96)*sqrt(p_hat*(1-p_hat)/data2$Total)





