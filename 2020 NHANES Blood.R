library('dplyr')
library('readxl')
rm(list = ls())
#source('Weights and PK.R')

# 1. Import

blood <- read_excel('input/NHANES PFAS.xlsx', sheet = 'PFAS_All', guess_max = 17000)

# 2. Tidy

blood <- blood %>% select(Cycle, RIAGENDR, RIDAGEYR, SSNPFOA, SSBPFOA, SSNPFOS, SSMPFOS) %>%
  filter(Cycle >= 20132014) 

blood<- data.frame(blood)
blood[is.na(blood)] <- 0
blood <- blood %>% mutate(PFOA = (SSNPFOA + SSBPFOA)*1000,
                          PFOS = (SSNPFOS + SSMPFOS)*1000)%>%
  select(-SSBPFOA,-SSNPFOA,-SSMPFOS,-SSNPFOS) %>%
  mutate( RIAGENDR = case_when( (RIAGENDR == "1") ~ "Male",
                                (RIAGENDR == "2") ~ "Female"))
names(blood)[1:3]<- c("Year","Gender","Age")
# units are ug/ L per Labels sheet in NHANES input file, hence*1000 above
# to switch to ng/L. 


## 4. Calculations


# values reported in paper as ng/mL (2011 PFOA paper)

blood <- blood %>% mutate(PFOA = PFOA/1000,
                          PFOS = PFOS/1000)

blood <- split(blood,list(blood$Year))

summary(blood$`20132014`$PFOA)
summary(blood$`20132014`$PFOS)
get.blood.summary <- function(m){
  
  PFOA <- quantile(m$PFOA,c(0.1,0.5,0.95,0.99))
  PFOS <- quantile(m$PFOS,c(0.1,0.5,0.95,0.99))
  result <- rbind(PFOA,PFOS)
  result <- cbind(rownames(result),rep(m$Year[1],2),"ng/mL",result)
  colnames(result)<- c("Chemical","Year",
                       "Units","10%","Median","95%","99%")
  
  return(result)
}


bloodserum <- lapply(blood,get.blood.summary)



F2014 <- bloodserum$Female.20132014
M2014 <- bloodserum$Male.20132014


# Result Printed (Not Exported)

F2014
M2014




# Creation of Histogram 

plotme<- rbind(blood$Male.20132014,blood$Female.20132014)
summary(plotme$PFOA)

rm(list=setdiff(ls(),"plotme"))


# Tidying for Histogram

PFOA<- as.character(plotme$PFOA)
PFOS<- as.character(plotme$PFOS)

chem<- c(rep("PFOA",length(PFOA)),rep("PFOS",length(PFOS)))
trueplot<- data.frame(cbind(chem,c(PFOA,PFOS)))
colnames(trueplot)<- c("Chemical","Concentration")
class(trueplot$Concentration)  
trueplot$Concentration<- as.numeric(as.character(trueplot$Concentration))
class(trueplot$Concentration)
trueplot$Gender <- rep(plotme$Gender,2)
rm(list=setdiff(ls(),"trueplot"))

#trueplot$Concentration<-as.numeric(trueplot$Concentration)


library('ggplot2')
ggplot(trueplot,aes(x=Concentration, fill=Chemical))+
  geom_histogram(position = 'dodge',binwidth = .2)+
  scale_fill_manual(values=c("#e56d74", "#6db0e5"))+
  scale_x_continuous(breaks = seq(0, 20, 1), lim = c(0, 20))+
  ylim(0,100)+
  facet_grid(Gender~.) +
  labs(title = "NHANES 2013-2014 Serum Concentration for Adult Females and Males",
       x= "Concentration (ng/mL)",
       y= "Count")
  

#
# Note: Consider httk and carry age 
#       in months and ethnicity to
#       input to generate body wts.
#       (not currently required)


