library('dplyr')
library('readxl')
rm(list = ls())
#source('Weights and PK.R')

# 1. Import

blood <- read_excel('input/NHANES PFAS.xlsx', sheet = 'PFAS_All', guess_max = 17000)

# 2. Tidy

blood <- blood %>% select(Cycle, RIAGENDR, RIDAGEYR, SSNPFOA, SSBPFOA, SSNPFOS, SSMPFOS) %>%
  filter(RIDAGEYR >= 12 & Cycle >= 20132014) 

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

blood <- split(blood,list(blood$Gender,blood$Year))



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
