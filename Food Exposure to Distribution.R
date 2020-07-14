library('dplyr')
library('readxl')
rm(list = ls())
source('Weights and PK.R')

# 1. Import

blood <- read_excel('input/NHANES PFAS.xlsx', sheet = 'PFAS_All', guess_max = 17000)

# 2. Tidy

blood <- blood %>% select(Cycle, RIAGENDR, RIDAGEYR, SSNPFOA, SSBPFOA, SSNPFOS, SSMPFOS) %>%
                   filter(RIDAGEYR >= 12 & Cycle >= 20132014) 

blood<- data.frame(blood)
blood[is.na(blood)] <- 0
blood <- blood %>% mutate(PFOA = (SSNPFOA + SSBPFOA)*1000)%>%
                   mutate(PFOS = (SSNPFOS + SSMPFOS)*1000)%>%
                   select(-SSBPFOA,-SSNPFOA,-SSMPFOS,-SSNPFOS)
names(blood)[1:3]<- c("Year","Gender","Age")
# units are ug/ L per Labels sheet in NHANES input file, hence*1000 above
# to switch to ng/L. 


## 4. Calculations



table(blood$SSNPFOS)

table(blood$SSNPFOA, useNA = "ifany")