library('dplyr')
library('readxl')
rm(list = ls())
#source('Weights and PK.R')

# 1. Import

blood <- read_excel('input/NHANES PFAS.xlsx', sheet = 'PFAS_All', guess_max = 17000)

# 2. Tidy

blood <- blood %>% select(Cycle, RIAGENDR, RIDAGEYR, SSNPFOA, SSBPFOA, SSNPFOS, SSMPFOS) %>%
  filter(RIDAGEYR >= 18 & Cycle == 20132014) 

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
blood <- blood %>% mutate(PFOA = PFOA/1000,
                          PFOS = PFOS/1000)


summary(blood$PFOA)
summary(blood$PFOS)
table(blood$PFOS)

sum(blood$PFOS > 20)


PFOA<- as.character(blood$PFOA)
PFOS<- as.character(blood$PFOS)

chem<- c(rep("PFOA",length(PFOA)),rep("PFOS",length(PFOS)))
trueplot<- data.frame(cbind(chem,c(PFOA,PFOS)))
colnames(trueplot)<- c("Chemical","Concentration")
class(trueplot$Concentration)  
trueplot$Concentration<- as.numeric(as.character(trueplot$Concentration))
class(trueplot$Concentration)

rm(list=setdiff(ls(),"trueplot"))

ad <- ggplot(trueplot,aes(x=Concentration, fill=Chemical))+
        geom_histogram(position = 'dodge',binwidth = .2)+
        scale_fill_manual(values=c("#e56d74", "#6db0e5"))+
        scale_x_continuous(breaks = seq(0, 20, 1), lim = c(0, 20))+
        ylim(0,150)+
        labs(title = "NHANES 2013-2014 Serum Concentration (Adults)",
        x= "Concentration (ng/mL)",
        y= "Count")
      
ad


ad %+% geom_vline(aes(xintercept=4.2, color = "Our_PFOA",), size = 2, linetype = "dotted")+
       geom_vline(aes(xintercept=6.3, color = "Our_PFOS",), size = 2, linetype = "dotted")+
       geom_vline(aes(xintercept=1.87, color = "NHANES_PFOA",), size = 1.2, linetype = "dotted")+
       geom_vline(aes(xintercept=5.15, color = "NHANES_PFOS",), size = 1, linetype = "dotted")
  
scale_color_manual(name = "Medians", values = c(NHANES_PFOA = "Black", NHANES_PFOS = "#c75c07",
                                                Our_PFOA = "Black", Our_PFOS = "Gray"))

