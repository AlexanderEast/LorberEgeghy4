
#
# Food Range
#
library('rio')

rm(list=ls())

foodcomparison <- function(GM,GSD){

set.seed(12345)  

ourmedian <- summary(rlnorm(200,log(GM),log(GSD)))[3]*70*.9

hundredmedians<- data.frame(t(replicate(100,summary(rlnorm(200,log(GM),log(GSD))*70*.9))))

results<-  c(range(hundredmedians$Median),
    diff(range(hundredmedians$Median)),
    as.numeric(ourmedian))

results<-t(data.frame(results))
colnames(results)<- c("Min Median","Max Median","Range","Seed 12345 Median")

return(results)
}

Food_Range<- foodcomparison(.715,2.6)




# 1. Import People from Conc Input. 

rm(list=ls())

data  <- (import_list("input/Master6_4_2020.xlsx")[("Exposure_Factors")])
data  <- data$Exposure_Factors
names <- data$Individual
data  <- lapply(as.list(1:dim(data)[1]), function(x) data[x[1],])
names(data)<-names
rm(names)

get.EF<- function(x){
  

individual <- x$Individual
dietgroup  <- x$`Dietary Group`
n <-x$n

# Dust Ingestion Factor
factor_di <- (x$`Dust Ingestion Rate (g/day)`*
                x$`Dust Ingestion AF`)

# Dermal Dust Factor
factor_dd <- (x$`Dermal Dust Load (g/m3)`*
                x$`Dermal Dust Transfer Coefficient (m2/h)`*
                x$`Dermal Dust Time (hr)`*
                x$`Dermal Dust AF`)

# Indoor Air Factor
factor_ia <- (x$`Inhalation Rate (m3/day)`*
                x$`Fraction Time Indoors (h/day)`*
                x$`Inhalation AF`)

# Outdoor Air Factor
factor_oa <-   (x$`Inhalation Rate (m3/day)`*
                  x$`Fraction Time Outdoors (h/day)`*
                  x$`Inhalation AF`)

# Water Factor
factor_w <- (x$`Water Intake (L/day)`*
               x$`Water AF`)

# Food Factor 
factor_f <- (x$`Bodyweight (kg)`*x$`Dietary AF`)


# Soil Factor
factor_s <- (x$`Soil Ingestion (g/day)`*
               x$`Soil AF`)


id <- data.frame(rbind(individual,dietgroup,n,factor_ia,factor_oa,
                       factor_w,factor_di,factor_dd,factor_f,factor_s))

colnames(id)<-"Input"

return(id)
}

Child.EF<-lapply(data, get.EF)[("Child")]
Adults.EF<-lapply(data, get.EF)[("Adult")]
Child.EF <- Child.EF$Child
Adults.EF<- Adults.EF$Adult

rm(data)


# Previous PFOA PFOS GMs and GSDs (Can be found in Previous Papers 
# & Peter's SAS Script.)

# PFOA Input
PFOA <- data.frame( Route = c("Indoor Air","Outdoor Air","Water",
                              "Dust Ingestion","Dust Dermal",
                              "Dietary Ingestion"),
                    GM = c(0.00603,0.0111,13.98,
                           NA,NA,
                           .715),
                    GSD = c(1.5,1.9,3.13,
                          NA,NA,
                          2.6))
# PFOS Input
PFOS <- data.frame( Route = c("Indoor Air","Outdoor Air","Water",
                              "Dust Ingestion","Dust Dermal",
                              "Dietary Ingestion"),
                    GM = c(0.022,.44,21.4,201,201,1.77),
                    GSD = c(1.89,1.89,2.6,5.4,5.4,2.6))


adult.add <- data.frame("Exposure_Factor" = Adults.EF[(4:9),], 
                     "n" = rep(Adults.EF[3,],6))
           
           
child.add <- data.frame("Exposure_Factor" = Child.EF[(4:9),], 
                        "n" = rep(Adults.EF[3,],6))

PFOS_CHILD<- cbind(PFOS,child.add)
PFOS_ADULT<- cbind(PFOS,adult.add)
PFOA_CHILD<- cbind(PFOA,child.add)
PFOA_ADULT<- cbind(PFOS,adult.add)


cards<-list(PFOS_CHILD,PFOS_ADULT,PFOA_CHILD,PFOA_ADULT)

rm(list=setdiff(ls(),"cards"))






