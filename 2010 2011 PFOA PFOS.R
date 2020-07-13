
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

# Previous PFOA PFOS calculations

# PFOA

data<- (import_list("Master6_4_2020.xlsx")[("Exposure_Factors")])
data<- data$Exposure_Factors

names<- data$Individual
data = lapply(as.list(1:dim(data)[1]), function(x) data[x[1],])

names(data)<-names

# get.EF

x<- data$Child

individual<- x$Individual

n<-x$n

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

# Soil Factor
factor_s <- (x$`Soil Ingestion (g/day)`*
               x$`Soil AF`)









#
PFOA <- data.frame( Route = c("Water"),
                    GM = 5,
                    GSD = 6)
