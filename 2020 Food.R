### 2020 Food

rm(list=ls())

source('Weights and PK.R')
library('dplyr')
library('readxl')

PFOA <- read_excel('input/Eurofood_PFOS_PFOA_072020.xlsx', sheet = 'PFOA', guess_max = 17000)
PFOS <- read_excel('input/Eurofood_PFOS_PFOA_072020.xlsx', sheet = 'PFOS', guess_max = 17000)


PFOAA <- PFOA[(PFOA$`Age class` == "Adults"),]
PFOAA <- PFOAA[(complete.cases(PFOAA)),]

PFOSA <- PFOS[(PFOS$`Age class` == "Adults"),]
PFOSA <- PFOSA[(complete.cases(PFOSA)),]

PFOAC <- PFOA[(PFOA$`Age class` == "Other children"),]
PFOAC <- PFOAC[(complete.cases(PFOAC)),]

PFOSC <- PFOS[(PFOS$`Age class` == "Other children"),]
PFOSC <- PFOSC[(complete.cases(PFOSC)),]

rm(PFOA,PFOS,Simple.Dose.PK,Simple.Serum.PK)

get.adult.food.2020 <- function(x){

set.seed(12345)
  
x$Min <- 0
x$SD  <- x$`LB 95th Exposure` - x$Min/4
x$GM  <- x$`LB Mean Exposure`/ (1+ .05 * (x$SD/x$`LB Mean Exposure`)^2)

x_WM<-  WM(x$GM,x$`Number of subjects`)
x_WSD<- WSD(x$GM,x$`Number of subjects`)

dist <- (rlnorm(200,log(x_WM),abs(log(x_WSD)))) * 70 

dist_summary<-c(x_WM*70,quantile(dist,c(0,.5,.95)),mean(dist))
names(dist_summary)<-c("True Median","Min","Median","95th Percentile","Mean")

return(dist_summary)
}
get.child.food.2020 <- function(x){
  
  set.seed(12345)
  
  x$Min <- 0
  x$SD  <- x$`LB 95th Exposure` - x$Min/4
  x$GM  <- x$`LB Mean Exposure`/ (1+ .05 * (x$SD/x$`LB Mean Exposure`)^2)
  
  x_WM<-  WM(x$GM,x$`Number of subjects`)
  x_WSD<- WSD(x$GM,x$`Number of subjects`)
  
  dist <- (rlnorm(200,log(x_WM),abs(log(x_WSD)))) * 13.3
  
  dist_summary<-c(x_WM* 13.3,quantile(dist,c(0,.5,.95)),mean(dist))
  names(dist_summary)<-c("True Median","Min","Median","95th Percentile","Mean")
  
  
  return(dist_summary)
}

PFOA_Adults <- get.adult.food.2020(PFOAA)
PFOS_Adults <- get.adult.food.2020(PFOSA)
PFOA_Children <- get.child.food.2020(PFOAC)
PFOS_Children <- get.child.food.2020(PFOSC)


PFOA_Adults
PFOS_Adults
PFOA_Children
PFOS_Children


9+24.2+24.4
