# Food Read from Input File

rm(list=ls())


data <- read_excel('input/Input_072020.xlsx', sheet = 'EFSA Food', guess_max = 17000)

source('Common.R')


individuals<-get.people()


data<- data[complete.cases(data$`Number of subjects`,data$`LB Mean Exposure`,data$`LB 95th Exposure`),]
data$`Age class`<-tolower(data$`Age class`)


food <- function(x){

EF.Dietgroup<- tolower(x$`Dietary Group`)
EF.BW<-x$`Bodyweight (kg)`
EF.n<- x$n
agefood<- data[str_detect(EF.Dietgroup,data$`Age class`),]
agefood<-split(agefood,agefood$Chemical)

food.distribution <- function(y){
set.seed(as.numeric(read_excel('input/Input_072020.xlsx', sheet = 'Seed')))

y$Min <- 0
y$SD  <- y$`LB 95th Exposure` - y$Min/4
y$GM  <- y$`LB Mean Exposure`/ (1+ .05 * (y$SD/y$`LB Mean Exposure`)^2)

y_WM<-  WM(y$GM,y$`Number of subjects`)
y_WSD<- WSD(y$GM,y$`Number of subjects`)

dist <- (rlnorm(EF.n,log(y_WM),abs(log(y_WSD)))) * EF.BW

return(dist)
}

result<- data.frame(sapply(agefood,food.distribution))

colnames(result)<- str_c("Exposure to Dietary ", colnames(result))

return(result)
}



# ______________________________ Pass to Results and Boxplots.R ______________________________ #

foodexposures<- lapply(individuals,food)
rm(data,individuals,list=lsf.str())

# ____________________________________________________________________________________________ #


