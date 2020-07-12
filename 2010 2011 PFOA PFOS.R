


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

Food_Range<- foodcomparison(5,2.6)
