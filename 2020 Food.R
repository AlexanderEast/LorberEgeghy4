### Food

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

foodcomparison(5,1.5)


rm(list = ls())


GM<-5
GSD<-1.4
Reps<-1

sample.size.comparison<- function(GM,GSD,Reps){

summaries<- data.frame(t(replicate(Reps,summary(rlnorm(200,log(GM),log(GSD))))))
names(summaries)<- c("Min","1stQ","Median","Mean","3rdQ","Max")

summary1<- sapply(summaries,mean)


summaries<- data.frame(t(replicate(Reps,summary(rlnorm(2000,log(GM),log(GSD))))))
names(summaries)<- c("Min","1stQ","Median","Mean","3rdQ","Max")

summary2<- sapply(summaries,mean)


summaries<- data.frame(t(replicate(Reps,summary(rlnorm(20000,log(GM),log(GSD))))))
names(summaries)<- c("Min","1stQ","Median","Mean","3rdQ","Max")

summary3<- sapply(summaries,mean)

summaries<- data.frame(t(replicate(Reps,summary(rlnorm(200000,log(GM),log(GSD))))))
names(summaries)<- c("Min","1stQ","Median","Mean","3rdQ","Max")

summary4<- sapply(summaries,mean)

summaries<- data.frame(t(replicate(Reps,summary(rlnorm(2000000,log(GM),log(GSD))))))
names(summaries)<- c("Min","1stQ","Median","Mean","3rdQ","Max")

summary5<- sapply(summaries,mean)

summarytotal<- rbind(summary1,summary2,summary3,summary4,summary5)
rownames(summarytotal)<- c("n=200","n=2,000","n=20,000","n=200,000","n=2,000,000")

return(summarytotal)
}

sample.size.comparison(100,2.3,20)




log(log(5))
