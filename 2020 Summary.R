# PFOA / PFOS Aggregate Route Exposure Model
# Alex East & Kosha Patel 7/27/20, ORAU/EPA



# ______________________________  Source & Tidy  ______________________________ #

rm(list=ls())
source('2020 Food.R')
source('2020 Media.R')
library('rio')
exposure.distributions<<- Map(c,exposures,foodexposures)
rm(list=setdiff(ls(), c("data","exposure.distributions")))



# ______________________________  Number of Datasets Used  ______________________________ #






# ______________________________  Datasets used With GM/GSD  ______________________________ #


# rearrange data columns 


# ______________________________  Summary Statistics ______________________________ #

for.summary <- function(x){
x<-data.frame(x)
colnames(x)<- str_replace_all(colnames(x),"[[:punct:]]"," ")

sumstats <- t(sapply(x,quantile,c(0,.10,.5,.75,.95,1)))
mymean   <- sapply(x,mean)
sumstats <- cbind(mymean,sumstats)
colnames(sumstats)<-c("Mean","Min","10th%","Median","75th%","95th","Max")


sumstats<- signif(sumstats,5)
rn <- rownames(sumstats)
sumstats<- apply(sumstats, 2, as.character)
rownames(sumstats)<-rn

y<-sumstats[str_detect(rownames(sumstats),c("PFOA Typical")),]
z<-sumstats[str_detect(rownames(sumstats),c("PFOS Typical")),]
a<-sumstats[str_detect(rownames(sumstats),c("PFOA Contaminated")),]
b<-sumstats[str_detect(rownames(sumstats),c("PFOS Contaminated")),]

result<- data.frame(rbind(y,z,a,b))
result$Path <- rownames(result)
colnames(result)<-c("Mean","Min","10th %","Median","75th %","95th %","Max","Path")
result<-result[,c(ncol(result), 1:(ncol(result)-1))]

return(result)
}


summary<- bind_rows(lapply(exposure.distributions,for.summary), .id = "Group")
summary$Path<- str_c(summary$Group," ",summary$Path)
summary <- within(summary, rm(Group))

# ______________________________  Tidy Generated Data  ______________________________ #

for.boxplot<-function(x){

x<-data.frame(x)
colnames(x)<- str_replace_all(colnames(x),"[[:punct:]]"," ")
x<- data.frame(do.call(c,x))

# Chemical
x$Chemical[str_detect(rownames(x),"PFOA")]<- "PFOA"
x$Chemical[str_detect(rownames(x),"PFOS")]<- "PFOS"

# Scenario
x$Scenario[str_detect(rownames(x),"Typical")]<- "Typical"
x$Scenario[str_detect(rownames(x),"Contaminated")]<- "Contaminated"

# Route
x$Route[str_detect(rownames(x),"Indoor Air")]     <- "Indoor Inhalation"
x$Route[str_detect(rownames(x),"Outdoor Air")]    <- "Outdoor Inhalation"
x$Route[str_detect(rownames(x),"Water")]          <- "Water Ingestion"
x$Route[str_detect(rownames(x),"Soil")]           <- "Soil Ingestion"
x$Route[str_detect(rownames(x),"Dermal Dust")]    <- "Dust Ingestion"
x$Route[str_detect(rownames(x),"Ingestion Dust")] <- "Dust Inhalation"
x$Route[str_detect(rownames(x),"Dietary")]        <- "Dietary Ingestion"

names(x)[1] <- "Intake"
rownames(x) <- NULL
x<- x[,c("Chemical","Route","Scenario","Intake")]

return(x)
}

distdata <-bind_rows(lapply(exposure.distributions,for.boxplot), .id = "Group")

# ______________________________  Sum Median PFOA/PFOS  ______________________________ #

for.medians<-function(x){

x<- data.frame(x)
colnames(x)<- str_replace_all(colnames(x),"[[:punct:]]"," ")

lapply(x[(str_detect(colnames(x),"PFOA Typical")),],median)

MedianPFOA <- sum(sapply(x[(str_detect(colnames(x),"PFOA Typical"))],median))
MedianPFOS <- sum(sapply(x[(str_detect(colnames(x),"PFOS Typical"))],median))

medians <- data.frame(c(MedianPFOA),c(MedianPFOS))
colnames(medians)<-c("Median PFOA ng/day"," Median PFOS ng/day")
return(medians)
}

medians<- bind_rows(lapply(exposure.distributions,for.medians),.id = "Group")




# ______________________________  Export Results ______________________________ #


results<-list(medians,summary,data,distdata)
names(results)<- c("Daily Sum Median Intake (ng)","Summary Statistics","Datasets Used","Raw Generated Data")


date<-Sys.Date()
date<- str_replace_all(date,"[[:punct:]]","")
filename <- str_c("Aggregate Intake Model Results ",date,".xlsx")

export(results,filename)
