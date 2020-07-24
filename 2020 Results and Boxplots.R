# 1. Summary Statistics and Boxplots


# A. Combine food and non-food distributions. 










# B. Create Summary Statistics . 



x<-exposures$Child


sumstats <- t(sapply(x,quantile,c(0,.10,.5,.75,.95,1)))
mymean   <- sapply(x,mean)
sumstats <- cbind(mymean,sumstats)
colnames(sumstats)<-c("Mean","Min","10th%","Median","75th%","95th","Max")


MedianPFOA<- sum(sumstats[str_detect(rownames(sumstats),c("PFOA Typical")),][,("Median")])
MedianPFOS<- sum(sumstats[str_detect(rownames(sumstats),c("PFOS Typical")),][,("Median")])




class(sumstats)

names<- rownames(sumstats)

sumstats<- signif(sumstats,5)
sumstats<- apply(sumstats, 2, as.character)

names(sumstats)<-names


sumstats[order(row.names(sumstats)),]

y<-x[str_detect(rownames(x),c("PFOA Typical")),]



sumstats




# 2. Boxplots




# ______________________________ Three Exports ______________________________ #

# 1. Data Sheet (For Figure in Paper)
# 2. Raw Generated Data (For Boxplots)
# 3. Summary Statistics (For Results)



