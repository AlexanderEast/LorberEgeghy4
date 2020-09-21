
rm(list = ls())
source('2020 Summary.R')

data<- results$`Raw Generated Data`
rm(results)


addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

addline_format(c("Dust Ingestion", "Dietary Ingestion",
                 "Water Ingestion", "Dermal Dust", "Indoor Inhalation",
                 "Outdoor Inhalation", "Soil Ingestion"))



data<- data[(data$Scenario == "Typical"),]
data$Route <- factor(data$Route, levels = c("Dust Ingestion", "Dietary Ingestion",
                                            "Water Ingestion", "Dermal Dust", "Indoor Inhalation",
                                            "Outdoor Inhalation", "Soil Ingestion"))





data$Group[(data$Group == "Child")] <- "Children"
data$Group[(data$Group == "Adult")] <- "Adults"

table(data$Group)

ggplot(data, aes( x = Route, y = Intake, fill = Chemical)) +
  geom_boxplot(outlier.color = 'Black', 
               outlier.alpha = .25,
               outlier.size = 1)+
  scale_y_continuous(trans = "log",
                     breaks = c(.0001,.001,.01,1,10,100,1000,10000,100000),
                     labels = function(x) sprintf("%g", x))+
                     facet_grid(Chemical~Group)+
  labs(title = "Typical PFOA and PFOS Intakes Across Routes for Adults and Children ",
       x = "Exposure Route",
       y = "Estimated Intake (ng/day)",
       fill = "Chemical") +
  scale_fill_manual(values=c("#1b8cff","#ff8e1b"))+
  theme_gray() 



PFOA <- data[(data$Chemical == "PFOA"),]
PFOS <- data[(data$Chemical == "PFOS"),]


p<- ggplot(PFOA, aes( x = Route, y = Intake)) +
  geom_boxplot(outlier.color = 'Black', 
               outlier.alpha = .1,
               outlier.size = 1,
               width = 0.30,
               fill = "#e56d74")+
  scale_y_continuous( trans = "log",
                      limits = c(0.00001,10000),
                      breaks = c(0.00001,.0001,.001,.01,.1,1,10,100,1000,10000),
                      labels = function(x) sprintf("%g", x))+
  scale_x_discrete(labels = addline_format(c("Dust Ingestion", "Dietary Ingestion",
                                             "Water Ingestion", "Dermal Dust", "Indoor Inhalation",
                                             "Outdoor Inhalation", "Soil Ingestion")))+
  facet_grid(.~Group)+
  labs(title = "Typical PFOA Intakes Across Routes for Adults and Children ",
       x = "Exposure Route",
       y = "Estimated Intake (ng/day)")

p + theme_gray()


#699fb3

ggplot(PFOS, aes( x = Route, y = Intake)) +
  geom_boxplot(outlier.color = 'Black', 
               outlier.alpha = .1,
               outlier.size = 1,
               width = 0.35,
               fill = "#6db0e5")+
  scale_y_continuous(trans = "log",
                     limits = c(0.00001,10000),
                     breaks = c(0.00001,.0001,.001,.01,.1,1,10,100,1000,10000),
                     labels = function(x) sprintf("%g", x))+
  scale_x_discrete(labels = addline_format(c("Dust Ingestion", "Dietary Ingestion",
                                             "Water Ingestion", "Dermal Dust", "Indoor Inhalation",
                                             "Outdoor Inhalation", "Soil Ingestion")))+
  facet_grid(.~Group)+
  labs(title = "Typical PFOS Intakes Across Routes for Adults and Children ",
       x = "Exposure Route",
       y = "Estimated Intake (ng/day)")+ 
  theme_gray() 






