


source('2020 Summary.R')

table(BPFOA$Route)
tidy_dist<-results$`Raw Generated Data`

BPFOA <- tidy_dist[(tidy_dist$Chemical == "PFOA"),]
BPFOS <- tidy_dist[(tidy_dist$Chemical == "PFOS"),]




BPFOA$Route[BPFOA$Route == "Dust Ingestion"] <- "Dermal Dust"



rm(list=setdiff(ls(), c("BPFOA","BPFOS")))

BPFOA %>% mutate(Group = factor(Group, 
                                levels = c("Adult", "Child"))) %>% 
  ggplot(aes(x = Route, y = Intake, fill = Group))+
  scale_y_continuous(trans = "log",
                     labels = function(x) sprintf("%g", x),
                     breaks = c(.0001,.001,.01,1,10,100,1000))+
  geom_boxplot(outlier.colour = "black",
               outlier.shape =  16,
               notch = FALSE) +
  scale_fill_discrete(limits = c("Child", "Adult"),
                      labels = c("Child", "Adult")) + 
  labs(title = "Typical PFOA Intake Across Exposure Routes",
       x = "Exposure Route",
       y = "Estimated Intake (ng/day)",
       fill = "Age Group") +
  theme_gray() 

BPFOS %>% mutate(Group = factor(Group, 
                                levels = c("Seniors", "Adult", "Child"))) %>% 
  ggplot(aes(x = Route, y = Intake, fill = Group))+
  scale_y_continuous(trans = "log2",
                     labels = function(x) sprintf("%g", x))+
  geom_boxplot(outlier.colour = "black",
               outlier.shape =  16,
               notch = FALSE) +
  scale_fill_discrete(limits = c("Child", "Adult", "Seniors"),
                      labels = c("Child", "Adult", "Senior")) + 
  labs(title = "Typical PFOS Intake Across Exposure Routes",
       x = "Exposure Route",
       y = "Estimated Intake (ng/day)",
       fill = "Age Group") +
  theme_gray() 



