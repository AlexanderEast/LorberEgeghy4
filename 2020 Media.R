# 2020 Concentration 

library('dplyr')
library('readxl')
library('stringr')
rm(list = ls())


# 1. Import

data <- read_excel('input/Input_7_17_2020.xlsx', sheet = 'Data', guess_max = 17000)




str_detect(data$Media_Type, "effluent|groundwater|ground|sediment")


data$Media_Type


#source('Weights and PK.R')