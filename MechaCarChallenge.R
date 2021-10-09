#Deliverable 1: Linear Regression to Predict MPG

library(dplyr)
library(tidyverse)

#  Read in the .csv file.
MechaCar_data <- read.csv(file="MechaCar_mpg.csv",check.names = F,stringsAsFactors = F)

# Perform linear regression
reg <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_data)
summary(reg)

#Deliverable 2: Summary Statistics on Suspension Coils

#  Read in the .csv file.
suspension_Coils_data <- read.csv(file="Suspension_Coil.csv",check.names = F,stringsAsFactors = F)

#Create dataframe using summarize() function of the PSI column
total_summary <- suspension_Coils_data %>% summarize(Mean = mean(PSI), Median=median(PSI), 
                                                     Variance=var(PSI), SD= sd(PSI), .groups = 'keep' )
total_summary

#Create dataframe using groupby() and summarize() to group manufactoring lot of the PSI column
lot_summary <- suspension_Coils_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median=median(PSI), 
                                                                                   Variance=var(PSI), SD= sd(PSI), .groups = 'keep' )
lot_summary


#Deliverable 3: T-Test on Suspension Coils
#compare sample versus population means
all_Lots = t.test(suspension_Coils_data$PSI,mu=1500)
#Manufactoring lot 1
lot1 = t.test(subset(suspension_Coils_data, Manufacturing_Lot == "Lot1")$PSI,mu=1500)
lot1
#Manufactoring lot 2
lot2 = t.test(subset(suspension_Coils_data, Manufacturing_Lot == "Lot2")$PSI,mu=1500)
lot2
#Manufactoring lot 3
lot3= t.test(subset(suspension_Coils_data, Manufacturing_Lot == "Lot3")$PSI,mu=1500)
lot3


