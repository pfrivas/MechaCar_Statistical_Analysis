############ Deliverable 1 #################

# Use the library() function to load the dplyr package.
library(dplyr)

# import and read csv as a dataframe
MechaCar_data <- read_csv("./MechaCar_mpg.csv") #import MechaCar data
head(MechaCar_data) #display first rows of imported data

# linear regression model
lm(mpg ~ vehicle_length 
   + vehicle_weight 
   + spoiler_angle 
   + AWD 
   + ground_clearance, data=MechaCar_data) #generate multiple linear regression model

# summary statistics
summary(lm(mpg ~ vehicle_length 
           + vehicle_weight 
           + spoiler_angle 
           + AWD 
           + ground_clearance,data=MechaCar_data)) #generate summary statistics

############ Deliverable 2 #################

# import csv and read as a table
suspension_coil_data <- read_csv("./Suspension_Coil.csv") #import suspension coil data
head(suspension_coil_data) #display first rows of imported data

#stat summary with summarize()
total_summary <- suspension_coil_data %>% summarize(Mean=mean(PSI),
                                                    Median=median(PSI),
                                                    Variance=var(PSI),SD=sd(PSI), 
                                                    .groups = 'keep') #create total summary table
total_summary # display total summary table

# stat summary wtih group_by()
lot_summary <- suspension_coil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),
                                                                                  Median=median(PSI),
                                                                                  Variance=var(PSI),
                                                                                  SD=sd(PSI), 
                                                                                  .groups = 'keep') #create lot summary table
lot_summary # display lot summary table

# randomly sapling the data
global_sample_table <- suspension_coil_data %>% sample_n(50) #randomly sample 50 data points

# visualizing the distribution with the density plot

plt <- ggplot(suspension_coil_data,aes(x=PSI)) #import all coil data into ggplot2
plt + geom_density() #visualize distribution with density plot

plt <- ggplot(global_sample_table,aes(x=PSI)) #import sample coil data into ggplot2
plt + geom_density() #visualize distribution with density plot

plt <- ggplot(global_sample_table,aes(x=log10(PSI))) #import sample coil data into ggplot2
plt + geom_density() #visualize distribution with density plot


############ Deliverable 3 #################


# t-test for all the lots, pop mu = 1500 psi
t.test(global_sample_table$PSI,mu=mean(suspension_coil_data$PSI)) #compare sample versus population mean

# t-test for lot 1
t.test(subset(suspension_coil_data$PSI,suspension_coil_data$Manufacturing_Lot == "Lot1"),mu=mean(suspension_coil_data$PSI)) #compare Lot1 sample versus population mean

# t-test for lot 2
t.test(subset(suspension_coil_data$PSI,suspension_coil_data$Manufacturing_Lot == "Lot2"),mu=mean(suspension_coil_data$PSI)) #compare Lot2 sample versus population mean

# t-test for lot 3
t.test(subset(suspension_coil_data$PSI,suspension_coil_data$Manufacturing_Lot == "Lot3"),mu=mean(suspension_coil_data$PSI)) #compare Lot3 sample versus population mean