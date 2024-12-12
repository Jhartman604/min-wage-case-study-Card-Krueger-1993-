#####
#Exam 3
# Jack Hartman
# ECON 418
# 12/11/2024
######


######
#preliminaries 
######

# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Turn off scientific notation
options(scipen = 999)

# load in data
setwd("/Users/johnhartman/Desktop/Fall 2024/ECON 418/exams")
my_data <- read.csv("ECON_418-518_Exam_3_Data.csv")

install.packages("data.table")

library(data.table)

# Load the dataset into a data.table
my_data <- fread("/Users/johnhartman/Desktop/Fall 2024/ECON 418/exams/ECON_418-518_Exam_3_Data.csv")

# Check 
head(my_data)  
str(my_data)
names(my_data)

#####
#question ii
#####

# Create indicator columns
my_data[, is_nov := ifelse(time_period == "Nov", 1, 0)]   # 1 if November, 0 otherwise
my_data[, is_nj := ifelse(state == 1, 1, 0)]              # 1 if New Jersey, 0 if Pennsylvania

# Calculate the mean total employment
mean_employment <- my_data[, .(mean_total_emp = mean(total_emp, na.rm = TRUE)), by = .(state, time_period)]

# Print the results
print(mean_employment)


#######
#question iii
#######


#  mean employment for each state and time period
mean_emp_state0_feb <- my_data[state == 0 & time_period == "Feb", mean(total_emp, na.rm = TRUE)]
mean_emp_state0_nov <- my_data[state == 0 & time_period == "Nov", mean(total_emp, na.rm = TRUE)]
mean_emp_state1_feb <- my_data[state == 1 & time_period == "Feb", mean(total_emp, na.rm = TRUE)]
mean_emp_state1_nov <- my_data[state == 1 & time_period == "Nov", mean(total_emp, na.rm = TRUE)]
# change in employment for each state
change_state0 <- mean_emp_state0_nov - mean_emp_state0_feb
change_state1 <- mean_emp_state1_nov - mean_emp_state1_feb
# Compute the Difference-in-Differences (DiD) estimate
DiD <- change_state1 - change_state0
DiD


#####
#question iv
#####

# Fit the DiD model using lm()
DiD_model <- lm(total_emp ~ post_treatment * nj, data = my_data_panel)

# View the model summary
summary(DiD_model)

# Given values
beta <- 7.750          # Coefficient for post_treatment:nj (ATT estimate)
SE <- 1.731            # Standard error for the ATT estimate
z <- 1.96              # z-value for 95% confidence interval

# Compute the confidence interval
lower_bound <- beta - z * SE
upper_bound <- beta + z * SE

# Print the results
cat("95% Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")

#####
#question vii
######

# Fit the DiD model with restaurant fixed effects
model_fe <- lm(total_emp ~ post_treatment * nj + factor(restaurant_id), data = my_data_panel)

# Summary of the model
summary(model_fe)

