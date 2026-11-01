###############################################################################
#                                                                             #
#        W4479 - Applied project using R, winter semester 2024 / 2025         #
#                                                                             #
#                     Problem 2: Simple Linear Regression                     #
#                                                                             #
###############################################################################

# Information:

# Use this file to save your code for solving Problem 2 of the applied 
# project.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# <------- Start here with your own code --------> # 

# Load libraries

library(ggplot2)
library(dplyr)
#---------------------------------------------------------------  
#------ 2A.Give the source of your data, describe the two variables and create a table
#with the following information for each of the two variables: number of observations,
# minimum value, maximum value, sample mean, sample variance.
#--------------------------------- ------------------------------ 
# Load dataset 
data <- read.csv("P_Data_Extract_From_World_Development_Indicators.csv", sep=";")

# Estructure of dataset
str(data)
# Rename columns for simplicity
colnames(data) <- c("Country","Code", "Year", "Time_Code","Households_consumption_expenditure", "GDP")

# Ensure the dataset has just valid values
data <- data %>% filter(!is.na(Households_consumption_expenditure), !is.na(GDP))

# just for simplicity we ajust both into billions
data$Households_consumption_expenditure_Billions <- data$Households_consumption_expenditure / 1e9
data$GDP_Billions <- data$GDP / 1e9

# Summary statistics for each variables
summary_stats <- data.frame(
  Variable = c("GDP_Billions", "Households_consumption_expenditure_Billions"),
  Observations = c(nrow(data), nrow(data)),
  Minimum = c(min(data$GDP_Billions), min(data$Households_consumption_expenditure_Billions)),
  Maximum = c(max(data$GDP_Billions), max(data$Households_consumption_expenditure_Billions)),
  Mean = c(mean(data$GDP_Billions), mean(data$Households_consumption_expenditure_Billions)),
  Variance = c(var(data$GDP_Billions), var(data$Households_consumption_expenditure_Billions))
)

print(summary_stats)

#---------------------------------------------------------------  
#------ 2B.Fit a simple linear regression model to the data using OLS
#and state the fitted model.
#--------------------------------- ------------------------------ 

# Fit a simple linear regression model
model <- lm(Households_consumption_expenditure_Billions ~ GDP_Billions, data = data)
summary(model)



#---------------------------------------------------------------  
#------ 2C.Show the observations together with the fitted regression line from b) in a single
#graphic. Comment on the fit of the line.
#--------------------------------- ------------------------------ 

# Plot the observations and fitted regression line
plot <- ggplot(data, aes(x = GDP_Billions, y = Households_consumption_expenditure_Billions)) +
  geom_point(aes(shape = "1"), color = "black", size = 3) +
  geom_smooth(aes(linetype = "1"), method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(title = "Household Consumption Expenditure in Relation to GDP in Germany (1970–2023), 
       Both in Billions of 2015 Dollars"
       , x = "GDP", y = "Households consumption expenditure")+
  scale_shape_manual(name = "Points:", values = 16, labels = "Observations") +
  scale_linetype_manual(name = "Lines:", values = 1, labels = "Regression line")+
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# Print the graphic.! 
print(plot)



#---------------------------------------------------------------  
#------ 2d) Interpret both the estimated intercept and the estimated slope from the model fitted
# in b). Comment on whether or not these interpretations are logical.
#--------------------------------- ------------------------------ 

intercept <- coef(model)[1]
slope <- coef(model)[2]
cat("Intercept:", intercept, "\n")
cat("Slope:", slope, "\n")


regr_summary <- summary(model)
print(regr_summary)



r2 <- regr_summary$r.squared
RSS <- sum(model$residuals^2)
TSS <- sum((data$GDP_Billions - mean(data$Households_consumption_expenditure_Billions))^2)
ESS <- r2 / (1 - r2) * RSS      # calculate ESS from RSS and r^2
round(c(r.squared = r2, RSS = RSS, ESS = ESS), digits = 3)


#---------------------------------------------------------------  
#------ 2f) State the (approximate) 99%-confidence interval both for β1, i.e. the intercept, and
# β2, i.e. the slope.
#--------------------------------- ------------------------------ 

# Degrees of freedom
n <- nrow(data)
df <- n - 2

# Critical t-value for 99% confidence interval (two-tailed)
t.99 <- qt(0.995, df = df)

# Intercept (β1)
b1 <- coef(model)[[1]]
se.b1 <- summary(model)$coefficients[1, 2]
b1.low <- b1 - t.99 * se.b1
b1.high <- b1 + t.99 * se.b1

# Slope (B2)
b2 <- coef(model)[[2]]
se.b2 <- summary(model)$coefficients[2, 2]
b2.low <- b2 - t.99 * se.b2
b2.high <- b2 + t.99 * se.b2

# Combine confidence intervals
confint_99 <- data.frame(
  Coefficient = c("Intercept (B1)", "Slope (B2)"),
  `Lower Bound (0.005)` = c(b1.low, b2.low),
  Estimate = c(b1, b2),
  `Upper Bound (0.995)` = c(b1.high, b2.high)
)
print(confint_99)


#---------------------------------------------------------------  
#------ 2g) Conduct a one-tail hypothesis test with regard to the slope at a significance level of
# 5% with self-selected null and alternative hypotheses.
#--------------------------------- ------------------------------ 

t_value <- b2 / se.b2  # Calculate t-statistic

# One-tail hypothesis test: Calculate the critical value and p-value
alpha <- 0.05
critical_t <- qt(1 - alpha, df)        # Critical t-value for one-tail test
p_value_test <- pt(t_value, df, lower.tail = FALSE)  # One-tail p-value

# Output results
list(t_value = t_value, critical_t = critical_t, p_value_test = p_value_test)

