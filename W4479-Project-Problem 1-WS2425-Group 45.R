###############################################################################
#                                                                             #
#        W4479 - Applied project using R, winter semester 2024 / 2025         #
#                                                                             #
#                        Problem 1: Monte Carlo Study                         #
#                                                                             #
###############################################################################

# Information:

# Use this file to save your code for solving Problem 1 of the applied 
# project.
# Remove '1234567' in line 20 and instead, replace it with the matriculation 
# number of one of your group members. Run lines 20 to 87 first to make 
# your group's simulated data available in R before continuing with your own 
# code.

# Start at line 90 with your own code.

MatrNr <- 4027045

simP1 <- function(seed = MatrNr, b1 = 3.4, b2 = 0.65, M = 1000) {
  n <- c(35, 120, 500) # spl size
  N <- 1000 # pop size
  # M - Monte-Carlo simulations
  
  rchisq_dm <- function(n, df, mean = 0, sd = 1) {
    (rchisq(n = n, df = df) - df) / sqrt(2 * df) * sd + mean
  }
  
  set.seed(seed)
  Xmin <- 0
  Xmax <- 250
  X_pop = runif(N, Xmin, Xmax)
  u_norm = rnorm(N, sd = 6)
  ui_rchisq = rchisq_dm(N, df = 4, sd = 6)
  Y_norm = b1 + b2 * X_pop + u_norm
  Y_rchisq = b1 + b2 * X_pop + ui_rchisq
  est_all = list()
  for (i in 1:3) {
    est = matrix(0, M, 4)
    for (j in 1:M) {
      ind_spl = sort(sample(1:N, n[i], replace = FALSE))
      est[j, 1] = lm(Y_norm[ind_spl] ~ X_pop[ind_spl])$coefficients[1]
      est[j, 2] = lm(Y_norm[ind_spl] ~ X_pop[ind_spl])$coefficients[2]
      est[j, 3] = lm(Y_rchisq[ind_spl] ~ X_pop[ind_spl])$coefficients[1]
      est[j, 4] = lm(Y_rchisq[ind_spl] ~ X_pop[ind_spl])$coefficients[2]
    }
    colnames(est) = c("Intercept", "Slope", "Intercept", "Slope")
    est_all[[i]] = est
  } 
  names(est_all) = c("n=35", "n=120", "n=500")
  return(est_all)
} 
est_all = simP1(MatrNr)

M = 1000
est35n <- data.frame(cbind("Sample No." = rep(1,M), 
                           "Number of Observations" = rep(35, M), "Error Function" = "Normal",
                           est_all$`n=35`[,c(1,2)]))
est35chi <- data.frame(cbind("Sample No." = rep(1,M), 
                             "Number of Observations" = rep(35, M), "Error Function" = "chi-squared",
                             est_all$`n=35`[,c(3,4)]))
est35 <- rbind(est35n, est35chi)


est120n <- data.frame(cbind("Sample No." = rep(2,M), 
                            "Number of Observations" = rep(120, M), "Error Function" = "Normal",
                            est_all$`n=120`[,c(1,2)]))
est120chi <- data.frame(cbind("Sample No." = rep(2,M), 
                              "Number of Observations" = rep(120, M), "Error Function" = "chi-squared",
                              est_all$`n=120`[,c(3,4)]))
est120 <- rbind(est120n, est120chi)


est500n <- data.frame(cbind("Sample No." = rep(3,M), 
                            "Number of Observations" = rep(500, M), "Error Function" = "Normal",
                            est_all$`n=500`[,c(1,2)]))
est500chi <- data.frame(cbind("Sample No." = rep(3,M), 
                              "Number of Observations" = rep(500, M), "Error Function" = "chi-squared",
                              est_all$`n=500`[,c(3,4)]))
est500 <- rbind(est500n, est500chi)

sim_df <- rbind(est35, est120, est500)
sim_df$Intercept <- as.numeric(sim_df$Intercept)
sim_df$Slope <- as.numeric(sim_df$Slope)
head(sim_df)

# <------- Start here with your own code --------> # 



#install.packages("dplyr") #install if you don´t have it 
library(dplyr)

names(sim_df)
#---------------------------------------------------------------  
# ---->  1a. Calculate means and variances of intercept and slope by sample size and error dist.
#---------------------------------------------------------------  

results_1a <- sim_df %>%
  group_by(`Number.of.Observations`, `Error.Function`) %>%
  summarise(
    mean_intercept = mean(Intercept),
    var_intercept  = var(Intercept),
    mean_slope     = mean(Slope),
    var_slope      = var(Slope),
    .groups = "drop"
  )


print(results_1a)

#---------------------------------------------------------------  
#------ 1B.Create a Q-Q normal plot of the 1000 obtained values for each of the combinations
#--------------------------------- ------------------------------ 
# (It's similar to the question in the tutorial 4 exercise 4.5.6)
# Here we do a for loop through N =(35, 120, 500) and both distri.


library(ggplot2)
library(ggpubr)

for (n_val in c(35, 120, 500)) {
  
  # 1) First we divide the data into Subset one for Normal errors, sample size = n_val
  sub_data_normal <- subset(sim_df,
                            Number.of.Observations == n_val &
                              Error.Function == "Normal")
  
  #  Subset data for Chi-squared errors, sample size = n_val
  sub_data_chi <- subset(sim_df,
                         Number.of.Observations == n_val &
                           Error.Function == "chi-squared")
  
  # ------------ Intercept Q-Q Plots  ------------------
  
  # a) Intercept - we use the subset of the Normal Distribution
  plot_int_normal <- ggplot(sub_data_normal, aes(sample = Intercept)) +
    stat_qq(color = "black") +
    stat_qq_line(color = "red") +
    labs(
      title = paste("Intercept - Normal\nn =", n_val),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5)  # Center the title
    )
  
  # b) Intercept - chi-squared
  plot_int_chi <- ggplot(sub_data_chi, aes(sample = Intercept)) +
    stat_qq(color = "black") +
    stat_qq_line(color = "red") +
    labs(
      title = paste("Intercept - chi-squared\nn =", n_val),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5)  # Center the title
    )
  # ----------- Slope Q-Q Plots ------------------
  
  # c) Slope - Normal
  plot_slope_normal <- ggplot(sub_data_normal, aes(sample = Slope)) +
    stat_qq(color = "black") +
    stat_qq_line(color = "red") +
    labs(
      title = paste("Slope - Normal\nn =", n_val),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5)  # Center the title
    )
  
  # d) Slope - chi-squared
  plot_slope_chi <- ggplot(sub_data_chi, aes(sample = Slope)) +
    stat_qq(color = "black") +
    stat_qq_line(color = "red") +
    labs(
      title = paste("Slope - chi-squared\nn =", n_val),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )+
    theme(
      plot.title = element_text(hjust = 0.5)  # Center the title
    )
# ------- Arrange Plots to look it nice ------------------
    # Row 1: Intercept Normal vs. Intercept chi-squared
  row1 <- ggarrange(plot_int_normal, plot_int_chi, ncol = 2, nrow = 1)
   # Row 2: Slope Normal vs. Slope chi-squared
  row2 <- ggarrange(plot_slope_normal, plot_slope_chi, ncol = 2, nrow = 1)
  
  ## Combine row1 and row2 into a 2x2 layout
  combined_plot <- ggarrange(row1, row2, ncol = 1, nrow = 2)

  
  print(combined_plot)
}

#---------------------------------------------------------------  
#------ 1C.test the normality of the 1000 obtained values for each
#          of the combinations of nj = 35, 120, 500 with error
#         distribution (1) and (2) using a Jarque-Bera test.
#--------------------------------- ------------------------------ 
# (It's also similar to the question in the tutorial 4 exercise 4.5.6) 

# First we create a especial function to calculate the Jarque-Bera 
# this return everything what we need 
JB_test <- function(x, alpha = 0.05) {
  x_c <- x - mean(x) #Center x
  n <- length(x_c) #sample size
  S <- (1 / n) * sum(x_c^3) / ((1 / n) * sum(x_c^2))^1.5  # Skewness
  K <- (1 / n) * sum(x_c^4) / ((1 / n) * sum(x_c^2))^2    # Kurtosis
  JB <- n / 6 * (S^2 + (K - 3)^2 / 4)  # Jarque-Bera statistic
  # Jarque-Bera statistic
  JB <- n / 6 * (S^2 + (K - 3)^2 / 4)
  # value (df=2)
  p_value <- 1 - pchisq(JB, df = 2)
  crit <- qchisq(0.95, df = 2)         # 5% critical value of chi-squared
  # Just to do it all in one step: reject or not at the chosen alpha
  reject <- (p_value < alpha)
  return(list(JB_stat = JB, p_value = p_value,crit=crit, reject = reject))
}


# THis is for store the results of the function in each iteration 
results_JB <- data.frame(
  n            = integer(),
  error_dist   = character(),
  parameter    = character(),   
  JB_stat      = numeric(),
  p_value      = numeric(),
  crit         = numeric(),
  reject       = logical(),
  stringsAsFactors = FALSE
)

# iterations! 
for (n_val in c(35, 120, 500)) {
  for (err_dist in c("Normal", "chi-squared")) {
      # NEW_Subset the data for this scenario
    sub_data <- subset(sim_df, Number.of.Observations == n_val &
            Error.Function == err_dist)
    
    
    # Jarque-Bera for the INTERCEPT
    jb_int <- JB_test(sub_data$Intercept)
    results_JB <- rbind(results_JB,
      data.frame(
                  n = n_val,
                  error_dist = err_dist,
                  parameter = "Intercept",
                  JB_stat = jb_int$JB_stat,
                  p_value = jb_int$p_value,
                  crit_value=jb_int$crit,
                  reject  = jb_int$reject
      )
    )
    
    # Jarque-Bera  for the SLOPE
    jb_slp <- JB_test(sub_data$Slope)
    results_JB <- rbind(results_JB,
      data.frame(
                    n = n_val,
                    error_dist = err_dist,
                    parameter = "Slope",
                    JB_stat = jb_slp$JB_stat,
                    p_value = jb_slp$p_value,
                    crit_value=jb_int$crit,
                    reject  = jb_slp$reject
      )
    )
  }
}
# Print the results
print(results_JB)


# just to see the result in one file
# write.csv(results_JB, "mi_tabla.csv", row.names = FALSE) 




