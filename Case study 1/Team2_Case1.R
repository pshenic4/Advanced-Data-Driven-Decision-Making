# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(broom)
library(lm.beta)
library(ggplot2)


# clear the database
rm(list=ls())

# Reading the data
url <- "https://github.com/pshenic4/Advanced-Data-Driven-Decision-Making/blob/main/Case%20study%201/Case_Stud_%20I.csv"
df <- read_csv(url)

# Step 1
## Plotting the data
### `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
ggplot(df, aes(x = number_of_solar_panels, y = manufacturing_cost)) +
  geom_point(color = 'blue') +
  geom_smooth(color = 'red')+
  labs(title = "Scatter Plot of Production vs. Manufacturing Cost", 
       x = "Total Production (Solar Panels)", 
       y = "Manufacturing Cost per Unit ($)") +
  theme_minimal()


## Logarithm transformation

df <- df %>% mutate(
  log_production = log(number_of_solar_panels),
  log_cost = log(manufacturing_cost)
)

## Plottin g transformed data
ggplot(df, aes(x = log_production, y = log_cost)) +
  geom_point(color = 'red') +
  geom_smooth(method = "lm", color = 'black', se = FALSE) +
  labs(title = "Log-Log Scatter Plot of Production vs. Cost with Fitted Curve", 
       x = "Log of Total Production", 
       y = "Log of Manufacturing Cost") +
  theme_minimal()

# Step 2 
## Regression analysis (using R to estimate the regression model)
model <- with(df, lm(log_cost ~ log_production))
summary(lm.beta(model))

################# interpretation of results for the question 2 ##########################

# Extract key model fit statistics
r_squared <- summary(model)$r.squared  # R-squared value
adj_r_squared <- summary(model)$adj.r.squared  # Adjusted R-squared
f_statistic <- summary(model)$fstatistic[1]  # F-statistic
p_value <- pf(f_statistic, summary(model)$fstatistic[2], summary(model)$fstatistic[3], lower.tail=FALSE)  # Model p-value

# Extract coefficient for learning rate calculation
beta_1 <- coef(model)[2]  # Slope (log-log elasticity)
learning_rate <- 2^beta_1  # Learning rate formula

# Print interpretation of results
cat("\n### Interpretation of the Regression Results ###\n")

# Model equation
cat("\n1. Model Equation:\n")
cat("log(Manufacturing Cost) = 7.8503 - 0.155 * log(Total Production)\n")

# How well does the model explain the data?
cat("\n2. Model Fit:\n")
cat("- R-squared:", r_squared, "-> This means 95.02% of the variation in manufacturing cost is explained by production.\n")
cat("- Adjusted R-squared:", adj_r_squared, "-> Since it's close to R², the model remains strong even after adjusting for predictors.\n")
cat("- F-statistic:", f_statistic, "with p-value:", p_value, "-> The extremely small p-value confirms the model is statistically significant.\n")

# Interpretation of coefficients
cat("\n3. Interpretation of Coefficients:\n")
cat("- Intercept (7.8503) -> When log_production is 0 (i.e., production = 1 unit), the estimated manufacturing cost is exp(7.8503) ≈ $2555.12.\n")
cat("- Slope (-0.155) -> A 1% increase in production results in a 0.155% decrease in manufacturing cost, confirming economies of scale.\n")

# Learning rate interpretation
cat("\n4. Learning Rate:\n")
cat("- Learning Rate (2^beta_1) =", learning_rate, "\n")
cat("- This means that when production doubles, the manufacturing cost per unit reduces to", round(learning_rate * 100, 2), "% of the previous cost.\n")
cat("- Since LR ≈ 90%, this implies a 10% cost reduction when production doubles.\n")

cat("\n### Conclusion: The model provides a strong explanation of the relationship between production and manufacturing cost, with clear economies of scale observed! ###\n")
 

############################################################################## 


# Step 3
## Calculate the expected average manufacturing cost per solar panel for the 400 solar panels that would be produced using the experience curve estimate b)
## Predict future costs for 4700, 4800, 4900, 5000 panels

# Generate a sequence of production values from 100 to 5000
production_seq <- seq(100, 5000, by = 50)

# Predict manufacturing costs using the experience curve equation
log_predicted_costs <- coef(model)[1] + coef(model)[2] * log(production_seq)
predicted_costs <- exp(log_predicted_costs)

# Create a data frame for plotting
curve_data <- data.frame(Production = production_seq, Cost = predicted_costs)

# Visualize the experience curve
ggplot(curve_data, aes(x = Production, y = Cost)) +
  geom_line(color = "blue", size = 1.2) +  # Experience curve line
  geom_point(data = df, aes(x = number_of_solar_panels, y = manufacturing_cost), color = "red", size = 2) +  # Actual data points
  scale_x_log10() +  # Log scale for better visualization
  scale_y_log10() +  # Log scale for cost
  labs(title = "Experience Curve: Production vs. Manufacturing Cost",
       x = "Number of Solar Panels (Log Scale)",
       y = "Manufacturing Cost per Unit (Log Scale)") +
  theme_minimal()


# Step 7: Compute 95% Confidence Interval for cost predictions
conf_interval <- confint(model, level = 0.95)
b_lower <- conf_interval[2,1]
b_upper <- conf_interval[2,2]
predicted_log_costs_lower <- coef(model)[1] + b_lower * log_future_production
predicted_log_costs_upper <- coef(model)[1] + b_upper * log_future_production
predicted_costs_lower <- exp(predicted_log_costs_lower)
predicted_costs_upper <- exp(predicted_log_costs_upper)

# Display confidence intervals
cat("\n95% Confidence Interval for Costs:\n")
for (i in seq_along(future_production)) {
  cat("For", future_production[i], "units: $", round(predicted_costs_lower[i], 2), "- $", round(predicted_costs_upper[i], 2), "per unit\n")
}

