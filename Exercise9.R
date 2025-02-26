# Name: Kelbi Hall
# Date: 2025-02-26
# Purpose: ESS 330 - Analyze and Predict Ozone Levels using Linear Regression

# Load necessary libraries
library(tidyverse)
library(broom)
library(ggplot2)

# Load the airquality dataset
data("airquality")

# Step 1: Clean the data (Remove missing Ozone values)
cleaned_data <- airquality %>% drop_na(Ozone)

# Step 2: Fit a Linear Model to predict Ozone using Solar Radiation, Wind, and Temperature
ozone_model <- lm(Ozone ~ Solar.R + Wind + Temp, data = cleaned_data)

# Step 3: Extract R-squared value and explain it
r_squared <- summary(ozone_model)$r.squared
cat("The R² value is", round(r_squared, 2), "indicating that", 
    round(r_squared * 100, 1), "% of the variation in Ozone levels is explained 
    by Solar Radiation, Wind, and Temperature.\n")

# Step 4: Use broom::augment() to generate predictions
augmented_data <- augment(ozone_model)

# Step 5: Compute the correlation between actual and predicted Ozone values
correlation <- cor(augmented_data$Ozone, augmented_data$.fitted, use = "complete.obs")

# Step 6: Create a scatter plot of Actual vs. Predicted Ozone levels
ozone_plot <- ggplot(augmented_data, aes(x = Ozone, y = .fitted)) +
  geom_point(alpha = 0.7) +  # Scatter plot points
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 1:1 reference line
  labs(
    title = "Actual vs. Predicted Ozone Levels",
    x = "Actual Ozone",
    y = "Predicted Ozone",
    subtitle = paste("Correlation:", round(correlation, 2))
  ) +
  theme_minimal()

# Step 7: Display the plot
print(ozone_plot)

# Step 8: Save the plot to the img directory
if (!dir.exists("img")) {
  dir.create("img")
}

ggsave("img/actual_vs_predicted_ozone.png", plot = ozone_plot, width = 8, height = 6)

# End of script
