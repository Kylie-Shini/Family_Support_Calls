# Load necessary libraries
library(ggplot2)      # For creating plots
library(readr)        # For reading CSV files
library(gganimate)    # For creating animations
library(gifski)       # To save as GIF
library(dplyr)        # For filtering data

# Step 1: Read the data
path <- read.csv("C:/Users/Beboj/OneDrive/Documents/Family support calls.csv")

# Step 2: Filter out rows where dad, mom, or sister are equal to 1
path_filtered <- path %>%
  filter(dad != 1 & mom != 1 & sister != 1)

# Check if the filtered data is empty
if (nrow(path_filtered) == 0) {
  stop("Filtered data is empty after removing rows with 1 values.")
}

# Step 3: Handle missing values in the filtered data
# Remove rows with NA in any of the predictor or response variables
path_filtered <- path_filtered[!is.na(path_filtered$dad) & !is.na(path_filtered$mom) & !is.na(path_filtered$sister) & !is.na(path_filtered$duration), ]

# Check if rows were removed
print(nrow(path_filtered))  # Make sure we still have data after removing NAs

# Step 4: Convert columns to numeric if they are factors or characters
path_filtered$dad <- as.numeric(as.character(path_filtered$dad))
path_filtered$mom <- as.numeric(as.character(path_filtered$mom))
path_filtered$sister <- as.numeric(as.character(path_filtered$sister))

# Step 5: Perform the multiple linear regression on filtered data
model <- lm(duration ~ dad + mom + sister, data = path_filtered)

# Step 6: View the model summary to get coefficients
summary(model)

# Extract model coefficients
coefficients <- coef(model)
beta0 <- coefficients[1]  # Intercept (beta0)
beta1 <- coefficients[2]  # Coefficient for dad
beta2 <- coefficients[3]  # Coefficient for mom
beta3 <- coefficients[4]  # Coefficient for sister

# Step 7: Create a sequence for duration values for animation
duration_range <- seq(min(path_filtered$duration), max(path_filtered$duration), length.out = 100)

# Create an empty data frame to store the results for each frame
prediction_data <- data.frame(
  duration = rep(duration_range, 3),
  predictor = rep(c("Dad", "Mom", "Sister"), each = length(duration_range)),
  predicted_value = NA,
  time = rep(1:100, 3)  # Add time variable for step-by-step animation
)

# Loop through the duration range and calculate the predicted values
for (i in 1:length(duration_range)) {
  target_duration <- duration_range[i]
  
  # Solve for dad:
  predicted_dad <- (target_duration - beta0 - beta2 * median(path_filtered$mom) - beta3 * median(path_filtered$sister)) / beta1
  
  # Solve for mom:
  predicted_mom <- (target_duration - beta0 - beta1 * median(path_filtered$dad) - beta3 * median(path_filtered$sister)) / beta2
  
  # Solve for sister:
  predicted_sister <- (target_duration - beta0 - beta1 * median(path_filtered$dad) - beta2 * median(path_filtered$mom)) / beta3
  
  # Store the predicted values
  prediction_data$predicted_value[prediction_data$predictor == "Dad" & prediction_data$duration == target_duration] <- predicted_dad
  prediction_data$predicted_value[prediction_data$predictor == "Mom" & prediction_data$duration == target_duration] <- predicted_mom
  prediction_data$predicted_value[prediction_data$predictor == "Sister" & prediction_data$duration == target_duration] <- predicted_sister
}

# Step 8: Perform simple linear regressions for each variable on the filtered data
model_dad <- lm(dad ~ duration, data = path_filtered)
model_mom <- lm(mom ~ duration, data = path_filtered)
model_sister <- lm(sister ~ duration, data = path_filtered)

# Step 9: Predictions for each model
new_times <- data.frame(duration = seq(min(path_filtered$duration), max(path_filtered$duration), length.out = 100))

# Get predictions for each variable (dad, mom, sister)
predictions_dad <- predict(model_dad, newdata = new_times, interval = "confidence")
predictions_mom <- predict(model_mom, newdata = new_times, interval = "confidence")
predictions_sister <- predict(model_sister, newdata = new_times, interval = "confidence")

# Check the structure of predictions
head(predictions_dad)  # Ensure this contains fit, lwr, upr
head(predictions_mom)
head(predictions_sister)

# Combine predictions with new_times and include a column for the variable name
prediction_data_dad <- cbind(new_times, predictions_dad, variable = "dad")
prediction_data_mom <- cbind(new_times, predictions_mom, variable = "mom")
prediction_data_sister <- cbind(new_times, predictions_sister, variable = "sister")

# Combine the three prediction datasets
prediction_data_full <- rbind(prediction_data_dad, prediction_data_mom, prediction_data_sister)

# Step 10: Create the combined plot with dynamic predictions
combined_plot <- ggplot(path_filtered, aes(x = duration)) +
  # Scatter points for original data with gradual appearance (use transition_states)
  geom_point(aes(y = dad), color = "blue", alpha = 0.6) +
  geom_point(aes(y = mom), color = "green", alpha = 0.6) +
  geom_point(aes(y = sister), color = "purple", alpha = 0.6) +
  
  # Add regression lines and ribbons for confidence intervals
  geom_line(data = prediction_data_full, aes(y = fit, color = variable), size = 1) +
  geom_ribbon(data = prediction_data_full, aes(ymin = lwr, ymax = upr, fill = variable), alpha = 0.2) +
  
  # Labels and customizations
  labs(title = "Prediction of Call Duration with Dad, Mom, and Sister",
       subtitle = "Regression Line and Predictions Over Duration",
       x = "Duration of Call",
       y = "Predicted Duration",
       color = "Variable",
       fill = "Variable") +
  scale_color_manual(values = c("dad" = "blue", "mom" = "green", "sister" = "purple")) +
  scale_fill_manual(values = c("dad" = "blue", "mom" = "green", "sister" = "purple")) +
  
  # Use transition_states() to animate variables appearing one by one
  transition_states(states = prediction_data$predictor, 
                    transition_length = 0.5,  # Faster transition length
                    state_length = 0.5) +     # Faster state duration
  ease_aes('linear')  # Smooth transition effect

# Step 11: Render the animation and save as looping GIF
anim <- animate(combined_plot, nframes = 20, width = 600, height = 400,  # Reduced frames for faster speed
                renderer = gifski_renderer("prediction_animation_filtered.gif"), loop = TRUE)

# Optionally display the animation
anim
