#installing and importing libraries
install.packages("psych")
install.packages("GGally")
library(qqplotr)
library(readxl)
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)
library(psych)


#read excel
data<-read_excel('Energy Efficiency Data.xlsx')


#Exploratory data analysis
dim(data)
names(data)
head(data)

col_names <- c('Relative_Compactness', 'Surface_Area','Wall_Area','Roof_Area','Overall_Height','Orientation','Glazing_Area',
               'Glazing_Area_Distribution','Heating_Load','Cooling_Load')
names(data) <- col_names
#Verify column names exist in the dataset
print(names(data))

# Inspecting for any NA
sum(is.na(data))
# Inspecting for null values
is.null(data) 
# Inspecting for any duplicate values
sum(duplicated(data)) 

# Compute descriptive statistics
descriptive_stats <- describe(data)
# Print results
print(descriptive_stats)


#visualisation
#Boxplot for Key Variables (Outlier Detection)
# Boxplot for Cooling Load
ggplot(data, aes(y = Cooling_Load)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(title = "Boxplot of Cooling_Load", y = "Cooling Load") +
  theme_minimal()
# Boxplot for Heating Load
ggplot(data, aes(y = Heating_Load)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.7) +
  labs(title = "Boxplot of Heating_Load", y = "Heating Load") +
  theme_minimal()
#Scatterplot to Explore Relationships
# Scatterplot: Surface_Area vs Heating_Load
ggplot(data, aes(x = Surface_Area, y = Heating_Load)) +
  geom_point(color = "darkred", alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Heating Load vs Surface Area", x = "Surface Area", y = "Heating Load") +
  theme_minimal()
# Scatterplot: Glazing_Area vs Cooling_Load
ggplot(data, aes(x = Glazing_Area, y = Cooling_Load)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Cooling Load vs Glazing Area", x = "Glazing Area", y = "Cooling Load") +
  theme_minimal()
# Scatterplot: Overall_Height vs Cooling_Load
ggplot(data, aes(x = Overall_Height, y = Cooling_Load)) +
  geom_point(color = "pink", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Cooling Load vs Overall_Height", x = "Glazing Area", y = "Cooling Load") +
  theme_minimal()

# Identify continuous variables
continuous_vars <- c("Relative_Compactness", "Surface_Area", "Wall_Area", 
                     "Roof_Area", "Overall_Height", "Glazing_Area", 
                     "Heating_Load", "Cooling_Load")
# Compute correlation matrix for continuous variables
cor_matrix <- cor(data[, continuous_vars], use = "complete.obs")

# Correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", number.cex = 0.7,
         title = "Correlation Heatmap")

# Density Plot for Heating Load
ggplot(data, aes(x = Heating_Load)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  labs(title = "Density Plot of Heating Load", x = "Heating Load", y = "Density") +
  theme_minimal()

# Desity Plot for Cooling Load
ggplot(data, aes(x = Cooling_Load)) +
  geom_density(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Density Plot of Cooling Load", x = "Cooling Load", y = "Density") +
  theme_minimal()

# Grouped Bar Plot for Heating_Load by Orientation
ggplot(data, aes(x = Orientation, y = Heating_Load, fill = Orientation)) +
  stat_summary(fun = mean, geom = "bar", color = "black") +
  labs(title = "Mean Heating Load by Orientation", x = "Orientation", y = "Mean Heating Load") +
  theme_minimal()


#normalisation
# Identify continuous variables
continuous_vars <- c("Relative_Compactness", "Surface_Area", "Wall_Area", 
                     "Roof_Area", "Overall_Height", "Glazing_Area", 
                     "Heating_Load", "Cooling_Load")

# Log-transform continuous variables
data_log_transformed <- data %>%
  mutate(across(all_of(continuous_vars), 
                ~ log(. + 1),  # Apply log(x + 1)
                .names = "{col}_log"))

# Combine log-transformed variables with the rest of the dataset
final_data_log <- data %>%
  select(-all_of(continuous_vars)) %>%  # Remove original continuous variables
  bind_cols(data_log_transformed %>% select(ends_with("_log")))  # Add log-transformed variables


# Reset final_data_log to data
data <- final_data_log
head(data)


#correlation analysis
# Define continuous variables (log-transformed if applicable)
continuous_log_vars <- names(data)[grepl("_log$", names(data))]
# Q-Q Plots
for (var in continuous_log_vars) {
  qqnorm(data[[var]], main = paste("Q-Q Plot of", var))
  qqline(data[[var]], col = "red")  # Add reference line
}
# Shapiro-Wilk Test for Normality
shapiro_results <- sapply(continuous_log_vars, function(var) shapiro.test(data[[var]])$p.value)
shapiro_results <- as.data.frame(shapiro_results)
colnames(shapiro_results) <- "P-Value"
shapiro_results

# Compute Pearson and Spearman correlation matrices
cor_pearson <- round(cor(data[, continuous_log_vars], method = "pearson"), 2)
cor_spearman <- round(cor(data[, continuous_log_vars], method = "spearman"), 2)
# Print Pearson and Spearman correlation matrices
print("Pearson Correlation Matrix")
print(cor_pearson)
print("Spearman Correlation Matrix")
print(cor_spearman)

# Visualize Correlation Heatmap for Pearson
corrplot(cor_pearson, method = "color", addCoef.col = "black", 
         title = "Pearson Correlation Heatmap", type = "upper")
# Visualize Correlation Heatmap for Spearman
corrplot(cor_spearman, method = "color", addCoef.col = "black", 
         title = "Spearman Correlation Heatmap", type = "upper")




#regression analysis
#Step 1: Refine Predictor Set and Fit Models

# Fit regression model for Heating_Load_log
model_heating <- lm(Heating_Load_log ~ Relative_Compactness_log + Wall_Area_log + 
                      Overall_Height_log + Glazing_Area_log + 
                      Orientation + factor(Glazing_Area_Distribution), 
                    data = data)
# Summary of the Heating_Load_log model
summary(model_heating)

# Fit regression model for Cooling_Load_log
model_cooling <- lm(Cooling_Load_log ~ Relative_Compactness_log + Wall_Area_log + 
                      Overall_Height_log + Glazing_Area_log + 
                      Orientation + factor(Glazing_Area_Distribution), 
                    data = data)
# Summary of the Cooling_Load_log model
summary(model_cooling)

#Remove the non-significant predictor (Orientation) and re-fit the regression model.

# Refine the Heating_Load_log model by removing Orientation
model_heating_refined <- lm(Heating_Load_log ~ Relative_Compactness_log + Wall_Area_log + 
                              Overall_Height_log + Glazing_Area_log + 
                              factor(Glazing_Area_Distribution), 
                            data = data)
# Summary of the refined Heating_Load_log model
summary(model_heating_refined)

# Refine the Cooling_Load_log model by removing Orientation
model_cooling_refined <- lm(Cooling_Load_log ~ Relative_Compactness_log + Wall_Area_log + 
                              Overall_Height_log + Glazing_Area_log + 
                              factor(Glazing_Area_Distribution), 
                            data = data)
# Summary of the refined Cooling_Load_log model
summary(model_cooling_refined)

#Step 2: Check Multicollinearity Using VIF

# Load the car package 
library(car)

# Calculate VIF for Heating_Load_log model
vif(model_heating_refined)

# Calculate VIF for Cooling_Load_log model
vif(model_cooling_refined)

#Investigate Multicollinearity:
#Strong correlation between Relative_Compactness_log and Overall_Height_log is likely causing high VIF values. Check their pairwise correlation.
cor(data$Relative_Compactness_log, data$Overall_Height_log)

# Refine Heating_Load_log model
model_heating_final <- lm(Heating_Load_log ~ Wall_Area_log + 
                            Overall_Height_log + Glazing_Area_log + 
                            factor(Glazing_Area_Distribution), data = data)
summary(model_heating_final)

# Refine Cooling_Load_log model
model_cooling_final <- lm(Cooling_Load_log ~ Wall_Area_log + 
                            Overall_Height_log + Glazing_Area_log + 
                            factor(Glazing_Area_Distribution), data = data)
summary(model_cooling_final)

# Re-check VIF for both refined models
vif(model_heating_final)
vif(model_cooling_final)

#outlier
#for heating
# Compute Cook's Distance
cooks_dist <- cooks.distance(model_heating_final)  # Replace 'model' with your regression model
threshold_cooks <- 4 / nrow(data)  # Common threshold for Cook's Distance
influential_points <- which(cooks_dist > threshold_cooks)
# Print Influential Points
print("Influential Points (Cook's Distance):")
print(influential_points)
# Plot Cook's Distance
plot(cooks_dist, main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Observation Index", pch = 19, col = "blue")
abline(h = threshold_cooks, col = "red", lwd = 2)

data_cleaned <- data[-influential_points, ]  # Exclude influential rows
model_cleaned <- lm(Heating_Load_log ~ Wall_Area_log + Overall_Height_log + Glazing_Area_log + factor(Glazing_Area_Distribution), data = data_cleaned)
summary(model_cleaned)



#for cooling
# Compute Cook's Distance
cooks_dist_c <- cooks.distance(model_cooling_final)  # Replace 'model' with your regression model
threshold_cooks_c <- 4 / nrow(data)  # Common threshold for Cook's Distance
influential_points_c <- which(cooks_dist_c > threshold_cooks_c)
# Print Influential Points
print("Influential Points (Cook's Distance):")
print(influential_points_c)
# Plot Cook's Distance
plot(cooks_dist_c, main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Observation Index", pch = 19, col = "blue")
abline(h = threshold_cooks_c, col = "red", lwd = 2)

data_cleaned_c <- data[-influential_points_c, ]  # Exclude influential rows
model_cleaned_c <- lm(Cooling_Load_log ~ Wall_Area_log + Overall_Height_log + Glazing_Area_log + factor(Glazing_Area_Distribution), data = data_cleaned)
summary(model_cleaned_c)


# Performance metrics for Heating_Load
rmse_original_heating <- sqrt(mean(resid(model_heating_final)^2))
rmse_cleaned_heating <- sqrt(mean(resid(model_cleaned)^2))
cat("RMSE (Original Heating):", rmse_original_heating, "\n")
cat("RMSE (Cleaned Heating):", rmse_cleaned_heating, "\n")

# Performance metrics for Cooling_Load
rmse_original_cooling <- sqrt(mean(resid(model_cooling_final)^2))
rmse_cleaned_cooling <- sqrt(mean(resid(model_cleaned_c)^2))
cat("RMSE (Original Cooling):", rmse_original_cooling, "\n")
cat("RMSE (Cleaned Cooling):", rmse_cleaned_cooling, "\n")

# R-squared comparison
cat("R-squared (Original Heating):", summary(model_heating_final)$r.squared, "\n")
cat("R-squared (Cleaned Heating):", summary(model_cleaned)$r.squared, "\n")

cat("R-squared (Original Cooling):", summary(model_cooling_final)$r.squared, "\n")
cat("R-squared (Cleaned Cooling):", summary(model_cleaned_c)$r.squared, "\n")

# Residual diagnostics for Heating_Load_log
par(mfrow = c(2, 2))
plot(model_cleaned, main = "Heating_Load Cleaned Model Diagnostics")

#Residual Diagnostics
# Residual diagnostics for Cooling_Load_log
par(mfrow = c(2, 2))
plot(model_cleaned_c, main = "Cooling_Load Cleaned Model Diagnostics")


#ridge regression
install.packages("glmnet")
library(glmnet)
# Prepare data for Ridge Regression (Heating)
x_heating <- model.matrix(Heating_Load_log ~ Wall_Area_log + Overall_Height_log + 
                            Glazing_Area_log + factor(Glazing_Area_Distribution), 
                          data = data_cleaned)[, -1]
y_heating <- data_cleaned$Heating_Load_log

# Prepare data for Ridge Regression (Cooling)
x_cooling <- model.matrix(Cooling_Load_log ~ Wall_Area_log + Overall_Height_log + 
                            Glazing_Area_log + factor(Glazing_Area_Distribution), 
                          data = data_cleaned_c)[, -1]
y_cooling <- data_cleaned_c$Cooling_Load_log

# Fit Ridge Regression for Heating
ridge_heating <- glmnet(x_heating, y_heating, alpha = 0)  # Alpha = 0 for Ridge
print(ridge_heating)

# Fit Ridge Regression for Cooling
ridge_cooling <- glmnet(x_cooling, y_cooling, alpha = 0)  # Alpha = 0 for Ridge
print(ridge_cooling)

# Cross-validation for Heating
cv_ridge_heating <- cv.glmnet(x_heating, y_heating, alpha = 0)
best_lambda_heating <- cv_ridge_heating$lambda.min
cat("Best Lambda (Heating):", best_lambda_heating, "\n")

# Cross-validation for Cooling
cv_ridge_cooling <- cv.glmnet(x_cooling, y_cooling, alpha = 0)
best_lambda_cooling <- cv_ridge_cooling$lambda.min
cat("Best Lambda (Cooling):", best_lambda_cooling, "\n")

# Refit Ridge Regression for Heating
ridge_model_heating <- glmnet(x_heating, y_heating, alpha = 0, lambda = best_lambda_heating)
print(coef(ridge_model_heating))  # Coefficients

# Refit Ridge Regression for Cooling
ridge_model_cooling <- glmnet(x_cooling, y_cooling, alpha = 0, lambda = best_lambda_cooling)
print(coef(ridge_model_cooling))  # Coefficients

# Predictions and RMSE for Heating
pred_heating <- predict(ridge_model_heating, s = best_lambda_heating, newx = x_heating)
rmse_heating <- sqrt(mean((y_heating - pred_heating)^2))
cat("RMSE (Ridge - Heating):", rmse_heating, "\n")

# Predictions and RMSE for Cooling
pred_cooling <- predict(ridge_model_cooling, s = best_lambda_cooling, newx = x_cooling)
rmse_cooling <- sqrt(mean((y_cooling - pred_cooling)^2))
cat("RMSE (Ridge - Cooling):", rmse_cooling, "\n")

# Heating Plot
ggplot(data_cleaned, aes(x = pred_heating, y = y_heating)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs. Predicted Heating Load (Ridge Regression)", 
       x = "Predicted Heating Load (Log)", 
       y = "Actual Heating Load (Log)") +
  theme_minimal()

# Cooling Plot
ggplot(data_cleaned_c, aes(x = pred_cooling, y = y_cooling)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs. Predicted Cooling Load (Ridge Regression)", 
       x = "Predicted Cooling Load (Log)", 
       y = "Actual Cooling Load (Log)") +
  theme_minimal()

# Predictions for Heating
predictions_ridge_heating <- predict(ridge_model_heating, s = best_lambda_heating, newx = x_heating)

# Predictions for Cooling
predictions_ridge_cooling <- predict(ridge_model_cooling, s = best_lambda_cooling, newx = x_cooling)

# Mean Absolute Error (MAE) for Heating
mae_heating <- mean(abs(y_heating - predictions_ridge_heating))
cat("MAE (Ridge - Heating):", mae_heating, "\n")

# Mean Absolute Error (MAE) for Cooling
mae_cooling <- mean(abs(y_cooling - predictions_ridge_cooling))
cat("MAE (Ridge - Cooling):", mae_cooling, "\n")

# R-squared and Adjusted R-squared for Heating
rss_heating <- sum((y_heating - predictions_ridge_heating)^2)
tss_heating <- sum((y_heating - mean(y_heating))^2)
r_squared_heating <- 1 - rss_heating / tss_heating
adjusted_r_squared_heating <- 1 - (1 - r_squared_heating) * ((nrow(data_cleaned) - 1) / (nrow(data_cleaned) - ncol(x_heating) - 1))
cat("R-squared (Ridge - Heating):", r_squared_heating, "\n")
cat("Adjusted R-squared (Ridge - Heating):", adjusted_r_squared_heating, "\n")

# R-squared and Adjusted R-squared for Cooling
rss_cooling <- sum((y_cooling - predictions_ridge_cooling)^2)
tss_cooling <- sum((y_cooling - mean(y_cooling))^2)
r_squared_cooling <- 1 - rss_cooling / tss_cooling
adjusted_r_squared_cooling <- 1 - (1 - r_squared_cooling) * ((nrow(data_cleaned_c) - 1) / (nrow(data_cleaned_c) - ncol(x_cooling) - 1))
cat("R-squared (Ridge - Cooling):", r_squared_cooling, "\n")
cat("Adjusted R-squared (Ridge - Cooling):", adjusted_r_squared_cooling, "\n")

# Cross-validation for Heating
cv_heating <- cv.glmnet(x_heating, y_heating, alpha = 0, nfolds = 10)
cat("Cross-validated RMSE (Heating):", sqrt(mean(cv_heating$cvm)), "\n")

# Cross-validation for Cooling
cv_cooling <- cv.glmnet(x_cooling, y_cooling, alpha = 0, nfolds = 10)
cat("Cross-validated RMSE (Cooling):", sqrt(mean(cv_cooling$cvm)), "\n")

# Residuals for Heating
residuals_heating <- y_heating - predictions_ridge_heating
# Residual Plots for Heating
par(mfrow = c(2, 2))
plot(predictions_ridge_heating, residuals_heating, main = "Residuals vs Fitted (Heating)", xlab = "Fitted Values", ylab = "Residuals", col = "blue")
qqnorm(residuals_heating, main = "Q-Q Plot of Residuals (Heating)")
qqline(residuals_heating, col = "red")
hist(residuals_heating, main = "Histogram of Residuals (Heating)", xlab = "Residuals", col = "lightblue", breaks = 20)
plot(density(residuals_heating), main = "Density Plot of Residuals (Heating)", xlab = "Residuals", col = "blue")

# Residuals for Cooling
residuals_cooling <- y_cooling - predictions_ridge_cooling
# Residual Plots for Cooling
par(mfrow = c(2, 2))
plot(predictions_ridge_cooling, residuals_cooling, main = "Residuals vs Fitted (Cooling)", xlab = "Fitted Values", ylab = "Residuals", col = "green")
qqnorm(residuals_cooling, main = "Q-Q Plot of Residuals (Cooling)")
qqline(residuals_cooling, col = "red")
hist(residuals_cooling, main = "Histogram of Residuals (Cooling)", xlab = "Residuals", col = "lightgreen", breaks = 20)
plot(density(residuals_cooling), main = "Density Plot of Residuals (Cooling)", xlab = "Residuals", col = "green")



#lasso regression
# Lasso Regression for Heating
# Prepare data for Lasso Regression (Heating)
x_heating <- model.matrix(Heating_Load_log ~ Wall_Area_log + Overall_Height_log + 
                            Glazing_Area_log + factor(Glazing_Area_Distribution), 
                          data = data_cleaned)[, -1]
y_heating <- data_cleaned$Heating_Load_log

# Prepare data for Lasso Regression (Cooling)
x_cooling <- model.matrix(Cooling_Load_log ~ Wall_Area_log + Overall_Height_log + 
                            Glazing_Area_log + factor(Glazing_Area_Distribution), 
                          data = data_cleaned_c)[, -1]
y_cooling <- data_cleaned_c$Cooling_Load_log

# Fit Lasso Regression for Heating
lasso_heating <- glmnet(x_heating, y_heating, alpha = 1)  # Alpha = 1 for Lasso
print(lasso_heating)

# Fit Lasso Regression for Cooling
lasso_cooling <- glmnet(x_cooling, y_cooling, alpha = 1)  # Alpha = 1 for Lasso
print(lasso_cooling)

# Cross-validation for Heating
cv_lasso_heating <- cv.glmnet(x_heating, y_heating, alpha = 1)
best_lambda_heating <- cv_lasso_heating$lambda.min
cat("Best Lambda (Heating):", best_lambda_heating, "\n")

# Cross-validation for Cooling
cv_lasso_cooling <- cv.glmnet(x_cooling, y_cooling, alpha = 1)
best_lambda_cooling <- cv_lasso_cooling$lambda.min
cat("Best Lambda (Cooling):", best_lambda_cooling, "\n")

# Refit Lasso Regression for Heating
lasso_model_heating <- glmnet(x_heating, y_heating, alpha = 1, lambda = best_lambda_heating)
print(coef(lasso_model_heating))  # Coefficients

# Refit Lasso Regression for Cooling
lasso_model_cooling <- glmnet(x_cooling, y_cooling, alpha = 1, lambda = best_lambda_cooling)
print(coef(lasso_model_cooling))  # Coefficients

# Predictions and RMSE for Heating
pred_heating <- predict(lasso_model_heating, s = best_lambda_heating, newx = x_heating)
rmse_heating <- sqrt(mean((y_heating - pred_heating)^2))
cat("RMSE (Lasso - Heating):", rmse_heating, "\n")

# Predictions and RMSE for Cooling
pred_cooling <- predict(lasso_model_cooling, s = best_lambda_cooling, newx = x_cooling)
rmse_cooling <- sqrt(mean((y_cooling - pred_cooling)^2))
cat("RMSE (Lasso - Cooling):", rmse_cooling, "\n")

# Heating Plot
ggplot(data_cleaned, aes(x = pred_heating, y = y_heating)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs. Predicted Heating Load (Lasso Regression)", 
       x = "Predicted Heating Load (Log)", 
       y = "Actual Heating Load (Log)") +
  theme_minimal()

# Cooling Plot
ggplot(data_cleaned_c, aes(x = pred_cooling, y = y_cooling)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs. Predicted Cooling Load (Lasso Regression)", 
       x = "Predicted Cooling Load (Log)", 
       y = "Actual Cooling Load (Log)") +
  theme_minimal()

# Mean Absolute Error (MAE) for Heating
mae_heating <- mean(abs(y_heating - pred_heating))
cat("MAE (Lasso - Heating):", mae_heating, "\n")

# Mean Absolute Error (MAE) for Cooling
mae_cooling <- mean(abs(y_cooling - pred_cooling))
cat("MAE (Lasso - Cooling):", mae_cooling, "\n")

# R-squared for Heating
rss_heating <- sum((y_heating - pred_heating)^2)
tss_heating <- sum((y_heating - mean(y_heating))^2)
r_squared_heating <- 1 - rss_heating / tss_heating
cat("R-squared (Lasso - Heating):", r_squared_heating, "\n")

# R-squared for Cooling
rss_cooling <- sum((y_cooling - pred_cooling)^2)
tss_cooling <- sum((y_cooling - mean(y_cooling))^2)
r_squared_cooling <- 1 - rss_cooling / tss_cooling
cat("R-squared (Lasso - Cooling):", r_squared_cooling, "\n")
# Residuals for Heating
residuals_heating <- y_heating - pred_heating

# Residual Plots for Heating
par(mfrow = c(2, 2))  # Set layout for 2x2 plots
plot(pred_heating, residuals_heating, main = "Residuals vs Fitted (Heating - Lasso)", 
     xlab = "Fitted Values", ylab = "Residuals", col = "blue")
qqnorm(residuals_heating, main = "Q-Q Plot of Residuals (Heating - Lasso)")
qqline(residuals_heating, col = "red")
hist(residuals_heating, main = "Histogram of Residuals (Heating - Lasso)", 
     xlab = "Residuals", col = "lightblue", breaks = 20)
plot(density(residuals_heating), main = "Density Plot of Residuals (Heating - Lasso)", 
     xlab = "Residuals", col = "blue")

# Residuals for Cooling
residuals_cooling <- y_cooling - pred_cooling

# Residual Plots for Cooling
par(mfrow = c(2, 2))  # Set layout for 2x2 plots
plot(pred_cooling, residuals_cooling, main = "Residuals vs Fitted (Cooling - Lasso)", 
     xlab = "Fitted Values", ylab = "Residuals", col = "green")
qqnorm(residuals_cooling, main = "Q-Q Plot of Residuals (Cooling - Lasso)")
qqline(residuals_cooling, col = "red")
hist(residuals_cooling, main = "Histogram of Residuals (Cooling - Lasso)", 
     xlab = "Residuals", col = "lightgreen", breaks = 20)
plot(density(residuals_cooling), main = "Density Plot of Residuals (Cooling - Lasso)", 
     xlab = "Residuals", col = "green")




#elastic net regression
# Prepare data for Elastic Net Regression (Heating)
x_heating <- model.matrix(Heating_Load_log ~ Wall_Area_log + Overall_Height_log + 
                            Glazing_Area_log + factor(Glazing_Area_Distribution), 
                          data = data_cleaned)[, -1]
y_heating <- data_cleaned$Heating_Load_log

# Prepare data for Elastic Net Regression (Cooling)
x_cooling <- model.matrix(Cooling_Load_log ~ Wall_Area_log + Overall_Height_log + 
                            Glazing_Area_log + factor(Glazing_Area_Distribution), 
                          data = data_cleaned_c)[, -1]
y_cooling <- data_cleaned_c$Cooling_Load_log

# Cross-validation for Elastic Net (Heating)
cv_elastic_heating <- cv.glmnet(x_heating, y_heating, alpha = 0.5, nfolds = 10)  # Alpha = 0.5 for Elastic Net
best_lambda_heating <- cv_elastic_heating$lambda.min
cat("Best Lambda (Elastic Net - Heating):", best_lambda_heating, "\n")

# Cross-validation for Elastic Net (Cooling)
cv_elastic_cooling <- cv.glmnet(x_cooling, y_cooling, alpha = 0.5, nfolds = 10)
best_lambda_cooling <- cv_elastic_cooling$lambda.min
cat("Best Lambda (Elastic Net - Cooling):", best_lambda_cooling, "\n")

# Refit Elastic Net for Heating
elastic_model_heating <- glmnet(x_heating, y_heating, alpha = 0.5, lambda = best_lambda_heating)
coef(elastic_model_heating)  # View coefficients

# Refit Elastic Net for Cooling
elastic_model_cooling <- glmnet(x_cooling, y_cooling, alpha = 0.5, lambda = best_lambda_cooling)
coef(elastic_model_cooling)  # View coefficients

# Predictions and RMSE for Heating
pred_heating <- predict(elastic_model_heating, s = best_lambda_heating, newx = x_heating)
rmse_heating <- sqrt(mean((y_heating - pred_heating)^2))
cat("RMSE (Elastic Net - Heating):", rmse_heating, "\n")

# Predictions and RMSE for Cooling
pred_cooling <- predict(elastic_model_cooling, s = best_lambda_cooling, newx = x_cooling)
rmse_cooling <- sqrt(mean((y_cooling - pred_cooling)^2))
cat("RMSE (Elastic Net - Cooling):", rmse_cooling, "\n")

# Mean Absolute Error (MAE) for Heating
mae_heating <- mean(abs(y_heating - pred_heating))
cat("MAE (Elastic Net - Heating):", mae_heating, "\n")

# Mean Absolute Error (MAE) for Cooling
mae_cooling <- mean(abs(y_cooling - pred_cooling))
cat("MAE (Elastic Net - Cooling):", mae_cooling, "\n")

# R-squared for Heating
rss_heating <- sum((y_heating - pred_heating)^2)
tss_heating <- sum((y_heating - mean(y_heating))^2)
r_squared_heating <- 1 - rss_heating / tss_heating
cat("R-squared (Elastic Net - Heating):", r_squared_heating, "\n")

# R-squared for Cooling
rss_cooling <- sum((y_cooling - pred_cooling)^2)
tss_cooling <- sum((y_cooling - mean(y_cooling))^2)
r_squared_cooling <- 1 - rss_cooling / tss_cooling
cat("R-squared (Elastic Net - Cooling):", r_squared_cooling, "\n")

# Residuals for Heating
residuals_heating <- y_heating - pred_heating

# Residual Plots for Heating
par(mfrow = c(2, 2))
plot(pred_heating, residuals_heating, main = "Residuals vs Fitted (Heating)", xlab = "Fitted Values", ylab = "Residuals", col = "blue")
qqnorm(residuals_heating, main = "Q-Q Plot of Residuals (Heating)")
qqline(residuals_heating, col = "red")
hist(residuals_heating, main = "Histogram of Residuals (Heating)", xlab = "Residuals", col = "lightblue", breaks = 20)
plot(density(residuals_heating), main = "Density Plot of Residuals (Heating)", xlab = "Residuals", col = "blue")

# Residuals for Cooling
residuals_cooling <- y_cooling - pred_cooling

# Residual Plots for Cooling
par(mfrow = c(2, 2))
plot(pred_cooling, residuals_cooling, main = "Residuals vs Fitted (Cooling)", xlab = "Fitted Values", ylab = "Residuals", col = "green")
qqnorm(residuals_cooling, main = "Q-Q Plot of Residuals (Cooling)")
qqline(residuals_cooling, col = "red")
hist(residuals_cooling, main = "Histogram of Residuals (Cooling)", xlab = "Residuals", col = "lightgreen", breaks = 20)
plot(density(residuals_cooling), main = "Density Plot of Residuals (Cooling)", xlab = "Residuals", col = "green")






#Hypothesis testing

#Test: One-way ANOVA
#Glazing Area Distribution Significantly Impacts Heating and Cooling Loads
#Null Hypothesis (H₀): The mean Heating or Cooling Load does not vary significantly across different Glazing Area Distributions.
#Alternative Hypothesis (H₁): The mean Heating or Cooling Load varies significantly across Glazing Area Distributions.
# Heating Load
#Expected Outcome: If the p-value < 0.05, reject H₀, indicating that Glazing Area Distribution impacts Heating or Cooling Loads.

anova_heating <- aov(Heating_Load_log ~ factor(Glazing_Area_Distribution), data = data_cleaned)
summary(anova_heating)
# Cooling Load
anova_cooling <- aov(Cooling_Load_log ~ factor(Glazing_Area_Distribution), data = data_cleaned_c)
summary(anova_cooling)

# Correlation test for Heating Load
cor_test_heating <- cor.test(data_cleaned$Wall_Area_log, data_cleaned$Heating_Load_log)
cor_test_heating
# Correlation test for Cooling Load
cor_test_cooling <- cor.test(data_cleaned_c$Wall_Area_log, data_cleaned_c$Cooling_Load_log)
cor_test_cooling


#chisq
# Create a contingency table
orientation_vs_glazing <- table(data$Orientation, data$Glazing_Area_Distribution)

# Perform the Chi-Square Test
chi_test <- chisq.test(orientation_vs_glazing)

# Print results
print(chi_test)

# Subset data for two Glazing Area Distribution groups
group1 <- subset(data_cleaned, Glazing_Area_Distribution == 1)$Heating_Load_log
group2 <- subset(data_cleaned, Glazing_Area_Distribution == 2)$Heating_Load_log

# Wilcoxon Rank-Sum Test
wilcox_test <- wilcox.test(group1, group2, alternative = "two.sided")
print(wilcox_test)


# Divide Wall Area into two groups
wall_area_low <- subset(data_cleaned, Wall_Area_log <= median(data_cleaned$Wall_Area_log))$Heating_Load_log
wall_area_high <- subset(data_cleaned, Wall_Area_log > median(data_cleaned$Wall_Area_log))$Heating_Load_log

# Divide Overall Height into two groups
height_low <- subset(data_cleaned, Overall_Height_log <= median(data_cleaned$Overall_Height_log))$Heating_Load_log
height_high <- subset(data_cleaned, Overall_Height_log > median(data_cleaned$Overall_Height_log))$Heating_Load_log
# t-test for Wall Area (Heating Load)
t_test_wall_area <- t.test(wall_area_low, wall_area_high, alternative = "two.sided")
print(t_test_wall_area)
# t-test for Overall Height (Heating Load)
t_test_height <- t.test(height_low, height_high, alternative = "two.sided")
print(t_test_height)



