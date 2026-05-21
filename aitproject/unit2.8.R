#libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

# uploading the dataset and remove unwanted # comments 
data_sets <- read.csv("data.csv", comment.char="#")

data_sets
head(data_sets)

# 1. Create a properly labeled scatterplot of the dataset
scatterplot <- ggplot(data_sets, aes(x= Year, y = Anomaly))+
  geom_point(color = 'blue', size = 2)+
  theme_minimal()+
  ggtitle("Relation of temerature near global land and oceans") +
  xlab("Year ") +
  ylab("Anamoly's") 

#linear regression model
liner_regression_model = lm( data_sets$Year~ data_sets$Anomaly)

                        
data_sets$Year <- as.integer(substr(data_sets$Date, 1, 4))
data_sets$Month <- as.integer(substr(data_sets$Date, 5, nchar(as.character(data_sets$Date))))
average_dataset <- data_sets %>%
  group_by(Year) %>%
  summarise(Anomaly = mean(Anomaly, na.rm = TRUE))

#ScatterPlot for average 
ggplot(average_dataset, aes(x = Year, y = Anomaly)) +
  geom_point(color = "black", alpha = 0.9) +                          
  labs(title = "Relation of temerature near global land and oceans",
       x = "Year",
       y = "Anamoly's") +
  theme_minimal()

#Model fit of Linear Regression line
ggplot(average_dataset, aes(x = Year, y = Anomaly)) +
  geom_point(color = "blue", alpha = 0.5) +                    
  geom_line(color = "lightblue", size = 1) +                     
  geom_smooth(method = "lm", color = "black", se = FALSE, alpha = 0.6) + 
  labs(title = "Global Land and Ocean Temperature Anomaly ",
       x = "Year",
       y = "Anamoly's") +
  theme_minimal()



model_fit_dataset <- lm(Anomaly ~ Year, data = average_dataset)

#Residual values
average_dataset$residual <- residuals(model_fit_dataset)
average_dataset$fitted <- fitted(model_fit_dataset)

#Residual Plot
ggplot(average_dataset, aes(x = fitted, y = residual)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Residual Plot",
       x = "Adjusted Values",
       y = "Residuals") +
  theme_minimal()

model_fit_dataset <- lm(Anomaly ~ Year, data = average_dataset)

prediction <- predict(model_fit_dataset)
residual <- average.year$Anomaly - prediction
qqnorm(residual, main = "Q-Q Plot")
qqline(residual, col = "BLUE")

# 1984 to 3000 years
upcoming_years <- data.frame(Year = 1984:3000)

rm(future.data)

# future predictions
future_predictions <- predict(model_fit_dataset, newdata = upcoming_years)
future_dataset <- data.frame(Year = upcoming_years$Year, Predictions.Anomaly = future_predictions)

# Displaying few rows 
head(future_dataset)

#Predicting the years when the global average temperature anomaly reaching 2.0 °C 
year_2_temp <- future_dataset$Year[which(future_dataset$Predictions.Anomaly >= 2.0)[1]]
#Predicting the years when the global average temperature anomaly reaching 2.0 °C
year_2.5_temp <- future_dataset$Year[which(future_dataset$Predictions.Anomaly >= 2.5)[1]]

# Displaying the results
cat("The year that a temperature anomaly of 2.0°C occurs:", year_2_temp, "\n")
cat("The year that a temperature anomaly of 2.5°C occurs:", year_2.5_temp, "\n")

# Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE)
linear_model <- lm(Anomaly ~ Year, data = average_dataset)
mae_linear <- mean(abs(residuals(linear_model)))
rmse_linear <- sqrt(mean(residuals(linear_model)^2))
poly_model <- lm(Anomaly ~ poly(as.numeric(Year), 2), data = average_dataset)


mae_poly <- mean(abs(residuals(poly_model)))
rmse_poly <- sqrt(mean(residuals(poly_model)^2))

cat("Linear Model MAE:", mae_linear, "RMSE:", rmse_linear, "\n")
cat("Polynomial Model MAE:", mae_poly, "RMSE:", rmse_poly, "\n")

