#libraries
library(tidyverse)
library(dplyr) 
library(readr)
library(maps)
library(mapproj)
library(plotly)
library(ggpubr) 

sparrows_data = read.csv("Sparrows.csv")

head(sparrows_data)

#1 

x = sparrows_data$Wing.Length
y = sparrows_data$Weight

data= data.frame(x,y)

plot(x~y)

cor(x,y) # correlation

fit1 = lm(x~y) #fitting a linear model
fit1

plot(x~y)
abline(fit1) # [1] 0.783512

s <- ggplot(sparrows_data,aes(x = Wing.Length,y = Weight, color = Nest.Type)) +
    geom_point(shape = 20) +
    labs(
      x = "wing length of sparrow birds",
      y = "weight",
      title = "sparrow data")+
  stat_cor(method = "pearson")

s

#2


lr_sparrows <- lm(Weight ~ Wing.Length , data = sparrows_data)
summary(lr_sparrows)

intercept <- coef(lr_sparrows)[1]
slope <- coef(lr_sparrows)[2]
r_squared <- summary(lr_sparrows)$r.squared

cat("Regression equation: Weight =", round(intercept, 2), "+", round(slope, 2), "* Wing Length\n")
cat("R-squared:", round(r_squared, 4), "\n")


#3
a <-  s + stat_smooth(method = lm,color = "blue")
a

#4

summary(lr_sparrows)

# Conclusion based on p-value
p_value <- summary(lr_sparrows)$coefficients[2,4]
if (p_value < 0.01) {
  cat("The slope is significant at α = 0.01 (p =", p_value, ")\n")
} else {
  cat("The slope is not significant at α = 0.01 (p =", p_value, ")\n")
}

#5

update_sparrow_data <- data.frame(Wing.Length = 33)

predict(lr_sparrows, update_sparrow_data, interval="confidence",level = 0.95)

predict(lr_sparrows, update_sparrow_data, interval = "prediction", level = 0.95)

#6

# Fit models and plot for each "Nest Type"
nest_types <- unique(sparrows_data$Nest.Type)

for (nest in nest_types) {
  # Subset data
  subset_data <- subset(sparrows_data, Nest.Type == nest)
  
  # Fit model
  model_nest <- lm(Weight ~ Wing.Length, data = subset_data)
  cat("Nest Type:", nest, "\n")
  print(summary(model_nest))
  
  # Plot for each nest type with fitted line
   ggplot(subset_data, aes(x = Wing.Length, y = Weight)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = paste("Scatterplot with Fitted Model Line for", nest),
           x = "Wing Length (mm)", y = "Weight (g)") +
      theme_minimal()
  }


#7


#8
ggplot(sparrows_data , aes(x = Wing.Length, y = Weight, color = Nest.Type)) +
  geom_point() +
  geom_smooth(aes(group = Nest.Type), method = "lm", se = FALSE) +
  labs(title = "Scatterplot with All Fitted Model Lines by Nest Type",
       x = "Wing Length (mm)", y = "Weight (g)") +
  theme_minimal()





















