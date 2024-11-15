#Part a)fit a simple linear regression model where horsepower is the predictor and mpg is the response
Auto <- read.csv("D:/Courses/Supply Chain Data Analytics 2024/Week 3/Tutorial/Auto.csv")
View(Auto)

#fit a regression model where horsepower is the predictor and mpg is the response
model <- lm(mpg ~ acceleration)
model
#summary of the model
summary(model)

#Part b)check the assumptions of regression
#Plot residuals vs fitted values to check for linearity and equal variance assumptions
plot(model$fitted.values, model$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)
# it seems that the linearity assumption and equal variance assumption are satisfied

#plot histogram of residuals to check for normality assumption
hist(model$residuals, 
     breaks = 20,  # Number of bins (can be adjusted)
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue", 
     border = "black")

# it seems that the normality assumption is satisfied.

#Part c)predict the mpg for a car with acceleration of 16 
predict(model, data.frame(acceleration=c(16)), interval="confidence")
predict(model, data.frame(acceleration=c(16)), interval="prediction")

#Part d)plot the data and the regression line
plot(acceleration, mpg)
abline(model)