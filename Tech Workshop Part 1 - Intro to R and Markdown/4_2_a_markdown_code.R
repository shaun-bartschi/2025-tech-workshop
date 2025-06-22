library(tidyverse)

# First, plot the relationship between horsepower and mpg for cars in the 
# mtcars dataset (which is a dataset internal to R). Show a local regression
# smooth, along with the linear model estimate. 
ggplot(mtcars, aes(x = hp, y = mpg)) + 
  geom_point() + 
  geom_smooth(se = FALSE) +
  geom_smooth(se = FALSE, method = "lm", col = "red")

# The trend is not very linear, but for simplicity, lets assume that it is 
# "linear-ish". Fit a linear model and obtain the coeffcients of the model
# that uses horsepower to predict fuel economy. 
tlm <- lm(mpg ~ hp, data = mtcars)
summary(tlm)

# Use LaTeX to create a model that has the following form
# y_hat = b_0 + b_1*h_p
# where b_0 and b_1 are the model coefficients. 

# Knit the results to pdf to see if your equation rendered properly. 