install.packages("UsingR")
library(UsingR)
data(mtcars)
lm((mtcars$mpg~mtcars$wt))

# Questions to answer
#Is an automatic or manual transmission better for MPG
#Quantify the MPG difference between automatic and manual transmissions