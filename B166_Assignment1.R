################
# This code is to apply Holtz Winters Forecast for 
# Amazon quarterly data set 
################
library("forecast")
rm(list=ls())

old_foo <- read.csv('amazon.csv')


foo <- old_foo[62:3, 1:2] # flip and cut data till 2019 

# make sure values are in ther respective types 
foo$Sales <- as.numeric(foo$Sales)
foo$Date <- as.Date(foo$Date, format = "%m/%d/%Y")

plot(foo$Date[1:56],foo$Sales[1:56], type = 'l',
     main = 'Quarterly Sales of Amazon from 2005 - 2017',
     xlab = 'Date', ylab = 'Sales (in million $)',
     xlim = c(as.Date("2005-03-31"), as.Date("2019-06-31"))) #check data if working 

# turn data into time series 
month.ts <- ts(foo$Date, start = c(2005, 3), end = c(2017,12), frequency=4)
sales.ts <- ts(foo$Sales, start = 2005, end = c(2017, 4), frequency=4)

# observe trend and seasonal behavior of data 
# this is set to multiplicative because we have an increase range of fluctuations 
# in the trend of the sales 
plot(decompose(log(sales.ts), type = "multiplicative"))

# find best parameters using the HoltWinters function 
# this function finds the best case for the SSE 
fit <- HoltWinters(log(sales.ts))

# predict the next 4 variables using the fit model 
forecast(fit, 4)

# find metrics and variables in the fit model 
fit$SSE # SSE = 0.121

# find valeus of alpha, beta and gamma 
fit
# Smoothing parameters:
# alpha: 0.5938002
# beta : 0.05209555
# gamma: 1


# plot graphs 

# plot Holtzwinter model 
plot(fit, ylim = c(7,12), xlim = c(2005,2019), main = " ", lw = 3)
par(new = TRUE)

# plot predicted values 
plot(forecast(fit,4),ylim = c(7,12), 
     xlim = c(2005,2019), main = "Holtz Winter Forecast", type = "line")
par(new = TRUE)

# plot actual data for comparison 
plot(log(foo$Sales[1:56]) ~ foo$Date[1:56],  ylim = c(7,12), 
     xlim = c(as.Date("2005-03-31"), as.Date("2019-03-31")), type = 'l', 
     main = "", xlab = "",ylab = "", xaxt ='n', yaxt = 'n')
par(new = TRUE)

#add legends
legend("topleft", legend = c("Actual", "Holt- Winters Fit", "Predicted"),
       pch = c(16,16), col = c("black", "red", "blue"))

### END OF CODE ### 
