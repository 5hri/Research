#Set directory
#setwd('/Users/rexcheung/Dropbox/School/School Work/Winter 2015/STA 137/Discussion/Discussion1/')
#Set your own directory, either using code or manually

#Load csv file
#To download the data in the correct format, do right click -> 'save as' from smartsite. DO NOT open and copy/paste the data into excel/word, it will NOT work. 
data = read.csv('report-3_2.csv', header = T)

#Clean up data
#Notice the beginning of the data consists of a few zero. We want to find the last of these occurance
zeros = which(data[,2] == 0)
last.index = zeros[length(zeros)]
newdata = data[(last.index+1):nrow(data),]
weeks = newdata[,1]
searches = newdata[,2]
#Plot the data
plot(searches, type = 'l', xaxt = 'n', main = 'Time Series on "time series searches"', xlab = 'Dates')
axis(1, at = seq(1, length(searches), by = 100), labels = weeks[seq(1, length(searches), by = 100)])
#The downward trend does not mean that people are not checking as much for time series anymore, but that people have started to search a lot more, so as a proportion of all searches it has gone down. The graph is scaled so all values are between 0 and 100, so the y-axis has no real meaning.

#We will use different methods to detrend the data
#Method 1: Using polynomial fit
#Try different models
#Compare models using adjusted R^2
n = nrow(newdata)
design.matrix = data.frame(y = newdata[,2], Intercept = rep(1, n)) #Start the design matrix with a column of 1s
var.name = names(design.matrix)
fit = vector('list', 10)
Rsquared = numeric(10)
Adj.Rsquared = numeric(10)
for(p in 1:10){
  design.matrix = cbind(design.matrix, (1:n)^p)   #Each time attach a new column
  var.name = c(var.name, paste('t', p, sep = ''))  #Append a new column name
  colnames(design.matrix) = var.name
  fit[[p]] = lm(y ~ . - 1, data = design.matrix)
  Rsquared[p] = summary(fit[[p]])$r.squared
  Adj.Rsquared[p] = summary(fit[[p]])$adj.r.squared
}

print(Adj.Rsquared)
#We see that the adj. R^2 value goes up then down at the 7th value.

#Can also use AIC/BIC
model.selection = sapply(fit, function(i){
  return(c(AIC(i), BIC(i)))
})
rownames(model.selection) = c('AIC', 'BIC')
print(model.selection)
#6th model for AIC
#4th model for BIC

#Look at the 4th, 6th and 7th model
summary(fit[[4]])
summary(fit[[6]])
summary(fit[[7]])
#Seems like 4th degree is the best
lines(fit[[4]]$fitted.values, col = 'red')
lines(fit[[6]]$fitted.values, col = 'blue')
lines(fit[[7]]$fitted.values, col = 'green')
################################################################################################################################################
#Method 2: Filtering with moving average
#Two sided moving averages, with q from 1 to 10
two.sided.filters = vector('list', 10)
for(q in 1:10){
 two.sided.filters[[q]] = filter(searches, sides = 2, rep(1,(q*2+1))/(q*2+1))
}

#Plot the filters
plot(searches, type = 'l', xaxt = 'n', main = 'Two sided moving average filter', xlab = 'Dates')
axis(1, at = seq(1, length(searches), by = 100), labels = weeks[seq(1, length(searches), by = 100)])
for(q in c(2,4,6,8,10)){ #Plot only 5 of them
  lines(two.sided.filters[[q]], col = q)
}
legends = paste('q = ', c(2,4,6,8,10), sep = '')
legend('topright', legend = legends, col = c(2,4,6,8,10), lty = 1)

#Focus on a section of the data, say the first 100 points
plot(searches[1:100], type = 'l', xaxt = 'n', main = 'Two sided moving average filter (first 100 points)', xlab = 'Dates')
axis(1, at = seq(1, 100, by = 25), labels = weeks[seq(1, 100, by = 25)])
for(q in c(2,4,6,8,10)){
  lines(two.sided.filters[[q]], col = q)
}
legends = paste('q = ', c(2,4,6,8,10), sep = '')
legend('topright', legend = legends, col = c(2,4,6,8,10), lty = 1)
#We can see as q increases, it gets smoother

#Let's also look at the last 100 data points
plot(searches[421:520], type = 'l', xaxt = 'n', main = 'Two sided moving average filter (last 100 points)', xlab = 'Dates')
axis(1, at = seq(1, 100, by = 25), labels = weeks[seq(421, 520, by = 25)])
for(q in c(2,4,6,8,10)){
  lines(two.sided.filters[[q]][421:520], col = q)
}
legends = paste('q = ', c(2,4,6,8,10), sep = '')
legend('topright', legend = legends, col = c(2,4,6,8,10), lty = 1)

#Plot residuals
plot(searches - two.sided.filters[[2]], main = 'Two sided filter: q = 2', ylab = 'Residuals', xlab = 'Date', xaxt = 'n')
axis(1, at = seq(1, length(searches), by = 50), labels = weeks[seq(1, length(searches), by = 50)])

plot(searches - two.sided.filters[[5]], main = 'Two sided filter: q = 5', ylab = 'Residuals', xlab = 'Date', xaxt = 'n')
axis(1, at = seq(1, length(searches), by = 50), labels = weeks[seq(1, length(searches), by = 50)])

plot(searches - two.sided.filters[[8]], main = 'Two sided filter: q = 8', ylab = 'Residuals', xlab = 'Date', xaxt = 'n')
axis(1, at = seq(1, length(searches), by = 50), labels = weeks[seq(1, length(searches), by = 50)])
#As q goes up, the variance goes up as well. The filter becomes too smooth

#One sided moving averages
#small a => smoother
one.sided.filters = vector('list', 9)
a = seq(0.1, 0.9, by = 0.1)
for(q in 1:9){
  one.sided.filters[[q]][1] = searches[1]
  for(j in 2:length(searches)){
    one.sided.filters[[q]][j] = one.sided.filters[[q]][j-1]*(1-a[q]) + searches[j]*a[q]
  }
}

#Plot the filters
plot(searches, type = 'l', xaxt = 'n', main = 'One sided moving average filter', xlab = 'Dates')
axis(1, at = seq(1, length(searches), by = 100), labels = weeks[seq(1, length(searches), by = 100)])
for(q in c(2,4,6,8)){ #Plot only 4 of them
  lines(one.sided.filters[[q]], col = q)
}
legends = paste('a = ', c(0.2,0.4,0.6,0.8), sep = '')
legend('topright', legend = legends, col = c(2,4,6,8), lty = 1)

#Focus on a section of the data, say the first 100 points
plot(searches[1:100], type = 'l', xaxt = 'n', main = 'One sided moving average filter (first 100 points)', xlab = 'Dates')
axis(1, at = seq(1, 100, by = 25), labels = weeks[seq(1, 100, by = 25)])
for(q in c(2,4,6,8)){
  lines(one.sided.filters[[q]][1:100], col = q)
}
legends = paste('a = ', c(0.2,0.4,0.6,0.8), sep = '')
legend('topright', legend = legends, col = c(2,4,6,8), lty = 1)
#We can see as a goes down, it gets smoother

#Also look at the last 100 data points
plot(searches[421:520], type = 'l', xaxt = 'n', main = 'One sided moving average filter (last 100 points)', xlab = 'Dates')
axis(1, at = seq(1, 100, by = 25), labels = weeks[seq(421, 520, by = 25)])
for(q in c(2,4,6,8)){
  lines(one.sided.filters[[q]][421:520], col = q)
}
legends = paste('a = ', c(0.2,0.4,0.6,0.8), sep = '')
legend('topright', legend = legends, col = c(2,4,6,8), lty = 1)

#Plot residuals
plot(searches - one.sided.filters[[2]], main = 'One sided filter: a = 0.2', ylab = 'Residuals', xlab = 'Date', xaxt = 'n', type = 'l')
axis(1, at = seq(1, length(searches), by = 50), labels = weeks[seq(1, length(searches), by = 50)])

plot(searches - one.sided.filters[[5]], main = 'one sided filter: a = 0.5', ylab = 'Residuals', xlab = 'Date', xaxt = 'n', type = 'l')
axis(1, at = seq(1, length(searches), by = 50), labels = weeks[seq(1, length(searches), by = 50)])

plot(searches - one.sided.filters[[8]], main = 'one sided filter: q = 0.8', ylab = 'Residuals', xlab = 'Date', xaxt = 'n', type = 'l')
axis(1, at = seq(1, length(searches), by = 50), labels = weeks[seq(1, length(searches), by = 50)])
#The fluctuation drops as a goes up, meaning the filter captures more of the present data fluctuation, i.e. filter is less smooth

################################################################################################################################################
#Method 3: Differencing
d1 = diff(searches)
plot(2:length(searches), d1, type = 'l', xaxt = 'n', main = 'With one differencing')
axis(1, at = seq(1, length(d1), by = 100), labels = weeks[seq(2, length(d1)+1, by = 100)])
d2 = diff(d1)
plot(3:length(searches), d2, type = 'l', xaxt = 'n', main = 'With two differencing')
axis(1, at = seq(1, length(d1), by = 100), labels = weeks[seq(3, length(d1)+1, by = 100)])

