#Figure 1.7
lake = read.table("lake.dat")
lake = ts(lake, start=1875)
t = 1:length(lake)
lsfit = lm(lake~t)
plot(t, lake, xlab="", ylab="", main="")
lines(lsfit$fit)
#Plot residuals
plot(lsfit$resid)
lines(lsfit$resid)

#The moving average filters for this example can be produced in R in the following way:
t = 1:length(lake)
ma2 = filter(lake, sides=2, rep(1,5)/5)
ma10 = filter(lake, sides=2, rep(1,21)/21)
ma35 = filter(lake, sides=2, rep(1,71)/71)
par(mfrow=c(2,3))
plot(t, ma2, xlab="", ylab="")
lsfit = lm(ma2~t)
lines(lsfit$fit)
plot(lsfit$resid)
lines(lsfit$resid)
#you can do the above process for ma10 and ma35

#differencing to detrend
d1 = diff(lake)
d2 = diff(d1)
d3 = diff(d2)
d4 = diff(d3)
par(mfrow=c(1,4))
plot.ts(d1, xlab="", ylab="")
plot.ts(d2, xlab="", ylab="")
plot.ts(d3, xlab="", ylab="")
plot.ts(d4, xlab="", ylab="")








