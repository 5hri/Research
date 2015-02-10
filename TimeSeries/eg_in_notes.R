#Example 1.1.1
spots = read.table("sunspots.dat")
spots = ts(spots, start=1700, frequency=1)
plot(spots, xlab="time", ylab="", main="Number of Sun spots")

#Example 1.1.2
lynx = read.table("lynx.dat")
lynx = ts(log10(lynx), start=1821, frequency=1)
plot(lynx, xlab="", ylab="", main="Number of trapped lynx")

#Example 1.1.3
bills03 = read.table("bills03.dat",skip = 14);
bills06 = read.table("bills06.dat",skip = 14);
bills12 = read.table("bills12.dat",skip = 14);
par(mfrow=c(3,1))
plot.ts(bills03, xlab="(a)", ylab="",main="Yields of 3-month Treasury Bills")
plot.ts(bills06, xlab="(b)", ylab="",main="Yields of 6-month Treasury Bills")
plot.ts(bills12, xlab="(c)", ylab="",main="Yields of 12-month Treasury Bills")
par(mfrow=c(1,1))

#White noise
z = rnorm(1000, 0, 1)
y = rnorm(1000, 0, 1) #two WN z & y do not have same mean as in my TE process
plot.ts(z, xlab="", ylab="", main="")
