library(MASS)
install.packages("hdrcde", repos="http://cran.fhcrc.org")
library(hdrcde)
install.packages("mvoutlier")
library(mvoutlier)

row.names(Tcrt1) = c(1:960)
temp = as.matrix(row.names(Tcrt1))

par(mfcol=c(1,2))

for (i in 150:300) {
  plot(Tcrt1[,1],Tcrt1[,2],xlim=c(-35,25), ylim=c(-25, 12))
  #text(Tcrt1[,1],Tcrt1[,2], row.names(Tcrt1), cex=0.6, pos=4, col="black")
  
  par(new=TRUE)
  
  plot(Tcrt1[i,1],Tcrt1[i,2],xlim=c(-35,25), ylim=c(-25, 12), col="red",lwd = 4)
  title(main=i, col.main="red", font.main=4,)
  Sys.sleep(0.1)
}

#for fault 3
for (i in 150:300) {
  plot(Tcrt3[,1],Tcrt3[,2],xlim=c(-6.5,6.5), ylim=c(-6, 6))
  #text(Tcrt1[,1],Tcrt1[,2], row.names(Tcrt1), cex=0.6, pos=4, col="black")
  
  par(new=TRUE)
  
  plot(Tcrt3[i,1],Tcrt3[i,2],xlim=c(-6.5,6.5), ylim=c(-6, 6), col="red",lwd = 4)
  title(main=i, col.main="red", font.main=4,)
  Sys.sleep(0.1)
}


for (i in 1:1) {
  plot(Pref[,1],Pref[,2],xlim=c(-0.5,0.5), ylim=c(-0.5, 0.5))
  #text(Tcrt1[,1],Tcrt1[,2], row.names(Tcrt1), cex=0.6, pos=4, col="black")
  
  par(new=TRUE)
  
  plot(Pref[i,1],Pref[i,2],xlim=c(-0.5,0.5), ylim=c(-0.5, 0.5), col="red",lwd = 4)
  title(main=i, col.main="red", font.main=4)
  Sys.sleep(0.1)
}

#loading Plot for faults (eigenvectors are the loadings, determine which var contribute)

plot(eigen(cov(Xcrt1))$vectors)
text(eigen(cov(Xcrt1))$vectors, cnames, cex=0.6, pos=1, col="black")
#identify(eigen(cov(Xcrt1))$vectors)

  # for fault 3
plot(eigen(cov(Xcrt3))$vectors)
text(eigen(cov(Xcrt3))$vectors, cnames, cex=0.6, pos=1, col="black")

#Eigenvalues plot for faults (determine # of PC required) 
plot(eigen(cov(Xcrt1))$values)

#parallel coord
parcoord(Tcrt1[1:960,],col=rainbow(length(Tcrt1[,1])), var.label=TRUE)

#highest density region
hdr.boxplot.2d(Tcrt[,1],Tcrt[,2],prob=c(68,95,99),show.points = TRUE,
                       xlab="a", ylab="b", xextend=0.15, yextend=0.15)
identify(hdr.boxplot.2d(Tcrt[,1],Tcrt[,2],prob=c(68,95,99),show.points = TRUE,
                        xlab="a", ylab="b", xextend=0.15, yextend=0.15))

text(Tcrt1[,1],Tcrt1[,2], row.names(Tcrt1), cex=0.6, pos=1, col="black")      
        
# CUMSUM
plot(cumsum(Tcrt[,1]))
min(cumsum(Tcrt[,1]))

#  spec.pgram(cumsum(diff(Tcrt3[,1]))), acf(cumsum(diff(Tcrt3[,1]))),
# pacf(cumsum(diff(Tcrt3[,1]))),plot.ts(cumsum(diff(Tcrt3[,1])))

