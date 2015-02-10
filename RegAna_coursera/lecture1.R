#install.packages("UsingR")
library(UsingR);data(galton)
par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)

#Slide 7 example : min of Meu
library(manipulate)
myHist<-function(mu){
  hist(galton$child,col="blue",breaks=100)
  lines(c(mu,mu),c(0,150),col="red",lwd=5)
  mse<-mean((galton$child-mu)^2)
  text(63,150,paste("mu=",mu))
  text(63,140,paste("MSE=",round(mse,2)))
}
manipulate(myHist(mu),mu=slider(62,74,step=0.5))

hist(galton$child,col="blue",breaks=100)
meanChild<-mean(galton$child)
lines(rep(meanChild,100),seq(0,150,length=100),col="red",lwd=5)

plot(galton$parent,galton$child,pch=19,col="blue")

# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
  if(is.numeric(x)) {
    round(x, getOption('digits'))
  } else {
    paste(as.character(x), collapse = ', ')
  }
})
knit_hooks$set(plot = knitr:::hook_plot_html)
runif(1)

#MSE changes as slope of the line changes and passes through mean child and parent height
par(mfrow=c(1,1))
myPlot<-function(beta){
  y<-galton$child-mean(galton$child)
  x<-galton$parent-mean(galton$parent)
  freqData<-as.data.frame(table(x,y))
  names(freqData)<-c("child","parent","freq")
  plot(
    as.numeric(as.vector(freqData$parent)),
    as.numeric(as.vector(freqData$child)),
    pch=21,col="black",bg="lightblue",
    cex=.15*freqData$freq,
    xlab="parent",
    ylab="child"
  )
  abline(0,beta,lwd=3)
  points(0,0,cex=2,pch=19)
  mse<-mean((y-beta*x)^2)
  title(paste("beta=",beta,"mse=",round(mse,3)))
}
manipulate(myPlot(beta),beta=slider(0.6,1.2,step=0.02))

#value that min the Least sq criterion 
#size of circle represents number of points at that particular XY combination
fit= lm(I(child-mean(child))~I(parent-mean(parent))-1,data=galton)
    # sub mean from child, sub mean from parent, -1 just means that set intercept to zero 

#
y<-galton$child
x<-galton$parent
beta1<-cor(y,x)* sd(y)/sd(x)
beta0<-mean(y)-beta1*mean(x)
rbind(c(beta0,beta1),coef(lm(y~x)))

#Quiz answers 
#1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
fitdata= lm(I(x-mean(x))~I(w-mean(w))-1)

#2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(y ~ x))[2])

#3
data(mtcars)
par(mfrow=c(1,1))
myPlot<-function(beta){
  y<-mtcars$weight-mean(mtcars$weight)
  x<-mtcars$mpg-mean(mtcars$mpg)
  freqData<-as.data.frame(table(x,y))
  names(freqData)<-c("weight","mpg","freq")
  plot(
    as.numeric(as.vector(freqData$mpg)),
    as.numeric(as.vector(freqData$weight)),
    pch=21,col="black",bg="lightblue",
    cex=.15*freqData$freq,
    xlab="mpg",
    ylab="weight"
  )
  abline(0,beta,lwd=3)
  points(0,0,cex=2,pch=19)
  mse<-mean((y-beta*x)^2)
  title(paste("beta=",beta,"mse=",round(mse,3)))
}
manipulate(myPlot(beta),beta=slider(0.6,1.2,step=0.02))

fit= lm(I(mtcars$weight-mean(mtcars$weight))~I(mtcars$mpg-mean(mtcars$mpg))-1)

#6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xc <- x - mean(x)
xsd= xc/sd(x)
xsd

#7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
beta0
beta1

#9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)





