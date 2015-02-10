library(gdata)
mydata = read.xls("tep_00.xlsx")
View(mydata)
plot(mydata[1:10,]$Total.feed~mydata[1:10,]$Recycle.flow)
cnames = colnames(mydata)
cnames
