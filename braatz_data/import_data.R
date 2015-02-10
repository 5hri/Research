install.packages("R.matlab")
library("R.matlab")


colnames = c(1:52)

for (i in 0:9) {
  temp = paste("d0",i,"_te",".dat",sep="")
  assign(paste("raw_data0",i,sep=""),as.matrix(read.table(temp, col.names=colnames)))
}

for (i in 10:21) {
  temp = paste("d",i,"_te",".dat",sep="")
  assign(paste("raw_data",i,sep=""),as.matrix(read.table(temp, col.names=colnames)))
  }

View(raw_data01)
