
install.packages("gdata")
library("gdata")


Pref = as.matrix(read.xls("Pref.xls", header = FALSE))
Tref = as.matrix(read.xls("Tref.xls", header = FALSE))
Eref = as.matrix(read.xls("Eref.xls", header = FALSE))
Xref = as.matrix(read.xls("Xref.xls", header = FALSE))
Tcrt = as.matrix(read.xls("Tcrt.xls", header = FALSE))
Xcrt = as.matrix(read.xls("Xcrt.xls", header = FALSE))
 

for (i in 1:21) {
  temp = paste("Xcrt",i,".xls",sep="")
  assign(paste("Xcrt",i,sep=""),as.matrix(read.xls(temp, header = FALSE)))
  
  temp = paste("Tcrt",i,".xls",sep="")
  assign(paste("Tcrt",i,sep=""),as.matrix(read.xls(temp, header = FALSE)))
}
