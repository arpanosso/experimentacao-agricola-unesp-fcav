d<-read.table("clipboard",h=TRUE)
d
require(ExpDes.pt)
dic(d$Trat,d$Y)
fat2.dic(d$A,d$B,d$Y)
