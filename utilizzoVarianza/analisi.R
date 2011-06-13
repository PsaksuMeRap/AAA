# TODO: Add comment
# 
# Author: claudio
###############################################################################

library(lattice)
dati <- read.csv("Book1.csv",header=TRUE,stringsAsFactors=FALSE)

names(dati)[2] <- "eurGov13"
names(dati)[17] <- "chfGov13"
names(dati)[25] <- "eurCorp"
names(dati)[28] <- "eurHY"
names(dati)[41] <- "eurConv"
names(dati)[44] <- "eurStoxx50"
names(dati)[53] <- "gold"

names(dati)[60] <- "vstoxx"

vstoxx <- dati[["vstoxx"]]
hist(dati[["vstoxx"]])


factor1 <- as.factor(
		as.integer(vstoxx>0) +
		as.integer(vstoxx>0.1) +
		as.integer(vstoxx>0.2) +
		as.integer(vstoxx>0.3) +
		as.integer(vstoxx>0.4)
)


xyplot(dati[["CHF"]]~dati[["vstoxx"]] | factor1)

xyplot(dati[["eurGov13"]]~dati[["vstoxx"]] | factor1)

xyplot(dati[["eurStoxx50"]]~dati[["vstoxx"]] | factor1)


a <- function(series){
	x <- data.frame(nrOss=tapply(X=vstoxx,factor1,FUN=length))
	nomi <- c("  < 0"," 0-10","10-20","20-30","30-40","  >40")
	rownames(x) <- nomi
	x <- cbind(x,mediaVxx=tapply(X=vstoxx,factor1,FUN=mean)*100)
	
	x <- cbind(x,"q_25"=tapply(X=series,factor1,FUN=quantile,probs=0.25))
	x <- cbind(x,median=tapply(X=series,factor1,FUN=median)*100)
	x <- cbind(x,mean=tapply(X=series,factor1,FUN=mean)*100)
	x <- cbind(x,"q_75"=tapply(X=series,factor1,FUN=quantile,probs=0.75))
	
	print(x)

}