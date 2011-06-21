# TODO: Add comment
# 
# Author: claudio
###############################################################################
rm(list=ls(all=TRUE))
options(browser="google-chrome")
options(help_type="html")
library(lattice)

home <- "/home/claudio/eclipse/AAA/utilizzoVarianza/"
setwd(home)

dati <- read.csv("Book1.csv",header=TRUE,stringsAsFactors=FALSE)

names(dati)[ 2] <- "eurGov13"
names(dati)[17] <- "chfGov13"
names(dati)[25] <- "eurCorp"
names(dati)[28] <- "eurHY"
names(dati)[41] <- "eurConv"
names(dati)[44] <- "eurStoxx50"
names(dati)[53] <- "gold"

names(dati)[60] <- "vstoxx"

vstoxx <- dati[["vstoxx"]]
hist(dati[["vstoxx"]])

vsxx <- rep(45.03,length(vstoxx)+1)
tmp <- 45.03
for (i in 1:length(vstoxx)) {
	tmp <- tmp * (1+vstoxx[i])
	vsxx[i+1] <- tmp
}
names(vsxx) <- c("2003-04-02",dati[,1])

factor1 <- as.factor(
		as.integer(vstoxx>0) +
		as.integer(vstoxx>0.1) +
		as.integer(vstoxx>0.2) +
		as.integer(vstoxx>0.3) +
		as.integer(vstoxx>0.4)
)

factor2 <- as.factor(
		as.integer(vstoxx>18) +
		as.integer(vstoxx>24) +
		as.integer(vstoxx>30) +
		as.integer(vstoxx>38) +
		as.integer(vstoxx>45)		
)


xyplot(dati[["CHF"]]~dati[["vstoxx"]] | factor1)

xyplot(dati[["eurGov13"]]~dati[["vstoxx"]] | factor1)

xyplot(dati[["eurStoxx50"]]~dati[["vstoxx"]] | factor1)


analisi <- function(series,conditioningSeries,
		levels=c(0,10,20,30,40)){
	levels=levels/100
	tmp <- rep(0,length(conditioningSeries))
	for (i in levels) {
		tmp <- tmp + as.integer(conditioningSeries > i)
	}
	factor1 <- factor(tmp,levels=0:length(levels))	
	
	x <- data.frame(nrOss=tapply(X=conditioningSeries,factor1,FUN=length))
	
	levels=levels*100
	lengthLevels <- length(levels) + 1
	nomi <- rep(NA_character_,lengthLevels)
	nomi[1] <- c(paste(" <",levels[1],sep=""))
	if (lengthLevels>1) {
		for (i in 1:(length(levels))) {
			if (i!=length(levels)) {
				nomi[i+1] <- c(paste(levels[i],":",levels[i+1],sep=""))
			} else {
				nomi[i+1] <- c(paste(">",levels[i],sep=""))
			}
		}
		
	}

	rownames(x) <- nomi
	x <- cbind(x,mediaSerieCondizionante=tapply(X=conditioningSeries,factor1,FUN=mean)*100)
	
	x <- cbind(x,"q_25"=tapply(X=series,factor1,FUN=quantile,probs=0.25)*100)
	x <- cbind(x,median=tapply(X=series,factor1,FUN=median)*100)
	x <- cbind(x,mean=tapply(X=series,factor1,FUN=mean)*100)
	x <- cbind(x,"q_75"=tapply(X=series,factor1,FUN=quantile,probs=0.75)*100)
	x <- cbind(x,rapporto=x[["mediaSerieCondizionante"]]/x[["q_25"]])
	
	print(x,digits=4)

}

analisi(dati[["eurStoxx50"]],vstoxx)

analisi(dati[["eurConv"]],vstoxx)
analisi(dati[["eurHY"]],vstoxx)

livelli <- c(-20,-10,-6,-3,0,3,5,8)
analisi(vstoxx,dati[["eurStoxx50"]],livelli)


