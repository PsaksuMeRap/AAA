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
dati1 <- read.csv("eurostoxx600banche.csv",header=TRUE,stringsAsFactors=FALSE)

names(dati)[ 2] <- "eurGov13"
names(dati)[17] <- "chfGov13"
names(dati)[25] <- "eurCorp"
names(dati)[28] <- "eurHY"
names(dati)[41] <- "eurConv"
names(dati)[44] <- "eurStoxx50"
names(dati)[53] <- "gold"

names(dati)[60] <- "vstoxx"

vstoxx <- dati[["vstoxx"]]
eurstoxx50 <- dati[["eurStoxx50"]]
eurstoxx600Banche <- dati1[[2]]

vsxx <- rep(45.03,length(vstoxx)+1)
tmp <- 45.03
for (i in 1:length(vstoxx)) {
	tmp <- tmp * (1+vstoxx[i])
	vsxx[i+1] <- tmp
}
names(vsxx) <- c("2003-04-02",dati[,1])
vsxx <- vsxx[-1]

# filtra le serie rispetto al livello di vola
isLarger <- vsxx>25
vstoxxL25 <- vstoxx[isLarger]
vstoxxS25 <- vstoxx[!isLarger]

eurstoxx50L25 <- eurstoxx50[isLarger]
eurstoxx50S25 <- eurstoxx50[!isLarger]



#xyplot(dati[["CHF"]]~dati[["vstoxx"]] | factor1)

#xyplot(dati[["eurGov13"]]~dati[["vstoxx"]] | factor1)

#xyplot(dati[["eurStoxx50"]]~dati[["vstoxx"]] | factor1)


myXyplot<- function(series,condizionaSu,levels=c(0,10,20,30,40)) {
	levels=levels/100
	tmp <- rep(0,length(condizionaSu))
	for (i in levels) {
		tmp <- tmp + as.integer(condizionaSu > i)
	}
	factor1 <- factor(tmp,levels=0:length(levels))	
	xyplot(series ~ condizionaSu | factor1)
}


analisi <- function(series,condizionaSu,levels=c(0,10,20,30,40)){
	
	n <- max(nchar(as.character(livelli)))	
	xx <- function(string,n){
		paste(
				paste(rep(" ", n - nchar(string)),sep="",collapse=""),
			string,sep="")
	}

	levels=levels/100
	tmp <- rep(0,length(condizionaSu))
	for (i in levels) {
		tmp <- tmp + as.integer(condizionaSu > i)
	}
	factor1 <- factor(tmp,levels=0:length(levels))	
	
	x <- data.frame(nrOss=tapply(X=condizionaSu,factor1,FUN=length))
	
	levels=levels*100
	lengthLevels <- length(levels) + 1
	nomi <- rep(NA_character_,lengthLevels)
	nomi[1] <- c(paste(xx(" ",n)," < ",xx(levels[1],n),sep=""))
	if (lengthLevels>1) {
		for (i in 1:(length(levels))) {
			if (i!=length(levels)) {
				nomi[i+1] <- paste(xx(levels[i],n)," : ",xx(levels[i+1],n),sep="")
			} else {
				nomi[i+1] <- paste(xx(" ",n)," > ",xx(levels[i],n),sep="")
			}
		}
		
	}
	rownames(x) <- nomi
	x <- cbind(x,mediaII=tapply(X=condizionaSu,factor1,FUN=mean)*100)

	x <- cbind(x,"q_25"=tapply(X=series,factor1,FUN=quantile,probs=0.25)*100)
	x <- cbind(x,median=tapply(X=series,factor1,FUN=median)*100)
	x <- cbind(x,mean=tapply(X=series,factor1,FUN=mean)*100)
	x <- cbind(x,"q_75"=tapply(X=series,factor1,FUN=quantile,probs=0.75)*100)
	tmp <- by(data=data.frame(a=series,b=condizionaSu),INDICES=factor1,FUN=cor)
	tmp1 <- lapply(tmp,function(x){return(x[2,1])})
	tmp1[unlist(lapply(tmp1,is.null))] <- NA
	x <- cbind(x,"corr"=unlist(tmp1))
	x <- cbind(x,"q_25/mediaII"=x[["q_25"]]/x[["mediaII"]])
	
	print(x,digits=4)

}


# cosa succede se il vstoxx 
livelli <- c(-10,-8,-6,-3,0,2,4,6,7)
analisi(vstoxx,dati[["eurStoxx50"]],livelli)

livelli <- c(-30,-20,-10,-6,-3,0,5,10,15,20,30,40)
analisi(eurstoxx50,vstoxx,livelli)
myXyplot(eurStoxx50,vstoxx,livelli)


livelli <- c(-3.5,0,3,6,10)
analisi(vstoxxL25,eurstoxx50L25,livelli)
livelli <- c(-3.5,0,3)
analisi(vstoxxS25,eurstoxx50S25,livelli)

myXyplot(eurstoxx50L25,vstoxxL25,livelli)

livelli <- c(-20,-10,-6,-3,0,5,10,15,20,30,40)
analisi(eurstoxx50S25,vstoxxS25,livelli)
myXyplot(eurstoxx50S25,vstoxxS25,livelli)


plot(vstoxxL25,eurstoxx50L25)
cor(vstoxxL25,eurstoxx50L25)

plot(vstoxxS25,eurstoxx50S25)
cor(vstoxxS25,eurstoxx50S25)


plot(c(vstoxxS25,vstoxxL25),c(eurstoxx50S25,eurstoxx50L25),
		col=c(rep("black",length(vstoxxS25)),rep("red",length(eurstoxx50L25))))

cor(vstoxx,eurstoxx50)
