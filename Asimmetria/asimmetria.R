# TODO: Add comment
# 
# Author: claudio
###############################################################################

home <- "/home/claudio/eclipse/AAA/Asimmetria/"
setwd(home)

dati <- read.csv("./dati/ytm_gianni.csv")
dati[[1]] <- as.Date(as.character(dati[[1]]),format="%m/%d/%Y")

names(dati) <- c("Data","EMU CORP","US CORP BBB","ITRAXX CROSSOVER 5YR","EUROPEAN CUR HY","US HY MASTER II","EMBI COMPOSITE")


fun <- function(dat) {
	
	sommario <- summary(dat)
	Fn <- ecdf(dat[[1]])
	a <- summary(Fn)
	x <- seq(a["Min."],a["Max."],length=50)
	y <- Fn(x)
	xEy <- data.frame(Yield=x,Frequenza=y*100)
	
	h <- hist(dat[[1]],plot=FALSE)
	
	Max <- max(h$counts)
	
	hist(dat[[1]],main=names(dat),ylim=c(0,trunc(1.25*Max)),xlab="Yield to maturity ")
	boxplot(dat,horizontal=TRUE,boxwex=trunc(0.25*Max),at=Max+trunc(0.125*Max),add=TRUE,axes=FALSE)
	
	pdf(paste("distribuzione_",names(dat),".pdf",sep=""))
	hist(dat[[1]],main=names(dat),ylim=c(0,trunc(1.25*Max)),xlab="Yield to maturity ")
	boxplot(dat,horizontal=TRUE,boxwex=trunc(0.25*Max),at=Max+trunc(0.125*Max),add=TRUE,axes=FALSE)
	dev.off()
	print(names(dat))
	print(sommario)
	print(xEy)
}

for (i in 2:length(dati))  fun(dat=dati[i])


