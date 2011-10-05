# TODO: Add comment
# 
# Author: claudio
###############################################################################

home <- "/home/claudio/eclipse/AAA/Asimmetria/"
setwd(home)

nomi <- read.csv("./dati/asimmetria rischio credito puro.csv",header = FALSE,nrows=1,as.is=TRUE)
dati <- read.csv("./dati/asimmetria rischio credito puro.csv",header = TRUE)
dati[[1]] <- as.Date(as.character(dati1[[1]]),format="%m/%d/%Y")
dati[[1]] <- as.character(dati1[[1]],format="%d %b %Y")
names(dati) <- unlist(nomi,use.names=FALSE)

dat <- dati[c(1,2)]

fun <- function(dat) {
	data <- dat[[1]]
	dat <- dat[-1]
	sommario <- summary(dat)
	
	inizio <- min((1:length(dat[[1]]))[!is.na(dat[[1]])])
	fine <- max((1:length(dat[[1]]))[!is.na(dat[[1]])])
	
	lastValue <- dat[[1]][fine]
	nbObs <- sum(!is.na(dat[[1]]))
	
	Fn <- ecdf(dat[[1]])
	a <- summary(Fn)
	x <- seq(a["Min."],a["Max."],length=35)
	y <- Fn(x)
	xEy <- data.frame(Yield=x,Frequenza=y*100)
	
	h <- hist(dat[[1]],plot=FALSE)
	h$counts <- h$counts/sum(h$counts)
	
	Max <- max(h$counts)
	
	title <- paste(names(dat),"\nDal ",data[inizio]," al ",data[fine]," (",
			nbObs," oss. settimanali)",sep="")
	k = 1000
	plot(h,main=title,ylim=c(0,trunc(k*1.25*Max)/k),ylab="Frequenze relative",xlab="Yield to maturity ")
	boxplot(dat,horizontal=TRUE,boxwex=trunc(k*0.25*Max)/k,at=Max+trunc(k*0.125*Max)/k,add=TRUE,axes=FALSE)
	abline(v=lastValue,col="red")
	
	pdf(paste("distribuzione_",names(dat),".pdf",sep=""))
	plot(h,main=title,ylim=c(0,trunc(k*1.25*Max)/k),ylab="Frequenze relative",xlab="Yield to maturity ")
	boxplot(dat,horizontal=TRUE,boxwex=trunc(k*0.25*Max)/k,at=Max+trunc(k*0.125*Max)/k,add=TRUE,axes=FALSE)
	abline(v=lastValue,col="red")
	dev.off()
	
	filename <- paste("distribuzione_",names(dat),".csv",sep="")
	write(names(dat[1])[1],file=filename)
	write("",file=filename,append=TRUE)
	write(sommario,file=filename,append=TRUE)
	write("",file=filename,append=TRUE)
	write("Yield;Frequenza",file=filename,append=TRUE)
	write.table(xEy,append=TRUE,file=filename,sep=";",col.names = FALSE,row.names=FALSE)

}

for (i in 2:length(dati))  fun(dat=dati[c(1,i)])


