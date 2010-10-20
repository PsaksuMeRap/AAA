#determina il numero di sub indici: le prime tre colonne sono Date, Country, Branche, Ticker
numero.pre.indici <- 4
numero.sub.indici <- dim(df.table)[[2]] - numero.pre.indici
Paesi <- unique(df.table[,"Country"])
numero.paesi <- length(Paesi)
if (numero.paesi > 1) {
	paese.unico = F
} else {
	paese.unico = T
}

#crea i grafici
for (i in 1:numero.sub.indici) {
	j <- numero.pre.indici + i
	non.nulli <- risultato[,j] != 0 & !is.na(risultato[,j])
	risultato.tmp <- risultato[non.nulli,c(1,j)]

	#crea il vettore dei fattori "Paese"

	Paesi <- factor(risultato.tmp[,"Paese"])
	numero.osservazioni.per.paese = tapply(risultato.tmp[,1],Paesi,length)
	numero.osservazioni <- sum(numero.osservazioni.per.paese)
	numero.paesi <- length(levels(Paesi))

	numero.pagine = numero.paesi %/% 4
	if (numero.paesi > numero.pagine*4) {
		numero.pagine = numero.pagine + 1
	}

	for (q in 1:numero.pagine) {
		png(filename = paste("c:/R/graf_asp",i,q,".png",sep=""), width = 600, height = 900, pointsize = 12)
		par(oma=c(1,1,4,1))
		delta <- (q-1)*4

		if (paese.unico) {
			par(mfrow=c(1,1))
		} else {
   			par(mfrow=c(2,2))
		}

		inizio <- 1 + delta
		fine <- min(numero.paesi, delta+4)
		for (paese in levels(Paesi)[inizio:fine]) {
        		paese.desiderato <- risultato.tmp[,1] == paese
			hist(risultato.tmp[paese.desiderato,2],freq=TRUE,breaks=11,main=paese,xlab=paste(numero.osservazioni.per.paese[paese]," osservazioni",sep=""))
		}
		mtext(names(risultato.tmp)[2],line=0,side=3,outer=TRUE)
		dev.off()
	}
}

