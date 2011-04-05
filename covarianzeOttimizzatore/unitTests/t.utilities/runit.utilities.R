test.cambiaNomi <- function() {
	vecchiNomi <- c("Claudio","Luca","Pippo")	
	nuoviNomi.df <- data.frame(I(c("Luca","Claudio")),I(c("Micione","Dino")))
	colnames(nuoviNomi.df) <- c("Old names","New names")
	
	nuoviNomi <- cambiaNomi(vecchiNomi,nuoviNomi.df)
	
	checkEquals(nuoviNomi,c("Dino","Micione","Pippo"))
	
}
