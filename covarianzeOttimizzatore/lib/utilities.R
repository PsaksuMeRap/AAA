cambiaNomi <- function(vecchiNomi,nuoviNomi.df) {
	v.nuoviNomi <- nuoviNomi.df[,"New names"]
	names(v.nuoviNomi) <- nuoviNomi.df[,"Old names"]
	
	isToChange <- is.element(vecchiNomi,nuoviNomi.df[,"Old names"])
	if (any(isToChange)) {
		vecchiNomi[isToChange] <- v.nuoviNomi[vecchiNomi[isToChange]]
	}
	return(vecchiNomi)
}



