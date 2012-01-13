# TODO: Add comment
# 
# Author: claudio
###############################################################################


# calcolo simbolico di u_t
create_representation_ut <- function(maxLag=4) {
	# crea la rappresentazione di u_t in termini dei coefficienti f_i
	# e delle variabili aleatorie h_t e z_t
	
	create_symbolWithRandom <- function(symbolName,randomName,index) {
		create_monomials(
				create_monomial(1,
						create_symbols(create_symbol(paste(symbolName,index,sep=""),1)),
						create_randomVariables(create_randomVariable(randomName,index,1))
				)
		)
	}
	
	u_t <- create_monomials(monomialFromString("e_t"))
	
	for (i in 1:maxLag) u_t <- u_t + create_symbolWithRandom(symbolName="f",randomName="e",index=i)
	
	# rimpiazza e_{t-i} con h_{t-i}*z_{t-i}
	where <- u_t
	for (lag in 0:maxLag) {
		what <- create_randomVariable("e",lag=lag,power=1)
		with <- monomialFromString(paste("h_{t-",lag,"}*z_{t-",lag,"}",sep=""))
		where <- explode(where,what,with)
	}
	
	return(where)
	
}


create_representation_ut_k <- function(u_t,power) {
	## crea la rappresentazione della k-esima potenza
	## di u_t in funzione dei coeff. f_i, b_i, delle
	## variabili aleatorie z_{t-k}, w_{t-k} e di h_{t-maxLag}
	
	if (power==0) return(create_monomials(create_monomial(number=1)))
	
	## identifica il ritardo massimo di h_{t-k}
	maxLag <- max(maxLag(u_t,"h"))
	
	## calcola u_t^k
	u_t.k <- u_t
	if (power>1) {
		for (i in 2:power) u_t.k <- u_t.k*u_t
	}
	
	## rimuovi le potenze dispari di "z" quando sono il termine con indice temporale
	## piu' grande
	u_t.k <- dropWhereFirstRandomIsOddPower(u_t.k,"z")
	
	## rimpiazza tutti gli h_{t-i}^k con h_{t-i}*h_{t-i}*... k-volte
	for (i in 0:(maxLag-1)) {
		rv <- create_randomVariable("h",lag=i)
		u_t.k <- disaggregate(u_t.k,rv)
	}
	
	## rimpiazza tutti gli h_{t-i} con l'espressione con h_{t-maxLag} quale unico lag di h
	for (i in 0:(maxLag-1)) {
		what <- create_randomVariable("h",lag=i)
		with <- create.h_t.expansion(fromLag=i,toLag=maxLag)
		u_t.k <- explode(u_t.k,what,with)
	}
	
	return(u_t.k)
	
}

expectation_f_b <- function(u_t.k) {
	## crea la rappresentazione di u_t.k con le 
	## variabili aleatorie z_{t-k}, w_{t-k} sostituite
	## dai rispettivi valori attesi
	
	
	## calcola E(u_t.2). 
	## Rimuovi dapprima tutti i termini la cui
	## variabile aleatoria z con indice temporale piu' alto ha potenza
	## dispari
	u_t.k <- dropWhereFirstRandomIsOddPower(u_t.k,"z")
	u_t.k <- shiftToZeroAndCompact(u_t.k)
	
	## determina il ritardo massimo
	maxLag <- max(maxLag(u_t.k,"h"))
	
	## calcola z_{t}^k e poi rimpiazza con E(z_{t}^k)
	maxpower <- max(maxPower(u_t.k,"z"))
	for (power in 1:maxpower) {
		what <- create_randomVariable(name="z",power=power)
		with <- create_monomials(create_monomial(number=E_z(power)))
		u_t.k <- explode(u_t.k,what,with)
	}
	
	## calcola w_{t}^k e poi rimpiazza con E(w_{t}^k)
	maxpower <- max(maxPower(u_t.k,"w"))
	for (lag in 0:maxLag) {
		for (power in 1:maxpower) {
			what <- create_randomVariable(name="w",power=power,lag=lag)
			with <- create_monomials(create_monomial(number=E_w(power)))
			u_t.k <- explode(u_t.k,what,with)
		}
	}
	
	return(u_t.k)
}


explode_wrt_symbol <- function(a,symbolName,symbolValue) {
	b <- a
	maxpower <- max(maxPower(b,symbolName,"symbol"))
	if (maxpower>0) {
		for (i in 1:maxpower) {
			what <- create_symbol(name=symbolName,power=i)
			with <- create_monomial(number=symbolValue^i)
			b <- explode(where=b,what,with)
		}
	}
	
	return(b)
}

