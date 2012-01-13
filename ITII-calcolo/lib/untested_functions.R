# TODO: Add comment
# 
# Author: claudio
###############################################################################


# calcolo simbolico di u_t
create_representation1_ut <- function(maxLag=4) {
	# crea la rappresentazione di u_t in termini dei coefficienti f_i
	# e delle variabili aleatorie h_t e z_t
	if (maxLag<=0) stop("Errore in 'create_representation1_ut' maxLag deve essere > 0")
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


create_representation1_ut_k <- function(u_t,power) {
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
	
	if (length(u_t.k)==0) return(create_monomials(create_monomial(number=0)))
	
	
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
	
	## calcola z_{t}^k e poi rimpiazza con E(z_{t}^k)
	maxpower <- max(maxPower(u_t.k,"z"))
	if (maxpower>-Inf) {
		for (power in 1:maxpower) {
			what <- create_randomVariable(name="z",power=power)
			with <- create_monomials(create_monomial(number=E_z(power)))
			u_t.k <- explode(u_t.k,what,with)
		}
	}
	## calcola w_{t}^k e poi rimpiazza con E(w_{t}^k)
	maxpower <- max(maxPower(u_t.k,"w"))
	
	## determina il ritardo e la potenza massimi
	maxLag <- max(maxLag(u_t.k,"h"))
	if (maxpower>-Inf) {
		for (lag in 0:maxLag) {
			for (power in 1:maxpower) {
				what <- create_randomVariable(name="w",power=power,lag=lag)
				with <- create_monomials(create_monomial(number=E_w(power)))
				u_t.k <- explode(u_t.k,what,with)
			}
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

create_representation2_ut_k <- function(u_t,power,max.Lag=max.Lag) {
	
	## calcolo simbolico di u_t
	u_t <- create_representation1_ut(maxLag=max.Lag)
	
	## calcolo simbolico di u_t^k
	u_t.k <- create_representation1_ut_k(u_t,power=power)
	
	## sostituzione di z e w con i valori attesi corrispondenti
	E_u_t.k <- expectation_f_b(u_t.k)
	
	return(E_u_t.k)
}


explode_wrt_parameters <- function(where,symbolName="f",coeff,lag=0) {
	## explode wrt symbolName coefficients
	## where: a monomial/s where to explode	
    ## symboloName: the name of the symbol to replace
	## coeff: the vector of values
	## lag: the value to subtract to the name index (useful when index name
	## does not start at 1, i.e. 0 or 2.
	
	## the values f_i, i in 1:length(coeff) will be replaced by
	## the corresponding values
	
	for (i in 1:length(coeff)) {
		symbol <- paste(symbolName,i-lag,sep="")
		where <- explode_wrt_symbol(a=where,symbolName=symbol,coeff[i])
	}
	return(where)
}


explode_wrt_h_t <- function(where,power=1,max.Lag=0) {
	# explode wrt h_{t}^k coefficients
	# calcola h_{t}^k e poi rimpiazza con E(h_{t}^k)
	for (lag in 0:max.Lag) {
		for (power_ in 1:power) {
			what <- create_randomVariable(name="h",power=power_,lag=lag)
			Eh <- E_h(g=b0,beta=b2,alpha=b1,m=power_)
			with <- create_monomials(create_monomial(number=Eh))
			where <- explode(where,what,with)
		}
	}
	
	return(where)
}

compute_E_u_t.k <- function(power=1,max.Lag=0,fCoefficients,bCoefficients) {
	## questa funzione ritorna il valore atteso di u_t^k

	##1) crea la rappresentazione di u_t^2 con già E(z_t) e E(u_t) calcolati
	##   e con h_{t-max.Lag}^k e basta
	E_u_t.k <- create_representation2_ut_k(u_t,power,max.Lag=max.Lag)
	
	##2) calcolo di E_u_t.k rispetto ad h_t e basta
	##2.1) elimina i coefficienti f_i
	E_u_t.k <- explode_wrt_parameters(where=E_u_t.k,symbolName="f",coeff=fCoefficients[-1])
	
	##2.2) elimina i coefficienti b0, b1 e b2
	E_u_t.k <- explode_wrt_parameters(where=E_u_t.k,symbolName="b",coeff=bCoefficients,lag=1)
	
	##3) calcolo finale di E_u_t.k
	## calcola h_{t}^k e rimpiazzalo con E(h_{t}^k)
	E_u_t.k <- explode_wrt_h_t(where=E_u_t.k,power,max.Lag)

	result <- compact(E_u_t.k)[[1]]$number
	return(result)
}