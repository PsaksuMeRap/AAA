# TODO: Add comment
# 
# Author: ortellic
###############################################################################

explode <- function(where,what,with) UseMethod("explode",where)

explode.monomial <- function(where,what,with) {
	# where: a monomial possibly containing what
	# what: the random variable or symbol to be injected in "where"
	# with: the monomials "replacing" what 

	if (class(with)=="monomial") {
		with <- create_monomials(with)
	}
	
	if (is.element("randomVariable",class(what))) {
		lengthRandoms = length(where$randoms) 
		if (lengthRandoms==0) return(create_monomials(where))
		
		# is the randomVariable "what" in the "where" monomial?
		areEquals <- sapply(where$randoms,"==",what)
	
		nbWhat <- sum(areEquals)
		# if no match return a monomials with "where"
		if (nbWhat==0) return(create_monomials(where))
		
		# remove "what" from where$randoms
		where$randoms[areEquals] <- NULL
		
		# create a temporary copy of with
		tmp <- with
		# if more than one "what" was found in "where" multiply tmp accordingly 
		if (nbWhat>1) for (i in 2:nbWhat) tmp <- tmp * with
		
		result <- create_monomials()
		for (i in tmp) {
			result <- result + where * i 
		}
		
		return(result)
	}
	
	if (class(what)=="symbol") {
		lengthSymbols = length(where$symbols) 
		if (lengthSymbols==0) return(create_monomials(where))
		
		# is the symbol "what" in the "where" monomial?
		areEquals <- sapply(where$symbols,"==",what)
		
		nbWhat <- sum(areEquals)
		# if no match return a monomials with "where"
		if (nbWhat==0) return(create_monomials(where))
		
		# remove "what" from where$randoms
		where$symbols[areEquals] <- NULL
		
		# create a temporary copy of with
		tmp <- with
		# if more than one "what" was found in "where" multiply tmp accordingly 
		if (nbWhat>1) for (i in 2:nbWhat) tmp <- tmp * with
		
		result <- create_monomials()
		for (i in tmp) {
			result <- result + where * i 
		}
		
		return(result)
	}
	
	stop("Error in explode.monomial: what is not of class 'randomVariable' or 'symbol'")
}


explode.monomials <- function(where, what, with) {
	# where: a monomials possibly containing what
	# what: the random variable or symbol to be injected in "where"
	# with: the monomials "replacing" what 
	
	if (class(with)=="monomial") {
		with <- create_monomials(with)
	}
	
	if (length(where)==0) return(where)
	result <- lapply(where,explode,what,with)
	result <- unlist(result, recursive=FALSE)
	
	class(result) <- "monomials"
	return(result)
}



isFirstRandomAnOddPower <- function(x,randomName) UseMethod("isFirstRandomAnOddPower",x)
# questa funzione restituisce TRUE se nella parte randoms la variabile aleatoria
# più vicina nel tempo fra tutte quelle con nome "randomName" ha potenza dispari. 

isFirstRandomAnOddPower.monomial <- function(x,randomName) {
	randoms <- x$randoms
	if (length(randoms)==0) return(FALSE)

	# identifica se c'è una variabile con nome randomName tra le variabili randoms
	isDesiredRandom <- extractFromList(randoms,"name") == randomName
	if (any(isDesiredRandom)) {
		randoms <- randoms[isDesiredRandom]
		class(randoms) <- "randomVariables"
		randoms <- sort(randoms)
		if (randoms[[1]]$power %% 2) TRUE else FALSE
	} else {
		return(FALSE)
	}
}


isFirstRandomAnOddPower.monomials <- function(x,randomName) {
	
	result <- sapply(x,isFirstRandomAnOddPower,randomName)
	return(result)
}


dropWhereFirstRandomIsOddPower <- function(x,randomName) {

	isOdd <- sapply(x,isFirstRandomAnOddPower,randomName)
	result <- x[!isOdd]
	class(result) <- "monomials"
	return(result)
}


shiftToZero <- function(x) UseMethod("shiftToZero",x)

shiftToZero.monomial <- function(x) {
	
	# this function shifts the lags in the randoms part
	if (length(x$randoms)==0) return(x)
	lags <- extractFromList(x$randoms,"lag")
	if (all(lags>0)) {
		a <- min(lags)
		for (i in 1:length(x$randoms)) {
			x$randoms[[i]]$lag <- x$randoms[[i]]$lag - a
		}
		return(x)
	} else {
		return(x)
	}
	
}

shiftToZero.monomials <- function(x) {
	
	if (length(x)==0) return(x)
	result <- lapply(x,shiftToZero)
	class(result) <- "monomials"
	return(result)
}


compactMonomials <- function(x) {
	if (class(x)!="monomials") stop("Error compactSum: argument is not of class monomials.")
	# x: a monomials whose terms must be compacted
	if (length(x)<=1) return(x)	
	tmp <- create_monomials()
	for (y in x) tmp <- tmp + create_monomials(y)
	return(tmp)
}


shiftToZeroAndCompact <- function(x) {
	x <- shiftToZero(x)
	if (class(x)=="monomial") return(x) else return(compactMonomials(x))	
}


create.h_t.expansion <- function(fromLag=0,toLag=1) {
	# fromLag: il lag iniziale di h_{t-fromLag} = b0 + b1*w_{t-fromLag-1}* ...
	# toLag: il ritardo massimo nell'espressione finale
	
	if (toLag-fromLag < 1) stop("Error in create.h_t.expansion: fromLag must be smaller than toLag")
	# fai come se fromLag fosse 0 e alla fine shifta tutto di fromLag periodi
	if (fromLag!=0) {
		timeShift <- fromLag
		fromLag <- 0
		toLag <- toLag - timeShift
	} else {
		timeShift <- 0
	}
	
	ht <- monomialsFromString("b0 + b1*w_{t-1}*h_{t-1} + b2*h_{t-1}")
	where <- ht
	with  <- ht
	if (toLag>1) {
		for (i in 1:(toLag-1)) {
			what <- create_randomVariable(name="h",lag=i)
			with <- Lag(with)
			where <- explode(where,what,with)
		}
	}
	if (timeShift) where <- Lag(where,power=timeShift)
	return(where)
}
