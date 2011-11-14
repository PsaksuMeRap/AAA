# TODO: Add comment
# 
# Author: claudio
###############################################################################



create_monomials<- function(monomial=NULL) {
	
	monomials <- list()
	class(monomials) <- "monomials"
	
	# if (is.null(monomial)) monomial <- create_monomial()
	if (!is.null(monomial)) monomials[[1]] <- monomial
	
	return(monomials)
}


"+.monomials" <- function(a,b) {
	
	if (length(b)==0) return(a)
	if (length(a)==0) return(b)
	
	tmp <- list()
	for(mon_b in b) {
		copy <- TRUE
		for (i in 1:length(a)) {
			if (mon_b$symbol==a[[i]]$symbol & mon_b$randoms==a[[i]]$randoms) {
				a[[i]]$number=a[[i]]$number+mon_b$number
				if (a[[i]]$number==0) a[[i]] <- NULL
				copy <- FALSE
				break
			}
		}
		if (copy) tmp[[length(tmp)+1]] <- mon_b
	}
	
	if (length(tmp)>0) { a <- c(a,tmp); class(a) <- "monomials" }
	return(a)
	
}



"*.monomials" <- function(a,b) {
	
	la <- length(a)
	lb <- length(b)
	if (lb == 0) return(b)
	if (la == 0) return(a)
	
	cartProd.df <- expand.grid(a,b)
	result <- mapply("*",cartProd.df[[1]],cartProd.df[[2]])
	class(result) <- "monomials"
	result <- compact(result)
	return(result)
	
}



compact <- function(a) UseMethod("compact",a)

compact.monomials <- function(a) {
	
	la <- length(a)
	if (la <= 1) return(a)
	
	tmp <- list()
	tmp[[1]] <- a[[1]]
	for(monomial in a[-1]) {
		copy <- TRUE
		for (i in 1:length(tmp)) {
			if (monomial$symbol==tmp[[i]]$symbol & monomial$randoms==tmp[[i]]$randoms) {
				tmp[[i]]$number=tmp[[i]]$number+monomial$number
				copy <- FALSE
				break
			}
		}
		if (copy) tmp[[length(tmp)+1]] <- monomial
	}
	
	class(tmp) <- "monomials"
	return(tmp)	
}


toString.monomials <- function(monomials) {
	if (length(monomials)==0) return("")
	
	result <- sapply(monomials,toString)
	
	if (length(result)>1) {
		sign <- sapply(monomials,function(x){if(x$number>=0) return(" + ") else return(" - ")})
		first <- result[1]
		result <- sapply(result,function(x){return(sub('^-',"",x))})
		result <- paste(sign[-1],result[-1],sep="",collapse="")
		result = paste(first,result,sep="")
		return(result)
	}
	
	return(toString(monomials[[1]]))
}


sort.monomials <- function(x) {
	# x: a list of class monomials. The components are monomial whose symbols and randoms must be ordered
	result <- lapply(x,sort)
	class(result) <- "monomials"
	return(result)
}