# TODO: Add comment
# 
# Author: ortellic
###############################################################################

test.explode.randomVariable <- function() {
	DEACTIVATED()
	explode.randomVariable <- function(what,where,with) {
		# what: the random variable to be injected in "where"
		# where: a monomial possibly containing what
		# with: the monomials "replacing" what 
		
		l_where = length(where$randoms) 
		if (l_where==0) return(create_monomials(where))
		
	}

	# create the random variable epsilon_t^2
	what <- create_randomVariable(power=2)
	tmp <- constructMonomial()
	randoms <- tmp[[1]]$randoms * tmp[[2]]$randoms * create_randomVariables(what)
	
	where <- create_monomial(randoms=randoms)
	x <- constructMonomial()
	with = x[[1]] + x[[2]] + create_monomials(x[[3]])
	
	# determini if replacement is needed
	areEquals <- sapply(where$randoms,"==",what)
	
	# qui fare in modo che ci sia un solo elemento uguale (applicare compact?)
	nbWhat <- sum(areEquals)
	if (nbWhat==0) return(where)
	tmp <- with
	if (nbWhat>1) for (i in 2:nbWhat) tmp <- tmp * with
	
	result <- create_monomials()
	for (i in tmp) {
		result <- result + where * i 
	}
	
}