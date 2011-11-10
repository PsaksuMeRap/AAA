# TODO: Add comment
# 
# Author: ortellic
###############################################################################

test.explode.randomVariable <- function() {

	explode.randomVariable <- function(what,where,with) {
		# what: the random variable to be injected in "where"
		# where: a monomial possibly containing what
		# with: the monomials "replacing" what 
		
		l_where = length(where$randoms) 
		if (l_where==0) return(create_monomials(where))
		
		
	}

	# create the random variable epsilon_t^2
	what <- create_randomVariable(power=2)
	where <- create_monomial(randoms=create_randomVariables(what))
	tmp <- constructMonomial()
	where <- where * tmp[[1]]
	where <- where * create_monomials(tmp[[2]])
	
	# determini if replacement is needed
	areEquals <- sapply(where$randoms,"==",what)
}