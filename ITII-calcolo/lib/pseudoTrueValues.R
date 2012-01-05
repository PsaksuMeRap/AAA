# TODO: Add comment
# 
# Author: claudio
###############################################################################


pseudoTrueValues <- function(ma,p=1) {
	## ma is the vector of the MA parameters 
	## ma[1] is the constant and is mandatory
	## ma[2] must be always 1.0, the weight of lag0
	## p is the order of the AR process
	
	## q is the order of the MA process
	q = length(ma) - 2
	
	tildeA = matrix(0,nrow=p+1,ncol=p+q+1)
	Alpha = ma[-1]
	
	if (p==0) return(list(pseudoTrueValues=ma[1],f=Alpha))
	
	for (i in 1:(p+1)) tildeA[i,i:(i+q)]=Alpha
	
	A = tildeA%*%t(tildeA)
	x = - solve(A[-1,-1])%*%A[-1,1]
	tildex = c(1,x)
	pseudoCostante = sum(tildex)*ma[1]
	return(list(pseudoTrueValues=c(pseudoCostante,-x),f=t(tildeA)%*%tildex))
	
	# x_t = a + alpha(L)*e_t DGP MA(q) con alpha(0) = 1
	# d(L)x_t = c + u_t     PseudoTrueModell AR(p) con d(0)= 1
	# da cui
	# u_t = -c + d(L)x_t = -c + d(L)*alpha(L)*e_t
	# u_t = -c + f(L)*e_t
	
}
