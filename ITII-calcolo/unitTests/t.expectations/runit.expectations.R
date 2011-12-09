# TODO: Add comment
# 
# Author: ortellic
###############################################################################

test.E_w <- function() {
	checkEquals(E_w(0),1.0)
	checkEquals(E_w(1),sqrt(2/pi))
	checkEquals(E_w(2),1.0)
	checkEquals(E_w(4),3.0)
}


test.E_w <- function() {
	checkEquals(E_z(0),1.0)
	checkEquals(E_z(1),0.0)
	checkEquals(E_z(2),1.0)
	checkEquals(E_z(3),0.0)	
	checkEquals(E_z(4),3.0)
}


test.all_E_w <- function() {
	checkEquals(all_E_w(0),1.0)
	checkEquals(all_E_w(1),sqrt(2/pi))
	checkEquals(all_E_w(2),c(sqrt(2/pi),1.0))
	checkEquals(all_E_w(4),c(sqrt(2/pi),1.0,2*sqrt(2/pi),3))
	
}


test.all_E_z <- function() {
	checkEquals(all_E_z(0),1.0)
	checkEquals(all_E_z(1),0.0)
	checkEquals(all_E_z(2),c(0.0,1.0))
	checkEquals(all_E_z(4),c(0.0,1.0,0.0,3))
}


test.gamma_c <- function() {
	beta <- 2.0
	alpha <- 1.0
	
	k <- 0
	checkEquals(gamma_c(beta,alpha,k),1.0)
	
	k <- 1
	checkEquals(gamma_c(beta,alpha,k),beta + alpha * E_w(1))
	
	k <- 2
	checkEquals(gamma_c(beta,alpha,k),beta^2 + 2 * beta * alpha * E_w(1) + E_w(k))
	
	k <- 3
	checkEquals(gamma_c(beta,alpha,k),beta^3 + 3 * beta^2 * alpha * E_w(1) + 
					3 * beta * alpha^2 * E_w(2) + E_w(3))
	
	
}

test.gamma_gc <- function() {
	
	beta <- 2.0
	alpha <- 1.0
	g <- 2.0
	k2 <- 3; k <- gamma_c(beta,alpha,k2)
	
	k1 <- 0
	checkEquals(gamma_gc(g,beta,alpha,k1,k2),g^k1*k)
	
	k1 <- 1
	checkEquals(gamma_gc(g,beta,alpha,k1,k2),g^k1*k)
	
	k1 <- 2
	checkEquals(gamma_gc(g,beta,alpha,k1,k2),g^k1*k)
	
	k1 <- 3
	checkEquals(gamma_gc(g,beta,alpha,k1,k2),g^k1*k)

}
