# TODO: Add comment
# 
# Author: ortellic
###############################################################################



test.expect_tildeGamma_gc <- function() {
	# questa funzione calcola il valore atteso del prodotto
	# tra g^i e c^j dove g corrisponde a b0 e c = b1*|z_t| + b2
	# come da Teraesvirta
	
	expect_tildeGamma_gc <- function(b,powerG=1,powerC=1) {
		result <- b[1]^powerG*(b[2]*E_w(1)+b[3])^powerC
		
		return(result)
	}
	
	b <- vector(mode = "numeric", length = 3)
	b[1] <- 0.5
	b[2] <- 0.2
	b[3] <- 0.3
	
	checkEquals(expect_tildeGamma_gc(b),b[1]*(b[2]*E_w(1)+b[3]))
	checkEquals(expect_tildeGamma_gc(b,powerC=2),b[1]*(b[2]*E_w(1)+b[3])^2)


}


test.expect_h_t <- function() {
	
	expect_h_t <- function(power=1,b0,b1,b2) {
		
	}
	
	b0 <- 0.5
	b1 <- 0.2
	b2 <- 0.3
	
	
}
