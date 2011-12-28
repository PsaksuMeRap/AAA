# TODO: Add comment
# 
# Author: ortellic
###############################################################################


E_w <- function(power) {
	## questa funzione restituisce il momento E(w^power), w=|z| dove z ~ N(0,1)
	if (!power) return(1.0) # se power == 0 return 1.0
	
	if (power==1) return(sqrt(2/pi))
	
	moments <- 1.0:power
	moments[1:2] <- c(sqrt(2/pi),1)
	
	for (i in moments[-(1:2)]) {
		moments[i] <- (i-1) * moments[i-2]
	}
	
	return(moments[power])
}

all_E_w <- function(power) {
	## questa funzione restituisce 
	## a) tutti i momenti E(w^i), w=|z| dove z ~ N(0,1)
	## per valori di i in 1:power se power >= 1,
	## b) 1 se power = 0.
	
	if (!power) return(1.0) # se power == 0 return 1.0
	
	if (power==1) return(sqrt(2/pi))
	
	moments <- 1.0:power
	moments[1:2] <- c(sqrt(2/pi),1)
	
	for (i in moments[-(1:2)]) {
		moments[i] <- (i-1) * moments[i-2]
	}
	
	return(moments)
}


E_z <- function(power) {
	## questa funzione restituisce il momento E(z^power) dove z ~ N(0,1)
	if (!power) return(1.0) # se power == 0 return 1.0
	
	if (power==1) return(0.0)
	
	moments <- 1.0:power
	moments[1:2] <- c(0.0,1.0)
	
	for (i in moments[-(1:2)]) {
		moments[i] <- (i-1) * moments[i-2]
	}
	
	return(moments[power])
}


all_E_z <- function(power) {
	## questa funzione restituisce 
	## a) tutti i momenti E(z^i), dove z ~ N(0,1)
	## per valori di i in 1:power se power >= 1,
	## b) 1 se power = 0.
	
	if (!power) return(1.0) # se power == 0 return 1.0
	
	if (power==1) return(0.0)
	
	moments <- 1.0:power
	moments[1:2] <- c(0.0,1.0)
	
	for (i in moments[-(1:2)]) {
		moments[i] <- (i-1) * moments[i-2]
	}
	
	return(moments)
}


gamma_c <- function(beta, alpha, k) {
	## calcola il valore atteso di E(c_t^k) dove c_t = beta + alpha * |z_t|,
	## con z_t ~ N(0,1).
	
	## betaPowers: vettore delle potenze di beta in ordine decrescente
	## alphaPowers: vettore delle potenze di alpha in ordine crescente
	## binCoeff: vettore dei coefficienti binomiali
	## w_k: il vettore delle potenze di u_t = |z_t|
	
	if (k==0) return(1.0)
	
	betaPowers <- beta^(k:0)
	alphaPowers <- alpha^(0:k)
	binCoeff <- choose(k, k:0)
	w_k <- c(1.0,all_E_w(k))
	return(sum(betaPowers*alphaPowers*binCoeff*w_k))
}

gamma_gc <- function(g,beta,alpha,k1,k2) {
	# questa funzione calcola il valore atteso del prodotto
	# g^k1 * c^k2 dove g corrisponde a b0 e c = b1*|z_t| + b2
	# come da Teraesvirta
	
	return(g^k1*gamma_c(beta,alpha,k2))

}

E_h <- function(g,beta,alpha,m) {
	# questa funzione calcola il valore atteso E(h_t^m)
	# con e_t = h_t*z_t
	#
	# con h_t = b0 + b1*|e_{t-1}| + b2*h_{t-1}
    # o   h_t = b0 + (b2 + b1*|z_{t-1}|)*h_{t-1}
	# o   h_t = b0 + b1*w_{t-1}*h_{t-1} + b2*h_{t-1}      eq. (2)
	# o con notazione di teraesvirta
	#     h_t = g_t + c_{t-1} * h_{t-1}
	# con g_t := b0 e quindi deterministo e costante nel tempo
	#     c_t := b1*w_{t} + b2 = alpha*|z_{t}|+beta
	
	if (m == 0) return(1.0)
	
	if (m == 1) {
		gc <- gamma_c(beta,alpha,1)
			if (gc >= 1.0) stop(paste("Error in E_h: gamma_c >= 1!\nParameters:\ng       =",g,
							"\nalpha   =",alpha,"\nbeta    =",beta,"\nm       =",m,"\ngamma_c =",gc))
		return( g/(1-gc) )
	}

	result <- vector(mode="numeric", length=m)
	tmp <- 0.0
	for (j in 1:m) {
		tmp <- tmp + choose(m,j)*gamma_gc(g,beta,alpha,j,m-j) * E_h(g,beta,alpha,m-j)
	}
	gc <- gamma_c(beta,alpha,m)
	if (gc >= 1.0) stop(paste("Error in E_h: gamma_c >= 1!\nParameters:\ng       =",g,
						"\nalpha   =",alpha,"\nbeta    =",beta,"\nm       =",m,"\ngamma_c =",gc))
	return(tmp/(1-gc))
}
