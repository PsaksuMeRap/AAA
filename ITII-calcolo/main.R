# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RUnit")

stringsAsFactors = FALSE
# setwd("/home/claudio/workspace/AAA/ITII-calcolo/")
# setwd("\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/ITII-calcolo")
source("./lib/library.R")
source("./unitTests/testUtilities.R")

# il modello strutturale
# calcolo di x_t generato da un modello MA(2)
# x_t = e_t + a1*e_{t-1} + a2*e_{t-2}                 eq. (1)
a1 <- 1
a2 <- 0.25 # (1-0.5L)^2
# con e_t = h_t*z_t
#
# con h_t = b0 + b1*|e_{t-1}| + b2*h_{t-1}
# o   h_t = b0 + (b2 + b1*|z_{t-1}|)*h_{t-1}
# o   h_t = b0 + b1*w_{t-1}*h_{t-1} + b2*h_{t-1}      eq. (2)
# o con notazione di teraesvirta
#     h_t = g_t + c_{t-1} * h_{t-1}
# con g_t := b0 e quindi deterministo e costante nel tempo
#     c_t := b1*w_{t} + b2 = alpha*|z_{t}|+beta
b0 = 1.0
b1 = 0.3
b2 = 0.2

# il modello ausiliario 
# x_t = c1*x_{t-1} + c2*x_{t-2} + u_t                 eq. (3)
# 
# con u_t = h_t*z_t
# e   h_t^2 = d0 + d1*u_{t-1}^2 + d2*u_{t-2}^2
# o   u_t^2 = d0 + d1*u_{t-1}^2 + d2*u_{t-2}^2 + q_t  eq. (4)
# con q_t = u_t^2 - h_t^2

# dall'eq(3) e (1) otteniamo
# u_t = (1 - c1L - c2L^2) * x_t = (1 - c1L - c2L^2)*(1 - a1L - a2L^2) * e_t
# quindi
# u_t = (1 - f1L - f2L^2 - f3L^3 - f4L^4) * e_t
# 1) Parto dai valori a1 e a2 del modello strutturale MA(2)
#    e ricavo i pseudo valori f_i
c <- pseudoTrueValues(ma=c(0,1,a1,a2),p=2)
fCoefficients <- c$f[,1]

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

u_t <- create_representation_ut(maxLag=4)

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

u_t.2 <- create_representation_ut_k(u_t,power=2)

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

E_u_t.2 <- expectation_f_b(u_t.2)

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



tmp <- E_u_t.2
for (i in 2:length(fCoefficients)) {
	symbolName <- paste("f",i-1,sep="")
	print(symbolName)
	tmp <- explode_wrt_symbol(tmp,symbolName,fCoefficients[i])
}

tmp <- explode_wrt_symbol(a=tmp,"b0",b0)
tmp <- explode_wrt_symbol(tmp,"b1",b1)
tmp <- explode_wrt_symbol(tmp,"b2",b2)
tmp <- compact(tmp)

mettere nei test della somma e moltiplicazione di symbols
l'uguaglianza di potenze dopo la somma



# calcola h_{t}^k e poi rimpiazza con E(h_{t}^k)
where <- u_t.2
for (lag in 0:4) {
	for (power in 1:4) {
		what <- create_randomVariable(name="h",power=power,lag=lag)
		Eh <- E_h(g=b0,beta=b2,alpha=b1,m=power)
		with <- create_monomials(create_monomial(number=Eh))
		where <- explode(where,what,with)
	}
}
u_t.2 <- where; rm(where)

tmp <- shiftToZeroAndCompact(u_t.2)







p1 <- monomialsFromString("1 + -1*c1*L + -1*c2*L^2")
p2 <- monomialsFromString("1 +    a1*L +    a2*L^2")
product <- p1*p2

f <- list()
for (i in 0:4) {
	f[[i+1]] <- extractLagCoeff(product,i)
}

# "1 - 1*a1*L - 1*a2*L^2 - 1*c1*L + a1*c1*L^2 + a2*c1*L^3 - 1*c2*L^2 + a1*c2*L^3 + a2*c2*L^4"
# "1 "
# "1 + ( -c1 - a1 )*L + (a1*c1 - a2 -c2)*L^2 + (a2*c1 + a1*c2)*L^3 + a2*c2*L^4"
# "1 +           f1*L +               f2*L^2 +              f3*L^3 +    f4*L^4"




# 2) Calcolo i valori della rappresentazione di u_t in termini di e_t, ovvero
#    i coefficienti b1,...,b4
fCoefficients <- c$f[,1]

# 3) utilizzo i valori di b0, b1 e b2 del modello strutturale garch per calcolare
#    il valore atteso di h_t^k



# 4) utilizzo i valori attesi di h_t^k per calcolare l'espressione


