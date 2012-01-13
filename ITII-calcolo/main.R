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
maxLag = lengt(fCoefficients) -1 

# calcolo simbolico di u_t
u_t <- create_representation_ut(maxLag=maxLag)

# calcolo simbolico di u_t^2
u_t.2 <- create_representation_ut_k(u_t,power=2)

# sostituzione di z e w con i valori attesi corrispondenti
E_u_t.2 <- expectation_f_b(u_t.2)

# explode wrt f_i coefficients
tmp <- E_u_t.2
## explode wrt f_i
for (i in 2:length(fCoefficients)) {
	symbolName <- paste("f",i-1,sep="")
	tmp <- explode_wrt_symbol(tmp,symbolName,fCoefficients[i])
}

## explode wrt b0, b1 e b2
tmp <- explode_wrt_symbol(a=tmp,"b0",b0)
tmp <- explode_wrt_symbol(tmp,"b1",b1)
tmp <- explode_wrt_symbol(tmp,"b2",b2)
tmp <- compact(tmp)

E_u_t.2 <- tmp
rm(tmp)







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


