# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RUnit")

stringsAsFactors = FALSE
setwd("/home/claudio/workspace/AAA/ITII-calcolo/")
setwd("\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/ITII-calcolo")
source("./lib/library.R")
source("./unitTests/testUtilities.R")

# il modello strutturale
# calcolo di x_t generato da un modello MA(2)
# x_t = e_t + a1*e_{t-1} + a2*e_{t-2}                 eq. (1)

# con e_t = h_t*z_t
#
# con h_t = b0 + b1*|e_{t-1}| + b2*h_{t-1}
# o   h_t = b0 + (b2 + b1*|z_{t-1}|)*h_{t-1}
# o   h_t = b0 + b1*w_{t-1}*h_{t-1} + b2*h_{t-1}      eq. (2)
# o con notazione di teraesvirta
#     h_t = g_t + c_{t-1} * h_{t-1}
# con g_t := b0 e quindi deterministo e costante nel tempo
#     c_t := b1*w_{t} + b2 = alpha*|z_{t}|+beta

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

# calcolo di u_t

create_symbolWithRandom <- function(symbolName,randomName,index) {
	create_monomials(
			create_monomial(1,
					create_symbols(create_symbol(paste(symbolName,index,sep=""),1)),
					create_randomVariables(create_randomVariable(randomName,index,1))
			)
	)
}


u_t <- create_monomials(monomialFromString("e_t"))

for (i in 1:4) u_t <- u_t + create_symbolWithRandom(symbolName="f",randomName="e",index=i)

# rimpiazza e_{t-i} con h_{t-i}*z_{t-i}
where <- u_t
for (lag in 0:4) {
	what <- create_randomVariable("e",lag=lag,power=1)
	with <- monomialFromString(paste("h_{t-",lag,"}*z_{t-",lag,"}",sep=""))
	where <- explode(where,what,with)
}

u_t <- where; rm(where)

# calcola u_t^2
u_t.2 <- u_t*u_t
rm(u_t)

# rimuovi le potenze dispari di "z" quando sono il termine con indice temporale
# piu' grande
u_t.2 <- dropWhereFirstRandomIsOddPower(u_t.2,"z")

# rimpiazza tutti gli h_{t-i}^k con h_{t-i}*h_{t-i}*... k-volte
for (i in 0:3) {
	rv <- create_randomVariable("h",lag=i)
	u_t.2 <- disaggregate(u_t.2,rv)
}


# rimpiazza tutti gli h_{t-i} con l'espressione con h_{t-4} quale unico lag di h
where <- u_t.2
for (i in 0:3) {
	what <- create_randomVariable("h",lag=i)
	with <- create.h_t.expansion(fromLag=i,toLag=4)
	where <- explode(where,what,with)
}

u_t.2 <- where; rm(where)


# ora calcola E(u_t.2). Rimuovi dapprima tutti i termini la cui
# variabile aleatoria z con indice temporale piu' alto ha potenza
# dispari

tmp <- dropWhereFirstRandomIsOddPower(u_t.2,"z")
tmp <- shiftToZeroAndCompact(tmp)
u_t.2 <- tmp
rm(tmp)

# calcola z_{t}^k e poi rimpiazza con E(z_{t}^k)
where <- u.5
for (power in 1:4) {
	what <- create_randomVariable(name="z",power=power)
	with <- create_monomials(create_monomial(number=E_z(power)))
	where <- explode(where,what,with)
}






# calcola u_t^4
u_t.4 <- u_t.2 * u_t.2

u_t.4 <- shiftToZeroAndCompact(u_t.4)
u_t.4 <- dropWhereFirstRandomIsOddPower(u_t.4,"z")



# questo è il vecchio codice 
u_t <- create_monomials(monomialFromString("e_t"))

for (i in 1:4) u_t <- u_t + create_symbolWithRandom(symbolName="f",randomName="e",index=i)

u.1 <- sort(u_t*u_t*u_t*u_t)
u.2 <- shiftToZeroAndCompact(u.1)
u.3 <- dropWhereFirstRandomIsOddPower(u.2,"e")

# rimpiazza e_{t-i}^power con h_{t-i}^power*z_{t-i}^power
where <- u.3
for (lag in 0:4) {
	for (power in 1:4) {
		what <- create_randomVariable("e",lag=lag,power=power)
		with <- monomialFromString(paste("h_{t-",lag,"}^",power,"*z_{t-",lag,"}^",power,sep=""))
		where <- explode(where,what,with)
	}
}

u.4 <- where; rm(where)

# rimpiazza tutti gli h_{t-i} con l'espressione con h_{t-4} quale unico lag di h
lagMassimo <- max( maxLag(u.4,"h") )
lagMinimo  <- min( minLag(u.4,"h") )
where <- u.4
for (i in lagMinimo:(lagMassimo-1)) {
	with <- create.h_t.expansion(i,lagMassimo)
	for (power in 1:lagMassimo) {
		what <- create_randomVariable("h",lag=i,power=power)
		where <- explode(where,what,with)
		if (i != lagMassimo) with <- with * with
	}
}

u.5 <- where; rm(where)
u.6 <- shiftToZeroAndCompact(u.5);rm(u.5)


# calcola E(z_{t}^k)
where <- u.5
for (power in 1:4) {
	what <- create_randomVariable(name="z",power=power)
	with <- create_monomials(create_monomial(number=E_z(power)))
	where <- explode(where,what,with)
}

u4.4 <- where; rm(where)

p1 <- monomialsFromString("1 + -1*c1*L + -1*c2*L^2")
p2 <- monomialsFromString("1 + -1*a1*L + -1*a2*L^2")
toString(p1*p2)
# "1 - 1*a1*L - 1*a2*L^2 - 1*c1*L + a1*c1*L^2 + a2*c1*L^3 - 1*c2*L^2 + a1*c2*L^3 + a2*c2*L^4"
# "1 "
# "1 + ( -c1 - a1 )*L + (a1*c1 - a2 -c2)*L^2 + (a2*c1 + a1*c2)*L^3 + a2*c2*L^4"
# "1 +           f1*L +               f2*L^2 +              f3*L^3 + f4"

