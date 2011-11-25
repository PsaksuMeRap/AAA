# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RUnit")

stringsAsFactors = FALSE

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

u.1 <- where; rm(where)

# rimpiazza tutti gli h_{t-i} con l'espressione con h_{t-4} quale unico lag di h
for (i in 0:3) {
	where <- u.1
	what <- create_randomVariable("h",lag=i)
	print(toString(what))
	with <- create.h_t.expansion(i,4)
	# print(toString(with))
	where <- explode(where,what,with)
	print(toString(where))
}

Quit sopra non funzia!!!

u4 <- sort(u_t*u_t*u_t*u_t)

string1 <- toString(u4)

u4.1 <- shiftToZeroAndCompact(u4)
u4.2 <- dropWhereFirstRandomIsOddPower(u4.1,"e")

# rimpiazza e_{t-i} con h_{t-i}*z_{t-i}
where <- u4.2
for (lag in 0:4) {
	for (power in 1:4) {
		what <- create_randomVariable("e",lag=lag,power=power)
		with <- monomialFromString(paste("h_{t-",lag,"}^",power,"*z_{t-",lag,"}^",power,sep=""))
		where <- explode(where,what,with)
	}
}

u4.3 <- where; rm(where)

# calcola E(z_{t}^k)
where <- u4.3
for (power in 1:4) {
	what <- create_randomVariable(name="z",power=power)
	with <- create_monomials(create_monomial(number=E_z(power)))
	where <- explode(where,what,with)
}

u4.4 <- where; rm(where)
