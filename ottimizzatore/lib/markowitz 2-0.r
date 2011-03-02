
param <- function(epsfeas,epsopt,iprint,ncomp) {
	
#   ******************************************************************
#   FROM HERE ON YOU MUST MODIFY THE SUBROUTINE TO SET SOME ALGENCAN
#   ARGUMENTS RELATED TO DERIVATIVES, STOPPING CRITERIA AND OUTPUT:
#   ******************************************************************
	
	epsfeas <- 1.0e-05
	epsopt  <- 1.0e-05
	iprint  <- 1
	ncomp   <- 5
	
	
#   ******************************************************************
#   STOP HERE YOUR MODIFICATIONS OF SUBROUTINE PARAM.
#   ******************************************************************
}


inip <- function(n,x,l,u,m,lambda,equatn,linear,coded,checkder) {
#    On Entry:
# 
#    This subroutine has no input parameters.
	
#    Number of variables 
	
	n <- datiVincoli$nrPesi
	
#    Number of constraints (equalities plus inequalities) 
	
	m <- datiVincoli$nrVincUgualeAlgencan + datiVincoli$nrVincAlMaxAlgencan
	
#    Initial point 
	
	x <- rep(1/n,n)
	
#    Lower and upper bounds 
	
	l <- datiVincoli$l_bounds
	u <- datiVincoli$u_bounds
	
#    Lagrange multipliers approximation. 
	
	lambda <- array(0.0, c(m))
	
#    For each constraint i, set equatn(i) = 1. if it is an equality
#    constraint of the form c_i(x) = 0, and set equatn(i) = 0 if 
#    it is an inequality constraint of the form c_i(x) <= 0. 
	equatn <- rep(0,m)
	equatn[1:datiVincoli$nrVincUgualeAlgencan] <- 1
	equatn[-(1:datiVincoli$nrVincUgualeAlgencan)] <- 0
	
#    For each constraint i, set linear(i) = 1 if it is a linear
#    constraint, otherwise set linear(i) = 0 
	
	if (!datiBenchmark$conVincoloBenchmark)
	{
		linear <- rep(1,m)
	} else {
		linear <- rep(1,m)
		linear[m] <- 0
	}
	
	coded[1]  <- 1 # evalf
	coded[2]  <- 1 # evalg
	coded[3]  <- 1 # evalh
	coded[4]  <- 1 # evalc
	coded[5]  <- 1 # evaljac
	coded[6]  <- 1 # evalhc
	coded[7]  <- 0 # evalfc
	coded[8]  <- 0 # evalgjac
	coded[9]  <- 0 # evalhl
	coded[10] <- 0 # evalhlp
	
	checkder = 0
}

#    ******************************************************************
#    ******************************************************************

evalf <- function(n,x,f,flag) {
	
#    This subroutine must compute the objective function. For achieving
#    this objective YOU MUST MODIFY it according to your problem. See
#    below where your modifications must be inserted.
	
	flag <- 0
	
	f <- (x %*% covMatrix %*% x)[1,1]
	
}

#    ******************************************************************
#    ******************************************************************

evalg <- function(n,x,g,flag) {
	
#    This subroutine must compute the gradient vector of the objective
#    function. For achieving these objective YOU MUST MODIFY it in the
#    way specified below. However, if you decide to use numerical
#    derivatives (we dont encourage this option at all!) you dont need
#    to modify evalg.
	
	flag <- 0
	
	g <- 2.0 * as.vector(covMatrix %*% x)
	
}

#    ******************************************************************
#    ******************************************************************

evalh <- function(n,x,hlin,hcol,hval,hnnz,flag) {
	
#    This subroutine might compute the Hessian matrix of the objective 
#    function. For achieving this objective YOU MAY MODIFY it according 
#    to your problem. To modify this subroutine IS NOT MANDATORY. See 
#    below where your modifications must be inserted.
	
	flag <- 0
	
	hnnz <- hessian$nnzh
	hcol <- hessian$hcol 
	hlin <- hessian$hlin
	hval <- as.vector(2 * hessian$vechCovMatrix)
	
}

#    ******************************************************************
#    ******************************************************************

evalc <- function(n,x,ind,cind,flag) {
	
#    This subroutine must compute the ind-th constraint. For achieving 
#    this objective YOU MUST MOFIFY it according to your problem. See 
#    below the places where your modifications must be inserted.
	
	flag <- 0
	if (datiBenchmark$conVincoloBenchmark)
	{
		if (ind <= datiVincoli$nrVincUgualeAlgencan) {
			cind <- (datiVincoli$M[ind,] %*% x)[1,1] - datiVincoli$b[[ind]]
		} else {
			if (ind < datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
				cind <- (datiVincoli$M1[ind-datiVincoli$nrVincUgualeAlgencan,] %*% x)[1,1] + 
						- datiVincoli$b1[[ind-datiVincoli$nrVincUgualeAlgencan]]
			} else {
				if (ind == datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
					## add to improve the feasibility tolerance
					cind <- 10000*((x %*% (covMatrix %*% x) -2 * t(x)%*%(datiBenchmark$C1%*%datiBenchmark$v.pesiRiskAssets) +
									datiBenchmark$v.pesiRiskAssets %*% (datiBenchmark$C2%*%datiBenchmark$v.pesiRiskAssets) +
									- datiBenchmark$trackErrorConstrSquared)[1,1])
				} else {
					flag <- -1
				}
			}
		}
	} else { ## if (datiBenchmark$conVincoloBenchmark)
		if (ind <= datiVincoli$nrVincUgualeAlgencan) {
			cind <- (datiVincoli$M[ind,] %*% x)[1,1] - datiVincoli$b[[ind]]
		} else {
			if (ind <= datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
				cind <- (datiVincoli$M1[ind-datiVincoli$nrVincUgualeAlgencan,] %*% x)[1,1] + 
						- datiVincoli$b1[[ind-datiVincoli$nrVincUgualeAlgencan]]
			} else {
				flag <- -1
			}
		}
	}
	
} ## end procedure
#    ******************************************************************
#    ******************************************************************

evaljac <- function(n,x,ind,jcvar,jcval,jcnnz,flag) {
	
#    This subroutine must compute the gradient of the ind-th constraint. 
#    For achieving these objective YOU MUST MODIFY it in the way 
#    specified below.
	
	flag <- 0
	jcnnz <- datiVincoli$nrPesi 
	if (datiBenchmark$conVincoloBenchmark)
	{
		if (ind <= datiVincoli$nrVincUgualeAlgencan) {
			jcvar <- 1:jcnnz
			jcval <- as.vector(datiVincoli$M[ind,])
		} else {
			if (ind < datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
				jcvar <- 1:jcnnz
				jcval <- as.vector(datiVincoli$M1[ind-datiVincoli$nrVincUgualeAlgencan,])
			} else {
				if (ind == datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
					jcvar <- 1:jcnnz
					## multiplied by 10000 to improve feasibility tolerance
					jcval <- as.vector(2 * 10000 * ((covMatrix %*% x) - (datiBenchmark$C1%*%datiBenchmark$v.pesiRiskAssets)))
				} else {
					flag <- -1
				}
			}
		}
	} else {
		if (ind <= datiVincoli$nrVincUgualeAlgencan) {
			jcvar <- 1:jcnnz
			jcval <- as.vector(datiVincoli$M[ind,])
		} else {
			if (ind <= datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
				jcvar <- 1:jcnnz
				jcval <- as.vector(datiVincoli$M1[ind-datiVincoli$nrVincUgualeAlgencan,])
			} else {
				flag <- -1
			}
		}
	}
}  ## fine funzione 
#    ******************************************************************
#    ******************************************************************

evalhc <- function(n,x,ind,hclin,hccol,hcval,hcnnz,flag) {
	
#    This subroutine might compute the Hessian matrix of the ind-th
#    constraint. For achieving this objective YOU MAY MODIFY it 
#    according to your problem. To modify this subroutine IS NOT 
#    MANDATORY. See below where your modifications must be inserted.
	
	flag  <- 0
	if (datiBenchmark$conVincoloBenchmark & ind == datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {     
		hcnnz <- hessian$nnzh
		hccol  <- hessian$hcol 
		hclin <- hessian$hlin
		hcval <- as.vector(2 * 10000 * hessian$vechCovMatrix)
	} else {
		if (ind <= datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
			hcnnz <- 0
		} else {
			flag <- -1
		}
	}
	
}



#    ******************************************************************
#    ******************************************************************

evalfc <- function(n,x,f,m,constr,flag) {
	
	flag <- -1
	
}

#    ******************************************************************
#    ******************************************************************

evalgjac <- function(n,x,g,m,jcfun,jcvar,jcval,jcnnz,flag) {
	
	flag <- -1
	
}

#    *****************************************************************
#    *****************************************************************

evalhl <- function(n,x,m,lambda,sf,sc,hllin,hlcol,hlval,hlnnz,
		flag) {
	
	flag <- -1
	
}

#    ******************************************************************
#    ******************************************************************

evalhlp <- function(n,x,m,lambda,sf,sc,p,hp,gothl,flag) {
	
	flag <- -1
	
}

#    ******************************************************************
#    ******************************************************************

endp <- function(n,x,l,u,m,lambda,equatn,linear) {
	
#    This subroutine can be used to do some extra job after the solver
#    has found the solution,like some extra statistics, or to save the
#    solution in some special format or to draw some graphical
#    representation of the solution. If the information given by the
#    solver is enough for you then leave the body of this subroutine
#    empty.
#    
}
