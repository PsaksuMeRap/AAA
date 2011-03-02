    param <- function(rhoauto,rhotype,rhomult,rhofrac,
         m,rho,gtype,hptype,intype,precond,
         checkder,epsfeas,epsopt,maxoutit,maxtotit,
         maxtotfc,iprint,ncomp) {

    gtype   <- 0
    hptype  <- 0
 
    intype  <- 0
 
    precond <- "QNCGNA"
 
    rhoauto  <- 1
    rhotype  <- 1
    
    rhomult  <- 1.0e+01
    rhofrac  <- 0.5
 
    rho <- array(10.0, c(m))

    checkder <- 0

    epsfeas <- 1.0e-05
    epsopt  <- 1.0e-05

    maxoutit <- 50
    maxtotit <- 1000000
    maxtotfc <- 5 * maxtotit

    iprint <- 1
    ncomp  <- 5

    }




     inip <- function(n,x,l,u,m,lambda,equatn,linear) {
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

     g <- as.vector(2 * covMatrix %*% x)

     }

#    ******************************************************************
#    ******************************************************************

     evalh <- function(n,x,hlin,hcol,hval,nnzh,flag) {

#    This subroutine might compute the Hessian matrix of the objective 
#    function. For achieving this objective YOU MAY MODIFY it according 
#    to your problem. To modify this subroutine IS NOT MANDATORY. See 
#    below where your modifications must be inserted.

     flag <- 0

     nnzh <- hessian$nnzh
     hcol <- hessian$hcol 
     hlin <- hessian$hlin
     hval <- as.vector(2 * hessian$vechCovMatrix)

     }

#    ******************************************************************
#    ******************************************************************

     evalc <- function(n,x,ind,c,flag) {

#    This subroutine must compute the ind-th constraint. For achieving 
#    this objective YOU MUST MOFIFY it according to your problem. See 
#    below the places where your modifications must be inserted.

    flag <- 0
    if (datiBenchmark$conVincoloBenchmark)
    {
      if (ind <= datiVincoli$nrVincUgualeAlgencan) {
        c <- (datiVincoli$M[ind,] %*% x)[1,1] - datiVincoli$b[[ind]]
      } else {
        if (ind < datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
          c <- (datiVincoli$M1[ind-datiVincoli$nrVincUgualeAlgencan,] %*% x)[1,1] + 
              - datiVincoli$b1[[ind-datiVincoli$nrVincUgualeAlgencan]]
        } else {
          if (ind == datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
            ## add to improve the feasibility tolerance
            c <- 10000*((x %*% (covMatrix %*% x) -2 * t(x)%*%(datiBenchmark$C1%*%datiBenchmark$v.pesiRiskAssets) +
              datiBenchmark$v.pesiRiskAssets %*% (datiBenchmark$C2%*%datiBenchmark$v.pesiRiskAssets) +
              - datiBenchmark$trackErrorConstrSquared)[1,1])
             
 
          } else {
            flag <- -1
          }
        }
      }
    } else { ## if (datiBenchmark$conVincoloBenchmark)
      if (ind <= datiVincoli$nrVincUgualeAlgencan) {
        c <- (datiVincoli$M[ind,] %*% x)[1,1] - datiVincoli$b[[ind]]
      } else {
        if (ind <= datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
          c <- (datiVincoli$M1[ind-datiVincoli$nrVincUgualeAlgencan,] %*% x)[1,1] + 
              - datiVincoli$b1[[ind-datiVincoli$nrVincUgualeAlgencan]]
        } else {
          flag <- -1
        }
      }
    }

    } ## end procedure
#    ******************************************************************
#    ******************************************************************

     evaljac <- function(n,x,ind,indjac,valjac,nnzjac,flag) {

#    This subroutine must compute the gradient of the ind-th constraint. 
#    For achieving these objective YOU MUST MODIFY it in the way 
#    specified below.
   
     flag <- 0
     nnzjac <- datiVincoli$nrPesi 
    if (datiBenchmark$conVincoloBenchmark)
    {
      if (ind <= datiVincoli$nrVincUgualeAlgencan) {
        indjac <- 1:nnzjac
        valjac <- as.vector(datiVincoli$M[ind,])
      } else {
        if (ind < datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
          indjac <- 1:nnzjac
          valjac <- as.vector(datiVincoli$M1[ind-datiVincoli$nrVincUgualeAlgencan,])
        } else {
          if (ind == datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
          indjac <- 1:nnzjac
          ## multiplied by 10000 to improve feasibility tolerance
          valjac <- as.vector(2 * 10000 * ((covMatrix %*% x) - (datiBenchmark$C1%*%datiBenchmark$v.pesiRiskAssets)))
          } else {
            flag <- -1
          }
        }
      }
    } else {
      if (ind <= datiVincoli$nrVincUgualeAlgencan) {
        indjac <- 1:nnzjac
        valjac <- as.vector(datiVincoli$M[ind,])
      } else {
        if (ind <= datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
          indjac <- 1:nnzjac
          valjac <- as.vector(datiVincoli$M1[ind-datiVincoli$nrVincUgualeAlgencan,])
        } else {
          flag <- -1
        }
      }
    }
   }  ## fine funzione 
#    ******************************************************************
#    ******************************************************************

     evalhc <- function(n,x,ind,hclin,hccol,hcval,nnzhc,flag) {

#    This subroutine might compute the Hessian matrix of the ind-th
#    constraint. For achieving this objective YOU MAY MODIFY it 
#    according to your problem. To modify this subroutine IS NOT 
#    MANDATORY. See below where your modifications must be inserted.
  
     flag  <- 0
     if (datiBenchmark$conVincoloBenchmark & ind == datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {     
        nnzhc <- hessian$nnzh
        hccol  <- hessian$hcol 
        hclin <- hessian$hlin
        hcval <- as.vector(2 * 10000 * hessian$vechCovMatrix)
     } else {
        if (ind <= datiVincoli$nrVincUgualeAlgencan+datiVincoli$nrVincAlMaxAlgencan) {
          nnzhc <- 0
        } else {
          flag <- -1
        }
     }

     }

#    ******************************************************************
#    ******************************************************************
     evalhlp <- function(n,x,m,lambda,p,hp,goth,flag) {

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

