#    =================================================================
#    File: toyprob.r
#    =================================================================
# 
#    =================================================================
#    Module: Subroutines that define the problem
#    =================================================================
#
#    Last update of any of the component of this module: 
#
#    January 27, 2006.
#
#    Users are encouraged to download periodically updated versions of 
#    this code at the TANGO home page:
# 
#    www.ime.usp.br/~egbirgin/tango/ 
#
#    ******************************************************************
#    ******************************************************************

     inip <- function(n,x,l,u,m,lambda,equatn,linear) {

#    This subroutine must set some problem data. For achieving this
#    objective YOU MUST MODIFY it according to your problem. See below
#    where your modifications must be inserted.
# 
#    Parameters of the subroutine:
# 
#    On Entry:
# 
#    This subroutine has no input parameters.
# 
#    On Return
#
#    n        integer,
#             number of variables,
#
#    x        double precision x(n),
#             initial point,
#
#    l        double precision l(n),
#             lower bounds on x,
#
#    u        double precision u(n),
#             upper bounds on x,
#
#    m        integer,
#             number of constraints (excluding the bounds),
#
#    lambda   double precision lambda(m),
#             initial estimation of the Lagrange multipliers,
#
#    rho      double precision rho(m),
#             initial penalty parameters.
#
#    equatn   logical equatn(m)
#             for each constraint j, set equatn(j) = .true. if it is an
#             equality constraint of the form c_j(x) = 0, and set
#             equatn(j) = .false. if it is an inequality constraint of
#             the form c_j(x) <= 0,
#
#    linear   logical linear(m)
#             for each constraint j, set linear(j) = .true. if it is a
#             linear constraint, and set linear(j) = .false. if it is a
#             nonlinear constraint.

#    Number of variables 

     n <- 2

#    Number of constraints (equalities plus inequalities) 

     m <- as.integer(2)

#    Initial point 

     x[1] <- 0.0
     x[2] <- 0.0

#    Lower and upper bounds 

     l[1] <- - 10.0
     l[2] <- - 1.0e20

     u[1] <-   10.0
     u[2] <-   1.0e20

#    Lagrange multipliers approximation. 
   
     lambda <- array(0.0, c(m))

#    For each constraint i, set equatn(i) = 1. if it is an equality
#    constraint of the form c_i(x) = 0, and set equatn(i) = 0 if 
#    it is an inequality constraint of the form c_i(x) <= 0. 

     equatn[1] <- 0
     equatn[2] <- 0
  
#    For each constraint i, set linear(i) = 1 if it is a linear
#    constraint, otherwise set linear(i) = 0 

     linear[1] <- 0
     linear[2] <- 1
  
     }

#    ******************************************************************
#    ******************************************************************

     evalf <- function(n,x,f,flag) {

#    This subroutine must compute the objective function. For achieving
#    this objective YOU MUST MODIFY it according to your problem. See
#    below where your modifications must be inserted.
#
#    Parameters of the subroutine:
#
#    On Entry:
#
#    n        integer,
#             number of variables,
#
#    x        double precision x(n),
#             current point,
#
#    On Return
#
#    f        double precision,
#             objective function value at x,
#
#    flag     integer,
#             You must set it to any number different of 0 (zero) if
#             some error ocurred during the evaluation of the objective
#             function. (For example, trying to compute the square root
#             of a negative number, dividing by zero or a very small
#             number, etc.) If everything was o.k. you must set it
#             equal to zero.

     flag <- 0

     f <- x[2]

     }

#    ******************************************************************
#    ******************************************************************

     evalg <- function(n,x,g,flag) {

#    This subroutine must compute the gradient vector of the objective
#    function. For achieving these objective YOU MUST MODIFY it in the
#    way specified below. However, if you decide to use numerical
#    derivatives (we dont encourage this option at all!) you dont need
#    to modify evalg.
#
#    Parameters of the subroutine:
#
#    On Entry:
#
#    n        integer,
#             number of variables,
#
#    x        double precision x(n),
#             current point,
#
#    On Return
#
#    g        double precision g(n),
#             gradient vector of the objective function evaluated at x,
#
#    flag     integer,
#             You must set it to any number different of 0 (zero) if
#             some error ocurred during the evaluation of any component
#             of the gradient vector. (For example, trying to compute
#             the square root of a negative number, dividing by zero or
#             a very small number, etc.) If everything was o.k. you
#             must set it equal to zero.

     flag <- 0

     g[1] <- 0
     g[2] <- 1.0

     }

#    ******************************************************************
#    ******************************************************************

     evalh <- function(n,x,hlin,hcol,hval,nnzh,flag) {

#    This subroutine might compute the Hessian matrix of the objective 
#    function. For achieving this objective YOU MAY MODIFY it according 
#    to your problem. To modify this subroutine IS NOT MANDATORY. See 
#    below where your modifications must be inserted.
#   
#    Parameters of the subroutine:
#  
#    On Entry:
#  
#    n        integer,
#             number of variables,
#  
#    x        double precision x(n),
#             current point,
#  
#    On Return
#  
#    nnzh     integer,
#             number of perhaps-non-null elements of the computed 
#             Hessian,
#  
#    hlin     integer hlin(nnzh),
#             see below,
#  
#    hcol     integer hcol(nnzh),
#             see below,
#  
#    hval     double precision hval(nnzh),
#             the non-null value of the (hlin(k),hcol(k)) position 
#             of the Hessian matrix of the objective function must 
#             be saved at hval(k). Just the lower triangular part of
#             Hessian matrix must be computed,
#  
#    flag     integer,
#             You must set it to any number different of 0 (zero) if 
#             some error ocurred during the evaluation of the Hessian
#             matrix of the objective funtion. (For example, trying 
#             to compute the square root of a negative number, 
#             dividing by zero or a very small number, etc.) If 
#             everything was o.k. you must set it equal to zero.

     flag <- 0

     nnzh <- 0

     }

#    ******************************************************************
#    ******************************************************************

     evalc <- function(n,x,ind,c,flag) {

#    This subroutine must compute the ind-th constraint. For achieving 
#    this objective YOU MUST MOFIFY it according to your problem. See 
#    below the places where your modifications must be inserted.
#
#    Parameters of the subroutine:
#
#    On Entry:
#
#    n        integer,
#             number of variables,
#
#    x        double precision x(n),
#             current point,
#
#    ind      integer,
#             index of the constraint to be computed,
#
#    On Return
#
#    c        double precision,
#             i-th constraint evaluated at x,
#
#    flag     integer
#             You must set it to any number different of 0 (zero) if
#             some error ocurred during the evaluation of the
#             constraint. (For example, trying to compute the square
#             root of a negative number, dividing by zero or a very
#             small number, etc.) If everything was o.k. you must set
#             it equal to zero.

     flag <- 0

     if (ind == 1)
         c <- x[1] * x[1] + 1.0 - x[2]

     else if (ind == 2)   
         c <- 2.0 - x[1] - x[2]

     else
         flag <- -1

     }

#    ******************************************************************
#    ******************************************************************

     evaljac <- function(n,x,ind,indjac,valjac,nnzjac,flag) {

#    This subroutine must compute the gradient of the ind-th constraint. 
#    For achieving these objective YOU MUST MODIFY it in the way 
#    specified below.
# 
#    Parameters of the subroutine:
# 
#    On Entry:
# 
#    n        integer,
#             number of variables,
# 
#    x        double precision x(n),
#             current point,
# 
#    ind      integer,
#             index of the constraint whose gradient will be computed,
# 
#    On Return
# 
#    nnzjac   integer,
#             number of perhaps-non-null elements of the computed
#             gradient,
# 
#    indjac   integer indjac(nnzjac),
#             see below,
# 
#    valjac   double precision valjac(nnzjac),
#             the non-null value of the partial derivative of the i-th
#             constraint with respect to the indjac(k)-th variable must
#             be saved at valjac(k).
# 
#    flag     integer
#             You must set it to any number different of 0 (zero) if
#             some error ocurred during the evaluation of the
#             constraint. (For example, trying to compute the square
# 	      root of a negative number, dividing by zero or a very
#             small number, etc.) If everything was o.k. you must set
#             it equal to zero.
   
     flag <- 0

     if ( ind == 1 ) {
         nnzjac <- 2
   
         indjac[1] <- 1
         valjac[1] <- 2.0 * x[1]
   
         indjac[2] <- 2
         valjac[2] <- - 1.0
     }

     else if ( ind == 2 ) {
         nnzjac <- 2

         indjac[1] <- 1.0
         valjac[1] <- - 1.0

         indjac[2] <- 2.0
         valjac[2] <- - 1.0
     }

     else
         flag <- -1

     }

#    ******************************************************************
#    ******************************************************************

     evalhc <- function(n,x,ind,hclin,hccol,hcval,nnzhc,flag) {

#    This subroutine might compute the Hessian matrix of the ind-th
#    constraint. For achieving this objective YOU MAY MODIFY it 
#    according to your problem. To modify this subroutine IS NOT 
#    MANDATORY. See below where your modifications must be inserted.
#    
#    Parameters of the subroutine:
#    
#    On Entry:
#    
#    n        integer,
#             number of variables,
#    
#    x        double precision x(n),
#             current point,
#    
#    ind      integer,
#             index of the constraint whose Hessian will be computed,
#    
#    On Return
#    
#    nnzhc    integer,
#             number of perhaps-non-null elements of the computed 
#             Hessian,
#    
#    hclin    integer hclin(nnzhc),
#             see below,
#    
#    hccol    integer hccol(nnzhc),
#             see below,
#    
#    hcval    double precision hcval(nnzhc),
#             the non-null value of the (hclin(k),hccol(k)) position 
#             of the Hessian matrix of the ind-th constraint must 
#             be saved at hcval(k). Just the lower triangular part of
#             Hessian matrix must be computed,
#    
#    flag     integer,
#             You must set it to any number different of 0 (zero) if 
#              some error ocurred during the evaluation of the Hessian
#             matrix of the ind-th constraint. (For example, trying 
#             to compute the square root of a negative number, 
#             dividing by zero or a very small number, etc.) If 
#              everything was o.k. you must set it equal to zero.
  
     flag <- 0
     
     if ( ind == 1 ) {     
         nnzhc <- 1

         hclin[1] <- 1
         hccol[1] <- 1
         hcval[1] <- 2.0
     }

     else if ( ind == 2 )
         nnzhc <- 0

     else
         flag <- -1

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
#    Parameters of the subroutine:
#    
#    The parameters of this subroutine are the same parameters of
#    subroutine inip. But in this subroutine there are not output
#    parameter. All the parameters are input parameters.

     }

