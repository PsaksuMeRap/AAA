C     =================================================================
C     File: algencanma.f
C     =================================================================

C     =================================================================
C     Module: Main program
C     =================================================================

C     Last update of any of the component of this module: 
C
C     January 30, 2007.

C     Users are encouraged to download periodically updated versions of 
C     this code at the TANGO home page:
C
C     www.ime.usp.br/~egbirgin/tango/

C     *****************************************************************
C     *****************************************************************

C     ALGENCAN solves problems of the form
C     ------------------------------------
C
C     min f(x)
C
C     subject to
C
C             c_j(x)  = 0, j in E,
C             c_j(x) <= 0, j in I,
C             l <= x <= u,
C
C     where E is the set of indices of the equality constraints, I is
C     the set of indices of the inequality constraints, and there are
C     n variables and m constraints.
C
C     ALGENCAN is an Augmented Lagrangian method that uses GENCAN to
C     solve the bound-constrained problems.
C
C     ALGENCAN is part of the TANGO Project.
C
C     Visit the TANGO home page in the web:
C
C     www.ime.usp.br/~egbirgin/tango/

C     *****************************************************************

C     TANGO LICENSE:
C     --------------
C
C     TANGO is free software; you can redistribute it and/or modify it 
C     under the terms of the GNU General Public License as published by 
C     the Free Software Foundation. Non-free versions of TANGO are 
C     available under terms different from those of the General Public 
C     License. Professors J. M. Martínez (martinez@ime.unicamp.br, 
C     martinezimecc@gmail.com) or E. G. Birgin (egbirgin@ime.usp.br, 
C     egbirgin@gmail.com) should be contacted for more information 
C     related to such a license, future developments and/or technical 
C     support.
C
C     Every published work that uses ALGENCAN should cite:
C
C     R. Andreani, E. G. Birgin, J. M. Martínez and M. L. Schuverdt, 
C     "On Augmented Lagrangian methods with general lower-level 
C     constraints", to appear in SIAM Journal on Optimization.
C
C     and
C
C     R. Andreani, E. G. Birgin, J. M. Martínez and M. L. Schuverdt, 
C     "Augmented Lagrangian methods under the Constant Positive Linear 
C     Dependence constraint qualification", to appear in Mathematical
C     Programming.
C
C     Every published work that uses GENCAN should cite:
C
C     E. G. Birgin and J. M. Martínez, "Large-scale active-set 
C     box-constrained optimization method with spectral projected 
C     gradients", Computational Optimization and Applications 23, pp. 
C     101-125, 2002.
C
C     (See other related works at the TANGO home page.)

C     *****************************************************************

C     HOW TO USE ALGENCAN TO SOLVE YOUR OWN PROBLEM
C     --------------------------------------------
C  
C     You will find below 10 subroutines and that YOU SHOULD MODIFY to 
C     solve your own problem. The modifications that you must do are:
C  
C     1) In the SUBROUTINE INIP you must set the number of variables
C     of your problem (n), the initial point (x), the lower and upper
C     bounds on the variables (l and u), the number of constraints (m)
C     (set m equal to zero if your problems has no constraints), a
C     vector (equatn) that indicates, for each constraint, whether it 
C     is and equality constraint or an inequality (least-or-equal),
C     a vector (linear) that indicates, for each constraint, whether it 
C     is a linear constraint or not, and the initial estimation of the 
C     Lagrange multipliers (lambda) (you can set it equal to zero if 
C     you do not have a better estimative).
C  
C     2) SUBROUTINES EVALF and EVALC MUST also be MODIFIED to evaluate 
C     the objective function and the constraints, respectively.
C
C     3) SUBROUTINES EVALG and EVALJAC, to compute the gradient vector
C     of the objective function and the gradients of the constraints,
C     respectively, are NOT MANDATORY but HIGHLY RECOMMENDED. They
C     must be provided by the user if he/she set gtype = 0. If the 
C     user set gtype = 1 then finite differences approximations will
C     be used. (See the description of gtype parameter in subroutine 
C     param.) 
C
C     4) OPTIONALY, you can modify SUBROUTINES EVALH and EVALHC to
C     evaluate the Hessian matrices of the objective function and the
C     constraints. Both subroutines are just optional subroutines and
C     are required when hptype = 0. (See the description of hptype
C     parameter in subroutine param.)
C
C     5) OPTIONALY, another way of using second-order information is
C     to provide, not individual subroutines to compute the Hessians
C     of the objective function and the constraints, but to provide
C     a unique SUBROUTINE EVALHLP to compute the product of an
C     arbitrary vector times the Lagrangian Hessian matrix. There is
C     not a clear advantage in coding EVALHLP instead of coding
C     EVALH and EVALHC. This alternative was incorporated to be 
C     primarily used in the AMPL and CUTEr interfaces. This 
C     subroutine is optional and is required when hptype = 1. (See 
C     the description of hptype parameter in subroutine param.)
C  
C     6) OPTIONALLY, in the SUBROUTINE PARAM you may modify some 
C     arguments like feasibility and optimality tolerances and maximum 
C     number of iterations and functional evaluations. See the detailed
C     description of each argument in subroutine PARAM. If you prefer, 
C     leave all these parameters with the suggested values. 
C  
C     7) Alternatively, if you are interested in modifying any of the
C     parameters described in (5) but you prefer not to modify subroutine
C     PARAM, you can use the algencan.dat SPECIFICATION FILE. The
C     advantage of using the specification file is that you can test
C     the method with different parameters without compiling it every
C     time. This file IS NOT MANDATORY. So, if you will not modify the
C     default parameters or if you fill more comfortable modifying
C     subroutine PARAM, you do not need to worry about algencan.dat
C     specification file.
C  
C     The specification file can have any number of lines, with at most
C     80 characters per line. Blank lines are admitted and lines starting
C     with '*' or '#' are considered comment lines and will be ignored. 
C     Each line must have just a "keyword" or a keyword followed by an 
C     integer or a real value when required. As the interpreter is not 
C     case-sensitive, you can write the keywords in lower case, upper 
C     case or any combination of them. You can also insert blanks in any 
C     place before or after the keywords. Moreover, just the first 10 
C     characters of the keyword are relevant and the rest will be ignored. 
C     Available keywords are:
C  
C     ANALYTIC-GRADIENT                     
C     FINITE-DIFFERENCES-GRADIENT           
C     HESSIANS-PROVIDED                     
C     LAGRHESS-PRODUCT-PROVIDED             
C     INCREMENTAL-QUOTIENTS                 
C     BFGS-QN-APPROXIMATION                 
C     ADAPTIVE-HESSIAN                      
C     AUTO-BDSOLVER                         
C     GENCAN-BDSOLVER                       
C     BETRA-BDSOLVER                        
C     SPARSE-BETRA-BDSOLVER                 
C     UNPRECONDITIONED-CG                   
C     BFGS-QN-PRECONDITIONER                
C     AUTO-INITIAL-PENALTY-PARAMETERS       
C     MANUAL-INITIAL-PENALTY-PARAMETERS      <real-value> 
C     COLLECTIVE-PENALTY-PARAMETERS-UPDATE  
C     INDEPENDENT-PENALTY-PARAMETERS-UPDATE 
C     DESIRED-INFEASIBILITY-FRACTION         <real-value> 
C     PENALTY-PARAMETER-MULTIPLIER-INCREMENT <real-value> 
C     CHECK-DERIVATIVES                     
C     FEASIBILITY-TOLERANCE                  <real-value> 
C     OPTIMALITY-TOLERANCE                   <real-value> 
C     MAX-OUTER-ITERATIONS                   <integer-value>
C     MAX-INNER-ITERATIONS                   <integer-value>
C     MAX-FUNCTION-EVALUATIONS               <integer-value>
C     OUTPUT-DETAIL                          <integer-value>
C     NCOMP-ARRAY                            <integer-value>
C               
C     By default, ALGENCAN uses:
C  
C     ANALYTIC-GRADIENT               
C     ADAPTIVE-HESSIAN                
C     BFGS-QN-PRECONDITIONER          
C     AUTO-BDSOLVER                    
C     BFGS-QN-PRECONDITIONER           
C     AUTO-INITIAL-PENALTY-PARAMETERS  
C     COLLECTIVE-PENALTY-PARAMETERS-UPDATE  
C     DESIRED-INFEASIBILITY-FRACTION           0.5d0 
C     PENALTY-PARAMETER-MULTIPLIER-INCREMENT 1.0d+01 
C     FEASIBILITY-TOLERANCE                  1.0d-04  
C     OPTIMALITY-TOLERANCE                   1.0d-04
C     MAX-OUTER-ITERATIONS                        50
C     MAX-INNER-ITERATIONS                   1000000
C     MAX-FUNCTION-EVALUATIONS               5000000
C     OUTPUT-DETAIL                                1
C     NCOMP-ARRAY                                  5
C
C     and derivatives are not tested.
C  
C     8) Finally, SUBROUTINE ENDP can be modified by the user to write, 
C     save or compute any type of extra information related to the 
C     solution of the problem. Subroutine ENDP will be called just once 
C     after algencan found a solution of the problem. You can modify 
C     this subroutine for, for example, save the solution in a file 
C     chosed by you and in a special format to be processed later by 
C     another software.
C   
C     Besides the modifications that you must do to solve your own 
C     problem using ALGENCAN, ALGENCAN has many other arguments to
C     control a large variety of issues of the algorithmic behaviour. 
C     To see how to control these other arguments, see their detailed 
C     description at the begining of subroutine algencan (for arguments 
C     related to Augmented Lagrangians) and at the begining of 
C     subroutine gencan (for arguments related to the bound-constraints 
C     internal solver).

C     ******************************************************************
C     ******************************************************************

      program algencanma

      implicit none

C     PARAMETERS
      integer mmax,nmax
      parameter ( mmax      =  500000 )
      parameter ( nmax      =  500000 )

C     LOCAL SCALARS
      logical checkder,rhoauto
      character * 6 precond
      integer gtype,hptype,inform,intype,iprint,m,maxoutit,maxtotfc,
     +        maxtotit,n,ncomp,outiter,rhotype,totcgcnt,totfcnt,totgcnt,
     +        totiter
      double precision epsfeas,epsopt,f,nalpsupn,rhofrac,rhomult,snorm
      real time

C     LOCAL ARRAYS
      integer wi1(nmax)
      logical equatn(mmax),linear(mmax)
      double precision l(nmax),lambda(mmax),rho(mmax),u(nmax),wd1(mmax),
     +        wd2(mmax),wd3(nmax),wd4(mmax),wd5(mmax),wd6(nmax),
     +        wd7(nmax),wd8(nmax),wd9(nmax),wd10(nmax),wd11(nmax),
     +        wd12(nmax),wd13(nmax),wd14(nmax),wd15(nmax),wd16(nmax),
     +        wd17(nmax),wd18(nmax),wd19(nmax),x(nmax)

C     EXTERNAL SUBROUTINES
      external solver

C     SET UP PROBLEM DATA
      call inip(n,x,l,u,m,lambda,equatn,linear)

C     SET SOME SOLVER ARGUMENTS
      call param(rhoauto,rhotype,rhomult,rhofrac,m,rho,gtype,hptype,
     +intype,precond,checkder,epsfeas,epsopt,maxoutit,maxtotit,maxtotfc,
     +iprint,ncomp)

C     CALL OPTIMIZATION SOLVER
      call solver(n,x,l,u,m,lambda,equatn,linear,rhoauto,rhotype,
     +rhomult,rhofrac,rho,gtype,hptype,intype,precond,checkder,epsfeas,
     +epsopt,maxoutit,maxtotit,maxtotfc,iprint,ncomp,f,snorm,nalpsupn,
     +outiter,totiter,totfcnt,totgcnt,totcgcnt,time,inform,wi1,wd1,wd2,
     +wd3,wd4,wd5,wd6,wd7,wd8,wd9,wd10,wd11,wd12,wd13,wd14,wd15,wd16,
     +wd17,wd18,wd19)

C     WRITE ADDITIONAL OUTPUT INFORMATION CODED BY THE USER
      call endp(n,x,l,u,m,lambda,equatn,linear)

      stop

      end

C     ******************************************************************
C     ******************************************************************

      subroutine param(rhoauto,rhotype,rhomult,rhofrac,m,rho,gtype,
     +hptype,intype,precond,checkder,epsfeas,epsopt,maxoutit,maxtotit,
     +maxtotfc,iprint,ncomp)

C     SCALAR ARGUMENTS
      character * 6 precond
      logical checkder,rhoauto
      integer gtype,hptype,iprint,intype,m,maxoutit,maxtotfc,maxtotit,
     +        ncomp,rhotype
      double precision epsfeas,epsopt,rhofrac,rhomult

C     ARRAY ARGUMENTS
      double precision rho(m)

C     Parameters of the subroutine:
C     =============================
C
C     On Entry:
C     =========
C
C     M integer
C     ---------
C
C     Number of constraints. This parameter can be used by the user in 
C     case he/she would like to initialize the penalty parameters (one
C     per constraint).
C
C
C     On Return:
C     ==========
C
C     RHOAUTO logical
C     ---------------
C
C     indicates whether the initial penalty parameters will be the
C     ones given by the user (rhoauto = .false.) or will be computed 
C     automatically by the Augmented Lagrangian solver (rhoauto = 
C     .true.).
C
C     RHOTYPE integer
C     ---------------
C
C     indicates whether the penalty parameters will be updated all
C     toghether (rhotype=1) or individualy (rhotype=2)
C
C     RHOMULT double precision
C     ------------------------
C
C     when a penalty parameter is updated, it is multiplied by rhofrac.
C     So, rhofrac must be greater than 1. Suggested value rhofrac=10.0d0.
C
C     RHOFRAC double precision
C     ------------------------
C
C     this paramater between 0 and 1 is the improvement required in the 
C     infeasibility for not to update the penalty parameters. 
C
C     When rhotype=1, if the sup-norm of the constraints is smaller than 
C     or equal to rhofrac times the sup-norm of the constraints of the 
C     previous iteration then the penalty parameters are not updated. 
C     Otherwise, they are multiplied by rhomult.
C
C     When rhotype=2, the improvement in feasibility of each constraint 
C     is evaluated in separate. If the feasibility of a particular 
C     constraint is smaller than or equal to rhofrac times the
C     feasibility of the previous iteration then the penalty parameter
C     is not modified. Otherwise, it is multiplied by rhomult.
C
C     The suggested value for rhofrac is 0.5d0.
C
C     RHO double precision rho(m)
C     ---------------------------
C
C     This the vector of penalty parameters. The user can leave to the
C     method the task to find adequate initial values for the penalty 
C     parameters setting rhoauto = .true. Otherwise, he/she can set 
C     rhoauto = .false. and atribute a value for each one of the penalty 
C     parameters rho(i), i = 1, ..., m,
C
C     GTYPE integer
C     -------------
C
C     Type of first derivatives calculation according to the following 
C     convention:
C
C     0 means true first derivatives. In this case, subroutines evalg
C       and evaljac must be modified by the user to compute the 
C       gradient of the objective function and the gradients of the 
C       constraints, respectively.
C
C     1 means that a finite difference approximation will be used. In 
C       this case, subroutines evalg and evaljac may have an empty body 
C       but must be present. It is also recommended that those 
C       empty-body subroutines set flag = - 1. Last but not least, the 
C       option gtype = 1 is not cheap neither safe.
C
C     HPTYPE integer
C     --------------
C
C     Type of Hessian-vector product according to the following 
C     convention:
C
C     We will first describe the meaning if the choices in the Augmented 
C     Lagrangian framework. The way in which the product of the Hessian 
C     matrix of the Augmented Lagrangian by a vector will be done 
C     depends on the value of the parameter hptype in the following way:
C
C     9 means that an incremental quotients approximation without any 
C       extra consideration will be used. This option requires the 
C       evaluation of an extra gradient at each Conjugate Gradient 
C       iteration. If gtype = 0 then this gradient evaluation will be 
C       done using the user supplied subroutines evalg and evaljac 
C       (evalc will also be used). On the other hand, if gtype = 1, the 
C       gradient calculation will be done using just calls to the user 
C       provided subroutines evalf and evalc. nind calls will be done, 
C       where nind is the dimension of the current face of the 
C       active-set method. This option is not cheap neither safe.
C
C       If you did not code subroutines evalg and evaljac, to compute 
C       the gradient of the objective function and the Jacobian of the 
C       constraints then your options finished here.
C
C     0 means that subroutines to compute the Hessian of the objective 
C       function (evalh) and the Hessians of the constraints (evalhc) 
C       were provided by the user. So, the product of the Hessian of the 
C       Augmented Lagrangian times a vector will be computed using the
C       Hessians provided by these subroutines and then adding the first 
C       order term (for the first order term the user-supplied 
C       subroutine to compute the Jacobian of the constraints (evaljac) 
C       is also used).
C
C     1 means that, instead of providing individual subroutines to 
C       compute the Hessians of the objective function and the 
C       constraints, the user provided a subroutine to compute the 
C       product of an arbitrary vector times the Hessian of the 
C       Lagrangian.
C
C     2 means that incremental quotients will be used. The difference 
C       between hptype = 9 and hptype = 2 is that, in the latter case, 
C       the non-differentiability of the Hessian of the Augmented 
C       Lagrangian will be taken into account. In particular, when 
C       computing the gradient of the Augmented Lagrangian at 
C       (x + step p), the constraints that will be considered will be 
C       the same constraints that contributed to the computation of the
C       gradient of the Augmented Lagrangian at x.
C
C       If GENCAN is been used to solve a bound-constrained problem 
C       which is not the subproblem of an Augmented Lagrangian method, 
C       but an isolated bound-constrained problem, then there is no 
C       difference between this option and hptype = 9.
C
C       This option also requires the evaluation of an extra gradient at 
C       each Conjugate Gradient iteration. 
C
C     3 is similar to hptype = 2. The difference is that the 
C       contribution of the linear constraints in the Hessian matrix 
C       will be computed explicitly. If the problem has not linear 
C       constraints then this option is identical to hptype = 2. 
C       Moreover, if the problem has no constraints then this option is 
C       equal to hptype = 9.
C
C       This option also requires the evaluation of an extra gradient at 
C       each Conjugate Gradient iteration. 
C
C     4 means that the Hessian matrix will be approximated and then the 
C       product of the Hessian approximation by the vector will be 
C       computed exactly. In particular, the Hessian matrix will be 
C       approximated doing a BFGS correction to the Gauss-Newton
C       approximation of the Hessian. Before the BFGS correction, a 
C       structured spectral correction is done to force the Gauss-Newton 
C       approximation to be positive definite.
C
C       If the problem has not constraints then the approximation 
C       reduces to a BFGS approximation of the Hessian (without memory) 
C       and using the spectral approximation (instead of the identity) 
C       as initial approximation.
C
C       Numerical experiments suggested that this option is convenient 
C       just for constrained problems. This motivated the introduction 
C       of the next option.
C
C       This option does NOT require an extra gradient evaluation per 
C       iteration and, in this sense, each CG iteration is 
C       computationally cheaper than a CG iteration of the previous 
C       choices. However, the approximation of the Hessian matrix 
C       requires some information (mainly the Jacobian of the 
C       constraints) that must be saved during the gradient evaluation. 
C       To save this information requires an amount of memory 
C       proportional to the number of non-null elements of the Jacobian 
C       matrix.
C 
C       Quadratic subproblems are convex with this choice.
C
C     5 is an adaptive strategy that choose, at every iteration, between
C       2 and 4. When the gradient of the Augmented Lagrangian is 
C       computed, it is verified if at least a constraint contributes to 
C       the calculation. If this is the case, 4 is used. Otherwise, 2 is 
C       used.
C
C       For problems with equality constraints (that always contributes 
C       to the Augmented Lagrangian function) this option is identical 
C       to 4.
C
C       For problems without constraints this option is identical to 2.
C
C     6 is identical to 5 but the choice is made between 3 and 4 instead 
C       of between 2 and 4. 
C
C       For problems with equality constraints (that always contributes 
C       to the Augmented Lagrangian function) this option is identical 
C       to 4. 
C
C       For problems without constraints this option is identical to 3.
C
C     We will now describe the meaning if the choices for unconstrained
C     and bound-constrained problems. In this context the way in which 
C     the product of the Hessian matrix by a vector will be done depends 
C     on the value of the parameter hptype in the following way:
C
C     0 means that the subroutine to compute the Hessian of the 
C       objective function (evalh) was provided by the user. So, the 
C       product of the Hessian times a vector will be computed using the 
C       Hessian provided by this subroutine.
C
C     1 means that a subroutine (evalhlp) to compute the product of the 
C       Hessian of the objective function times an arbitrary vector is
C       being provided by the user.
C
C     9 means that an incremental quotients approximation will be used. 
C       This option requires the evaluation of an extra gradient at each
C       Conjugate Gradient iteration. If gtype = 0 then this gradient 
C       evaluation will be done using the user supplied subroutine 
C       evalg. On the other hand, if gtype = 1, the gradient calculation 
C       will be done using just calls to the user provided subroutine 
C       evalf. nind calls will be done, where nind is the dimension of 
C       the current face of the active-set method.
C
C       If you did not code subroutine evalg to compute the gradient of 
C       the objective function then your options finished here.
C
C     4 means that the Hessian matrix will be approximated and then the 
C       product of the Hessian approximation by the vector will be 
C       computed exactly. The approximation is a BFGS approximation of 
C       the Hessian (without memory) and using the spectral 
C       approximation (instead of the identity) as initial 
C       approximation.
C
C       Numerical experiments suggested that this option is not 
C       convenient for unconstrained or just bound-constrained problems. 
C       (Note that this option was developed to be used in the Augmented 
C       Lagrangian framework.)
C
C       This option does NOT require an extra gradient evaluation per 
C       iteration and, in this sense, each CG iteration is 
C       computationally cheaper than a CG iteration of the previous 
C       choices.
C
C       Quadratic subproblems are convex with this choice.
C
C     In the bound-constrained context, options hptype = 2,3,5 and 6
C     are all identical to hptype = 9.
C
C     INTYPE integer
C     --------------
C
C     This parameter can be used to select the algorithm that will be
C     used to solve the Augmented Lagrangian subproblems, or the problem
C     itself if it an unconstrained problem. In the present implementation
C     just one algorithm is available. So, the value of this variable is
C     ignored.
C
C     PRECOND character * 6
C     ---------------------
C
C     Indicates the type of preconditioning that will be used for 
C     Conjugates Gradients according to the following convention:
C
C     'NONE'   means no preconditioner at all.
C
C     'QNAGNC' means Quasi-Newton Correction of the Gauss-Newton 
C              approximation of the Hessian. The exact form is this 
C              preconditioner is described in:
C 
C              E. G. Birgin and J. M. Martínez, "Structured minimal-
C              memory inexact quasi-Newton method and secant 
C              preconditioners for Augmented Lagrangian Optimization", 
C              submitted, 2005.
C
C     CHECKDER logical
C     ----------------
C
C     If you are using finite differences aproximations for the 
C     derivatives (gtype = 1) then checkder must be set to FALSE. 
C     On the other hand, if you are using your own coded derivatives 
C     you may would like to test them against a finite differences 
C     approximation to verify if they are o.k. In this case, set 
C     checkder = TRUE.
C
C     EPSFEAS double precision
C     ------------------------
C
C     Feasibility tolerance for the sup-norm of the constraints.
C     (Ignored in the unconstrained and bound-constrained cases.)
C
C     EPSOPT double precision
C     -----------------------
C
C     Optimality tolerance for the sup-norm of the projected gradient of 
C     the Augmented Lagrangian in the constrained case and the sup-norm
C     of the projected gradient of the objective function in the 
C     unconstrained and the bound-constrained cases.
C
C     MAXOUTIT integer
C     ----------------
C
C     Maximum number of Augmented Lagrangian (outer) iterations.
C     (Ignored in the unconstrained and bound-constrained cases.)
C
C     MAXTOTIT integer
C     ----------------
C
C     Maximum total number of inner iterations in the Augmented 
C     Lagrangian context (total means summing up the inner iterations of 
C     each outer iteration). In the unconstrained and bound-constrained
C     cases it means just the maximum number of iterations.
C
C     MAXTOTFC integer
C     ----------------
C
C     Idem MAXTOTIT but for number of functional evaluations.
C
C     IPRINT integer
C     --------------
C                
C     Controls the ammount of information of the output according to the 
C     following convention:
C
C     0 means no output at all.
C
C     1 means information at each outer iteration but without any 
C       information of how the subproblems are being solved.
C
C     2 means the same as 1 plus information of each inner iteration.
C
C     3 means the same as 2 plus information of the line searches and 
C       the calculation of the truncated Newton direction (using CG) of 
C       each inner iteration.
C
C     In all cases, an output file named solution.txt with the final 
C     point, Lagrange mutipliers and penalty parameters will be 
C     generated. Moreover, the same output of the screen will be saved 
C     in a file named algencan.out.
C
C     NCOMP integer
C     -------------
C
C     Every time a vector is printed, just its first ncomp component 
C     will be displayed.

      gtype    = 0
      hptype   = 6

      intype   = 0

      precond  = 'QNCGNA'

      rhoauto  = .true.

      rhotype  = 1
      rhomult  = 1.0d+01
      rhofrac  = 0.5d0

      checkder = .false.

      epsfeas  = 1.0d-04
      epsopt   = 1.0d-04

      maxoutit = 50
      maxtotit = 1000000
      maxtotfc = 5 * maxtotit

      iprint   = 1
      ncomp    = 5

      end

