#   =================================================================
#   File: algencanma.r
#   =================================================================
#
#   =================================================================
#   Module: Main program
#   =================================================================
#
#   Last update of any of the component of this module: 
# 
#   February 14, 2006.
#
#   Users are encouraged to download periodically updated versions of 
#   this code at the TANGO home page:
# 
#   www.ime.usp.br/~egbirgin/tango/
#
#   *****************************************************************
#   *****************************************************************
#
#   ALGENCAN solves problems of the form
#   ------------------------------------
#
#   min f(x)
#
#   subject to
#
#           c_j(x)  = 0, j in E,
#           c_j(x) <= 0, j in I,
#           l <= x <= u,
#
#   where E is the set of indices of the equality constraints, I is
#   the set of indices of the inequality constraints, and there are
#   n variables and m constraints.
#
#   ALGENCAN is an Augmented Lagrangian method that uses GENCAN to
#   solve the bound-constrained problems.
#
#   ALGENCAN is part of the TANGO Project.
#
#   Visit the TANGO home page in the web:
#
#   www.ime.usp.br/~egbirgin/tango/
#
#   *****************************************************************
#
#   TANGO LICENSE:
#   --------------
#
#   TANGO is free software; you can redistribute it and/or modify it 
#   under the terms of the GNU General Public License as published by 
#   the Free Software Foundation. Non-free versions of TANGO are 
#   available under terms different from those of the General Public 
#   License. Professors J. M. Mart�nez (martinez@ime.unicamp.br, 
#   martinezimecc@gmail.com) or E. G. Birgin (egbirgin@ime.usp.br, 
#   egbirgin@gmail.com) should be contacted for more information 
#   related to such a license, future developments and/or technical 
#   support.
#
#   Every published work that uses ALGENCAN should cite:
#
#   R. Andreani, E. G. Birgin, J. M. Mart�nez and M. L. Schuverdt, 
#   "On Augmented Lagrangian methods with general lower-level 
#   constraints", to appear in SIAM Journal on Optimization.
#
#   and
#
#   R. Andreani, E. G. Birgin, J. M. Mart�nez and M. L. Schuverdt, 
#   "Augmented Lagrangian methods under the Constant Positive Linear 
#   Dependence constraint qualification", Mathematical
#   Programming, 111, pp. 5-32, 2008.
#
#   Every published work that uses GENCAN should cite:
#
#   E. G. Birgin and J. M. Mart�nez, "Large-scale active-set 
#   box-constrained optimization method with spectral projected 
#   gradients", Computational Optimization and Applications 23, pp. 
#   101-125, 2002.
#
#   (See other related works at the TANGO home page.)
#
#   *****************************************************************
#
#   HOW TO USE ALGENCAN TO SOLVE YOUR OWN PROBLEM
#   --------------------------------------------
#
#   You will find below 10 subroutines and that YOU SHOULD MODIFY to 
#   solve your own problem. The modifications that you must do are:
#
#   1) In the SUBROUTINE INIP you must set the number of variables
#   of your problem (n), the initial point (x), the lower and upper
#   bounds on the variables (l and u), the number of constraints (m)
#   (set m equal to zero if your problems has no constraints), a
#   vector (equatn) that indicates, for each constraint, whether it 
#   is and equality constraint or an inequality (least-or-equal),
#   a vector (linear) that indicates, for each constraint, whether it 
#   is a linear constraint or not, the initial estimation of the 
#   Lagrange multipliers (lambda) (you can set it equal to zero if 
#   you do not have a better estimative) and the initial penalty 
#   parameters (rho).
#
#   2) SUBROUTINES EVALF and EVALC MUST also be MODIFIED to evaluate 
#   the objective function and the constraints, respectively. You 
#   will find the way in which these subroutines must be modified 
#   below.
#
#   3) SUBROUTINES EVALG and EVALJAC, to compute the gradient vector
#   of the objective function and the gradients of the constraints,
#   respectively, are NOT MANDATORY but HIGHLY RECOMMENDED. They
#   must be provided by the user if he/she set gtype = 0. If the 
#   user set gtype = 1 then finite differences approximations will
#   be used. (See the description of gtype parameter in subroutine 
#   param.) You will find the way in which these subroutines must 
#   be modified below.
#
#   4) OPTIONALY, you can modify SUBROUTINES EVALH and EVALHC to
#   evaluate the Hessian matrices of the objective function and the
#   constraints. Both subroutines are just optional subroutines and
#   are required when hptype = 0. (See the description of hptype
#   parameter in subroutine param.)
#
#   5) OPTIONALY, another way of using second-order information is
#   to provide, not individual subroutines to compute the Hessians
#   of the objective function and the constraints, but to provide
#   a unique SUBROUTINE EVALHLP to compute the product of an
#   arbitrary vector times the Lagrangian Hessian matrix. There is
#   not a clear advantage in coding EVALHLP instead of coding
#   EVALH and EVALHC. This alternative was incorporated to be 
#   primarily used in the AMPL and CUTEr interfaces. This 
#   subroutine is optional and is required when hptype = 1. (See 
#   the description of hptype parameter in subroutine param.)
#
#   6) OPTIONALLY, in the SUBROUTINE PARAM you may modify tolerances 
#   and maximum of iterations related to the stopping criteria. There 
#   are also two arguments related to the output in the screen (iprint
#   and ncomp), a parameter related to the derivatives calculation 
#   (gtype), a parameter related to the Hessians (or matrix-vector 
#   product) approximation (hptype) and a parameter related to the 
#   Conjugates Gradients preconditioning (precond). Finally, there is
#   also a parameter that allows to test the analytic derivatives
#   coded by you using finite differences. If you prefer, leave all 
#   these parameters with the suggested values. 
#
#   7) Alternatively, if you are interested in modifying any of the
#   parameters described in (3) but you prefer not to modify subroutine
#   PARAM, you can use the algencan.dat SPECIFICATION FILE. The
#   advantage of using the specification file is that you can test
#   the method with different parameters without compiling it every
#   time. This file IS NOT MANDATORY. So, if you will not modify the
#   default parameters or if you fill more comfortable modifying
#   subroutine PARAM, you do not need to worry about algencan.dat
#   specification file.
#
#   The specification file can have any number of lines, with at most
#   80 characters per line. Blank lines are admitted and lines starting
#   with '*' are considered comment lines and will be ignored. Each 
#   line must have just a "keyword" or a keyword followed by an integer
#   or a real value when required. As the interpreter is not case-
#   sensitive, you can write the keywords in lower case, upper case or 
#   any combination of them. You can also insert blanks in any place 
#   before or after the keywords. Moreover, just the first 10 characters
#   of the keyword are relevant and the rest will be ignored. Available 
#   keywords are:
#
#   ANALYTIC-GRADIENT               
#   FINITE-DIFFERENCES-GRADIENT     
#   HESSIANS-PROVIDED  
#   LAGRHESS-PRODUCT-PROVIDED
#   INCREMENTAL-QUOTIENTS           
#   BFGS-QN-APPROXIMATION           
#   ADAPTIVE-HESSIAN                
#   UNPRECONDITIONED-CG             
#   BFGS-QN-PRECONDITIONER          
#   CHECK-DERIVATIVES
#   FEASIBILITY-TOLERANCE        <real-value>   
#   OPTIMALITY-TOLERANCE         <real-value>
#   MAX-OUTER-ITERATIONS         <integer-value>   
#   MAX-INNER-ITERATIONS         <integer-value>
#   MAX-FUNCTION-EVALUATIONS     <integer-value> 
#   OUTPUT-DETAIL                <integer-value>  
#   NCOMP-ARRAY                  <integer-value>
#
#   By default, ALGENCAN uses:
#
#   ANALYTIC-GRADIENT               
#   ADAPTIVE-HESSIAN                
#   BFGS-QN-PRECONDITIONER          
#   FEASIBILITY-TOLERANCE    1.0d-04  
#   OPTIMALITY-TOLERANCE     1.0d-04
#   MAX-OUTER-ITERATIONS          50
#   MAX-INNER-ITERATIONS     1000000
#   MAX-FUNCTION-EVALUATIONS 5000000
#   OUTPUT-DETAIL                  1
#   NCOMP-ARRAY                    5
#
#   and derivatives are not tested.
#
#   8) Finally, SUBROUTINE ENDP can be modified by the user to write, 
#   save or compute any type of extra information related to the 
#   solution of the problem. Subroutine ENDP will be called just once 
#   after algencan found a solution of the problem. You can modify 
#   this subroutine for, for example, save the solution in a file 
#   chosed by you and in a special format to be processed later by 
#   another software.
# 
#   Besides the modifications that you must do to solve your own 
#   problem using ALGENCAN, ALGENCAN has many other arguments to
#   control a large variety of issues of the algorithmic behaviour. 
#   To see how to control these other arguments, see their detailed 
#   description at the begining of subroutine algencan (for arguments 
#   related to Augmented Lagrangians) and at the begining of 
#   subroutine gencan (for arguments related to the bound-constraints 
#   internal solver).
#
#   ******************************************************************
#   ******************************************************************

    param <- function(rhoauto,rhotype,rhomult,rhofrac,
             m,rho,gtype,hptype,intype,precond,
             checkder,epsfeas,epsopt,maxoutit,maxtotit,
             maxtotfc,iprint,ncomp) {

#   Parameters of the function:
#   ===========================
#
#   On Entry:
#   =========
#
#   M integer
#   ---------
#
#   Number of constraints. This parameter can be used by the user in 
#   case he/she would like to initialize the penalty parameters (one
#   per constraint).
#
#
#   On Return:
#   ==========
#
#   RHOAUTO logical
#   ---------------
#
#   indicates whether the initial penalty parameters will be the
#   ones given by the user (rhoauto = .false.) or will be computed 
#   automatically by the Augmented Lagrangian solver (rhoauto = 
#   .true.).
#
#   RHOTYPE integer
#   ---------------
#
#   indicates whether the penalty parameters will be updated all
#   toghether (rhotype=1) or individualy (rhotype=2)
#
#   RHOMULT double precision
#   ------------------------
#
#   when a penalty parameter is updated, it is multiplied by rhofrac.
#   So, rhofrac must be greater than 1. Suggested value rhofrac=10.0d0.
#
#   RHOFRAC double precision
#   ------------------------
#
#   this paramater between 0 and 1 is the improvement required in the 
#   infeasibility for not to update the penalty parameters. 
#
#   When rhotype=1, if the sup-norm of the constraints is smaller than 
#   or equal to rhofrac times the sup-norm of the constraints of the 
#   previous iteration then the penalty parameters are not updated. 
#   Otherwise, they are multiplied by rhomult.
#
#   When rhotype=2, the improvement in feasibility of each constraint 
#   is evaluated in separate. If the feasibility of a particular 
#   constraint is smaller than or equal to rhofrac times the
#   feasibility of the previous iteration then the penalty parameter
#   is not modified. Otherwise, it is multiplied by rhomult.
#
#   The suggested value for rhofrac is 0.5d0.
#
#   RHO double precision rho(m)
#   ---------------------------
#
#   This the vector of penalty parameters. The user can leave to the
#   method the task to find adequate initial values for the penalty 
#   parameters setting rhoauto = .true. Otherwise, he/she can set 
#   rhoauto = .false. and atribute a value for each one of the penalty 
#   parameters rho(i), i = 1, ..., m,
#
#   GTYPE integer
#   -------------
#
#   Type of first derivatives calculation according to the following 
#   convention:
#
#   0 means true first derivatives. In this case, functions evalg
#     and evaljac must be modified by the user to compute the 
#     gradient of the objective function and the gradients of the 
#     constraints, respectively.
#
#   1 means that a finite difference approximation will be used. In 
#     this case, functions evalg and evaljac may have an empty body 
#     but must be present. It is also recommended that those 
#     empty-body functions set flag = - 1. Last but not least, the 
#     option gtype = 1 is not cheap neither safe.
#
#   HPTYPE integer
#   --------------
#
#   Type of Hessian-vector product according to the following 
#   convention:
#
#   We will first describe the meaning if the choices in the Augmented 
#   Lagrangian framework. The way in which the product of the Hessian 
#   matrix of the Augmented Lagrangian by a vector will be done 
#   depends on the value of the parameter hptype in the following way:
#
#   9 means that an incremental quotients approximation without any 
#     extra consideration will be used. This option requires the 
#     evaluation of an extra gradient at each Conjugate Gradient 
#     iteration. If gtype = 0 then this gradient evaluation will be 
#     done using the user supplied functions evalg and evaljac 
#     (evalc will also be used). On the other hand, if gtype = 1, the 
#     gradient calculation will be done using just calls to the user 
#     provided functions evalf and evalc. nind calls will be done, 
#     where nind is the dimension of the current face of the 
#     active-set method. This option is not cheap neither safe.
#
#     If you did not code functions evalg and evaljac, to compute 
#     the gradient of the objective function and the Jacobian of the 
#     constraints then your options finished here.
#
#   0 means that functions to compute the Hessian of the objective 
#     function (evalh) and the Hessians of the constraints (evalhc) 
#     were provided by the user. So, the product of the Hessian of the 
#     Augmented Lagrangian times a vector will be computed using the
#     Hessians provided by these functions and then adding the first 
#     order term (for the first order term the user-supplied 
#     function to compute the Jacobian of the constraints (evaljac) 
#     is also used).
#
#   1 means that, instead of providing individual functions to 
#     compute the Hessians of the objective function and the 
#     constraints, the user provided a function to compute the 
#     product of an arbitrary vector times the Hessian of the 
#     Lagrangian.
#
#   2 means that incremental quotients will be used. The difference 
#     between hptype = 9 and hptype = 2 is that, in the latter case, 
#     the non-differentiability of the Hessian of the Augmented 
#     Lagrangian will be taken into account. In particular, when 
#     computing the gradient of the Augmented Lagrangian at 
#     (x + step p), the constraints that will be considered will be 
#     the same constraints that contributed to the computation of the
#     gradient of the Augmented Lagrangian at x.
#
#     If GENCAN is been used to solve a bound-constrained problem 
#     which is not the subproblem of an Augmented Lagrangian method, 
#     but an isolated bound-constrained problem, then there is no 
#     difference between this option and hptype = 9.
#
#     This option also requires the evaluation of an extra gradient at 
#     each Conjugate Gradient iteration. 
#
#   3 is similar to hptype = 2. The difference is that the 
#     contribution of the linear constraints in the Hessian matrix 
#     will be computed explicitly. If the problem has not linear 
#     constraints then this option is identical to hptype = 2. 
#     Moreover, if the problem has no constraints then this option is 
#     equal to hptype = 9.
#
#     This option also requires the evaluation of an extra gradient at 
#     each Conjugate Gradient iteration. 
#
#   4 means that the Hessian matrix will be approximated and then the 
#     product of the Hessian approximation by the vector will be 
#     computed exactly. In particular, the Hessian matrix will be 
#     approximated doing a BFGS correction to the Gauss-Newton
#     approximation of the Hessian. Before the BFGS correction, a 
#     structured spectral correction is done to force the Gauss-Newton 
#     approximation to be positive definite.
#
#     If the problem has not constraints then the approximation 
#     reduces to a BFGS approximation of the Hessian (without memory) 
#     and using the spectral approximation (instead of the identity) 
#     as initial approximation.
#
#     Numerical experiments suggested that this option is convenient 
#     just for constrained problems. This motivated the introduction 
#     of the next option.
#
#     This option does NOT require an extra gradient evaluation per 
#     iteration and, in this sense, each CG iteration is 
#     computationally cheaper than a CG iteration of the previous 
#     choices. However, the approximation of the Hessian matrix 
#     requires some information (mainly the Jacobian of the 
#     constraints) that must be saved during the gradient evaluation. 
#     To save this information requires an amount of memory 
#     proportional to the number of non-null elements of the Jacobian 
#     matrix.
#
#     Quadratic subproblems are convex with this choice.
#
#   5 is an adaptive strategy that choose, at every iteration, between
#     2 and 4. When the gradient of the Augmented Lagrangian is 
#     computed, it is verified if at least a constraint contributes to 
#     the calculation. If this is the case, 4 is used. Otherwise, 2 is 
#     used.
#
#     For problems with equality constraints (that always contributes 
#     to the Augmented Lagrangian function) this option is identical 
#     to 4.
#
#     For problems without constraints this option is identical to 2.
#
#   6 is identical to 5 but the choice is made between 3 and 4 instead 
#     of between 2 and 4. 
#
#     For problems with equality constraints (that always contributes 
#     to the Augmented Lagrangian function) this option is identical 
#     to 4. 
#
#     For problems without constraints this option is identical to 3.
#
#   We will now describe the meaning if the choices for unconstrained
#   and bound-constrained problems. In this context the way in which 
#   the product of the Hessian matrix by a vector will be done depends 
#   on the value of the parameter hptype in the following way:
#
#   0 means that the function to compute the Hessian of the 
#     objective function (evalh) was provided by the user. So, the 
#     product of the Hessian times a vector will be computed using the 
#     Hessian provided by this function.
#
#   1 means that a function (evalhlp) to compute the product of the 
#     Hessian of the objective function times an arbitrary vector is
#     being provided by the user.
#
#   9 means that an incremental quotients approximation will be used. 
#     This option requires the evaluation of an extra gradient at each
#     Conjugate Gradient iteration. If gtype = 0 then this gradient 
#     evaluation will be done using the user supplied function 
#     evalg. On the other hand, if gtype = 1, the gradient calculation 
#     will be done using just calls to the user provided function 
#     evalf. nind calls will be done, where nind is the dimension of 
#     the current face of the active-set method.
#
#     If you did not code function evalg to compute the gradient of 
#     the objective function then your options finished here.
#
#   4 means that the Hessian matrix will be approximated and then the 
#     product of the Hessian approximation by the vector will be 
#     computed exactly. The approximation is a BFGS approximation of 
#     the Hessian (without memory) and using the spectral 
#     approximation (instead of the identity) as initial 
#     approximation.
#
#     Numerical experiments suggested that this option is not 
#     convenient for unconstrained or just bound-constrained problems. 
#     (Note that this option was developed to be used in the Augmented 
#     Lagrangian framework.)
#
#     This option does NOT require an extra gradient evaluation per 
#     iteration and, in this sense, each CG iteration is 
#     computationally cheaper than a CG iteration of the previous 
#     choices.
#
#     Quadratic subproblems are convex with this choice.
#
#   In the bound-constrained context, options hptype = 2,3,5 and 6
#   are all identical to hptype = 9.
#
#   INTYPE integer
#   --------------
#
#   This parameter can be used to select the algorithm that will be
#   used to solve the Augmented Lagrangian subproblems, or the problem
#   itself if it an unconstrained problem. In the present implementation
#   just one algorithm is available. So, the value of this variable is
#   ignored.
#
#   PRECOND character * 6
#   ---------------------
#
#   Indicates the type of preconditioning that will be used for 
#   Conjugates Gradients according to the following convention:
#
#   'NONE'   means no preconditioner at all.
#
#   'QNAGNC' means Quasi-Newton Correction of the Gauss-Newton 
#            approximation of the Hessian. The exact form is this 
#            preconditioner is described in:
#
#            E. G. Birgin and J. M. Mart�nez, "Structured minimal-
#            memory inexact quasi-Newton method and secant 
#            preconditioners for Augmented Lagrangian Optimization", 
#            submitted, 2005.
#
#   CHECKDER logical
#   ----------------
#
#   If you are using finite differences aproximations for the 
#   derivatives (gtype = 1) then checkder must be set to FALSE. 
#   On the other hand, if you are using your own coded derivatives 
#   you may would like to test them against a finite differences 
#   approximation to verify if they are o.k. In this case, set 
#   checkder = TRUE.
#
#   EPSFEAS double precision
#   ------------------------
#
#   Feasibility tolerance for the sup-norm of the constraints.
#   (Ignored in the unconstrained and bound-constrained cases.)
#
#   EPSOPT double precision
#   -----------------------
#
#   Optimality tolerance for the sup-norm of the projected gradient of 
#   the Augmented Lagrangian in the constrained case and the sup-norm
#   of the projected gradient of the objective function in the 
#   unconstrained and the bound-constrained cases.
#
#   MAXOUTIT integer
#   ----------------
#
#   Maximum number of Augmented Lagrangian (outer) iterations.
#   (Ignored in the unconstrained and bound-constrained cases.)
#
#   MAXTOTIT integer
#   ----------------
#
#   Maximum total number of inner iterations in the Augmented 
#   Lagrangian context (total means summing up the inner iterations of 
#   each outer iteration). In the unconstrained and bound-constrained
#   cases it means just the maximum number of iterations.
#
#   MAXTOTFC integer
#   ----------------
#
#   Idem MAXTOTIT but for number of functional evaluations.
#
#   IPRINT integer
#   --------------
#              
#   Controls the ammount of information of the output according to the 
#   following convention:
#
#   0 means no output at all.
#
#   1 means information at each outer iteration but without any 
#     information of how the subproblems are being solved.
#
#   2 means the same as 1 plus information of each inner iteration.
#
#   3 means the same as 2 plus information of the line searches and 
#     the calculation of the truncated Newton direction (using CG) of 
#     each inner iteration.
#
#   In all cases, an output file named solution.txt with the final 
#   point, Lagrange mutipliers and penalty parameters will be 
#   generated. Moreover, the same output of the screen will be saved 
#   in a file named algencan.out.
#
#   NCOMP integer
#   -------------
#
#   Every time a vector is printed, just its first ncomp component 
#   will be displayed. 
#
#   ******************************************************************
#   FROM HERE ON YOU MUST MODIFY THE SUBROUTINE TO SET SOME ALGENCAN
#   ARGUMENTS RELATED TO DERIVATIVES, STOPPING CRITERIA AND OUTPUT:
#   ******************************************************************


    gtype   <- 0
    hptype  <- 6
 
    intype   <- 0
 
    precond <- "QNCGNA"
 
    rhoauto  <- 1
 
    rhotype  <- 1
    rhomult  <- 1.0e+01
    rhofrac  <- 0.5
 
    rho <- array(10.0, c(m))

    checkder <- 0

    epsfeas <- 1.0e-04
    epsopt  <- 1.0e-04

    maxoutit <- 50
    maxtotit <- 1000000
    maxtotfc <- 5 * maxtotit

    iprint <- 1
    ncomp  <- 5


#   ******************************************************************
#   STOP HERE YOUR MODIFICATIONS OF SUBROUTINE PARAM.
#   ****************************************************************** 
}

#   Load the problem definition file

    source("toyprob.r")

#   Load the solver wrapper

    dyn.load("/home/claudio/eclipse/AAA/ottimizzatore/lib/solver/algencan.so")

#   Call the solver

   .Call("algencan",body(evalf),body(evalg),body(evalh),body(evalc),
    body(evaljac),body(evalhc),body(evalhlp),body(inip),body(endp),
    body(param),sys.frame(0))
