/* ==================================================================
   Module: Interface between R and ALGENCAN
   ==================================================================

   Last update of any of the component of this module: 
 
   November 28, 2007.

   Users are encouraged to download periodically updated versions of 
   this code at the TANGO home page:
 
   www.ime.usp.br/~egbirgin/tango/

   ******************************************************************
   ******************************************************************

   TANGO LICENSE:
   --------------

   TANGO is free software; you can redistribute it and/or modify it 
   under the terms of the GNU General Public License as published by 
   the Free Software Foundation. Non-free versions of TANGO are 
   available under terms different from those of the General Public 
   License. Professors J. M. Mart�nez (martinez@ime.unicamp.br, 
   martinezimecc@gmail.com) or E. G. Birgin (egbirgin@ime.usp.br, 
   egbirgin@gmail.com) should be contacted for more information 
   related to such a license, future developments and/or technical 
   support.

   Every published work that uses ALGENCAN should cite:

   R. Andreani, E. G. Birgin, J. M. Mart�nez and M. L. Schuverdt, 
   "On Augmented Lagrangian methods with general lower-level 
   constraints", to appear in SIAM Journal on Optimization.

   and

   R. Andreani, E. G. Birgin, J. M. Mart�nez and M. L. Schuverdt, 
   "Augmented Lagrangian methods under the Constant Positive Linear 
   Dependence constraint qualification", Mathematical
   Programming, 111, pp. 5-32, 2008.

   Every published work that uses GENCAN should cite:

   E. G. Birgin and J. M. Mart�nez, "Large-scale active-set 
   box-constrained optimization method with spectral projected 
   gradients", Computational Optimization and Applications 23, pp. 
   101-125, 2002.

   (See other related works at the TANGO home page.)

   ****************************************************************** */

/* ******************************************************************
   ****************************************************************** */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "rwrapper.h"
#include <R.h>
#include <Rdefines.h>


/* ******************************************************************
   ****************************************************************** */

   SEXP createRRealScalar(double x) {

   SEXP ans;

   PROTECT(ans = allocVector(REALSXP, 1));
   REAL(ans)[0] = x;
   UNPROTECT(1);

   return ans;

   }

/* ******************************************************************
   ****************************************************************** */

   SEXP createRRealVector(int size,double* x) {

   SEXP ans;
   int i;

   if(x == NULL){
     PROTECT(ans = allocVector(REALSXP, 1));
     REAL(ans)[0] = 0.0;
     UNPROTECT(1);  
   }
   else{
     PROTECT(ans = allocVector(REALSXP, size));
     for (i = 0; i < size; i++)
       REAL(ans)[i] = x[i];
     UNPROTECT(1);  
   }

   return ans;

   }

/* ******************************************************************
   ****************************************************************** */

   SEXP createRIntScalar(int x) {

   SEXP ans;

   PROTECT(ans = allocVector(INTSXP, 1));
   INTEGER(ans)[0] = x;
   UNPROTECT(1);

   return ans;

   }

/* ******************************************************************
   ****************************************************************** */

   SEXP createRIntVector(int size,int* x) {

   SEXP ans;
   int i;

   if(x == NULL){
     PROTECT(ans = allocVector(INTSXP, 1));
     INTEGER(ans)[0] = 0;
     UNPROTECT(1);  
   }
   else{
     PROTECT(ans = allocVector(INTSXP, size));
     for (i = 0; i < size; i++)
       INTEGER(ans)[i] = x[i];
     UNPROTECT(1);  
   }

   return ans;

   }

/* ******************************************************************
   ****************************************************************** */

   void inip (int* n,double** x,double** l, double** u,int* m,
   double** lambda,int** equatn,int** linear) {

   int i;

   SEXP n_r,m_r,x_r,l_r,u_r,lambda_r,rho_r,equatn_r,linear_r;

   *n = 0;
   *m = 0;
   
   defineVar(install("x")     ,createRRealVector(*n,NULL),environment_r);
   defineVar(install("l")     ,createRRealVector(*n,NULL),environment_r);
   defineVar(install("u")     ,createRRealVector(*n,NULL),environment_r);
   defineVar(install("lambda"),createRRealVector(*m,NULL),environment_r);
   defineVar(install("equatn"),createRIntVector(*m,NULL) ,environment_r);
   defineVar(install("linear"),createRIntVector(*m,NULL) ,environment_r);

   EVAL(inip_r);

   n_r      = findVar(install("n"),     environment_r);
   x_r      = findVar(install("x"),     environment_r);
   m_r      = findVar(install("m"),     environment_r);
   l_r      = findVar(install("l"),     environment_r);
   u_r      = findVar(install("u"),     environment_r);
   lambda_r = findVar(install("lambda"),environment_r);
   equatn_r = findVar(install("equatn"),environment_r);
   linear_r = findVar(install("linear"),environment_r);

   *n = (INTEGER(AS_INTEGER(EVAL(n_r))))[0];
   *m = (INTEGER(AS_INTEGER(EVAL(m_r))))[0];

   *x      = (double *) malloc(*n * sizeof(double));
   *l      = (double *) malloc(*n * sizeof(double));
   *u      = (double *) malloc(*n * sizeof(double));
   *lambda = (double *) malloc(*m * sizeof(double));
   *equatn = (int    *) malloc(*m * sizeof(int   ));
   *linear = (int    *) malloc(*m * sizeof(int   ));

   for(i = 0; i < *n; i++) {
     (*x)[i] = (REAL(EVAL(x_r)))[i];
     (*l)[i] = (REAL(EVAL(l_r)))[i];
     (*u)[i] = (REAL(EVAL(u_r)))[i];
   }

   for(i = 0; i < *m; i++) {
     (*lambda)[i] = (REAL(EVAL(lambda_r)))[i];
     (*equatn)[i] = (INTEGER(AS_INTEGER(EVAL(equatn_r))))[i];
     (*linear)[i] = (INTEGER(AS_INTEGER(EVAL(linear_r))))[i];
   }

   }

/* ******************************************************************
   ****************************************************************** */

   void evalf(int n,double* x,double* f,int* flag) {

   SEXP flag_r,f_r;
        
   defineVar(install("n"),createRIntScalar(n)   ,environment_r);
   defineVar(install("x"),createRRealVector(n,x),environment_r);

   EVAL(evalf_r);
   
   f_r    = findVar(install("f"),   environment_r);
   flag_r = findVar(install("flag"),environment_r);
  
   *f    = (REAL(EVAL(f_r)))[0];
   *flag = (INTEGER(AS_INTEGER(EVAL(flag_r))))[0];

   }

/* ******************************************************************
   ****************************************************************** */

   void evalg(int n,double* x,double* g,int* flag) {

   int i;
   SEXP flag_r,g_r;

   defineVar(install("n"),createRIntScalar(n)      ,environment_r);
   defineVar(install("x"),createRRealVector(n,x)   ,environment_r);
   defineVar(install("g"),createRRealVector(n,NULL),environment_r);

   EVAL(evalg_r);
   
   g_r    = findVar(install("g"),   environment_r);
   flag_r = findVar(install("flag"),environment_r);
  
   for (i = 0; i < n; i++)
     g[i] = (REAL(EVAL(g_r)))[i];

   *flag = (INTEGER(AS_INTEGER(EVAL(flag_r))))[0];

   }

/* ******************************************************************
   ****************************************************************** */

   void evalh(int n,double* x,int* hlin,int* hcol,double* hval,int* nnzh,
   int* flag) {

   int i;
   double zero[1] = {0};
   SEXP hlin_r,hcol_r,hval_r,nnzh_r,flag_r;

   defineVar(install("n")   ,createRIntScalar(n)      ,environment_r);
   defineVar(install("x")   ,createRRealVector(n,x)   ,environment_r);
   defineVar(install("hlin"),createRIntVector(1,NULL) ,environment_r);
   defineVar(install("hcol"),createRIntVector(1,NULL) ,environment_r);
   defineVar(install("hval"),createRRealVector(1,NULL),environment_r);

   EVAL(evalh_r);
   
   nnzh_r = findVar(install("nnzh"),environment_r);
   flag_r = findVar(install("flag"),environment_r);
   hlin_r = findVar(install("hlin"),environment_r);
   hcol_r = findVar(install("hcol"),environment_r);
   hval_r = findVar(install("hval"),environment_r);
  
   *nnzh = (INTEGER(AS_INTEGER(EVAL(nnzh_r))))[0];

   for (i = 0; i < *nnzh; i++) {
     hlin[i] = (INTEGER(AS_INTEGER(EVAL(hlin_r))))[i];
     hcol[i] = (INTEGER(AS_INTEGER(EVAL(hcol_r))))[i];
     hval[i] = (REAL(EVAL(hval_r)))[i];
   }

   *flag = (INTEGER(AS_INTEGER(EVAL(flag_r))))[0];

   }

/* ******************************************************************
   ****************************************************************** */

   void evalc(int n,double* x,int ind,double* c,int* flag) {

   SEXP flag_r,c_r;

   defineVar(install("n")  ,createRIntScalar(n)   ,environment_r);
   defineVar(install("x")  ,createRRealVector(n,x),environment_r);
   defineVar(install("ind"),createRIntScalar(ind) ,environment_r);

   EVAL(evalc_r);
   
   c_r    = findVar(install("c"),   environment_r);
   flag_r = findVar(install("flag"),environment_r);
  
   *c    = (REAL(EVAL(c_r)))[0];
   *flag = (INTEGER(AS_INTEGER(EVAL(flag_r))))[0];

   }

/* ******************************************************************
   ****************************************************************** */

   void evaljac(int n,double* x,int ind,int* indjac,double* valjac,int* nnzjac, 
   int* flag) {

   int i;
   double zero[1] = {0};
   SEXP flag_r,indjac_r,valjac_r,nnzjac_r;

   defineVar(install("n")     ,createRIntScalar(n)      ,environment_r);
   defineVar(install("x")     ,createRRealVector(n,x)   ,environment_r);
   defineVar(install("ind")   ,createRIntScalar(ind)    ,environment_r);
   defineVar(install("indjac"),createRIntVector(1,NULL) ,environment_r);
   defineVar(install("valjac"),createRRealVector(1,NULL),environment_r);

   EVAL(evaljac_r);
   
   nnzjac_r = findVar(install("nnzjac"),environment_r);
   indjac_r = findVar(install("indjac"),environment_r);
   valjac_r = findVar(install("valjac"),environment_r);
   flag_r   = findVar(install("flag")  ,environment_r);
  
   *nnzjac = (INTEGER(AS_INTEGER(EVAL(nnzjac_r))))[0];

   for (i = 0; i < *nnzjac; i++) {
     indjac[i] = (INTEGER(AS_INTEGER(EVAL(indjac_r))))[i];
     valjac[i] = (REAL(EVAL(valjac_r)))[i];
   }

   *flag = (INTEGER(AS_INTEGER(EVAL(flag_r))))[0];

   }

/* ******************************************************************
   ****************************************************************** */

   void evalhc(int n,double* x,int ind,int* hclin,int* hccol,double* hcval,
   int* nnzhc,int* flag) {

   int i;
   double zero[1] = {0};
   
   SEXP hclin_r,hccol_r,hcval_r,nnzhc_r,flag_r;

   defineVar(install("n")    ,createRIntScalar(n)      ,environment_r);
   defineVar(install("x")    ,createRRealVector(n,x)   ,environment_r);
   defineVar(install("ind")  ,createRIntScalar(ind)    ,environment_r);
   defineVar(install("hclin"),createRIntVector(1,NULL) ,environment_r);
   defineVar(install("hccol"),createRIntVector(1,NULL) ,environment_r);
   defineVar(install("hcval"),createRRealVector(1,NULL),environment_r);

   EVAL(evalhc_r);
   
   nnzhc_r = findVar(install("nnzhc"),environment_r);
   hclin_r = findVar(install("hclin"),environment_r);
   hccol_r = findVar(install("hccol"),environment_r);
   hcval_r = findVar(install("hcval"),environment_r);
   flag_r  = findVar(install("flag") ,environment_r);

   *nnzhc = (INTEGER(AS_INTEGER(EVAL(nnzhc_r))))[0];

   for (i = 0; i < *nnzhc; i++) {
     hclin[i] = (INTEGER(AS_INTEGER(EVAL(hclin_r))))[i];
     hccol[i] = (INTEGER(AS_INTEGER(EVAL(hccol_r))))[i];;
     hcval[i] = (REAL(EVAL(hcval_r)))[i];;
   }

   *flag = (INTEGER(AS_INTEGER(EVAL(flag_r))))[0];

   }

/* ******************************************************************
   ****************************************************************** */

   void evalhlp(int n,double *x,int m,double *lambda,double *p,
		double *hp,int *goth,int *flag) {

   int i;
   SEXP hp_r,goth_r,flag_r;

   defineVar(install("n")     ,createRIntScalar(n)        ,environment_r);
   defineVar(install("x")     ,createRRealVector(n,x)     ,environment_r);
   defineVar(install("m")     ,createRIntScalar(m)        ,environment_r);
   defineVar(install("lambda"),createRRealVector(m,lambda),environment_r);
   defineVar(install("p")     ,createRRealVector(n,p)     ,environment_r);
   defineVar(install("hp")    ,createRRealVector(n,hp)    ,environment_r);
   defineVar(install("goth")  ,createRIntScalar(*goth)    ,environment_r);

   EVAL(evalhlp_r);

   hp_r   = findVar(install("hp") ,environment_r); 
   goth_r = findVar(install("goth") ,environment_r); 
   flag_r = findVar(install("flag") ,environment_r); 

   for (i = 0; i < n; i++)
     hp[i] = (REAL(EVAL(hp_r)))[i];

   *goth  = (INTEGER(AS_INTEGER(EVAL(goth_r))))[0];
   *flag  = (INTEGER(AS_INTEGER(EVAL(flag_r))))[0];
   
   }

/* ******************************************************************
****************************************************************** */

   void endp (int n,double* x,double* l,double* u,int m,double* lambda,
   int* equatn, int* linear) {

   defineVar(install("n")     ,createRIntScalar(n)        ,environment_r);
   defineVar(install("x")     ,createRRealVector(n,x)     ,environment_r);
   defineVar(install("l")     ,createRRealVector(n,l)     ,environment_r);
   defineVar(install("u")     ,createRRealVector(n,u)     ,environment_r);
   defineVar(install("m")     ,createRIntScalar(m)        ,environment_r);
   defineVar(install("lambda"),createRRealVector(m,lambda),environment_r);
   defineVar(install("equatn"),createRIntVector(m,equatn) ,environment_r);
   defineVar(install("linear"),createRIntVector(m,linear) ,environment_r);

   EVAL(endp_r);

   }

/* ******************************************************************
   ****************************************************************** */

   void param(int *rhoauto,int *rhotype,double *rhomult,double *rhofrac,
   int m,double **rho,int *gtype,int *hptype,int *intype, char *precond,
   int *checkder,double *epsfeas,double *epsopt,int *maxoutit,int *maxtotit,
   int *maxtotfc,int *iprint,int *ncomp) {

   int i;

   SEXP rhoauto_r,rhotype_r,rhomult_r,rhofrac_r,rho_r,gtype_r,
        hptype_r,intype_r,precond_r,checkder_r,epsfeas_r,epsopt_r, 
        maxoutit_r,maxtotit_r, maxtotfc_r,iprint_r,ncomp_r;

   defineVar(install("hptype")    ,createRIntScalar(-1)     ,environment_r);
   defineVar(install("m")         ,createRIntScalar(m)      ,environment_r);
   defineVar(install("rho")       ,createRRealVector(m,NULL),environment_r);

   EVAL(param_r);
   rhoauto_r  =  findVar(install("rhoauto") ,environment_r);
   rhotype_r  =  findVar(install("rhotype") ,environment_r);
   rhomult_r  =  findVar(install("rhomult") ,environment_r);
   rhofrac_r  =  findVar(install("rhofrac") ,environment_r);
   rho_r      =  findVar(install("rho")     ,environment_r);
   gtype_r    =  findVar(install("gtype")   ,environment_r);
   hptype_r   =  findVar(install("hptype")  ,environment_r);
   intype_r   =  findVar(install("intype")  ,environment_r);
   precond_r  =  findVar(install("precond") ,environment_r);
   checkder_r =  findVar(install("checkder"),environment_r);
   epsfeas_r  =  findVar(install("epsfeas") ,environment_r);
   epsopt_r   =  findVar(install("epsopt")  ,environment_r);
   maxoutit_r =  findVar(install("maxoutit"),environment_r);
   maxtotit_r =  findVar(install("maxtotit"),environment_r);
   maxtotfc_r =  findVar(install("maxtotfc"),environment_r);
   iprint_r   =  findVar(install("iprint")  ,environment_r);
   ncomp_r    =  findVar(install("ncomp")   ,environment_r);

   *rhoauto = (INTEGER(AS_INTEGER(EVAL(rhoauto_r))))[0];
   *rhotype = (INTEGER(AS_INTEGER(EVAL(rhotype_r))))[0];
   *rhomult = (REAL(EVAL(rhomult_r)))[0];
   *rhofrac = (REAL(EVAL(rhofrac_r)))[0];
   *rho    = (double *) malloc(m * sizeof(double));
   for(i = 0; i < m; i++) {
     (*rho)[i]    = (REAL(EVAL(rho_r)))[i];
   }
   *gtype    = (INTEGER(AS_INTEGER(EVAL(gtype_r))))[0];
   *hptype   = (INTEGER(AS_INTEGER(EVAL(hptype_r))))[0];
   strcpy(precond,CHAR(STRING_ELT(precond_r,0)));
   *checkder = (INTEGER(AS_INTEGER(EVAL(checkder_r))))[0];
   *epsfeas  = (REAL(EVAL(epsfeas_r)))[0];
   *epsopt   = (REAL(EVAL(epsopt_r)))[0];
   *maxoutit = (INTEGER(AS_INTEGER(EVAL(maxoutit_r))))[0];
   *maxtotit = (INTEGER(AS_INTEGER(EVAL(maxtotit_r))))[0];
   *maxtotfc = (INTEGER(AS_INTEGER(EVAL(maxtotfc_r))))[0];
   *iprint   = (INTEGER(AS_INTEGER(EVAL(iprint_r))))[0];
   *ncomp    = (INTEGER(AS_INTEGER(EVAL(ncomp_r))))[0];



   }

/* ******************************************************************
   ****************************************************************** */

   void allocWorkspace(int n,int m,int** wi1,double** wd1,double** wd2,
   double** wd3,double** wd4,double** wd5,double** wd6,double** wd7,
   double** wd8,double** wd9,double** wd10,double** wd11,double** wd12,
   double** wd13,double** wd14,double** wd15,double** wd16,double** wd17,
   double** wd18,double** wd19) {

   *wi1  = (int    *) malloc(n * sizeof(int   ));
   *wd1  = (double *) malloc(m * sizeof(double));
   *wd2  = (double *) malloc(m * sizeof(double));
   *wd3  = (double *) malloc(n * sizeof(double));
   *wd4  = (double *) malloc(m * sizeof(double));
   *wd5  = (double *) malloc(m * sizeof(double));
   *wd6  = (double *) malloc(n * sizeof(double));
   *wd7  = (double *) malloc(n * sizeof(double));
   *wd8  = (double *) malloc(n * sizeof(double));
   *wd9  = (double *) malloc(n * sizeof(double));
   *wd10 = (double *) malloc(n * sizeof(double));
   *wd11 = (double *) malloc(n * sizeof(double));
   *wd12 = (double *) malloc(n * sizeof(double));
   *wd13 = (double *) malloc(n * sizeof(double));
   *wd14 = (double *) malloc(n * sizeof(double));
   *wd15 = (double *) malloc(n * sizeof(double));
   *wd16 = (double *) malloc(n * sizeof(double));
   *wd17 = (double *) malloc(n * sizeof(double));
   *wd18 = (double *) malloc(n * sizeof(double));
   *wd19 = (double *) malloc(n * sizeof(double));

   }

/* ******************************************************************
   ****************************************************************** */
   void memFree(int *wi1,double *wd1,double *wd2,double *wd3,double *wd4, 
   double *wd5,double *wd6,double *wd7,double *wd8,double *wd9,double *wd10,
   double *wd11,double *wd12,double *wd13,double *wd14,double *wd15,double *wd16,
   double *wd17,double *wd18,double *wd19){


   free(wi1   );
   free(wd1   );
   free(wd2   );
   free(wd3   );
   free(wd4   );
   free(wd5   );
   free(wd6   );
   free(wd7   );
   free(wd8   );
   free(wd9   );
   free(wd10  );
   free(wd11  );
   free(wd12  );
   free(wd13  );
   free(wd14  );
   free(wd15  );
   free(wd16  );
   free(wd17  );
   free(wd18  );
   free(wd19  );

   }

/* ******************************************************************
   ****************************************************************** */

   SEXP algencan(SEXP evalf_ptr,SEXP evalg_ptr,SEXP evalh_ptr,
   SEXP evalc_ptr,SEXP evaljac_ptr,SEXP evalhc_ptr,SEXP evalhlp_ptr,
   SEXP inip_ptr, SEXP endp_ptr,SEXP param_ptr,
   SEXP environment_ptr) {

   int checkder,gtype,hptype,inform,intype,iprint,m,maxoutit,maxtotfc,
       maxtotit,n,ncomp,outiter,rhoauto,rhotype,totcgcnt,totfcnt,
       totgcnt,totiter;
   double f,nalpsupn,epsfeas,epsopt,rhofrac,rhomult,snorm,time;

   char precond[7];
   int *equatn,*linear,*wi1;
   double *l,*lambda,*rho,*u,*wd1,*wd2,*wd3,*wd4,*wd5,*wd6,*wd7,*wd8, 
       *wd9,*wd10,*wd11,*wd12,*wd13,*wd14,*wd15,*wd16,*wd17,*wd18,
       *wd19,*x;
   SEXP return_value;

   evalf_r       = evalf_ptr;
   evalg_r       = evalg_ptr;
   evalh_r       = evalh_ptr;
   evalc_r       = evalc_ptr;
   evaljac_r     = evaljac_ptr;
   evalhc_r      = evalhc_ptr;
   evalhlp_r     = evalhlp_ptr;
   inip_r        = inip_ptr;
   endp_r        = endp_ptr;
   param_r       = param_ptr;
   environment_r = environment_ptr;

   inip(&n,&x,&l,&u,&m,&lambda,&equatn,&linear);

   allocWorkspace(n,m,&wi1,&wd1,&wd2,&wd3,&wd4,&wd5,&wd6,&wd7,&wd8,&wd9,
   &wd10,&wd11,&wd12,&wd13,&wd14,&wd15,&wd16,&wd17,&wd18,&wd19);
  
   param(&rhoauto,&rhotype,&rhomult,&rhofrac,m,&rho,&gtype,&hptype,
   &intype,precond,&checkder,&epsfeas,&epsopt,&maxoutit,&maxtotit,
   &maxtotfc,&iprint,&ncomp);
   
   C2FLOGICALV(equatn,m);
   C2FLOGICALV(linear,m);

   Solver(n,x,l,u,m,lambda,equatn,linear,rhoauto,rhotype,rhomult,rhofrac,rho,
   gtype,hptype,intype,precond,checkder,epsfeas,epsopt,maxoutit,maxtotit,
   maxtotfc,iprint,ncomp,f,snorm,nalpsupn,outiter,totiter,totfcnt,
   totgcnt,totcgcnt,time,inform,wi1,wd1,wd2,wd3,wd4,wd5,wd6,wd7,wd8,wd9,
   wd10,wd11,wd12,wd13,wd14,wd15,wd16,wd17,wd18,wd19);

   endp(n,x,l,u,m,lambda,equatn,linear);

   memFree(wi1,wd1,wd2,wd3,wd4,wd5,wd6,wd7,wd8,wd9,wd10,wd11,wd12,wd13,wd14,
   wd15,wd16,wd17,wd18,wd19);

   defineVar(install("AlgencanReturnValue"),createRIntScalar(0),
   environment_r);
   defineVar(install("ncomp"),createRIntScalar(ncomp),environment_r);
   defineVar(install("f"),createRRealScalar(f),environment_r);
   defineVar(install("snorm"),createRRealScalar(snorm),environment_r);
   defineVar(install("nalpsupn"),createRRealScalar(nalpsupn),environment_r);
   defineVar(install("outiter"),createRIntScalar(outiter),environment_r);
   defineVar(install("totiter"),createRIntScalar(totiter),environment_r);
   defineVar(install("totfcnt"),createRIntScalar(totfcnt),environment_r);
   defineVar(install("totgcnt"),createRIntScalar(totgcnt),environment_r);
   defineVar(install("totcgcnt"),createRIntScalar(totcgcnt),environment_r);
   defineVar(install("time"),createRRealScalar(time),environment_r);
   defineVar(install("inform"),createRIntScalar(inform),environment_r);

   return_value = findVar(install("AlgencanReturnValue"),environment_r);

   return return_value;

   }