#include "cfortran.h"
#include <R.h>
#include <Rdefines.h>

SEXP evalf_r,evalg_r,evalh_r,evalc_r,evaljac_r,evalhc_r,evalhlp_r,
     inip_r,endp_r,param_r,environment_r;

void inip(int* n,double** x,double** l,double** u,int* m,double** lambda, 
     int** equatn,int** linear);

void evalf(int n,double* x,double* f,int* flag);

void evalg(int n,double* x,double* g,int* flag);

void evalh(int n,double* x,int* hlin,int* hcol,double* hval,int* nnzh,
     int* flag);

void evalc(int n,double* x,int ind,double* c,int* flag);

void evaljac(int n,double* x,int ind,int* indjac,double* valjac,int* nnzjac,
     int* flag);

void evalhc(int n,double* x,int ind,int* hclin,int* hccol,double* hcval,
     int* nnzhc,int* flag);

void evalhlp(int n,double *x,int m,double *lambda,double *p,
	     double *hp,int *goth,int *flag);

void endp(int n,double* x,double* l,double* u,int m,double* lambda,
     int* equatn,int* linear);

void param(int *rhoauto,int *rhotype,double *rhomult,double *rhofrac,
     int m,double **rho,int* gtype,int* hptype,int* intype,char* precond,
     int* checkder,double* epsfeas,double* epsopt,int* maxoutit,int* maxtotit,
     int* maxtotfc,int* iprint,int* ncomp);

void allocWorkspace(int n,int m,int **wi1,double **wd1,double **wd2,
     double **wd3,double **wd4,double **wd5,double **wd6,double **wd7,double **wd8,
     double **wd9,double **wd10,double **wd11,double **wd12,double **wd13,
     double **wd14,double **wd15,double **wd16,double **wd17,double **wd18,
     double **wd19);

void memFree(int *wi1,double *wd1,double *wd2,double *wd3,double *wd4, 
     double *wd5,double *wd6,double *wd7,double *wd8,double *wd9,double *wd10,
     double *wd11,double *wd12,double *wd13,double *wd14,double *wd15,double *wd16,
     double *wd17,double *wd18,double *wd19);

FCALLSCSUB4(evalf,EVALF,evalf,INT,DOUBLEV,PDOUBLE,PINT)
     
FCALLSCSUB4(evalg,EVALG,evalg,INT,DOUBLEV,DOUBLEV,PINT)

FCALLSCSUB5(evalc,EVALC,evalc,INT,DOUBLEV,INT,PDOUBLE,PINT)

FCALLSCSUB7(evaljac,EVALJAC,evaljac,INT,DOUBLEV,INT,INTV,DOUBLEV,INTV,PINT)

FCALLSCSUB7(evalh,EVALH,evalh,INT,DOUBLEV,INTV,INTV,DOUBLEV,PINT,PINT)

FCALLSCSUB8(evalhc,EVALHC,evalhc,INT,DOUBLEV,INT,INTV,INTV,DOUBLEV,PINT,PINT)

FCALLSCSUB8(evalhlp,EVALHLP,evalhlp,INT,DOUBLEV,INT,DOUBLEV,DOUBLEV,DOUBLEV,\
PLOGICAL,PINT)

PROTOCCALLSFSUB55(SOLVER,solver,INT,DOUBLEV,DOUBLEV,DOUBLEV,INT,DOUBLEV,LOGICALV,\
LOGICALV,LOGICAL,INT,DOUBLE,DOUBLE,DOUBLEV,INT,INT,INT,STRING,LOGICAL,DOUBLE,\
DOUBLE,INT,INT,INT,INT,INT,PDOUBLE,PDOUBLE,PDOUBLE,PINT,PINT,PINT,PINT,PINT,\
PDOUBLE,PINT,INTV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,\
DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,\
DOUBLEV,DOUBLEV,DOUBLEV)

#define Solver(n,x,l,u,m,lambda,equatn,linear,rhoauto,rhotype,rhomult,\
rhofrac,rho,gtype,hptype,intype,precond,checkder,epsfeas,epsopt,maxoutit,\
maxtotit,maxtotfc,iprint,ncomp,f,snorm,nalpsupn,outiter,totiter,totfcnt,\
totgcnt,totcgcnt,time,inform,wi1,wd1,wd2,wd3,wd4,wd5,wd6,wd7,wd8,wd9,wd10,\
wd11,wd12,wd13,wd14,wd15,wd16,wd17,wd18,wd19) \
CCALLSFSUB55(SOLVER,solver,INT,DOUBLEV,DOUBLEV,DOUBLEV,INT,DOUBLEV,LOGICALV,\
LOGICALV,LOGICAL,INT,DOUBLE,DOUBLE,DOUBLEV,INT,INT,INT,STRING,LOGICAL,DOUBLE,\
DOUBLE,INT,INT,INT,INT,INT,PDOUBLE,PDOUBLE,PDOUBLE,PINT,PINT,PINT,PINT,PINT,\
PDOUBLE,PINT,INTV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,\
DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLEV,\
DOUBLEV,DOUBLEV,DOUBLEV,\
n,x,l,u,m,lambda,equatn,linear,rhoauto,rhotype,rhomult,rhofrac,rho,gtype,\
hptype,intype,precond,checkder,epsfeas,epsopt,maxoutit,maxtotit,maxtotfc,\
iprint,ncomp,f,snorm,nalpsupn,outiter,totiter,totfcnt,totgcnt,totcgcnt,time,\
inform,wi1,wd1,wd2,wd3,wd4,wd5,wd6,wd7,wd8,wd9,wd10,wd11,wd12,wd13,wd14,wd15,\
wd16,wd17,wd18,wd19)

SEXP createRRealScalar(double x);

SEXP createRRealVector(int size, double* x);

SEXP createRIntScalar(int x);

SEXP createRIntVector(int size, int* x);

SEXP algencan(SEXP evalf_ptr,SEXP evalg_ptr,SEXP evalh_ptr,SEXP evalc_ptr,
     SEXP evaljac_ptr,SEXP evalhc_ptr,SEXP evalhlp_ptr,SEXP inip_ptr,
     SEXP endp_ptr,SEXP param_ptr,SEXP environment_ptr);
