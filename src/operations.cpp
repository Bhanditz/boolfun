#include "boolfun.h"

/* R package boolfun needs + and * defined for 
   multivariate polynomials with n variables in 
   GF(2) and (2^n) coefficients in GF(2).
   Those polynomials are represented by a vector
   of length 2^n holding the coefficient of each 
   monomial in the algebraic normal form. 
   The + and * operations use the following 
   functions where 'p' stands for polynomial, 
   'm' for monomial and the third argument is 
    the number of variables n. */

int* toBin  (int*,int,int);
int  toInt  (int*,int);
int* mul_mm (int*,int*,int);

/* D E F I N I T I O N S */
int* toBin(int* res, int x, int n) {
    int i;
    for( i=0; i<n; ++i ) {
        res[i] = x%2;
        x = x>>1;
    }
    return res;
}
int toInt(int* vec, int len) {
    int i, res;
    for( res=0, i=0; i<len; ++i )
        if( vec[i]!=0 )
            res = res + (1 << i);
    return res;
}
int* mul_mm (int* a, int* b, int n) { /* a <- a*b */
    int i;
    for(i=0; i < n; ++i) 
        a[i] = (a[i]!=0 || b[i]!=0)? 1 : 0;
    return a;
}

/* R   I N T E R F A C E */
SEXP add( SEXP Rp, SEXP Rq, SEXP Rn ) 
{
    /* D E C L     I N P U T */
    const int *p, *q, *n;

    /* D E C L     O U T P U T */
    int *res;
    SEXP Rres;

    /* D E C L     L O C A L */
    int i;

    /* I N I T     I N P U T */
    PROTECT(Rp = AS_INTEGER(Rp) );
    PROTECT(Rq = AS_INTEGER(Rq) );
    PROTECT(Rn = AS_INTEGER(Rn) );
    p = INTEGER_POINTER(Rp);
    q = INTEGER_POINTER(Rq);
    n = INTEGER_POINTER(Rn);

    /* I N I T     O U T P U T */
    PROTECT(Rres = NEW_INTEGER( (1<<(*n)) )); 
    res = INTEGER_POINTER(Rres);

    /* T H E   F U N C T I O N */
    for( i=0; i<(1<<(*n)); ++i ) 
        res[i] = ( p[i]+q[i] )%2;

    /* G O O D B Y E */
    UNPROTECT(4);
    return Rres;    
}

SEXP multiply( SEXP Rp, SEXP Rq, SEXP Rn ) {
    /* D E C L     I N P U T */
    const int *p, *q, *n;

    /* D E C L     O U T P U T */
    int *res;
    SEXP Rres;

    /* D E C L     L O C A L */
    int i, j, idx, *mon, *m, *tmp, *old;
    SEXP Rmon, Rm, Rtmp, Rold;

    /* I N I T     I N P U T */
    PROTECT(Rp = AS_INTEGER(Rp) );
    PROTECT(Rq = AS_INTEGER(Rq) );
    PROTECT(Rn = AS_INTEGER(Rn) );
    p = INTEGER_POINTER(Rp);
    q = INTEGER_POINTER(Rq);
    n = INTEGER_POINTER(Rn);

    /* I N I T     O U T P U T */
    PROTECT(Rres = NEW_INTEGER((1<<(*n)))); 
    res = INTEGER_POINTER(Rres);

    /* I N I T     L O C A L */
    PROTECT(Rm   = NEW_INTEGER((*n))); 
    PROTECT(Rmon = NEW_INTEGER((*n))); 
    PROTECT(Rtmp = NEW_INTEGER((1<<(*n))));
    PROTECT(Rold = NEW_INTEGER((1<<(*n)))); 
    m   = INTEGER_POINTER(Rm);
    mon = INTEGER_POINTER(Rmon);
    tmp = INTEGER_POINTER(Rtmp);
    old = INTEGER_POINTER(Rold);

    /* T H E   F U N C T I O N */
    for( i=0; i<(1<<(*n)); ++i ) 
        res[i] = 0;
    for( i=0; i<(1<<(*n)); ++i ) 
        if( p[i]!=0 ) { /* then multiply q with monomial i */
            mon = toBin(mon, i, (*n));
            for( j=0; j<(1<<(*n)); ++j ) {
                tmp[j] = 0;
                old[j] = res[j];
                res[j] = q[j];
            } 
            /* res = mul_pm( res, mon, (*n) ); (m <-> mon) */
            for(j=0; j<(1<<(*n)); ++j) 
              if(res[j]!=0) {
                m = toBin  (m, j, (*n)); /* get monomial j */
                m = mul_mm (m, mon, (*n)); /* m <- m * mon  */
                idx = toInt  (m, (*n));
                tmp[idx] = ( tmp[idx]+1 )%2; /* add m to tmp */
              }
            for(j=0; j<(1<<(*n)); ++j)
                res[j]=tmp[j];
            /* res = add_pp( res, old, (*n) ); */
            for(j=0; j<(1<<(*n)); ++j)
                res[j] = ( res[j]+old[j] )%2; 
        }

    /* G O O D B Y E */
    UNPROTECT(8);
    return Rres;    
}

