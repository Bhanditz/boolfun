#include "boolfun.h"

SEXP mobiusTransform( SEXP RTT, SEXP Rn )
{
    /* D E C L     I N P U T */
    const int *TT, *n;

    /* D E C L     O U T P U T */
    int *res;
    SEXP Rres;

    /* D E C L     L O C A L */
    int *t, *u, i, j, k;
    SEXP Rt, Ru;

    /* I N I T     I N P U T */
    PROTECT(RTT = AS_INTEGER(RTT) );
    PROTECT(Rn  = AS_INTEGER(Rn)  );
    TT = INTEGER_POINTER(RTT);
    n  = INTEGER_POINTER(Rn);

    /* I N I T     O U T P U T */
    PROTECT(Rres = NEW_INTEGER(1 << (*n))); /* 2^n */
    res = INTEGER_POINTER(Rres);

    /* I N I T     L O C A L */
    PROTECT(Rt = NEW_INTEGER( (1<<(*n)) >> 1 )); /*0.5*2^n*/
    PROTECT(Ru = NEW_INTEGER( (1<<(*n)) >> 1 ));
    u = INTEGER_POINTER(Ru);
    t = INTEGER_POINTER(Rt);

    /* T H E   F U N C T I O N */
    for(i=0; i < (1<<(*n)); ++i)
        res[i] = TT[i];
    for( i=0; i < ((1<<(*n))>> 1); ++i )
        u[i]=t[i]=0;
    for( i=0; i < (*n); ++i ) {
        for( j=0; j < ( (1<<(*n))>>1 ); ++j ) {
            t[j] = res[2*j];
            u[j] = (res[2*j]==res[2*j+1])? 0 : 1; }
        for( k=0; k<((1<<(*n)) >> 1 ); ++k ) {
            res[k] = t[k];
            res[((1<<(*n)) >> 1 ) + k] = u[k]; }
    }

    /* G O O D B Y E */
    UNPROTECT(5);
    return Rres;    
}

