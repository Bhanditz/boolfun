#include "boolfun.h"

SEXP walshTransform( SEXP RTT, SEXP Rn )
{
    /* D E C L     I N P U T */
    const int *TT, *n;

    /* D E C L     O U T P U T */
    int *res;
    SEXP Rres;

    /* D E C L     L O C A L */
    int i, j, m, halfm, t1, t2, r, a, b;

    /* I N I T     I N P U T */
    PROTECT(RTT = AS_INTEGER(RTT) );
    PROTECT(Rn  = AS_INTEGER(Rn)  );
    TT = INTEGER_POINTER(RTT);
    n  = INTEGER_POINTER(Rn);

    /* I N I T     O U T P U T */
    PROTECT(Rres = NEW_INTEGER(1 << (*n))); /* 2^n */
    res = INTEGER_POINTER(Rres);
    for( i=0; i < (1<<(*n)); ++i ) 
        res[i] = (TT[i]==0)? 1 : -1;

    /* T H E   F U N C T I O N */
    for( i = 1; i <= (*n); ++i ) {
        m  = (1 << i);
        halfm = m/2;
        for( r=0; r < (1<<(*n)); r += m ) {
            t1 = r;
            t2 = r + halfm;
            for( j=0; j < halfm; ++j, ++t1, ++t2 ) {
                a = res[t1];
                b = res[t2];
                res[t1] = a + b;
                res[t2] = a - b;
            }
        }
    }

    /* G O O D B Y E */
    UNPROTECT(3);
    return Rres;    
}

