#include "boolfun.h"

SEXP degree( SEXP RANF, SEXP Rn )
{
    /* D E C L     I N P U T */
    const int *ANF, *n;

    /* D E C L     O U T P U T */
    int *res;
    SEXP Rres;

    /* D E C L     L O C A L */
    int i, tmp, weight;

    /* I N I T     I N P U T */
    PROTECT(RANF = AS_INTEGER(RANF) );
    PROTECT(Rn  = AS_INTEGER(Rn)  );
    ANF = INTEGER_POINTER(RANF);
    n  = INTEGER_POINTER(Rn);

    /* I N I T     O U T P U T */
    PROTECT(Rres = NEW_INTEGER(1)); 
    res = INTEGER_POINTER(Rres);

    /* T H E   F U N C T I O N */
    if( ANF[ (1<<(*n))-1 ] != 0 )
        (*res) = (*n);
    else 
        for( (*res)=0, i=1; i < ((1<<(*n))-1); ++i ) 
            if( ANF[i]!=0 ) {
                /* compute (hamming) weight */
                for( weight=0, tmp = i; tmp>0; tmp>>=1 )
                    weight = weight + tmp%2;
                if( weight > (*res) )
                    (*res) = weight; 
            }
    /* G O O D B Y E */
    UNPROTECT(3);
    return Rres;    
}

