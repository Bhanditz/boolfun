#include "boolfun.h"

/*______________________________________________________________ A U X I L I A R Y     F U N C T I O N S */
int weight( int x ) {
    int res;
    for( res=0; x>0; x = x>>1 )
        res = res + x%2;
    return res;
}
int choose( int n, int k ) {
    int i, num = 1, den = 1;
    if( k<0 || k>n ) return 0;
    for( i=0; i<k; ++i ) {
        num *= n--;
        den *= (k-i); }
    return (num/den);
}
int preceq( int a, int b ) { /* returns true if a \preceq b, false otherwise */
    int res = 1;
    while( (a>0 || b>0) && (res==1) ) {
        if( a%2 > b%2 ) res = 0;
        a >>= 1; b >>= 1; }
    return res;
}
int* sort_increasing_deg( int* v, int len ) { 
    int i,j,tmp;
    for( i=0; i < len-1; ++i )
        for( j=i+1; j<len; ++j )
            if( weight(v[j]) < weight(v[i]) ) {
                tmp = v[j];
                v[j] = v[i];
                v[i] = tmp;
            }
    return v;
}
/*_____________________________________________________________________ A U X I L I A R Y    S T R U C T */
typedef struct {int _n, _m, **_v; } MAT;
/* 4 functions are defined for this struct, namely 'initialize_mat', 
'deallocate_mat', 'swap_columns' and 'add_line' */
MAT* initialize_mat( MAT* mat, int nrow, int ncol) {
    int i,j;
    mat = (MAT*) malloc (sizeof(MAT));
    mat->_n = nrow;
    mat->_m = ncol;
    mat->_v = (int**) malloc (nrow * sizeof(int*));
    for(i=0; i < nrow; ++i) {
        mat->_v[i] = (int*) malloc (ncol * sizeof(int));
        for(j=0; j < ncol; ++j)
            mat->_v[i][j] = 0;
    }
    return mat;
}
MAT* deallocate_mat( MAT* mat) {
    int i;
    if( mat != NULL ) {
        if(mat->_v != NULL) {
            for( i=0; i < mat->_n; ++i )
                free(mat->_v[i]);
            free(mat->_v);
        }
        free(mat);
    }
    return NULL;
}
MAT* swap_columns( MAT* mat, int a, int b ) {
    int* tmp,i;
    tmp = (int*) malloc ( mat->_n * sizeof(int) );
    for( i=0; i<mat->_n; ++i ) tmp[i] = mat->_v[i][a];
    for( i=0; i<mat->_n; ++i ) mat->_v[i][a] = mat->_v[i][b];
    for( i=0; i<mat->_n; ++i ) mat->_v[i][b] = tmp[i];
    free(tmp);
    return mat;
}
MAT* add_line( MAT* mat, int dst, int src ) {
    int j;
    for( j=0; j < mat->_m; ++j )
        mat->_v[dst][j] = (mat->_v[dst][j] + mat->_v[src][j] )%2;
    return mat;
}
/*__________________________________________________________ C O M P U T I N G    T H E    A N N I H I L A T O R    I M M U N I T Y */
int* get_monomials( int n, int d, int* res, int* N ) { 
    int i,k; /* returns all N monomials (n variables) of degree <= d */
    for( (*N)=0, k=0; k<=d; ++k )
        (*N) = (*N) + choose(n,k);
    res = (int*) malloc ((*N) * sizeof(int));
    for( k=0, i=0; i<(1<<n); ++i )
        if( weight(i) <= d )
           res[k++] = i;
    return res;
}
int* get_support( const int * tt, int n, int* res, int* N, int b ) { 
    int i,k,len;/* returns supp(b+f), b in {0,1} */
    len = 1<<n;
    for( (*N)=0, i=0; i<len; ++i )
        (*N) = (*N) + (tt[i] != b);
    res = (int*) malloc ((*N) * sizeof(int));
    for( k=0, i=0; i<len; ++i )
        if( tt[i] != b )
            res[k++] = i;
    return res;
}
MAT* get_matrix( const int * tt, int n, MAT* m, int* monomials, int Nm, int ai, int b ) { 
    int Ns, i, j, len, *support;/* returns matrix for b+f, b in {0,1} */
    len = 1<<n;
    support = NULL; 
    support = get_support( tt, n, support, &Ns, b );
    if (Ns == 0 || Ns == len) 
        m = NULL; /* constant function has algebraic immunity equal to 0 */
    else {
        m = (Nm>Ns)? initialize_mat(m,Nm,Nm): initialize_mat(m,Nm,Ns);
        for( i=0; i<Nm; ++i )
            for( j=0; j<Ns; ++j ) 
                m->_v[i][j] = preceq( monomials[i], support[j] ); 
    }
    free(support);
    return m;
}
int solve_matrix( MAT* m, int* monomials, int b ) {
    int i, j, l, res, *deg, processed_lines, zero_lines;
    deg = (int*) malloc (m->_n * sizeof(int));
    for( res = 0, i = 0; i < m->_n; ++i ) {
        deg[i] = weight(monomials[i]);
        if( deg[i] > res ) res = deg[i];
    }
    processed_lines = zero_lines = 0;
    for( i = 0; i < m->_n; ++i ) {
        for( j = 0; j < m->_m && m->_v[i][j] == 0; ++j );
        if( j == m->_m ) { 
            ++ zero_lines;
            if( deg[i] < res && deg[i]!=0 )
                res = deg[i];
        } else {
            ++ processed_lines;
            if( i!=j && i<m->_m && j<m->_m ) 
                m = swap_columns( m, i, j );
            for( l=i+1; l < m->_n && i < m->_m; ++l ) {
                if( i < m->_m && m->_v[l][i] != 0 ) {
                    m = add_line( m, l, i ); /* m[l,] <- m[l,] oplus m[i,] */
                    deg[l] = (deg[i] > deg[l])? deg[i] : deg[l];
                }
            }
        } /* else */
    }
    free (deg);
    return res;
}
/* ______________________________________________________________________________ I N T E R F A C E    T O     R */
SEXP algebraicImmunity(SEXP RTT, SEXP Rn) {
    /* D E C L     I N P U T */
    const int *TT, *n;

    /* D E C L     O U T P U T */
    int *res;
    SEXP Rres;

    /* D E C L     L O C A L */
    MAT *m0, *m1;
    int deg, *monomials, Nm; /* Nm is the length of monomials */
    int a, b; 

    /* I N I T     I N P U T */
    PROTECT(RTT = AS_INTEGER(RTT) );
    PROTECT(Rn  = AS_INTEGER(Rn)  );
    TT = INTEGER_POINTER(RTT);
    n  = INTEGER_POINTER(Rn);

    /* I N I T     O U T P U T */
    PROTECT(Rres = NEW_INTEGER(1)); 
    res = INTEGER_POINTER(Rres);

    /* I N I T     L O C A L */
    m0 = m1 = NULL;
    monomials = NULL;

    /* T H E   F U N C T I O N */
    deg = ((*n) >> 1) + ((*n) % 2);
    monomials = get_monomials( (*n), deg, monomials, &Nm );
    monomials = sort_increasing_deg( monomials, Nm );
    m0 = get_matrix( TT, (*n), m0, monomials, Nm, deg, 0 );
    if(m0==NULL) 
        (*res)=0; /* empty support ==> constant function */
    else {
        m1 = get_matrix( TT, (*n), m1, monomials, Nm, deg, 1 );
        a = solve_matrix( m0, monomials, 0 );
        b = solve_matrix( m1, monomials, 1 );
        (*res) = (a<b)? a : b;
    }

    /* G O O D B Y E */
    free(monomials);
    deallocate_mat(m0);
    deallocate_mat(m1);
    UNPROTECT(3);
    return Rres;
}

