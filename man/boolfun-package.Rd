
\name{boolfun-package}
\Rdversion{1.1}
\alias{boolfun-package}
\alias{boolfun}
\docType{package}

\title{
Cryptographic Boolean Functions
}

\description{
This package can be used to assess cryptographic properties of Boolean functions such as nonlinearity, algebraic immunity, resiliency, etc... It also implements functionality to handle Boolean polynomials, namely multiplication and addition of multivariate polynomials with coefficients and exponents in \{0,1\}. 
}

\details{
\tabular{ll}{
Package: \tab boolfun\cr
Version: \tab 0.2.6\cr
Date: \tab 2009\cr
Depends: \tab R (>= 2.3.0), R.oo\cr
Suggests: \tab RUnit \cr
License: \tab GPL (>= 3)\cr
Built: \tab R 2.9.1; i686-pc-linux-gnu; 2009-12-10 13:57:37 UTC; unix\cr
}

Index:
\preformatted{
                        Mobius Inversion
                        Fast Walsh Hadamard Transform
boolfun-package         Cryptographic Boolean Functions
}

Further information is available in the following vignettes:
\tabular{ll}{
\code{boolfun}    \tab Cryptographic Properties of Boolean functions (source, pdf)\cr
\code{polynomial} \tab Handling Boolean Polynomials with the boolfun Package (source, pdf)\cr
}
See the examples below for an overview of how to use the package.
}
\author{
F.Lafitte

Maintainer: Frederic Lafitte <frederic.lafitte@rma.ac.be>

}

\keyword{package}
\keyword{math}
\keyword{logic}

\seealso{
    \code{\link{BooleanFunction}}, \code{\link{Polynomial}}, \code{\link{Assignment}}, \code{\link[R.oo:R.oo-package]{R.oo}}
}

\examples{

    # Functions are defined by their truth tables (string or integer vector).
    f <- BooleanFunction( "00100111" )
    g <- BooleanFunction(c(0,1,1,0,1,0,0,1))
    h <- BooleanFunction( c(tt(f), tt(g)) ) # concatenation

    # You can print information on the function as follows.
    print ( h )     # Prints "Boolean function with 4 variables.".
    print ( tt(h) ) # Prints the truth table.

    # Note that the methods can be called 'object$method()' or 'method(object)': 
    print(paste(  "f has (deg,ai,nl,res) = (", f$deg(),f$ai(),f$nl(),f$res(),")"  ))
    print(paste(  "h has (deg,ai,nl,res) = (", deg(h), ai(h), nl(h), res(h), ")"  ))

    # Random Boolean functions   
    randomBFs <- c()
    data <- c( "degree", "algebraic immunity", "nonlinearity", "resiliency" )
    for( i in 1:500 ) {
        randomTT <- round(runif(2^5, 0,1))
        randomBF <- BooleanFunction(randomTT)
        data <- rbind( data, c(
                deg(randomBF), ai(randomBF),
                nl(randomBF), res(randomBF)))
    }
    print(data)
}


