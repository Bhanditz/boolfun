\name{utils}
\alias{weight}
\alias{modulo}
\alias{mod}
\alias{toBin}
\alias{tobin}
\title{Some Auxiliary Functions}

\description{
  Some functions used by the \code{\link{BooleanFunction}} object that might be useful for other purposes.
}
\usage{
toBin(a,n)
modulo(a,n)
weight(x)
}
\arguments{
    \item{a}{an integer.}
    \item{n}{an integer.}
    \item{x}{an integer or a vector of integers. If \code{x} is a vector the function will be applied to each of its components.}
}
\value{
  \code{toBin}  returns a binary representation of \code{x} as a vector of \code{n} integers. Note that the binary representation is reversed, that is, \code{toBin(8,4)} returns \code{(0,0,0,1)} instead of \code{(1,0,0,0)}.\cr
  \code{modulo} returns \code{a} mod \code{n}.\cr
  \code{weight} returns the hamming weight of the binary representation of \code{x}. If \code{x} is a vector, the hamming weight of each of its components is returned in a vector.
}
\author{
  F.Lafitte
}
\seealso{ \code{\link{BooleanFunction}} }

\examples{
    powers <- c( 2, 4, 8, 16, 32,  64 )
    if( any( weight(powers) != 1 ) )
        stop("This message should not print")
    for( i in 0:(2^10 -1) )
        if( sum(toBin(i,10)) != weight(i) )
            stop("This message should not print")
}

\keyword{misc}

