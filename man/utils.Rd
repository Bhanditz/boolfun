\name{utils}
\alias{weight}
\alias{toBin}
\alias{tobin}
\alias{toInt}
\alias{toint}
\alias{strip}

\title{Some Auxiliary Functions}

\description{
  Some functions used by the package and that might be useful for other purposes.
}
\usage{
toBin(a,n)
toInt(v)
weight(x)
strip(string,chars)
}
\arguments{
    \item{a}{an integer.}
    \item{n}{an integer.}
    \item{v}{a vector of integers in \eqn{\{0,1\}}{\{0,1\}}.}
    \item{x}{an integer or a vector of integers. If \code{x} is a vector the function will be applied to each of its components.}
    \item{string}{a string (e.g. \code{"hello"}) }
    \item{chars}{a vector of characters (e.g. \code{c("h","e","l","l","o")})}
}
\value{
  \code{toBin}  returns a binary representation of \code{x} as a vector of \code{n} integers. Note that the binary representation is reversed, that is, \code{toBin(8,4)} returns \code{(0,0,0,1)} instead of \code{(1,0,0,0)}.\cr
  \code{toInt}  returns the integer encoded in binary vector \code{v}. \cr
  \code{weight} returns the hamming weight of the binary representation of \code{x}. If \code{x} is a vector, the hamming weight of each of its components is returned in a vector.\cr
  \code{strip}  returns the input argument \code{string} stripped from the input characters \code{chars}. For example, \code{strip( "hello", c("o","e") )} returns "hll".  
}
\author{
  F.Lafitte
}
\seealso{ 
  \code{\link{BooleanFunction}},   \code{\link{Polynomial}} ,  \code{\link{Assignment}} 
}

\examples{
    powers <- c( 2, 4, 8, 16, 32,  64 )
    if( any( weight(powers) != 1 ) )
        stop("This message should not print")
    for( i in 0:(2^10 -1) )
        if( sum(toBin(i,10)) != weight(i) )
            stop("This message should not print")
}

\keyword{misc}

