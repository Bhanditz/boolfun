\name{Assignment}
\alias{Assignment}
\alias{int.Assignment}
\alias{len.Assignment}
\alias{weight.Assignment}
\alias{print.Assignment} 
\alias{string.Assignment}
\alias{equals.Assignment}
\alias{int}
\alias{len}
\title{Input to a Boolean function}

\description{
    Implements an element of \eqn{\{0,1\}^n}{{0,1}^n} that can be used as input to a \code{BooleanFunction} or \code{Polynomial} instance. 
}
\usage{
    x <- Assignment( initializer )
}
\arguments{
    \item{initializer}{a vector containing \eqn{2^n}{2^n} integers in \eqn{\{0,1\}}{\{0,1\}} or a string holding \eqn{2^n}{2^n} characters in \eqn{\{'0','1'\}}{\{'0','1'\}}.}
}
\value{
    The returned value \code{f} is an S3 object which is defined using the R.oo package. Methods of the returned value, say \code{weight()}, can be accessed in two ways using \code{x$weight()} or \code{weight(x)}. 
}

\section{Public Methods}{

    \tabular{rll}{
    \tab \code{int()}       \tab returns the integer encoded by the element of \eqn{\{0,1\}^n}{\{0,1\}^n}.  \cr 
    \tab                    \tab Note: \code{"100"} encodes the integer 1 instead of 4.                     \cr
    \tab \code{val()}       \tab returns a vector of \eqn{n}{n} integer holding the value of each variable. \cr 
    \tab \code{len()}       \tab returns the number of variables (\eqn{n}{n}). \cr 
    \tab \code{string()}    \tab returns a string representation of the corresponding monomial. For example, \cr
    \tab                    \tab \code{string(Assignment(c(1,0,1,0)))} returns \code{"x1*x3"} and           \cr 
    \tab                    \tab \code{string(Assignment(c(0,0,0))} returns \code{"1"}. \cr
    \tab \code{weight()}    \tab returns the Hamming weight, i.e. the number of variables that are set to 1. \cr
    \tab \code{equals()}    \tab   - \cr
    \tab \code{print()}     \tab   - \cr
    }
}
\author{
  F.Lafitte
}
\seealso{ 
    \code{\link{BooleanFunction}}, \code{\link{Polynomial}}, \code{\link[R.oo:Object]{R.oo:Object}}
}
\examples{
    x <- Assignment( c(0,1,0,1) )
    y <- Assignment( "010110" )
    print(x)  # returns "Assignment with 4 variables."
    string(x) # returns "x2*x4"
    int(x)    # returns 10 (i.e. 0001 encodes 8 instead of 1)
}
\keyword{misc}

