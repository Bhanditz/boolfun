\name{mobiusTransform}
\alias{mobiusTransform}
\alias{Algebraic Normal Form}
\alias{Mobius Inversion}
\title{Mobius Inversion}

\description{
In this package, the Mobius inversion is used to compute the coefficient of each monomial in the algebraic normal form of the input Boolean function. That is, \code{mobiusTransforms} returns a vector of length \eqn{2^n}{2^n} where each entry equals one if the corresponding monomial appears in the algebraic normal form (zero otherwise) - see Details. 
}
\usage{
mobiusTransform( truthTable )
}
\arguments{
  \item{truthTable}{a vector of integers containing \eqn{2^n}{2^n} binary entries - see \code{\link{BooleanFunction}}.}
}
\value{
  \code{mobiusTransform} returns a vector of integers in \eqn{\{0,1\}}{\{0,1\}} of same length as the input vector (i.e. \eqn{2^n}{2^n}). The i\eqn{^{th}}{^th} entry is \eqn{1}{1} if the monomial \eqn{i}{i} appears in the algebraic normal form of the input function.
}
\details{
    The value is computed in \eqn{\mathcal{O}(n2^n)}{O(n2^n)}. For more information, type \code{vignette(boolfun)}.
}
\seealso{ \code{\link{BooleanFunction}} }

\references{
    Graham, Knuth, Patashnik.
    Concrete Mathematics.
    second edition.
    pp.136.

    Ann Braeken.
    Cryptographic Properties of Boolean Functions and S-Boxes.
    phd thesis - 2006. 
    
    The vignette of this package. 
}

\examples{
tt <- c(0,1,1,0,1,0,0,1)
anf <- mobiusTransform(tt)
}

\keyword{misc}

