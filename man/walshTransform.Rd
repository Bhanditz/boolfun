\name{walshTransform}
\alias{walshTransform}
\alias{Fast Walsh Hadamard Transform}
\title{Fast Walsh Hadamard Transform}

\description{
  \code{walshTransform} returns the Walsh-Hadamard transform of the input truth table.
}
\usage{
walshTransform( truthTable )
}
\arguments{
  \item{truthTable}{a vector of integers containing \eqn{2^n}{2^n} binary entries - see \code{\link{BooleanFunction}}.}
}
\value{
  \code{walshTransform} returns a vector of integers having the same length as the input vector. The i\eqn{^{th}}{^th} entry can be seen as a "similarity" or "association" with the linear function determined by the integer \eqn{i}{i} (i.e. there are \eqn{2^n}{2^n} such functions). 
}
\details{
    The value is computed in \eqn{ \mathcal{O}(n2^n) }{O(n2^n)} using C code according to the Fast Walsh Transform algorithm. For more information, type \code{vignette(boolfun)}.
}
\seealso{ \code{\link{BooleanFunction}} }

\references{
    James L. Massey.
    The Discrete Fourier Transform in Coding and Cryptography.
    IEEE Inform. Theory Workshop, ITW 1998, pages 9--11.

    Ann Braeken.
    Cryptographic Properties of Boolean Functions and S-Boxes.
    phd thesis - 2006. 
    
    The vignette of this package. 
}

\examples{
tt <- c(0,1,1,0,1,0,0,1)
wh <- walshTransform(tt)
}

\keyword{misc}

