
\name{ai.BooleanFunction}
\alias{ai}
\alias{ai.BooleanFunction}
\alias{BooleanFunction.ai}

\title{Algebraic Immunity}

\description{
 Returns the algebraic immunity - see Details.
}

\usage{\method{ai}{BooleanFunction}(this, ...)}

\arguments{
  \item{this}{-}
  \item{...}{Not used}
}

\value{
  This method returns the smallest degree (integer) of a non-zero annihilator of the function (f) or its complement (1+f).
}

\details{
    The algebraic immunity is obtained by gaussian elimination using C++ code. For more information, type \code{vignette(boolfun)}.
}

\seealso{
  \code{\link{BooleanFunction}}
}

