\name{BooleanFunction}
\alias{BooleanFunction}
\alias{Boolean Function}
\alias{wh.BooleanFunction}
\alias{tt.BooleanFunction}
\alias{anf.BooleanFunction}
\alias{ANF.BooleanFunction}
\alias{deg.BooleanFunction}
\alias{n.BooleanFunction}
\alias{ci.BooleanFunction}
\alias{nl.BooleanFunction}
\alias{res.BooleanFunction}
\alias{isBal.BooleanFunction}
\alias{isRes.BooleanFunction} 
\alias{isCi.BooleanFunction} 
\alias{equals.BooleanFunction}
\alias{print.BooleanFunction} 
\alias{anf}
\alias{ANF}
\alias{wh}
\alias{tt}
\alias{deg}
\alias{n}
\alias{ci}
\alias{nl}
\alias{res}
\alias{isBal}
\alias{isRes}
\alias{isCi}

\title{Cryptographic Boolean Functions}

\description{
    The class \code{BooleanFunction} implements functionality to assess cryptographic properties of Boolean functions 
    such as nonlinearity, algebraic immunity, resiliency, correlation immunity, ... 
    For a full list see the section Public Methods below.  
}
\usage{
    f <- BooleanFunction( initializer )
}
\arguments{
    \item{initializer}{a vector containing \eqn{2^n}{2^n} integers in \eqn{\{0,1\}}{\{0,1\}} or a string holding \eqn{2^n}{2^n} characters in \eqn{\{'0','1'\}}{\{'0','1'\}}.}
}
\value{
    The returned value \code{f} is an S3 object which is defined using the R.oo package. Methods of the returned value, say \code{deg()}, can be accessed in two ways using \code{f$deg()} or \code{deg(f)}. 
}
\details{
    The representations are computed in \eqn{\mathcal{O}(n2^n)}{O(n2^n)} using C++ code. They are computed only once, the first time they are needed/called by the user and stored in private fields. The same applies to some properties, namely algebraic immunity, algebraic degree and correlation immunity. Efforts have been made to optimize execution time rather than memory usage. For more details, see the package vignette (using the R command \code{vignette(boolfun)}). 
}
\section{Private Fields}{
    Heavy computations are carried only once, the first time they are needed/called. The results are stored/cached in private fields. \cr
    Private fields: \cr
    \tabular{rll}{
    \tab \code{.ANF}    \tab    the algebraic normal form (vector)    \cr 
    \tab \code{.WH}     \tab    the walsh spectrum (vector)           \cr
    \tab \code{.TT}     \tab    the truth table (vector)              \cr
    \tab \code{.deg}    \tab    the algebraic degree (integer)        \cr
    \tab \code{.ai}     \tab    the algebraic immunity (integer)      \cr
    \tab \code{.ci}     \tab    the correlation immunity (integer)    \cr
    }
    
    Note: R does not block access to private fields. However, using them is not recommended, use the accessors (public methods) instead.
}
\section{Public Methods}{

    The following representations are implemented: \cr
    \tabular{rll}{
    \tab \code{tt()}    \tab    returns the truth table (vector of integers)                                    \cr
    \tab \code{wh()}    \tab    returns the walsh spectrum (vector of integers)                                 \cr     
    \tab \code{anf()}   \tab    returns the vector of coefficients for each of the \eqn{2^n}{2^n} monomials     \cr
    \tab \code{ANF()}   \tab    returns the algebraic normal form as an instance of \code{Polynomial}           \cr
    }
    Some general properties:                                                                                    \cr
    \tabular{rll}{
    \tab \code{n()}     \tab    returns the number of input variables (\eqn{n}{n})                              \cr
    \tab \code{deg()}   \tab    returns the algebraic degree                                                    \cr
    }
    Some properties relevant for cryptographic applications:                                                    \cr
    \tabular{rll}{
    \tab \code{ai()}    \tab    returns the algebraic immunity                                                  \cr
    \tab \code{nl()}    \tab    returns the nonlinearity                                                        \cr
    \tab \code{ci()}    \tab    returns the correlation immunity                                                \cr
    \tab \code{res()}   \tab    returns the resiliency                                                          \cr
    \tab \code{isBal()} \tab returns true if the function has as many 0s as 1s in its truth table              \cr
    \tab \code{isCi(t)} \tab returns true if the function has correlation immunity \code{t}                    \cr
    \tab \code{isRes(t)}\tab returns true if the function has resiliency \code{t}                              \cr
    }
}
\section{Overridden Methods}{
    \code{BooleanFunction} inherits from \code{Object} defined in the R.oo package. The following methods are overriden: \cr
    \tabular{rll}{
    \tab \code{equals()}    \tab    compares truth tables and returns true if they are the same     \cr
    \tab \code{print()}     \tab    displays the number of variables                                \cr
    }
}
\author{
  F.Lafitte
}
\seealso{ 
    \code{\link{Polynomial}}, \code{\link[R.oo:Object]{R.oo:Object}}, \code{\link{mobiusTransform}}, \code{\link{walshTransform}}
}
\examples{
    truthTable <- c(0,1,1,0,1,0,0,1)
    f <- BooleanFunction(truthTable)
    g <- BooleanFunction("00100111")
    h <- BooleanFunction( c( tt(f), tt(g) ) ) # concatenation
    print( h )
    # 
    print( paste("f has (deg,ai,nl,res) = (", deg(f), ai(f), nl(f), res(f), ")" ))
    print( paste("g has (deg,ai,nl,res) = (", g$deg(), g$ai(), g$nl(), g$res(), ")" ))
    print( isBal(h) )   
}
\keyword{package}
\keyword{math}
\keyword{logic}

