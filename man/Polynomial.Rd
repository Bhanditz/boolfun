\name{Polynomial}
\alias{Polynomial}
\alias{anf.Polynomial}
\alias{*.Polynomial}
\alias{+.Polynomial}
\alias{[[.Polynomial}
\alias{deg.Polynomial}
\alias{len.Polynomial}
\alias{n.Polynomial}
\alias{string.Polynomial}
\alias{equals.Polynomial}
\alias{print.Polynomial}
\alias{string}

\title{Multivariate Boolean Polynomial}

\description{
    Implements an element of \eqn{GF(2)[x_1,\dots,x_n] / <x_1^2=x_1,\dots,x_n^2=x_n>}{e}, that is, a multivariate polynomial (in \eqn{x_1,\dots ,x_n}{x1,...,xn} where \eqn{x_i^2=x_i}{xi^2=xi} holds for all \eqn{i}{i}) with coefficients in GF(2), the field with two elements. Some basic functionality such as addition and multiplication are implemented. 
}
\usage{
    x <- Polynomial( initializer )
}
\arguments{
    \item{initializer}{a vector containing \eqn{2^n}{2^n} integers in \eqn{\{0,1\}}{\{0,1\}} or a string holding \eqn{2^n}{2^n} characters in \eqn{\{'0','1'\}}{\{'0','1'\}}.}
}
\value{
    The returned value \code{f} is an S3 object which is defined using the R.oo package. Methods of the returned value, say \code{weight()}, can be accessed in two ways using \code{x$weight()} or \code{weight(x)}. 
}
\details{
    Addition and multiplication are computed using C code. For more information, type \code{vignette(polynomial)}.
}

\section{Public Methods}{
    \tabular{rll}{
    \tab \code{n()}         \tab   number of variables \eqn{n}{n} \cr
    \tab \code{anf()}       \tab   vector of \eqn{2^n}{2^n} coefficients \cr
    \tab \code{deg()}       \tab   algebraic degree  \cr     
    \tab \code{*.Polynomial(p)}\tab   returns (\code{this} * \code{p}) as new Polynomial \cr
    \tab \code{+.Polynomial(p)}\tab   returns (\code{this} + \code{p}) as new Polynomial \cr
    \tab \code{[[.Polynomial(x)}\tab  evaluate the polynomial on assignment (vector) \code{x} \cr
    \tab \code{string()}    \tab   algebraic normal form as string \cr
    \tab \code{print()}     \tab   displays the result of \code{string()} \cr
    \tab \code{len()}       \tab   returns \eqn{2^n}{2^n} \cr
    }
}
\author{
  F.Lafitte
}
\seealso{ 
    \code{\link{BooleanFunction}}, \code{\link{Assignment}}, \code{\link[R.oo:Object]{R.oo:Object}}
}
\examples{
    p <- Polynomial( c(0,1,0,1) )
    q <- Polynomial( "01011011" )
    r <- Polynomial( c(anf(p),anf(p),anf(q)) )
    print(p)  
    deg(p)
    p <- p*q + r
    print(p)
}
\keyword{misc}

