% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_functions.R
\name{beta_hat}
\alias{beta_hat}
\title{Estimate linear model via optimization}
\usage{
beta_hat(y, X)
}
\arguments{
\item{y}{A \code{double} value of the vector containing the response of interest.}

\item{X}{An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors.}
}
\value{
A \code{list} containing the following objects:
\describe{
 \item{beta_hat}{The estimated coefficients of the linear regression}
 \item{y}{The \code{double} vector containing the response used for the estimation}
 \item{X}{The \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors used for the estimation}
}
}
\description{
This function computes least-squares via convex minimization
}
\author{
Roberto Molinari
}
