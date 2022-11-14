ls_obj <- function(beta, y, X) {

  sum((y - X%*%beta)^2)

}

#' Estimate linear model via optimization
#' @description This function computes least-squares via convex minimization
#' @param y A \code{double} value of the vector containing the response of interest.
#' @param X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors.
#' @return A \code{list} containing the following objects:
#' \describe{
#'  \item{beta_hat}{The estimated coefficients of the linear regression}
#'  \item{y}{The \code{double} vector containing the response used for the estimation}
#'  \item{X}{The \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors used for the estimation}
#' }
#' @author Roberto Molinari
#' @export
beta_hat <- function(y, X) {

  beta_est <- optim(rep(0, ncol(X)), ls_obj, y = y, X = X)$par

  output <- list("beta_hat" = beta_est, "response" = y, "predictors" = X)
  class(output) = "ls_optim"

  return(output)

}

#' Simulate data from a linear model
#' @description This function simulates data of a response coming from a linear model
#' @param n A \code{double} value representing the desired sample size.
#' @param beta A \code{double} value of the vector containing the true parameter values of the model.
#' @param sigma2 A \code{double} value representing the desired variance of the residuals of the model.
#' @return A \code{list} containing the following objects:
#' \describe{
#'  \item{y}{The \code{double} vector of the \code{n} simulated responses from the linear model}
#'  \item{X}{The \eqn{n \times 4} \code{double} value of the matrix whose columns represent the different predictors}
#' }
#' @author Roberto Molinari
#' @export
sim_linear <- function(n, beta, sigma2, seed = NA) {

  if(!is.na(seed)) set.seed(seed)

  x1 <- rpois(n, lambda = 8)
  x2 <- rexp(n, rate = 3)
  x3 <- rnorm(n, -3, 5)
  int <- rep(1, n)

  X <- cbind(int, x1, x2, x3)
  y <- X%*%beta + rnorm(n, 0, sd = sqrt(sigma2))

  return(list("y" = y, "X" = X))

}

#' Plot fit from linear model
#' @description This function creates plots of the line of best fit for each predictor
#' @param object A \code{list} of class \code{ls_optim} with the results of the fit from the function \code{beta_hat}.
#' @return A \code{plot} representing the line of best fit for all predictors.
#' @author Roberto Molinari
#' @export
plot.ls_optim <- function(object, ...) {

  hold <- object
  response <- hold$response
  predictors <- hold$predictors
  beta_hat <- hold$beta_hat

  dm <- ncol(predictors)

  par(mfrow = c(1, dm - 1))

  for(i in 2:dm) {

    plot(predictors[, i], (response - predictors[, -c(1, i)]%*%beta_hat[-c(1, i)]), xlab = paste0("x",i), ylab = "response")
    abline(beta_hat[1], beta_hat[i], col = "red")

  }

}
