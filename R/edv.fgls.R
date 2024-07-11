#' FGLS model for estimated dependent variable
#'
#' @param ols Initial OLS model (lm in R)
#' @param DVvariance Numeric vector of variances of each y. The length should be equal the number of observations in the initial model.
#' @param n Numeric vector of number of observations (n) on which each y was estimated. The length should be equal the number of observations in the initial model.
#' @import stats
#' @return re-estimated model
#' @details
#' The function is based completely on the following paper: Lewis, J. B., & Linzer, D. A. (2005). Estimating regression models in which the dependent variable is based on estimates. *Political analysis*, 13(4), 345-364.
#' There are two possible strategies. If variance of each y is unknown, then Numeric vector of number of observations (n) on which each y was estimated should be provided. For example, y - mean of housholds income, then `n` is the number of housholds.
#' The second possible situation is when a variance of each y is known. In that case, you should provide `DVvariance` arguement in the function with numeric vector of such variances (as in case with the first situation with number of observations)
#'
#' @export
edv.fgls <- function(ols, DVvariance = NULL, n = NULL){

  vv <- residuals(ols)


  if(is.null(DVvariance)){
    if(is.null(n)){
      stop("If DV Variance is unknown, n should be provided")
    }

    sigma.sq.estimation = lm(I(vv^2) ~ 1 + I(1/n))
    if(coef(sigma.sq.estimation)[1]<0){
      sigma.sq.estimation = lm(I(vv^2) ~ -1 + I(1/n))
    }
    new.wght = 1/sqrt(predict(sigma.sq.estimation))

  }else{
    wght = DVvariance
    G <- matrix(nrow = nrow(ols$model), ncol = nrow(ols$model), data = 0)
    diag(G) <- wght
    X <- model.matrix(ols)
    sigma.sq = (  sum(vv^2) - sum(wght) + sum(diag( solve(t(X)%*%X) %*%t(X)%*%G%*%X )) ) /
      (nrow(ols$model)-length(coef(ols)))
    new.wght = 1/sqrt(wght + sigma.sq)
  }

  newols <- update(ols, weights = new.wght)

  return(newols)
}
