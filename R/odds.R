#' Odds Ratio
#' 
#' @description Computes odds ratio with confidence interval
#' 
#' @param x A 2x2 contingency table
odds <- function(x) {
  if(prod(dim(x)) != 4) stop("Odds ratio requires 2x2 input vars")
  or <- (x[1]*x[4])/(x[2]*x[3])
  
  log_or_sig <- sqrt(sum(1/x))
  log_or_ci <- log(or) + c(-1, 1)*qnorm(0.975)*log_or_sig
  c(or, exp(log_or_ci))
}
