#' Relative Risk
#' 
#' @description Computes relative risk with confidence intervals
#' 
#'  @param x A contingency table
#'  @param col Column number in which to compute relative risk
#' @export
relrisk <- function(x, col) {
  ## May need to be updated to allow for more than 2x2
  n <- rowSums(x)
  cond_prob <- prop.table(x, 1)
  
  pi1 <- cond_prob[1, col]
  pi2 <- cond_prob[2, col]
  
  ## relative risk
  rr <- pi1/pi2
  log_rr_sig <- sqrt((1-pi1)/(pi1*n[1]) + (1-pi2)/(pi2*n[2]))
  log_rr_ci <- log(rr) + c(-1, 1)*qnorm(0.975)*log_rr_sig
  c(rr, exp(log_rr_ci))
}
