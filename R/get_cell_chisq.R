#' Chi-Square of Cell Counts
#' 
#' @description Returns the chi-square values of cell counts in a contingency table
#' 
#' @param x A contingency table
#' @param type character vector indicating chi-square statistic to use. Default
#' is "Pearson", though "LR" and "adjusted" are also available
#' @export
get_cell_chisq <- function(x, type = "Pearson") {
  row_sums <- rowSums(x)
  col_sums <- colSums(x)
  n <- sum(x)
  
  mu_hat <- outer(row_sums, col_sums) / n
  pi_hat <- mu_hat/n
  
  if (type == "LR") {
    cell_chi_sq <- 2 * (x * log(x / mu_hat))
  } else if (type == "adjusted") {
    cell_chi_sq <- (max(0, x-mu_hat - 0.5))^2/mu_hat
  } else {
    ## Default is Pearson
    cell_chi_sq <- (x - mu_hat)^2/(mu_hat)
  }
  
  cell_chi_sq
}
