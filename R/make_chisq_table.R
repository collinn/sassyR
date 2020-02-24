#' SAS Chi-Square Table
#' 
#' @description This function emulates the 'ChiSq' table returned by SAS
#' 
#' @param x A contingency table
#' @param exact Boolean argument, default FALSE, to use Fisher's e xact test
#' for tables larger than 2x2
#' 
#' @examples 
#' A <- structure(1:4, dim = c(2L, 2L), 
#'                dimnames = list(c("A", "B"), c("X", "Y")))
#' make_chisq_table(A)
make_chisq_table <- function(x, exact = FALSE) {
  chi_sq_pearson <- get_cell_chisq(x, "Pearson") %>% sum
  chi_sq_lr <- get_cell_chisq(x, "LR") %>% sum
  chi_sq_adj <- get_cell_chisq(x, "adjusted") %>% sum
  df <- (nrow(x) - 1)*(ncol(x) - 1)
  
  ## Phi coefficient
  if(prod(dim(x)) != 4) {
    phi_coef <- sqrt(chi_sq_pearson/sum(x))
  } else {
    phi_coef <- (x[1]*x[4] - x[2]*x[3])/sqrt(prod(rowSums(x), colSums(x)))
  }
  
  ## Contingency coef
  cont_coef <- sqrt(chi_sq_pearson/(chi_sq_pearson + sum(x)))
  cramerV <- sqrt((chi_sq_pearson/sum(x))/min(dim(x) - 1))
  
  dat <- data.frame(DF = c(df, df, df, NA, NA, NA, NA),
                    Value = c(chi_sq_pearson, chi_sq_lr, chi_sq_adj,
                              NA, phi_coef, cont_coef, cramerV),
                    Prob = c(pchisq(chi_sq_pearson, df, lower.tail = FALSE),
                             pchisq(chi_sq_lr, df, lower.tail = FALSE),
                             pchisq(chi_sq_adj, df, lower.tail = FALSE),
                             NA, NA, NA, NA))
  
  rownames(dat) <- c("Chi-Square", "Likelihood Ratio Chi-Square",
                     "Continuity Adj. Chi-Square", "Mantel-Haenszel Chi-Square",
                     "Phi Coefficient", "Contingency Coefficient", "Cramer's V")
  
  if(exact == TRUE) {
    fish_test <- fisher.test(x, alternative = "less")
    return(list("chisq_table" = as.matrix(dat), "fisher_test" = fish_test$p.value))
  } else {
    fish_left <- fisher.test(x, alternative = "less")$p.value
    fish_right <- fisher.test(x, alternative = "greater")$p.value
    fish_two <- fisher.test(x)$p.value
    return(list("chisq_table" = as.matrix(dat),
                "fisher_test" = c("Fisher Test - Left" = fish_left,
                                  "Fisher Test - Right" = fish_right,
                                  "Fisher Test - Two Sided" = fish_two)))
  }
}