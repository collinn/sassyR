#' This function emulates the 'Measures' table returned by SAS
#' 
#' @param x A contingency table
#' 
#' @examples 
#' A <- structure(1:4, dim = c(2L, 2L), 
#'                dimnames = list(c("A", "B"), c("X", "Y")))
#' make_measures_table(A)
make_measures_table <- function(x) {
  fxns <- list(GoodmanKruskalGamma = function(...) GoodmanKruskalGamma(...),
               KendallTauB = function(...) KendallTauB(...),
               StuartTauC = function(...) StuartTauC(...),
               "SomersD C|R" = function(...) SomersDelta(..., direction = "column"),
               "SomersD R|C" = function(...) SomersDelta(..., direction = "row"),
               "Pearson Correlation" = function(...) c(NA, NA, NA),
               "Spearman Correlation" = function(...) c(NA, NA, NA),
               "Lambda Asymmetric C|R" = function(...) Lambda(..., direction = "column"),
               "Lambda Asymmetric R|C" = function(...) Lambda(..., direction = "row"),
               "Lambda Symmetric" = function(...) Lambda(..., direction = "symmetric"),
               "Uncertainty Coefficient C|R" = function(...) UncertCoef(..., direction = "column"),
               "Uncertainty Coefficient R|C" = function(...) UncertCoef(..., direction = "row"),
               "Uncertainty Coefficient Symmetric" = function(...) UncertCoef(..., direction = "symmetric"))
  
  tab <- lapply(fxns, function(f) f(x, conf.level = 0.95))
  tab <- lapply(tab, function(x) as.data.table(t(x))) %>% rbindlist(use.names = FALSE) %>% as.matrix
  rownames(tab) <- names(fxns)
  tab <- cbind(tab, (tab[, 1] - tab[, 2])/qnorm(0.975))
  colnames(tab) <- c("Value", "95% L", "95% U", "ASE")
  tab <- tab[, c(1, 4, 2, 3)]
  tab
}