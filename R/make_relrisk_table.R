#' Create SAS Relative Risk Table
#' 
#' This function emulates the RelRisk table returned by SAS
#' 
#' @param x A 2x2 contingency table
#' 
#' @examples 
#' A <- structure(1:4, dim = c(2L, 2L), 
#'                dimnames = list(c("A", "B"), c("X", "Y")))
#' make_relrisk_table(A)
#' @export
make_relrisk_table <- function(x) {
  if((nrow(x) != 2) | (ncol(x) != 2)) {
    stop("RelRisk table requires 2x2 input vars")
  }
  
  tab <- rbind(odds(x), relrisk(x, 1), relrisk(x, 2))
  colnames(tab) <- c("Value", "95% L", "95% U")
  rownames(tab) <- c("Odds Ratio", "Relative Risk (Col 1)",
                     "Relative Risk (Col 2)")
  tab
}
