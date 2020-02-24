#' Compact Function
#' 
#' @description Removes NULL entrires from list (stolen from plyr)
#' 
#' @param x A list
#' 
#' @examples 
#' l <- vector("list", length = 5)
#' l[[1]]] <- "A"
#' print(l)
#' l <- compact(l)
#' print(l)
compact <- function(x) {
  Filter(Negate(is.null), x)
}