#' Printing SAS Tables
#'
#' @description Method to print output from proc_freq and other sassy functions
#'
#' @param x Object of class \code{sas_table}
#' @param na.print Basically the only thing that has changed. This default
#' argument replaces NA values with "-"
#' @param digits This is set to 4 because default print is a bit much
#' @param ... these are your standard print options, based on the default class
#' of the object
print.sas_table <- function(x, na.print = "-", digits = 4, ...) {
  y <- x
  class(y) <- class(x)[2]
  print(y, na.print = na.print, digits = 4, ...)
}
