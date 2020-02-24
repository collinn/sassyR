#' Hawkeye Rhabdo Football Study
#' 
#' @description Rhabdo dataset including None, 1_to_3 and 4_to_5
#' 
#' @docType data
#' 
#' @usage data(Rhabdo1)
#' 
#' @format An object of class \code{"data.table"}
#' 
#' @references BIOS:7410 class notes
#' 
#' @examples 
#' data(Rhabdo1)
#' print(Rhabdo1)
#' tabs <- proc_freq(Rhabdo1, "Freq", "Shakes*Rhabdo / ChiSq CellChi2 Exact")
"Rhabdo1"