#' Hawkeye Football Annual Win/Loss Record
#' 
#' @docType data
#' 
#' @usage data(Hawkeye)
#' 
#' @format An object of class \code{"data.table"}
#' 
#' @references BIOS:7410 class notes
#' 
#' @examples 
#' data(Hawkeye)
#' print(Hawkeye)
#' tabs <- proc_freq(Hawkeye, "Freq", "Year*Win_Loss / Measures")
"Hawkeye"