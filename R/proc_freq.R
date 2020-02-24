#' (limited) Emulation of SAS proc_freq
#'
#' @param x A data.frame, data.table, or matrix containg data. Tables will
#' be generated with variables in the order that the data is coded. Data should
#' be in long format (see Hawkeye data)
#' @param weight The variable/column name in x the represents the frequency or 
#' count, as a length one character vector, i.e., "Freq"
#' @param tables This is similar to the 'TABLES' argument in SAS. This should
#' also be a length one character vector. At a minimum, this should contain
#' two variables from x that we are interested in tabulating, joined with an '*'.
#' Similar to SAS code, we can request a number of tables and options, including Measures, 
#' RelRisk, ChiSq, CellChi2, Exact, and CMH1. This argument should be of this form:
#' tables = "var1*var2 / table1 option1 table2 table3"
#' 
#' @examples
#' ## Not run
#' tab1 <-  proc_freq(Rhabdo1, "Freq", "Shakes*Rhabdo / ChiSq CellChi2")
#' tab2 <- proc_freq(Hawkeye, "Freq", "Year*Win_Loss / Measures")
#' tab3 <- proc_freq(HA, "Freq", "Trt_Grp*MI / RelRisk")

proc_freq <- function(x, weight, tables) {
  
  ## Make sure data.frame/table
  x <- as.data.table(x)
  nm <- colnames(x)
  
  ## Also should check that table is even passed something
  ## If its not, we should get vars from everything thats not 'weight'
  
  ## Get Vars and table requests
  table_args <- str_split(tables, "/") %>% unlist() %>% str_trim
  
  ## Start with variable names, check that they are valid
  vv <- str_split(table_args[1], "\\*") %>% unlist()
  if(any(!(vv %in% nm))) stop("Must include valid column names in var1*var2")
  if(!(weight %in% nm)) stop("Must include valid column name for weight")
  
  ## Keep order based on input (Order = Data)
  ro <- unique(x[, get(vv[1])]) %>% as.character
  co <- unique(x[, get(vv[2])]) %>% as.character
  
  ## Cast to table, reorder rows/columns based on input
  dd <- dcast(x, get(vv[1]) ~ get(vv[2]), value.var = weight)
  dd <- as.matrix(dd[, 2:3], rownames = dd[[1]])
  dd <- dd[ro, co]
  
  sample_size <- sum(dd)
  
  ########
  
  ## Now determine tables to create
  table_args <- table_args[-1] # remove var*var
  table_args <- str_split(table_args, " ") %>% unlist %>% str_to_lower
  
  ## Not efficient, but also not computationally intensive
  output <- vector("list", length = 2*length(table_args) + 1)
  i <- 1
  
  ## Create main freq table
  main_freq_tab <- make_sas_freq_table(dd,
                                       cellchi2 = ("cellchi2" %in% table_args))
  output[[i]] <- main_freq_tab
  names(output)[i] <- "frequency_table"
  i <- i + 1
  
  ## Make MEASURES table
  if("measures" %in% table_args) {
    output[[i]] <- make_measures_table(dd)
    names(output)[i] <- "measures_table"
    i <- i + 1
  }
  
  ## RelRisk
  if("relrisk" %in% table_args) {
    if(prod(dim(dd)) != 4) stop("Only 2x2 tables for RelRisk argument")
    output[[i]] <- make_relrisk_table(dd)
    names(output)[i] <- "relrisk_table"
    i <- i + 1
  }
  
  ## ChiSq
  if("chisq" %in% table_args) {
    b <- "exact" %in% table_args
    chitab <- make_chisq_table(dd, exact = b)
    output[[i]] <- chitab[[1]]
    names(output)[i] <- "chisq_table"
    i <- i + 1
    output[[i]] <- chitab[[2]]
    names(output)[i] <- "fisher_test"
  }
  
  ## CMH!
  if("cmh1" %in% table_args) {
    warning("CMH1 is currently not figured out. Sorry :(")
  }
  
  output[[i]] <- sample_size
  names(output)[i] <- "sample_size"
  output <- compact(output)
  
  ## Assign class for printing (see print.sas_table)
  output <- lapply(output, function(x) {
    class(x) <- c("sas_table", class(x))
    x
  })
  
  return(output)
}
