#' Emulate SAS Frequency Table 
#' 
#' Main function of proc_freq, used to generate the var1*var2 output
#' tables, with the option to include cell chi-square values. This function is primarily
#' intended to be called within \code{proc_freq}, so no error checking is done
#' 
#' @param x This is a casted contingency table, generated from data.table::dcast
#' @param cellchi2 Boolean value to include cell chi-square values. Default is FALSE 
#' 
#' @return Currently, this just returns a matrix, but when called from 
#' \code{proc_freq}, it has class \code{sas_table}
#' 
#' @examples
#' data(Hawkeye)
#' ctab <- dcast(Hawkeye, Year ~ Win_Loss, value.var = "Freq")
#' ctab <- as.matrix(ctab[, 2:3], rownames = ctab[[1]])
#' make_sas_freq_table(ctab)

make_sas_freq_table <- function(x, cellchi2 = FALSE) {

## Requires extra cell if computing cell-chi sq
if(cellchi2) {
  cc <- get_cell_chisq(x)
  l <- list(x, cc, 100*prop.table(x),
            100*prop.table(x, 1),
            100*prop.table(x, 2))
  
  ## Totals for Frequency and Percent
  for(i in c(1,3)) {
    l[[i]] <- cbind(l[[i]], rowSums(l[[i]]))
    l[[i]] <- rbind(l[[i]], colSums(l[[i]]))
  }
  for(i in c(2, 4, 5)) {
    l[[i]] <- suppressWarnings(cbind(l[[i]], NA))
  }
  
} else {
  l <- list(x, 100*prop.table(x),
            100*prop.table(x, 1),
            100*prop.table(x, 2))
  
  ## Totals for Frequency and Percent
  for(i in 1:2) {
    l[[i]] <- cbind(l[[i]], rowSums(l[[i]]))
    l[[i]] <- rbind(l[[i]], colSums(l[[i]]))
  }
  for(i in 3:4) {
    l[[i]] <- suppressWarnings(cbind(l[[i]], NA))
  }
}

## dimensions of table
nr <- nrow(x)
nc <- ncol(x)
nl <- 4 + cellchi2
## Throw error if nr, nl is empty
tab <- matrix(rep(NA, nl*nr*(nc+1)), ncol = nc + 1)

## populate table
for(i in seq_len(nl)) {
  for(j in seq_len(nr)) {
    tab[i + nl*(j - 1), ] <- l[[i]][j, ]
  }
}

## Pesky names, cellchi2 messin up my ordering
if(cellchi2) {
  rownames(tab) <- rep(c("Frequency","Cell Chi-Square", "Percent",
                         "Row Pct", "Col Pct"), nr)
  idx <- c(T, F, F, F, F)
  rownames(tab)[idx] <- paste0(rownames(x), "-", rownames(tab)[idx])
  
  tab_total <- rbind(l[[1]][nr+1, ], l[[3]][nr+1, ])
  rownames(tab_total) <- c("Total - Frequency", "Percent")
  colnames(tab) <- c(colnames(x), "Total")
  
} else {
  rownames(tab) <- rep(c("Frequency", "Percent",
                         "Row Pct", "Col Pct"), nr)
  idx <- c(T, F, F, F)
  rownames(tab)[idx] <- paste0(rownames(x), "-",rownames(tab)[idx])
  
  tab_total <- rbind(l[[1]][nr+1, ], l[[2]][nr+1, ])
  rownames(tab_total) <- c("Total - Frequency", "Percent")
  colnames(tab) <- c(colnames(x), "Total")
}

tab
}