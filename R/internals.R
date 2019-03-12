#x <- matrix(c("A","A","A","A","A","B","B","B","C","C","D","D","D","D","E","E","F","G","H","I","J","K","L","M"), ncol = 3)



flat_fac <- function(x){

  res <- rep(NA, nrow(x) * ncol(x))
  lvl <- rep(NA, nrow(x) * ncol(x))
  tr <- rep(NA, ncol(x))

  for(i in 1:(nrow(x))){
    for(j in 1:ncol(x)){
      res[j + ncol(x)*(i-1)] <- paste0("/Title (", x[i,j], ")",
                                       " /Page ", i,
                                       " /OUT pdfmark")
      lvl[j + ncol(x)*(i-1)] <- j

      if (!is.na(tr[j])){
        if(x[i,j] == tr[j]){
          res[j + ncol(x)*(i-1)] <- NA
          lvl[j + ncol(x)*(i-1)] <- NA
        } else {
          if(j < ncol(x)) tr[(j+1):ncol(x)] <- NA
        }
      }
      tr[j] <- x[i,j]
    }
  }

  res <- res[!is.na(res)]
  lvl <- lvl[!is.na(lvl)]

  lvl <- c(lvl, min(lvl))
  yy <- seq_along(lvl)
  ww <- rep(NA, length(lvl))
  for(i in 1:(length(lvl)-1)){
    resid <- lvl[-seq_len(yy[i])]
    resid <- resid[-((which(resid <= lvl[i]))[1]:length(resid))]
    ww[i] <- ifelse(length(resid) > 0, sum(resid == min(resid)), 0)

  }
  ww <- ww[-length(ww)]
  ww <- paste0("[/Count -", ww, " ")

  ww[ww == "[/Count -0 "] <- "["

  res <- paste0(ww, res)


}

#cat(res, sep = "\n")
