prepCdata <- function(cdat, yrstart, dcol, rnames, pnames, iwcav) {
  cdat$yrc <- year(cdat[, dcol])
  cdat$moc <- month(cdat[, dcol])
  cdat$dac <- day(cdat[, dcol])
  cdat$jdayc <- julian(cdat[, dcol], 
                       origin=as.Date(paste(yrstart-1, 
                                            "-10-01", sep="")))
  # determine column headings of concentration data
  mycols<-c("yrc","moc","dac","jdayc")
            
  for ( i in 1:length(pnames) ) {
    mycols<-c(mycols, rnames[i], pnames[i])
  }
  mycols <- c(mycols, iwcav)
  cdat <- cdat[, mycols]
  # columns of cdat are year, month, day, julian day, remark code, 
  # concentration value, and the selected ancillary variables
  
  pcktmp <- !is.na(cdat[,4])
  for (j in 1:length(iwcav) ) {
      pcktmp <- pcktmp & !is.na(cdat[,iwcav[j]])
  }
  cdat <- cdat[pcktmp,]
}  
