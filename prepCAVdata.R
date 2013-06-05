prepCAVdata <- function(cavdat, yrstart, dcol, iwcav) {
  cavdat$yrx <- year(cavdat[, dcol])
  cavdat$mox <- month(cavdat[, dcol])
  cavdat$dax <- day(cavdat[, dcol])
  cavdat$jdayx <- julian(cavdat[, dcol], 
                       origin=as.Date(paste(yrstart-1, 
                                            "-10-01", sep="")))
  # determine column headings of ancillary data
  mycols<-c("yrx","mox","dax","jdayx")
            
  mycols <- c(mycols, iwcav)
  cavdat <- cavdat[, mycols]
  # columns of cavdat are year, month, day, julian day, and the 
  # selected continuous variables remove rows with missing values 
  # for concentration or continuous variables
  
  pcktmp <- !is.na(cavdat[,5])
  if(length(cavdat[1,]) > 5)  {
    for (j in 6:length(cavdat[1,])) {
      pcktmp <- pcktmp & !is.na(cavdat[,j])
    }
  }
  cavdat <- cavdat[pcktmp,]
  cavdat
}  
