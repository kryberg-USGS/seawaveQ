# Don't actually need.  Anomalies can be precalculated using waterData

# user needs to have data already in R
# columns staid, dates, qualcode, and val or other data

importcqwfilev1 <- function(data, indq, indt, indc, inds) {
  #  imports cqw file, fills in missing values, and computes 30-day and 1-day anomalies
  #    (requires Karens "waterData" package
  #  filename is the name of the cqw file, in quotes and including path and extensions
  #  eg, "c:\\seawavecqwv1\\cqwpe25i1.csv"
  #  indt=1 if temperature is included, 0 otherwise
  #  indc=1 if conductivity is included, 0 otherwise
  #  inds=1 if sediment is included, 0 otherwise
  dat <- read.table(filename, sep=",", header=TRUE, colClasses="character")
  dat <- dat[,-2]
  dat[,3] <- as.Date(dat[,3], format='%m/%d/%Y')
  dat <- dat[order(dat[,3]),]
  row.names(dat) <- NULL
  for (j in 4:length(dat[1,])) {
    dat[,j] <- as.numeric(dat[,j])
  }
  dflow <- dat$P00060
  datout <- dat[,1:3]
  names(datout) <- c('agency','staid','dates')
  yrx <- as.numeric(substring(as.character(datout[,3]), 1 ,4))
  mox <- as.numeric(substring(as.character(datout[,3]), 6, 7))
  dax <- as.numeric(substring(as.character(datout[,3]), 9, 10))
  jday <- julian(datout[,3], origin=datout[1,3]) + 1
  datout <- data.frame(datout,yrx,mox,dax,jday)
  dflow <- dat$P00060
  if(is.na(dflow[1])) {
    dflow[1] <- mean(dflow[!is.na(dflow)][1:10])
  }
  if (indt==1) {
    dtemp <- (dat$P00010a + dat$P00010b) / 2
    if(is.na(dtemp[1])) {
      dtemp[1] <- mean(dtemp[!is.na(dtemp)][1:10])
    }
  }
  if (indc==1) {
    dcond <- (dat$P00095a + dat$P00095b) / 2
    if(is.na(dcond[1])) {
      dcond[1] <- mean(dcond[!is.na(dcond)][1:10])
    }
  }
  if (inds==1) {
    dsed <- dat$P80154 
    if(is.na(dsed[1])) {
      dsed[1] <- mean(dsed[!is.na(dsed)][1:10])
    }
  }
  val <- dflow
  tmpdat1 <- data.frame(datout[,c(2,3)], val)
  tmpdat1[,3] <- replace(tmpdat1[,3], tmpdat1[,3] < 0.1, 0.1)
  if(sum(is.na(tmpdat1[,3])) >= 0) {
    tmpdat1 <- fillMiss(tmpdat1, block=220, pmiss=20)
  }
  tmpdat1 <- compAnom(tmpdat1, which=3)[[1]]
  plotAnoms(tmpdat1)
  names(tmpdat1) <- c('staid', 'dates', 'dflow', 'flowa30', 'flowa1')
  if (indt==1) {
    val <- 10^(dtemp/10) 
    tmpdat2 <- data.frame(datout[,c(2,3)], val)
    if(sum(is.na(tmpdat2[,3])) >= 0){
      tmpdat2 <- fillMiss(tmpdat2, block=220, pmiss=20)
    }
    tmpdat2 <- compAnom(tmpdat2, which=3)[[1]]
    plotAnoms(tmpdat2)
    names(tmpdat2) <- c('staid', 'dates', 'expdtemp', 'tempa30', 'tempa1')
  }
  if (indc==1) {
    val <- dcond 
    tmpdat3 <- data.frame(datout[,c(2,3)], val)
    tmpdat3[,3] <- replace(tmpdat3[,3], tmpdat3[,3] < 1, 1)
    if(sum(is.na(tmpdat3[,3])) >= 0) {
      tmpdat3 <- fillMiss(tmpdat3, block=220, pmiss=20)
    }
    tmpdat3 <- compAnom(tmpdat3, which=3)[[1]]
    plotAnoms(tmpdat3)
    names(tmpdat3) <- c('staid', 'dates', 'dcond', 'conda30', 'conda1')
  }
  if (inds==1) {
    val <- dsed 
    tmpdat4 <- data.frame(datout[,c(2, 3)], val)
    tmpdat4[,3] <- replace(tmpdat4[,3], tmpdat4[,3] < 1, 1)
    if(sum(is.na(tmpdat4[,3])) >= 0) {
      tmpdat4 <- fillMiss(tmpdat4, block=220, pmiss=20)
    }
    tmpdat4 <- compAnom(tmpdat4, which=3)[[1]]
    plotAnoms(tmpdat4)
    names(tmpdat4) <- c('staid', 'dates', 'dsed', 'seda30', 'seda1')
  }
  datout <- data.frame(datout, tmpdat1[,3:5])
  if(indt==1) { 
    datout <- data.frame(datout, tmpdat2[,3:5])
  }
  if(indc==1) { 
    datout <- data.frame(datout, tmpdat3[,3:5])
  }
  if(inds==1) { 
    datout <- data.frame(datout, tmpdat4[,3:5])
  }
  datout
}
