importconcfilev1 <- function(filename,cqwdat)  {
#  imports conc file and merges it with anomalies
#eg, filename='c:\\seawavecqwv1\\pe25d2i1.csv', cqwdat=cqwpe25i1 (dataframe created by importcqwfilev1)
dat <- read.table(filename,sep=',',header=T,colClasses="character")
nxxx <- length(dat[1,])
# arrange columns in order of site,STAID,DATES,and R/P columns
pckcol <- rep(F,nxxx)
cntmp <- names(dat)
for (i in 1:nxxx) {
   if(cntmp[i]=='site' | cntmp[i]=='STAID' | cntmp[i]=='DATES') {pckcol[i] <- T}
   if(substring(cntmp[i],1,1)=='R' | substring(cntmp[i],1,1)=='P') {pckcol[i] <- T}
                  }
dat <- dat[,pckcol]
#  convert column 3 to proper date format and order the rows chronologically
dat[,3] <- as.Date(dat[,3],format='%m/%d/%Y')
 dat <- dat[order(dat[,3]),]
 row.names(dat) <- NULL
# convert P columns from character to numeric
 pckp <- substring(names(dat),1,1)=='P'
 nxx <- length(pckp)
 for (i in 1:nxx) {
  if(pckp[i]==TRUE) {dat[,i] <- as.numeric(dat[,i])}
                  }
#  pick off year, month, day, and compute julian dat wrt first day in cqwdat
  yrc <- as.numeric(substring(as.character(dat[,3]),1,4))
   moc <- as.numeric(substring(as.character(dat[,3]),6,7))
   dac <- as.numeric(substring(as.character(dat[,3]),9,10))
   jdayc <- julian(dat[,3],origin=cqwdat[1,3])+1
# merge concentration data and anomaly data and output new dataframe
  dat <- data.frame(dat[,1:3],yrc,moc,dac,jdayc,dat[,-c(1:3)],cqwdat[jdayc,-c(1:7)])
  row.names(dat) <- NULL
  dat
}

