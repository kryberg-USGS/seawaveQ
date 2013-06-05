### R code from vignette source 'vignette.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: vignette.Rnw:42-51
###################################################
# load waterData package, assuming it has already been installed on the system
library(seawaveQ)
# load example data that comes with the package
data(swData)
# show first few rows of water-quality data for Missouri River at Omaha, Nebraska
head(qwMoRivOmaha)
# get a description of the data including definitions of the columns
# by viewing the help documentation
?qwMoRivOmaha


###################################################
### code chunk number 2: vignette.Rnw:60-62
###################################################
# scatter plot showing unqualified, estimated, and censored  values
cenScatPlot(qwMoRivOmaha, pname="04035")


###################################################
### code chunk number 3: vignette.Rnw:69-84
###################################################
# scatter plot with many additional plotting arguments
# these options provide a plot closer to the plotting standards
#  of the U.S. Geological Survey however, these plots may not 
# meet all U.S. Geological Survey publication requirements
par(las=1, tcl=0.5)
cenScatPlot(qwMoRivOmaha, pname="04035", 
                       site="06610000 Missouri River at Omaha, Neb.",
                       ylabel="Simazine concentration, in micrograms per liter",
                       legcex=0.7, qwcols=c("R", "P"), ylim=c(0,0.1), yaxs="i", 
                       cex.lab=0.9, cex.axis=0.9, xlim=c(as.Date("1996-01-01"), 
                       as.Date("2004-01-01")), xaxs="i", xaxt="n")
axdates <- c("1996-01-01", "1998-01-01", "2000-01-01", 
                       "2002-01-01", "2004-01-01")
axis(1, as.Date(axdates), 
                       labels=c("1996", "1998", "2000", "2002", "2004"), cex.axis=0.9)


###################################################
### code chunk number 4: vignette.Rnw:91-93
###################################################
# simple box plots of water-quality concentrations
rosBoxPlot(qwMoRivOmaha, qwcols=c("R", "P"))


###################################################
### code chunk number 5: vignette.Rnw:100-110
###################################################
# same boxplot function with many additional plotting arguments
rosBoxPlot(qwMoRivOmaha, site="06610000 Missouri River at Omaha, Neb.",
                     log="y", yaxt="n", ylim=c(0.000001, 10), qwcols=c("R", "P"), 
                     ylab=c("Concentration, micrograms per liter"), col="skyblue1",
                     cex.axis=0.7, cex.sub=0.8, 
                     par(tcl=0.5, las=1, yaxs="i", mgp=c(3,0.5,0), mar=c(5,5,2,2), 
                     cex.main=0.9))
axis(2, at=c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10),
labels=c("0.000001", "0.00001", "0.0001", "0.001", "0.01",
"0.1", "1", "10"), cex.axis=0.7)


###################################################
### code chunk number 6: vignette.Rnw:117-123
###################################################
data(swData)
# show last few rows of water-quality data for Missouri River at Omaha, Nebraska
tail(cqwMoRivOmaha)
# get a description of the data including definitions of the columns
# by viewing the help documentation
?cqwMoRivOmaha


###################################################
### code chunk number 7: vignette.Rnw:132-137
###################################################
data(swData)
MoRivOmaha<-combineData(qwdat=qwMoRivOmaha, cqwdat=cqwMoRivOmaha,
qwcols=c("staid", "dates", "R", "P"))
# view combined data set
head(MoRivOmaha)


###################################################
### code chunk number 8: vignette.Rnw:145-157
###################################################
myfit1 <- fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha,
tanm="myfit1", pnames=c("04035", "04041"), yrstart=1995,
yrend=2003, tndbeg=1995, tndend=2003, iwcav=c("flowa30", "flowa1"),
dcol="dates", qwcols=c("R","P"))
myfit2 <- fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha,
tanm="myfit2", pnames=c("04035", "04041"), yrstart=1995,
yrend=2003, tndbeg=1995, tndend=2003, iwcav=c("seda30", "seda1"),
dcol="dates", qwcols=c("R","P"))
myfit3 <- fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha,
tanm="myfit3", pnames=c("04035", "04041"), yrstart=1995,
yrend=2003, tndbeg=1995, tndend=2003, iwcav=c("flowa30", "flowa1",
"seda30", "seda1"), dcol="dates", qwcols=c("R","P"))


###################################################
### code chunk number 9: vignette.Rnw:164-176
###################################################
# get the first element of the list for each model/constituent combination
# the data frame with information about each model/constituent combination
myfit1[[1]]
myfit2[[1]]
myfit3[[1]]

# get the second element of the list for each model/constituent combination
# the survival regression summary for each model/constituent combination
myfit1[[2]]
myfit2[[2]]
myfit3[[2]]



###################################################
### code chunk number 10: vignette.Rnw:183-188
###################################################

attributes(myfit1[[2]][[1]])
myfit1[[2]][[1]]$n
myfit1[[2]][[1]]$table



