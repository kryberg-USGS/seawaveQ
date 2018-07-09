pkgname <- "seawaveQ"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "seawaveQ-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('seawaveQ')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("IllRivValleyCty")
### * IllRivValleyCty

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: IllRivValleyCty
### Title: Water-quality data for 05586100 Illinois River at Valley City,
###   Ill.
### Aliases: IllRivValleyCty
### Keywords: datasets datasets

### ** Examples

data(swData)

# summary of water-quality concentrations
apply(IllRivValleyCty[,grep("P[[:digit:]]", 
dimnames(IllRivValleyCty)[[2]])], 2, summary)

# simple boxplot of water-quality concentrations
rosBoxPlot(IllRivValleyCty)

# same boxplot function with many additional plotting arguments
rosBoxPlot(IllRivValleyCty, 
           site="05586100 Illinois River at Valley City, Ill.", log="y", 
           yaxt="n", ylim=c(0.0000001, 1), qwcols=c("R", "P"), 
           ylab=c("Concentration, micrograms per liter"), col="skyblue1", 
           cex.axis=0.7, cex.sub=0.8, par(tcl=0.5, las=1, 
                                                        yaxs="i", 
                                                        mgp=c(3,0.5,0), 
                                                        mar=c(5,5,2,2),
                                                        cex.main=0.9))
axis(2, at=c(0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1), 
     labels=c("0.0000001", "0.000001", "0.00001", "0.0001", "0.001", "0.01", 
              "0.1", "1"), cex.axis=0.7)

# scatter plot of simazine concentrations
cenScatPlot(IllRivValleyCty, pname="04035")

# scatter plot with many additional plotting arguments
par(las=1, tcl=0.5)
cenScatPlot(IllRivValleyCty, pname="04035", 
            site="05586100 Illinois River at Valley City, Ill.",
            ylabel="Simazine concentration, in micrograms per liter", 
            legcex=0.7, 
            ylim=c(0,0.4), yaxs="i", cex.lab=0.9, cex.axis=0.9,
            xlim=c(as.Date("1996-01-01"), as.Date("2012-01-01")), 
            xaxs="i", xaxt="n")
axdates<-c("1996-01-01", "2000-01-01", "2004-01-01", "2008-01-01",
           "2012-01-01")
axis(1, as.Date(axdates), labels=c("1996", "2000", "2004", "2008",
                                   "2012"), cex.axis=0.9)

# Prometon scatter plot
cenScatPlot(IllRivValleyCty, pname="04037", 
            site="05586100 Illinois River at Valley City, Ill.",
            ylabel="Prometon concentration, in micrograms per liter", 
            legcex=0.7, 
            ylim=c(0,0.06), yaxs="i", cex.lab=0.9, cex.axis=0.9,
            xlim=c(as.Date("1996-01-01"), 
                   as.Date("2012-01-01")), xaxs="i", 
            xaxt="n")
axdates<-c("1996-01-01", "2000-01-01", "2004-01-01", "2008-01-01", 
           "2012-01-01")
axis(1, as.Date(axdates), labels=c("1996", "2000", "2004", "2008",
                                               "2012"), cex.axis=0.9)

# Metribuzin scatter plot
cenScatPlot(IllRivValleyCty, pname="82630", 
            site="05586100 Illinois River at Valley City, Ill.",
            ylabel="Metribuzin concentration, in micrograms per liter", 
            legcex=0.7, 
            ylim=c(0,0.3), yaxs="i", cex.lab=0.9, cex.axis=0.9,
            xlim=c(as.Date("1996-01-01"), 
                   as.Date("2012-01-01")), xaxs="i", 
            xaxt="n")
axdates<-c("1996-01-01", "2000-01-01", "2004-01-01", "2008-01-01", 
           "2012-01-01")
axis(1, as.Date(axdates), labels=c("1996", "2000", "2004", "2008",
                                               "2012"), cex.axis=0.9)
# EPTC scatter plot
cenScatPlot(IllRivValleyCty, pname="82668", 
            site="05586100 Illinois River at Valley City, Ill.",
            ylabel="EPTC concentration, in micrograms per liter", 
            legcex=0.7, ylim=c(0,0.08), yaxs="i", cex.lab=0.9, 
            cex.axis=0.9, xlim=c(as.Date("1996-01-01"), 
                   as.Date("2012-01-01")), xaxs="i", xaxt="n")
axdates<-c("1996-01-01", "2000-01-01", "2004-01-01", "2008-01-01", 
           "2012-01-01")
axis(1, as.Date(axdates), labels=c("1996", "2000", "2004", "2008","2012"), 
     cex.axis=0.9)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IllRivValleyCty", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("cenScatPlot")
### * cenScatPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cenScatPlot
### Title: Scatter plot of water-quality data
### Aliases: cenScatPlot
### Keywords: hplot

### ** Examples

data(swData)
# scatter plot of Simazine concentrations
cenScatPlot(IllRivValleyCty, pname = "04035")
# scatter plot with many additional plotting arguments
par(las = 1, tcl = 0.5)
cenScatPlot(IllRivValleyCty, pname = "04035", 
            site = "05586100 Illinois River at Valley City, IL",
            ylabel = "Simazine concentration, in micrograms per liter", 
            legcex = 0.7, ylim = c(0, 0.4), yaxs = "i", cex.lab = 0.9, 
            cex.axis = 0.9, xlim = c(as.Date("1996-01-01", "%Y-%m-%d"), 
            as.Date("2012-01-01", "%Y-%m-%d")), xaxs = "i", xaxt = "n")
axdates <- c("1996-01-01", "2000-01-01", "2004-01-01", "2008-01-01",
           "2012-01-01")
axis(1, as.Date(axdates, "%Y-%m-%d"), 
     labels = c("1996", "2000", "2004", "2008", "2012"), cex.axis = 0.9)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cenScatPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("combineData")
### * combineData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: combineData
### Title: Combine water-quality sample data and continuous ancillary
###   variables
### Aliases: combineData
### Keywords: manip

### ** Examples

data(swData)
MoRivOmaha <- combineData(qwdat = qwMoRivOmaha, cqwdat = cqwMoRivOmaha, 
qwcols = c("staid", "dates", "R", "P"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("combineData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compwaveconv")
### * compwaveconv

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compwaveconv
### Title: Seasonal Wave Computation
### Aliases: compwaveconv
### Keywords: datagen

### ** Examples

# evaluate seasonal wave for specified decimal seasons
# these example decimal dates represent days at points 0.25, 0.5, and 
# 0.75 percent of the way through the year and the end of the year
dseas <- c(0.25, 0.5, 0.75, 1)
swave <- compwaveconv(cmaxt = 0.483, jmod = 2, hlife = 4)
swave[floor(360 * dseas)]
plot(seq(0, 1, 1/360), swave, typ = "l")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compwaveconv", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cqwMoRivOmaha")
### * cqwMoRivOmaha

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cqwMoRivOmaha
### Title: Continuously monitored (daily) data for 06610000 Missouri River
###   at Omaha, Neb.
### Aliases: cqwMoRivOmaha
### Keywords: datasets datasets

### ** Examples

  data(swData)
  
  # summary of water-quality concentrations
  apply(cqwMoRivOmaha[,3:8], 2, summary)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cqwMoRivOmaha", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("examplecavdat")
### * examplecavdat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: examplecavdat
### Title: Example continuous ancillary variable data.
### Aliases: examplecavdat
### Keywords: datasets datasets

### ** Examples

data(swData)
head(examplecavdat)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("examplecavdat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("examplecavmat")
### * examplecavmat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: examplecavmat
### Title: Example continuous ancillary variable matrix.
### Aliases: examplecavmat
### Keywords: datasets

### ** Examples

data(swData)
head(examplecavmat)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("examplecavmat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("examplecdatsub")
### * examplecdatsub

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: examplecdatsub
### Title: Example water-quality data.
### Aliases: examplecdatsub
### Keywords: datasets

### ** Examples

  data(swData)
  head(examplecdatsub)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("examplecdatsub", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("examplecentmp")
### * examplecentmp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: examplecentmp
### Title: Example logical vector.
### Aliases: examplecentmp

### ** Examples

data(swData)
examplecentmp



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("examplecentmp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("exampleclog")
### * exampleclog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: exampleclog
### Title: Example of logarithmically transformed concentration data.
### Aliases: exampleclog

### ** Examples

data(swData)
exampleclog



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("exampleclog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("exampleqwcols")
### * exampleqwcols

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: exampleqwcols
### Title: Example data indicators.
### Aliases: exampleqwcols

### ** Examples

  data(swData)
  exampleqwcols



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("exampleqwcols", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("examplestpars")
### * examplestpars

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: examplestpars
### Title: Example matrix for internal use.
### Aliases: examplestpars

### ** Examples

  data(swData)
  examplestpars



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("examplestpars", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("exampletndlin")
### * exampletndlin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: exampletndlin
### Title: Example numeric vector used internally.
### Aliases: exampletndlin

### ** Examples

data(swData)
head(exampletndlin)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("exampletndlin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("exampletndlinpr")
### * exampletndlinpr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: exampletndlinpr
### Title: Example numeric vector used internally.
### Aliases: exampletndlinpr

### ** Examples

data(swData)
head(exampletndlinpr)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("exampletndlinpr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("exampletseas")
### * exampletseas

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: exampletseas
### Title: Example numeric vector used internally.
### Aliases: exampletseas

### ** Examples

data(swData)
head(exampletseas)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("exampletseas", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("exampletseaspr")
### * exampletseaspr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: exampletseaspr
### Title: Example numeric vector used internally.
### Aliases: exampletseaspr

### ** Examples

data(swData)
head(exampletseaspr)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("exampletseaspr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("exampletyr")
### * exampletyr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: exampletyr
### Title: Example numeric vector used internally.
### Aliases: exampletyr

### ** Examples

data(swData)
head(exampletyr)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("exampletyr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("exampletyrpr")
### * exampletyrpr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: exampletyrpr
### Title: Example numeric vector used internally.
### Aliases: exampletyrpr

### ** Examples

data(swData)
head(exampletyrpr)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("exampletyrpr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fitMod")
### * fitMod

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fitMod
### Title: Internal function that fits the seawaveQ model.
### Aliases: fitMod
### Keywords: models regression survival ts

### ** Examples

data(swData)
myRes <- fitMod(cdatsub=examplecdatsub, cavdat=examplecavdat, 
yrstart=1995, yrend=2003, tndbeg=1995, tndend=2003, tanm="myfit3", 
pnames=c("04041"), qwcols=c("R", "P"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fitMod", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fitMod2")
### * fitMod2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fitMod2
### Title: Internal function that fits the seawaveQ model with restricted
###   cubic splines.
### Aliases: fitMod2
### Keywords: models multivariate regression survival ts

### ** Examples

data(swData)
myRes <- fitMod2(cdatsub = examplecdatsub, cavdat = examplecavdat, 
yrstart = 1995, yrend = 2003, tndbeg = 1995, tndend = 2003, 
tanm = "myfit3", pnames = c("04041"), qwcols = c("R", "P"),
mclass = 2, numknots = 4)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fitMod2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fitswavecav")
### * fitswavecav

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fitswavecav
### Title: Fit seasonal wave and continuous ancillary data for trend
###   analysis
### Aliases: fitswavecav
### Keywords: models regression survival ts

### ** Examples

data(swData)
modMoRivOmaha<-combineData(qwdat=qwMoRivOmaha, cqwdat=cqwMoRivOmaha)
myfit1 <- fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha,
tanm="myfit1", pnames=c("04035", "04037", "04041"), yrstart=1995,
yrend=2003, tndbeg=1995, tndend=2003, iwcav=c("flowa30","flowa1"),
dcol="dates", qwcols=c("R","P"))
 ## Not run: 
##D myfit2 <- fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha,
##D tanm="myfit2", pnames=c("04035", "04037", "04041"), yrstart=1995,
##D yrend=2003, tndbeg=1995, tndend=2003, iwcav=c("seda30","seda1"),
##D dcol="dates", qwcols=c("R","P"))
##D myfit3 <- fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha,
##D tanm="myfit3", pnames=c("04035", "04037", "04041"), yrstart=1995,
##D yrend=2003, tndbeg=1995, tndend=2003, iwcav=c("flowa30","flowa1",
##D "seda30", "seda1"), dcol="dates", qwcols=c("R","P"))
## End(Not run)
# trend model results
myfit1[[1]]
# example regression call
myfit1[[2]][[1]]
# first few lines of observed concentrations
head(myfit1[[3]])
# first few lines of predicted concentrations
head(myfit1[[4]])
# summary statistics for predicted concentrations
head(myfit1[[5]])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fitswavecav", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("prepData")
### * prepData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: prepData
### Title: Prepares concentration data and continuous ancillary data
### Aliases: prepData
### Keywords: manip

### ** Examples

data(swData)
modMoRivOmaha<-combineData(qwdat=qwMoRivOmaha, cqwdat=cqwMoRivOmaha)
preppedDat <- prepData(modMoRivOmaha, cqwMoRivOmaha, yrstart=1995, 
yrend=2003, dcol="dates", pnames=c("04035", "04037", "04041"),  
iwcav=c("flowa30","flowa1"), qwcols=c("R","P"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("prepData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rosBoxPlot")
### * rosBoxPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rosBoxPlot
### Title: Boxplot of water-quality data
### Aliases: rosBoxPlot
### Keywords: hplot

### ** Examples

data(swData)
# summary of water-quality concentrations
apply(IllRivValleyCty[, grep("P[[:digit:]]", 
      dimnames(IllRivValleyCty)[[2]])], 2, summary)
# simple boxplot of water-quality concentrations
rosBoxPlot(IllRivValleyCty)
# same boxplot function with many additional plotting arguments
rosBoxPlot(IllRivValleyCty, 
           site="05586100 Illinois River at Valley City, IL", 
           log="y", yaxt="n", ylim=c(0.0000001, 1), qwcols=c("R", "P"),
           ylab=c("Concentration, micrograms per liter"), 
           col="skyblue1", cex.axis=0.7, cex.sub=0.8, 
           par(tcl=0.5, las=1, yaxs="i", mgp=c(3, 0.5, 0), 
               mar=c(5, 5, 2, 2),cex.main=0.9))
axis(2, 
     at=c(0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1), 
     labels=c("0.0000001", "0.000001","0.00001", "0.0001", "0.001",
              "0.01", "0.1", "1"), cex.axis=0.7)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rosBoxPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("seawaveQPlots")
### * seawaveQPlots

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: seawaveQPlots
### Title: Internal function that generates plots of data and model
###   results.
### Aliases: seawaveQPlots
### Keywords: dplot hplot

### ** Examples

data(swData)
myPlots <- seawaveQPlots(stpars=examplestpars, cmaxt=0.4808743, 
tseas=exampletseas, tseaspr=exampletseaspr, tndlin=exampletndlin,
tndlinpr=exampletndlinpr, cdatsub=examplecdatsub, cavdat=examplecavdat, 
cavmat=examplecavmat, clog=exampleclog, centmp=examplecentmp, 
yrstart=1995, yrend=2003, tyr=exampletyr, tyrpr=exampletyrpr, 
pnames=c("04041"), tanm="examplePlots04041")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("seawaveQPlots", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("seawaveQPlots2")
### * seawaveQPlots2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: seawaveQPlots2
### Title: Internal function that generates plots of data and model
###   results.
### Aliases: seawaveQPlots2
### Keywords: dplot hplot

### ** Examples

data(swData)
myPlots <- seawaveQPlots(stpars=examplestpars, cmaxt=0.4808743, 
tseas=exampletseas, tseaspr=exampletseaspr, tndlin=exampletndlin,
tndlinpr=exampletndlinpr, cdatsub=examplecdatsub, cavdat=examplecavdat, 
cavmat=examplecavmat, clog=exampleclog, centmp=examplecentmp, 
yrstart=1995, yrend=2003, tyr=exampletyr, tyrpr=exampletyrpr, 
pnames=c("04041"), tanm="examplePlots04041", mclass = 2, numknots = 4)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("seawaveQPlots2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
