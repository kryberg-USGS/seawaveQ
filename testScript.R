# test script

source('~/rPackages/seawaveq/seawaveQPlots.R')
source('~/rPackages/seawaveq/fitswavecav.R')

modMoRivOmaha<-combineData(qwdat=qwMoRivOmaha, cqwdat=cqwMoRivOmaha)
myfit <- fitswavecav(modMoRivOmaha, cqwMoRivOmaha, tanm="myfit1", 
                     pnames=c("04035", "04037", "04041"), 
                     tndbeg=1995, tndend=2003, 
                     iwcav=c("flowa30","flowa1"), qwcols=c("R","P"))

# placed in fitMod to get example data for seawavQPlots function

# examplestpars<<- stpars
# exampletseas<<-tseas
# exampletseaspr<<-tseaspr
# exampletndlin<<-tndlin
# exampletndlinpr<<-tndlinpr
# examplecavmat<<-cavmat
# exampleclog<<-clog
# examplecentmp<<-centmp
# exampletyr<<-tyr
# exampletyrpr<<-tyrpr

modMoRivOmaha<-combineData(qwdat=qwMoRivOmaha, cqwdat=cqwMoRivOmaha)
myExamp <- fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                       tanm="myexampleAIC", pnames=c("04037"), yrstart=1995, 
                       yrend=2003, tndbeg=1995, tndend=2003, 
                       iwcav=c("flowa30","flowa1"), dcol="dates", 
                       qwcols=c("R","P"))
myExamp2 <- fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                       tanm="myexampleAIC", pnames=c("04041"), yrstart=1995, 
                       yrend=2003, tndbeg=1995, tndend=2003, 
                       iwcav=c("seda30","seda1"), dcol="dates", 
                       qwcols=c("R","P"))
pe25combo<-combineData(pe25d2i1,cqwpe25i1)
myExamp3 <- fitswavecav(cdat=pe25combo, cavdat=cqwpe25i1, 
                        tanm="myexampleAIC", pnames=c("04041"), yrstart=1999, 
                        yrend=2005, tndbeg=1999, tndend=2005, 
                        iwcav=c("flowa30","flowa1"), dcol="dates", 
                        qwcols=c("R","P"))

myPreppedDat2 <- prepData(cdat=pe25combo, cavdat=cqwpe25i1, 
                         yrstart=1999, yrend=2005, dcol="dates", 
                         pnames=c("04041"), 
                         iwcav=c("flowa30","flowa1"), qwcols=exampleqwcols)

myPreppedDat <- prepData(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                         yrstart=1995, yrend=2003, dcol="dates", 
                         pnames=c("04041"), 
                         iwcav=c("flowa30","flowa1"), qwcols=exampleqwcols)

myRes <- fitMod(cdatsub=examplecdatsub, cavdat=examplecavdat, yrend=2003, 
                yrstart=1995, tndbeg=1995, tndend=2003, tanm="myexample", 
                pnames=c("04041"), qwcols=exampleqwcols)


myPlots <- seawaveQPlots(stpars=examplestpars, cmaxt=0.4808743, 
                  tseas=exampletseas, tseaspr=exampletseaspr, 
                  tndlin=exampletndlin, tndlinpr=exampletndlinpr, 
                  cdatsub=examplecdatsub, cavdat=examplecavdat, 
                  cavmat=examplecavmat, clog=exampleclog, 
                  centmp=examplecentmp, yrstart=1995, yrend=2003, 
                  tyr=exampletyr, tyrpr=exampletyrpr, pnames="04041", 
                  tanm="myexample")


myfit1 <- fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                      tanm="myfit1MoRivOmaha", pnames=c("04035", "04037", "04041"), 
                      yrstart=1995, yrend=2003, tndbeg=1995, tndend=2003, 
                      iwcav=c("flowa30","flowa1"), 
                      dcol="dates", qwcols=c("R","P"))
myfit2 <- fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                      tanm="myfit2MoRivOmaha", pnames=c("04035", "04037", "04041"), 
                      yrstart=1995, yrend=2003, tndbeg=1995, tndend=2003, 
                      iwcav=c("seda30", "seda1"), 
                      dcol="dates", qwcols=c("R","P"))
myfit3 <- fitswavecav(cdat=modMoRivOmaha, cavdat=cqwMoRivOmaha, 
                      tanm="myfit3MoRivOmaha", pnames=c("04035", "04037", "04041"), 
                      yrstart=1995, yrend=2003, tndbeg=1995, tndend=2003, 
                      iwcav=c("flowa30","flowa1", "seda30", "seda1"), 
                      dcol="dates", qwcols=c("R","P"))


myPlots <- seawaveQPlots(stpars=examplestpars, cmaxt=0.4808743, 
                         tseas=exampletseas, tseaspr=exampletseaspr, 
                         tndlin=exampletndlin, tndlinpr=exampletndlinpr, 
                         cdatsub=examplecdatsub, cavdat=examplecavdat, 
                         cavmat=examplecavmat, clog=exampleclog, 
                         centmp=examplecentmp, yrstart=1995, yrend=2003, 
                         tyr=exampletyr, tyrpr=exampletyrpr, 
                         pnames=c("04041"), tanm="examplePlots04041")