#' fits SEAWAVE-Q Model with continuous ancillary variables (streamflow
#' anomalies and other continuous variables such as conductivity or
#' sediment) 
#' @param cdata is the concentration data
#' @param cavdata is the continuous (daily) ancillary data
#' @param tanm is an a character identifier that names the trend 
#' analysis run.  It is used to label output files.
#' @param pnames are the parameters (water-quality constituents) to 
#' analyze (if using USGS parameters, omit the 
#' the starting 'P', such as "00945" for sulfate).  
#' @param yrstart is the starting year of the analysis.  Zero means 
#' the start date will be determined by the start date of cavdata, 
#' the continuous ancillary data.
#' @param yrend is the ending year of the analysis.  Zero means the 
#' end date will be determined by the end date of cavdata, the 
#' continuous ancillary data.
#' @param tndbeg is the beginning (in whole or decimal years) of the 
#' trend period. Zero means the begin date will be the beginning of the
#' concentration data, cdata.
#' @param tndend is the end (in whole or decimal years) of the trend 
#' period. Zero means the end date will be the end of the concentration
#' data, cdata.
#' @param iwcav is a character variable indicating which continuous
#' ancillary variables to include
#' @dcol is the column name for the dates, should be the same for 
#' both cdata and cavdata
#' @qwcols is a character vector with the beginning of the
#' column headers for remarks code (default is R), and beginning of 
#' column headers for concentration data (default is P for parameter).
#' @examples
#' myfit <- fitswavecav(qwMoRivOmaha, cqwMoRivOmaha, "myrun", 
#' yrstart=1996, yrend=2002, tndbeg=1996, tndend=2002)
fitswavecav <- function(cdata, cavdata, tanm="trend1", pnames, yrstart=0, 
                        yrend=0, tndbeg=0, tndend=0, iwcav, 
                        dcol="dates", qwcols=c("R", "P")) {
  require(lubridate)
  require(survival)
  dtmes <- c("yrstart, yrend, tndbeg, tndend should all be numeric, 
            greater than or equal to 0.")
  if (!is.numeric(c(yrstart, yrend, tndbeg, tndend))) stop(dtmes)
  if ( yrstart < 0 | yrend < 0 | tndbeg < 0 | tndend < 0 ) stop(dtmes)
  if(yrstart > yrend) {yrstart <- 0; yrend <- 0}
  if(tndbeg > tndend) { tndbeg <- 0; tndend <- 0}
  
  # year function is from the lubridate package
  if(yrstart != 0) { 
    yrstart <- max(yrstart, year(min(cavdata[,dcol])))
  }
  if(yrstart == 0) { yrstart <- min(year(cavdata[, dcol])) }
  if(yrend != 0) { 
    yrend <- min(yrend, year(max(cavdata[, dcol])))
  }
  if(yrend == 0) { yrend <- year(max(cavdata[, dcol]) ) }
  if(tndbeg == 0) {tndbeg <- yrstart}
  if(tndend == 0) {tndend <- yrend}
  
  cat("Trend begin is", tndbeg, "Trend end is", tndend, "\n", sep=" ")
  # continous ancillary variables
  cavdata <- subset(cavdata, year(cavdata[,dcol]) >= yrstart & 
                      year(cavdata[, dcol]) <= yrend)
  # concentration data
  cdata <- subset(cdata, year(cdata[,dcol]) >= yrstart & 
                    year(cdata[, dcol]) <= yrend)

  # ready to do analysis
  npars <- length(pnames)
  nparsmes <- c("There are no parameters to analyze. Users must pass
                some parameters names to the function using the pnames 
                argument.")
  if (npars < 1) stop(nparsmes)
  
  rnames <- paste(qwcols[1], pnames, sep='')
  pnames <- paste(qwcols[2], pnames, sep='')
  
  # set up output file for graphs 
  # output graphs to a pdf
  graphfile<-paste(tanm, ".pdf", sep="")
  pdf(graphfile, height=11.0, width=8.5)
  par(mfrow=c(2, 1), omi=c(0.5, 0.5, 0.5, 0.2), mai=c(0.5, 1, 0.5, 0.2))
  
  for  (iipar in (1:npars)) {    
    nobs <- length(cdata[1,])
    # number of continuous ancillary variables
    ncav <- length(iwcav)
    
    # column headings of concentration data
    colsc <- names(cdata)
    pckcol <- rep(FALSE, nobs)
    for (j in 1:nobs) {
      if(colsc[j]=='yrc') {
        pckcol[j] <- TRUE
      }
      if(colsc[j]=='moc') {
        pckcol[j] <- TRUE
      }
      if(colsc[j]=='dac') {
        pckcol[j] <- TRUE
      }
      if(colsc[j]=='jdayc') {
        pckcol[j] <- TRUE
      }
      if(colsc[j]==rnames[iipar]) {
        pckcol[j] <- TRUE
      }
      if(colsc[j]==pnames[iipar]) {
        pckcol[j] <- TRUE
      }
      for (k in 1:ncav) {
        if(colsc[j]==iwcav[k]) {
          pckcol[j] <- TRUE
        }
      }
      cdat <- cdata[,pckcol]
      # columns of cdat are year, month, day, julian day, remark code, 
      # concentration value, and the selected ancillary variables
    }  
    nobs <- length(cavdata[1,])
    colscav <- names(cavdata)
    pckcol <- rep(FALSE, nobs)
    for (j in 1:nobs) {
      if(colscav[j]=='yrx') {
          pckcol[j] <- TRUE
        }
      if(colscav[j]=='mox') {
          pckcol[j] <- TRUE
        }
      if(colscav[j]=='dax') {
          pckcol[j] <- TRUE
        }
      if(colscav[j]=='jday') {
          pckcol[j] <- TRUE
        }
      for (k in 1:ncav) {
          if(colscav[j]==iwcav[k]) {
            pckcol[j] <- TRUE
          }
        }
      cavdat <- cavdata[,pckcol]
      # columns of cavdat are year, month, day, julian day, and the 
      # selected continuous variables remove rows with missing values 
      # for concentration or continuous variables
    }    
      
    pcktmp <- !is.na(cdat[,6]) 
    if(length(cdat[1,]) > 6) {
      for (j in 7:length(cdat[1,])) {
        pcktmp <- pcktmp & !is.na(cdat[,j])
      }
    }
    cdat <- cdat[pcktmp,]
    pcktmp <- !is.na(cavdat[,4])
    if(length(cavdat[1,]) > 5)  {
      for (j in 5:length(cavdat[1,])) {
        pcktmp <- pcktmp & !is.na(cavdat[,j])
      }
    }
    
    cavdat <- cavdat[pcktmp,]

    if (yrstart == 0 ) { yrstart <- min(cavdat[,1]) }
    if (yrend == 0 ) { yrend <- max(cavdat[,1]) + 1 }
    yr <- cdat[[1]]
    mo <- cdat[[2]]
    da <- cdat[[3]]
    dyr <- yr + (mo - 1) / 12 + (da - 0.5) / 366
    yrpr <- cavdat[[1]]
    mopr <- cavdat[[2]]
    dapr <- cavdat[[3]]
    dyrpr <- yrpr + (mopr - 1) / 12 + (dapr - 0.5) / 366
    clog <- log10(cdat[,6])
    centmp <- cdat[,5]=='<'
    # check to see if at least 10 noncensored values
    if(sum(!centmp) > 9) {
      #  set up matrix with continuous variables
      if(length(cdat[1,])>6) {
            cavmat <- as.matrix(cdat[,7:length(cdat[1,])])
            cavmatpr <- as.matrix(cavdat[,5:length(cavdat[1,])])
      }
      # compute variables for decimal season and year and linear 
      # trend
     	tseas <- dyr - floor(dyr)
     	tyr <- dyr
  		tyrpr <- dyrpr
   		tseaspr <- (dyrpr - floor(dyrpr))
     	tmid <- (tndbeg + tndend) / 2
     	tndlin1 <- tyr-tmid
      tndlin1[tyr < tndbeg] <- tndbeg - tmid
      tndlin1[tyr > tndend] <- tndend - tmid 
   		tndlin1pr <- tyrpr-tmid
      tndlin1pr[tyrpr < tndbeg] <- tndbeg - tmid
      tndlin1pr[tyrpr > tndend] <- tndend - tmid
      # find cmaxt (decimal season of max concentration)
      tmpsm <- supsmu(tseas, clog)
    	xsm <- tmpsm$x
      ysm <- tmpsm$y
      nsm <- length(ysm)
      cmaxt <- xsm[order(ysm)[nsm]]
        
      # stpars2 and aovout2 store the model output
      # originally there were rows for each station.  
      # this version is for one station at a time.
      # There are now two rows so it can still be a matrix, but the 
      # second row is not used.
      # nexvars is the number of explanatory variables (wave, trend, 
      # and continuous variables, if any)
      nexvars <- 2 + length(cdat[1,]) - 6
      stpars2 <- matrix(nrow=2,ncol=4 + 2 * (nexvars + 1))
      aovout2 <- vector('list', 2)
      # parxxx and aovtmp are temporary objects to store results 
      # for 56 model possibilities
      parxxx <- matrix(nrow=56, ncol=3 + 2 * (nexvars + 1))
      aovtmp <- vector('list',56)
      # ready to loop through 56 model choices 
      # (14 models x 4 halflives)
      for (j in 1:14) {
        for (k in 1:4) {
          j2 <- (j - 1) * 4 + k
          awave <- compwaveconv(cmaxt, j, k)
          ipkt <- floor(360 * tseas)
          ipkt[ipkt==0] <- 1
          wavest <- awave[ipkt]
          ipkt <- floor(360 * tseaspr)
          ipkt[ipkt==0] <- 1
          wavestpr <- awave[ipkt]
          indcen <- !centmp
          intcpt <- rep(1, length(wavest))
          intcptpr <- rep(1, length(wavestpr))
          xxxmat <- cbind(intcpt, wavest, tndlin1)
          if (length(cdat[1,]) > 6) { 
            xxxmat <- cbind(xxxmat, cavmat) 
          }
          nctmp <- length(xxxmat[1,])
          clogtmp <- clog
              
          # requires survival package
          cat("j is", j, "k is", k, "length awave is", length(awave),
              "cmaxt is", cmaxt, "dims of xxxmat", dim(xxxmat), 
              "\n", sep=" ")
          tmpouta <- survreg(Surv(time=clogtmp, time2=indcen, 
                                  type='left') ~ xxxmat - 1,
                             dist='gaussian')
          parxxx[j2,] <- c(j2, tmpouta$scale, tmpouta$loglik[2],
                           tmpouta$coef, 
                           summary(tmpouta)$table[1:nctmp, 2]) 
          aovtmp[[j2]] <- summary(tmpouta)
        }
      }
		  # find largest likelihood (smallest negative likelihood)
		  likxxx <- (-parxxx[,3])
		  # eliminate models with negative coefficient for the seasonal wave
		  likxxx[parxxx[,5]<0] <- NA
		  # add 1 to likelihood for double humps (changed to zero for now)
		  likxxx[25:56] <- likxxx[25:56] + 0
		  pckone <- order(likxxx)[1]
		  stpars2[1,] <- c(parxxx[pckone,], cmaxt)
		  aovout2[[1]] <- aovtmp[[pckone]]
          
		  # produce plots for selected model
      pckone <- stpars2[1,1]
		  mod1 <- floor((pckone - 1) / 4) + 1
		  hlife1 <- pckone - (mod1 - 1) * 4
		  cmax1 <- cmaxt
		  ipkt <- floor(360 * tseas)
		  ipkt[ipkt==0] <- 1
		  wavexx <- compwaveconv(cmax1, mod1, hlife1)
		  wavest <- wavexx[ipkt]
		  ipktpr <- floor(360 * tseaspr)
		  ipktpr[ipktpr==0] <- 1
		  wavestpr <- wavexx[ipktpr]
		  xxxmat <- cbind(intcpt, wavest, tndlin1)
		  if(length(cdat[1,]) > 6) {
		    xxxmat <- cbind(xxxmat, cavmat)
		  }
		  xxxmatpr <- cbind(intcptpr, wavestpr, tndlin1pr)
		  if(length(cdat[1,]) > 6) {
		    xxxmatpr <- cbind(xxxmatpr, cavmatpr)
		  }
		  partmp <- stpars2[1, 4:(4 + length(xxxmat[1,]) - 1)]
		  fitadjx02 <- xxxmatpr %*% partmp
		  fitadjx12 <- as.matrix( partmp[1] * xxxmatpr[,1] + partmp[3] * 
                                xxxmatpr[,3])
		  cresx02 <- clog - xxxmat %*% partmp
		  fitadjxdat <- xxxmat %*% partmp
		  scltmp2 <- stpars2[1, 2]
		  cresx02std <- (cresx02) / scltmp2
		  ncenx <- sum(centmp)
		  # replace censored residuals with 
      # conditional normal random variables
		  cresx02std[centmp] <- qnorm(runif(ncenx) * 
                                    pnorm(cresx02std[centmp]))
		  cadjx0 <- clog
		  ylow <- floor(min(c(cadjx0, fitadjx02, fitadjx12)) -0.25)
		  yup <- ceiling(max(c(cadjx0, fitadjx02, fitadjx12)) + 1)
		  ytck <- 0.02 * (yup - ylow)
		  xlow <- yrstart
		  xup <- yrend + 1
		  xtck <- 0.012 * (xup - xlow)
		  
		  # ts plot of data and model 
		  ytmp <- cadjx0
		  ytmpxx <- fitadjx02
		  ytmpxx2 <- fitadjx12
		  sp95 <- 1.645 * scltmp2
		  plot(c(tyr, tyrpr), c(ytmp, ytmpxx), type='p', pch=' ', xaxs='i',
           yaxs='i', xaxt='n', yaxt='n', xlab='', ylab='', 
           xlim=c(xlow, xup), ylim=c(ylow, yup))
		  points(tyr[centmp], ytmp[centmp], pch=1, cex=1, col=2)
		  points(tyr[!centmp], ytmp[!centmp], pch=16,cex=0.9, col=2)
		  lines(tyrpr, ytmpxx, col=1, lwd=1.5)
		  lines(tyrpr, ytmpxx2, lwd=3, col="black")
		  for (j in seq(xlow, xup - 1, 1)) {
		    mtext(side=1, line=0.5, at=j + 0.5, cex=0.75, as.character(j))
		    lines(c(j, j), c(ylow, ylow + ytck), lwd=1)
		    lines(c(j, j), c(yup, yup - ytck), lwd=1)
		  }
		  for (j in seq(ylow, yup, 1)) {
		    mtext(side=2, line=0.5, at=j, adj=1, cex=0.75, las=1, 
		          format(10^j, scientific=FALSE, big.mark=','))
		    lines(y=c(j, j), x=c(xlow, xlow + xtck), lwd=1)
		    lines(y=c(j, j), x=c(xup, xup - xtck), lwd=1)
		  }
		  text(xlow + 0.5 * (xup - xlow), yup - 2.5 * ytck, adj=0, 
           cex=0.7, 'Measured concentrations')
		  points(xlow + 0.47 * (xup - xlow), yup - 2.5 * ytck, pch=16, 
             cex=0.8, col=2)
		  text(xlow + 0.5 * (xup - xlow), yup - 5 * ytck, adj=0, cex=0.7,
		       'Nondetections')
		  points(xlow + 0.47 * (xup - xlow), yup - 5 * ytck, pch=1, cex=0.8, 
             col=2)
		  text(xlow + 0.5 * (xup - xlow), yup - 7.5 * ytck, adj=0, cex=0.7,
		       'Fitted concentrations (SWAVE-CAV)')
		  lines(c(xlow + 0.45 * (xup - xlow), xlow + 0.49 * (xup - xlow)), 
		        c(yup - 7.5 * ytck, yup - 7.5 * ytck), lwd=1.5, col=1)
		  text(xlow + 0.5 * (xup - xlow), yup - 10 * ytck, adj=0, cex=0.65, 
           'Trend')
		  lines(c(xlow + 0.45 * (xup - xlow), xlow + 0.49 * (xup - xlow)), 
		        c(yup - 10 * ytck, yup - 10 * ytck), lwd=3.0, col=1)
		  mtext(side=3, line=0.5, adj=0.1, cex=1, pnames[iipar])
		  mtext(side=3, line=0.5, adj=0.5, cex=1, tanm)
		  mtext(side=2, outer=FALSE, line=3, cex=0.8, 
		        'Concentration, in micrograms per liter')
		        
		  #  plot of 50th and 95th percentile in arithmetic space
		  ylow <- 0
		  tmpmax <- ceiling(quantile(10^c(fitadjx02 + 1.96 * scltmp2), 
		                            prob=0.999) * 200) / 200
		  if(tmpmax <= 0.025) { yup <- tmpmax }
		  if(tmpmax > 0.025 & tmpmax <= 0.05) { yup <- 0.05 }
		  if(tmpmax > 0.05 & tmpmax <= 0.1) { yup <- 0.1 }
		  if(tmpmax > 0.1 & tmpmax <= 0.15) { yup <- 0.15 }
		  if(tmpmax > 0.15 & tmpmax <= 0.2) { yup <- 0.2 }
		  if(tmpmax > 0.2 & tmpmax <= 0.25) { yup <- 0.25 }
		  if(tmpmax > 0.25 & tmpmax <= 0.5) { yup <- 0.5 }
		  if(tmpmax > 0.5 & tmpmax <= 1.0) { yup <- 1.0 }
		  if(tmpmax > 1 & tmpmax <= 1.5) { yup <- 1.5 }
		  if(tmpmax > 1.5 & tmpmax <= 2) { yup <- 2 }
		  if(tmpmax > 2 & tmpmax <= 2.5) { yup <- 2.5 }
		  if(tmpmax > 2.5 & tmpmax <= 5) { yup <- 5 }
		  if(tmpmax > 5) { yup <- ceiling(tmpmax / 5) * 5 }
		  ystp <- yup / 5 
		  ytck <- 0.02 * (yup - ylow)
		  xlow <- yrstart
		  xup <- yrend + 1
		  xtck <- 0.012 * (xup - xlow)
		  ytmpxx <- 10^fitadjx02
		  ytmpxx95 <- 10^(fitadjx02 + 1.96 * scltmp2)
		  plot(c(tyrpr), c(ytmpxx), type='p', pch=' ',xaxs='i', yaxs='i', 
           xaxt='n', yaxt='n', xlab='', ylab='',  xlim=c(xlow, xup), 
           ylim=c(ylow, yup))
		  lines(tyrpr, ytmpxx, col=1, lwd=2)
		  lines(tyrpr, ytmpxx95, col=2, lwd=2)
		  for (j in seq(xlow, xup - 1, 1)) {
		    mtext(side=1, line=0.5, at=j + 0.5, cex=0.75, as.character(j))
		    lines(c(j, j), c(ylow, ylow + ytck), lwd=1)
		    lines(c(j, j), c(yup, yup - ytck), lwd=1)
		  }
		  for (j in seq(ylow, yup, ystp)) {
		    mtext(side=2, line=0.5, at=j, adj=1, cex=0.75, las=1, 
		          format(j, scientific=FALSE, big.mark=','))
		    lines(y=c(j, j), x=c(xlow, xlow + xtck), lwd=1)
		    lines(y=c(j, j), x=c(xup, xup - xtck), lwd=1)
		  }
		  text(xlow + 0.5 * (xup - xlow), yup - 3 * ytck, adj=0, cex=0.8, 
		       'Fitted 95th percentile concentration')
		  lines(c(xlow + 0.45 * (xup - xlow), xlow + 0.49 * (xup - xlow)),
		        c(yup - 3 * ytck, yup - 3 * ytck), lwd=2, col=2)
		  text(xlow + 0.5 * (xup - xlow), yup - 6 * ytck, adj=0, cex=0.8,
		       'Fitted median concentration')
		  lines(c(xlow + 0.45 * (xup - xlow), xlow + 0.49 * (xup - xlow)), 
		        c(yup - 6 * ytck, yup - 6 * ytck), lwd=2, col=1)
		  mtext(pnames[iipar], side=3, line=0.5, adj=0.1, cex=1)
		  mtext(tanm, side=3, line=0.5, adj=0.5, cex=1)
		  mtext('Concentration, in micrograms per liter', 
            side=2, outer=FALSE, line=3, cex=0.8)
          
		  # replace censored values with 
      # conditional normal random variables
		  cadjx0 <- clog
		  cadjx0[centmp] <- fitadjxdat[centmp] + cresx02std[centmp] * 
            scltmp2
		  
		  #  plot of fitted versus observed (randomized censored values)
		  ylow <- floor(min(c(cadjx0, fitadjxdat)) - 0.05)
		  yup <- ceiling(max(c(cadjx0, fitadjxdat)) + 0.05)
		  ytck <- 0.02 * (yup - ylow)
		  xlow <- ylow
		  xup <- yup
		  xtck <- 0.5 * ytck
		  plot(fitadjxdat, cadjx0, type='p', pch=' ', xlim=c(xlow, xup), 
		       ylim=c(ylow, yup), xaxs='i', yaxs='i', xaxt='n', yaxt='n', 
           xlab='', ylab='')
		  points(fitadjxdat[centmp], cadjx0[centmp], pch=1, cex=1, col=2)
		  points(fitadjxdat[!centmp], cadjx0[!centmp], pch=16, cex=0.9, 
             col=2)
		  lines(c(xlow, xup), c(xlow,xup), col=1, lwd=2)
		  for (j in seq(xlow, xup, 1)) {
		    mtext(side=1, line= 0.5, at=j, cex= 0.75, 
		          format(10^j, scientific=FALSE, big.mark=','))
		    lines(c(j, j), c(ylow, ylow + ytck), lwd=1)
		    lines(c(j, j), c(yup, yup - ytck), lwd=1)
		  }
		  for (j in seq(ylow, yup, 1)) {
		    mtext(side=2, line=0.5, at=j, adj=1, cex=0.75, las=1,
		          format(10^j, scientific=FALSE, big.mark=','))
		    lines(y=c(j, j),x=c(xlow, xlow + xtck), lwd=1)
		    lines(y=c(j, j), x=c(xup, xup - xtck), lwd=1)
		  }
		  mtext(side=3, line=0.5, adj=0.1, cex=1, pnames[iipar])
		  mtext(side=3, line=0.5, adj=0.5, cex=1, tanm)
		  mtext('Fitted Concentration, in micrograms per liter', side=1, 
            outer=FALSE, line=1.5, cex=0.8)
		  mtext('Measured Concentration, in micrograms per liter\n(censored 
            data randomized)', side=2, outer=FALSE, line=3.5, cex=0.8)
		        
		  #!!!! add standardized residual plots
		  xlow0 <- xlow
		  xup0 <- xup
		  ylow <- floor(min(c(cresx02std)) - 0.25)
		  yup <- ceiling(max(c(cresx02std)) + 0.25)
		  ytck <- 0.02 * (yup - ylow)
		  for (rplt in 1:3) {
		    ytmpsr <- cresx02std
		    #  residuals versus fitted
		    if(rplt==1) {  
		      xlow <- xlow0
		      xup <- xup0
		      xstp <- 1
		      xtck <- .012 * (xup - xlow)
		    }    
		    #  residuals versus time
		    if(rplt==2) {
		      xlow <- yrstart
		      xup <- yrend + 1
		      xstp <- 1
		      xtck <- 0.012 * (xup - xlow)
		    }
		    #  residuals versus month
		    if(rplt==3) {
		      xlow <- 0
		      xup <- 12
		      xstp <- 1
		      xtck <- 0.012 * (xup - xlow)
		    }
		    plot(tyr, ytmpsr, pch='', xaxs='i', yaxs='i', xaxt='n', 
             yaxt='n', xlab='', ylab='', xlim=c(xlow, xup), 
             ylim=c(ylow, yup))
		    lines(c(xlow, xup), c(0, 0),col=1, lwd=2)
		    if(rplt==1) {
		      points(fitadjxdat[centmp], ytmpsr[centmp], pch=1, cex=1, 
                 col=2)
		      points(fitadjxdat[!centmp], ytmpsr[!centmp], pch=16, cex=0.9, 
                 col=2)
		    }
		    if(rplt==2) { 
          points(tyr[centmp], ytmpsr[centmp], pch=1, cex=1, col=2)
          points(tyr[!centmp], ytmpsr[!centmp], pch=16, cex=0.9, col=2)
        }
        if(rplt==3) { 
          points(tseas[centmp] * 12, ytmpsr[centmp], pch=1, cex=1, 
                 col=2)
          points(tseas[!centmp] * 12, ytmpsr[!centmp], pch=16, cex=0.9, 
                 col=2)
        }
        for (j in seq(xlow, xup, xstp)) {
          if(rplt==1) {
            mtext(side=1, line=0.5, at=j, cex=0.75, 
                  format(10^j, scientific=FALSE, big.mark=','))
          }
          if(rplt==2 & j < xup) {
            mtext(side=1, line=0.5, at=j + 0.5, cex=0.75, 
                  as.character(j))
          }
          lines(c(j, j), c(ylow, ylow + ytck), lwd=1)
          lines(c(j, j),c(yup, yup - ytck), lwd=1)
        }
        if(rplt==3) {
          mtext(c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 
                  'Aug', 'Sept', 'Oct', 'Nov', 'Dec'), side=1, 
                line=0.5, at=seq(0.5, 11.5, 1), cex=0.75)
        }
        for (j in seq(ylow, yup, 1)) {
          mtext(as.character(j), side=2, line=0.5, at=j, adj=1, las=1, 
                cex=0.75)
          lines(y=c(j, j), x=c(xlow, xlow + xtck), lwd=1)
          lines(y=c(j, j), x=c(xup, xup - xtck), lwd=1)
        }
        mtext('Standardized residual\n(censored residuals randomized)',
              side=2, outer=FALSE, line=3, cex=0.8)
        mtext(pnames[iipar], side=3, line=0.5, adj=0.1, cex=1)
        mtext(tanm, side=3, line=0.5, adj=.5, cex=1)  
        if(rplt==1) {
          mtext('Fitted Concentration, in micrograms per liter', side=1,
                line=1.5, cex=0.8)
        }
      }
      #  close loop for checking if >10 uncensored values  
    }
    #  prepare output
    stpars2 <- round(stpars2, 5)
    row.names(stpars2) <- NULL
    stparsout <- matrix(stpars2[1,], nrow=1)
    if(iipar==1) {
      stparsoutall <- stparsout
    }
    if(iipar > 1) {
      stparsoutall <- rbind(stparsoutall, stparsout)
    }
  }
  dev.off()
  mod1 <- floor((stparsoutall[,1] - 1) / 4) + 1
  hlife1 <- stparsoutall[,1] - (mod1 - 1) * 4
  nxtmp <- length(stparsoutall[1,])
  stparsoutall <- cbind(mod1, hlife1, stparsoutall[,nxtmp], 
                        matrix(stparsoutall[, -c(1, nxtmp)], 
                               nrow=dim(stparsoutall)[1]))
  stparsoutall <- data.frame(pnames, stparsoutall)
  if(iwcav[1] != 'none') { 
    names(stparsoutall) <- c('pname', 'smod', 'hlife', 'cmax', 'scl', 
                             'loglik', 
                             paste('c', c('int', 'wave', 'tnd', iwcav), 
                                   sep=''), 
                             paste('se', c('int', 'wave', 'tnd', iwcav),
                                   sep=''))
  }
  if(iwcav[1] == 'none') {
    names(stparsoutall) <- c('pname', 'smod', 'hlife', 'cmax', 'scl', 
                             'loglik',
                             paste('c', c('int', 'wave', 'tnd'), 
                                   sep=''),
                             paste('se', c('int', 'wave', 'tnd'), 
                                   sep=''))
  }
  stparsoutall
}
