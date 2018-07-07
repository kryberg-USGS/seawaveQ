fitMod2 <- function (cdatsub, cavdat, yrstart, yrend, tndbeg, tndend, tanm, 
                     pnames, qwcols, mclass = 2, numknots = numknots) {
  require(rms)
  yr <- cdatsub[[1]]
  mo <- cdatsub[[2]]
  da <- cdatsub[[3]]
  dyr <- yr + (mo - 1)/12 + (da - 0.5)/366
  yrpr <- cavdat[[1]]
  mopr <- cavdat[[2]]
  dapr <- cavdat[[3]]
  dyrpr <- yrpr + (mopr - 1)/12 + (dapr - 0.5)/366
  ccol <- paste(qwcols[2], pnames, sep = "")
  clog <- log10(cdatsub[, ccol])
  cencol <- paste(qwcols[1], pnames, sep = "")
  centmp <- cdatsub[, cencol] == "<"
  if (length(cdatsub[1, ]) > 6) {
    cavmat <- as.matrix(cdatsub[, 7:length(cdatsub[1, ])])
  }  else {
    cavmat <- as.matrix(cdatsub)
  }
  tseas <- dyr - floor(dyr)
  tyr <- dyr
  tyrpr <- dyrpr
  tseaspr <- (dyrpr - floor(dyrpr))
  tmid <- (tndbeg + tndend)/2
  
  tndlin <- tyr - tmid
  tndlin[tyr < tndbeg] <- tndbeg - tmid
  tndlin[tyr > tndend + 1] <- tndend - tmid
  tndrcs <- rcs(tndlin, numknots)

  tndlinpr <- tyrpr - tmid
  tndlinpr[tyrpr < tndbeg] <- tndbeg - tmid
  tndlinpr[tyrpr > tndend + 1] <- tndend - tmid
  tndrcspr <- rcs(tndlinpr, attributes(tndrcs)$parms)

  tmpsm <- supsmu(tseas, clog)
  xsm <- tmpsm$x
  ysm <- tmpsm$y
  nsm <- length(ysm)
  cmaxt <- xsm[order(ysm)[nsm]]
  nexvars <- 2 + (length(cdatsub[1, ]) - 6) + (numknots - 1) 
  stpars <- matrix(nrow = 2, ncol = (4 + 2 * nexvars + (numknots - 1) + 1))
  aovout <- vector("list", 1)
  aicout <- vector("list", 2)
  bicout <- vector("list", 2)
  parx <- matrix(nrow = 56, ncol = (dim(stpars)[[2]] - 1))
  aovtmp <- vector("list", 56)
  aictmp <- vector("list", 56)
  bictmp <- vector("list", 56)
  wvmsg <- paste("Computing the best seasonal wave.")
  message(wvmsg)
  for (j in 1:14) {
    for (k in 1:4) {
      j2 <- (j - 1) * 4 + k
      awave <- compwaveconv(cmaxt, j, k, mclass = 2)
      ipkt <- floor(360 * tseas)
      ipkt[ipkt == 0] <- 1
      wavest <- awave[ipkt]
      ipkt <- floor(360 * tseaspr)
      ipkt[ipkt == 0] <- 1
      wavestpr <- awave[ipkt]
      indcen <- !centmp
      intcpt <- rep(1, length(wavest))
      xmat <- cbind(intcpt, wavest, tndrcs)
      if (length(cdatsub[1, ]) > 6) {
        xmat <- cbind(xmat, cavmat)
      }
      nctmp <- length(xmat[1, ])
      clogtmp <- clog
      tmpouta <- survreg(Surv(time = clogtmp, time2 = indcen, 
                              type = "left") ~ xmat - 1, dist = "gaussian")
      parx[j2, ] <- c(mclass, j2, tmpouta$scale, tmpouta$loglik[2], 
                      tmpouta$coef, summary(tmpouta)$table[1:nctmp, 2], 
                      summary(tmpouta)$table[grep("tndlin", 
                                                  row.names(summary(tmpouta)$table)), 4])
      aovtmp[[j2]] <- summary(tmpouta)
      aictmp[[j2]] <- extractAIC(tmpouta)[2]
      bictmp[[j2]] <- extractAIC(tmpouta, k = log(length(tmpouta$linear.predictors)))[2]
    }
  }
  likx <- (-parx[, 4])
  likx[parx[, 6] < 0] <- NA
  # This could be used to penalize models with two seasons of application
  likx[25:56] <- likx[25:56] + 0
  pckone <- order(likx)[1]
  stpars[1, ] <- c(parx[pckone, ], cmaxt)
  aovout[[1]] <- aovtmp[[pckone]]
  aicout[[1]] <- aictmp[[pckone]]
  bicout[[1]] <- bictmp[[pckone]]
  
  # generalized r-squared statistic based on  the likelihood-ratio test
  # see Allison (1995, pp. 247-249)
  # Allison, Paul D. 1995. Survival Analysis Using the SAS System: 
  # A Practical Guide. Cary, NC: SAS Institute Inc.
  lrt <- -2 * aovout[[1]]$loglik[1] - (-2 * aovout[[1]]$loglik[2])
  r2 <- round(1 - exp(-lrt / aovout[[1]]$n), digits = 2)
  
  regCallFile <- paste(tanm, "_survregCall.txt", sep = "")
  resmsg <- paste("Final model survreg results saved to ", 
                  regCallFile, ".", sep = "")
  message(resmsg)
  si <- sessionInfo()
  sink(regCallFile, append = TRUE, type = "output")
  cat("\n\n", format(Sys.time(), "%A %d %b %Y %X %p %Z"), 
      sep = "")
  cat("\n", si$R.version$version.string, "\n", 
      si$otherPkgs[[grep("seawaveQ", si$otherPkgs)]]$Package, " version ", 
      si$otherPkgs[[grep("seawaveQ", si$otherPkgs)]]$Version, "\n", si$platform, 
      "\n\nFinal model survreg results for ", pnames, sep = "")
  print(aovout[[1]])
  cat("Generalized r-squared is: ", r2, "\n", sep = " ")
  cat("AIC (Akaike's An Information Criterion) is: ", round(aicout[[1]], digits = 2), 
      "\n", sep = " ")
  cat("BIC (Bayesian Information Criterion) is: ", round(bicout[[1]], digits = 2), 
      "\n", sep = " ")
  jmod <- floor((stpars[1, 2] - 1)/4) + 1
  hlife <- stpars[1, 2] - (jmod - 1) * 4
  cat("Model class is ", mclass, "\nPulse input function is ", 
      jmod, "\nHalf life is ", hlife, "\nSeasonal value of the maximum concentration is ", 
      round(cmaxt, digits = 2), ".", "\n", sep = "")
  sink()
  plotDat <- seawaveQPlots2(stpars, cmaxt, tseas, tseaspr, tndlin, tndlinpr, 
                            tndrcs, tndrcspr, cdatsub, cavdat, cavmat, clog, centmp, 
                           yrstart, yrend, tyr, tyrpr, pnames, tanm, numknots = numknots)
  myRes <- list(stpars, aovout, plotDat, tndrcspr)
  myRes
}
