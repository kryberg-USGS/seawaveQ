seawaveqFit <- function(datinall, flowanomall, nblks, styrblk, 
                                     endyrblk, chemname, pcksta) {
  require(survival)
	# chlorpyrifosT1<-urbanfitseawaveqnew('38933',1,3,"Chlorpyrifos")
  sitelist <- urbanTrendSiteList2
  # eg, datinall=diazinonurbanzz,  flowanomall=urbanflowyy
  allstnums <- sitelist[[1]]
  allstnames <- sitelist[[3]]
  stnum <- allstnums[pcksta]
  stname <- allstnames[pcksta]
  stlab <- paste(stnum, stname)
  datstnum <- datinall[[2]]
  predstnum <- flowanomall[[1]]
  pck1 <- datstnum==stnum
  pck2 <- predstnum==stnum
  datinall <- datinall[pck1,]
  flowanomall <- flowanomall[pck2,]
  # determine total number of trends for all blocks
  ntnds <- 0
  ntb <- rep(NA, nblks)
  for (i in 1:nblks) {
    ntb[i] <- ntnds
    blklen <- endyrblk[i] - styrblk[i] + 1
    ntnds <- ntnds + blklen * (blklen + 1) / 2
  }
  blkandtndinfo <- matrix(nrow=ntnds, ncol=4)
  cnttmp <- 0
  for (iblk in 1:nblks)  {
    for (ibeg in styrblk[iblk]:(endyrblk[iblk])) {
      #   if(ibeg==styrblk[iblk]) {jbeg <- ibeg}
      #    if(ibeg>styrblk[iblk]) {jbeg <- ibeg+1}
      jbeg <- ibeg
      for (iend in jbeg:(endyrblk[iblk])) {
        cnttmp <- cnttmp + 1
        blkandtndinfo[cnttmp,] <- c(styrblk[iblk], endyrblk[iblk], ibeg, iend)
      }
    }
  }
  stpars2 <- matrix(nrow=ntnds, ncol=10)
  stpars2se <- matrix(nrow=ntnds, ncol=6)
  aovout2 <- vector('list', ntnds)
  nametmp <- c(' ', ntnds) 
  graphsheet(height=11.0, width=8.5, pages=T)
  par(mfrow=c(2,1), omi=c(.2,.2,.4,.2), mai=c(.5,1,.2,.2))     
  #begin loop for blocks       
  for (iblk in 1:nblks) {
    yrstart <- styrblk[iblk]
    yrend <- endyrblk[iblk]
    blklen <- yrend - yrstart + 1
    nobsyr <- rep(0, blklen)
    pck1 <- c(datinall[,3]>=yrstart & datinall[,3]<=yrend & 
      !is.na(datinall[,15]) & !is.na(datinall[,20]))
    datin <- datinall[pck1,]
    pck2 <- c(flowanomall[,2]>=yrstart & flowanomall[,2]<=(yrend + 1) & 
      !is.na(flowanomall[,3]))
    predin <- flowanomall[pck2,]
    clog <- log10(datin[,15])
    centmp <- datin[,14]=='<'
    tyr <- datin[,3]
    for (i in 1:blklen) {
      nobsyr[i] <- sum(tyr==(yrstart + i - 1))
    }
    # check for at least 10 uncensored values and at least 3 years of data 
    # with 4 or more obs per yr
    # at least one year with data has to be in each of the first two years and
    # last two years of the block
    if(sum(!centmp)>4 & sum(nobsyr>3)>3 & sum(nobsyr[1:3]>3)>0 & 
      sum(nobsyr[(blklen-2):blklen]>3)>0) {
      # set up flow anomalies
      qaone <- datin[,22]
      qaonepr <- predin[,5]
      qa100pr <- predin[,3]
      mtmp <- mean(qa100pr, na.rm=T)
      qa100pr <- qa100pr - mtmp
      qa100 <- datin[,20]
      qa100 <- qa100 - mtmp
      qa10pr <- predin[,4]
      qa10 <- datin[,21]
      # compute decimal years and seasons
      tseas <- (datin[,4] - 1) / 12 + datin[,5] / 366
      tyr <- datin[,3] + tseas
      tyrpr <- predin[,2]
      tseaspr <- (tyrpr - floor(tyrpr))
      # find cmaxtm, time of maximum concentration
      tmpsm <- supsmu(tseas, clog)
      xsm <- tmpsm$x
      ysm <- tmpsm$y
      nsm <- length(ysm)
      cmaxt <- xsm[order(ysm)[nsm]]
      # ready to loop through trend choices
      cnttmp <- 0
      for (ibeg in styrblk[iblk]:(endyrblk[iblk])) {
        jbeg <- ibeg
        #     if(ibeg==styrblk[iblk]) {jbeg <- ibeg}
        #     if(ibeg>styrblk[iblk]) {jbeg <- ibeg+1}
        for (iend in jbeg:(endyrblk[iblk])) {
          cnttmp <- cnttmp + 1
          # check data requirements for fitting current trend
          #    if(ibeg==iend) {chkdat <- 1}
          if(cnttmp==1) {
          	chkdat <- 1
          }
          #     if(ibeg<iend) {chkdat <- 2}
          #        if(ibeg<iend) {
          if(cnttmp>1)  {
            jbeg <- ibeg-styrblk[iblk] + 1
            jend <- iend-styrblk[iblk] + 1
            chkdat <- 2
            if(sum(nobsyr[1:jbeg]>3)==0) {
              chkdat <- 0
            }
            if(sum(nobsyr[jend:blklen]>3)==0) {
            	chkdat <- 0
            }
            if(sum(nobsyr[1:jend]>3)<2) {
            	chkdat <- 0
            }
            if(sum(nobsyr[jbeg:blklen]>3)<3) {
            	chkdat <- 0
            }
            if(iend==2008) {
            	chkdat <- 0
            }
            #       if(iblk==nblks & iend==endyrblk[nblks]) {chkdat <- 0}
            #       if(sum(nobsyr[1:jend]>3)<2) {chkdat <- 0}
            #       if(sum(nobsyr[jbeg:blklen]>3)<2) {chkdat <- 0}
          }
          if(chkdat>0)  {
            tmid <- ibeg + (iend + 1 - ibeg) / 2
            hwd <- (iend + 1 - ibeg) / 2
            tndlin1 <- tyr - tmid  
            tndlin1pr <- tyrpr - tmid 
            tndlin1[tndlin1<(-hwd)] <- -hwd
            tndlin1[tndlin1>hwd] <- hwd
            tndlin1pr[tndlin1pr<(-hwd)] <- -hwd
            tndlin1pr[tndlin1pr>hwd] <- hwd
            parxxx <- matrix(nrow=56, ncol=8)
            parxxxse <- matrix(nrow=56, ncol=6)
            aovtmp <- vector('list', 56)                                       
            for (j in 1:14) {
              for (k in 1:4) {
                awave <- compWave(cmaxt, j, k)
                ipkt <- floor(360*tseas)
                ipkt[ipkt==0] <- 1
                wavest <- awave[ipkt]
                ipkt <- floor(360 * tseaspr)
                ipkt[ipkt==0] <- 1
                wavestpr <- awave[ipkt]      
                indcen <- !centmp
                j2 <- (j-1) * 4 + k
                tmpv1 <- qa100
                tmpv1pr <- qa100pr
                tmpv2 <- qa10
                tmpv2pr <- qa10pr
                tmpv3 <- qaone
                tmpv3pr <- qaonepr
                clogtmp <- clog
                if (chkdat==2) {      
                  tmpouta <- survReg(Surv(time=clogtmp, time2=indcen, 
                                          type='left') ~ wavest + tmpv1 + tmpv2
                                     +tmpv3 + tndlin1, dist='gaussian')
                  parxxx[j2,] <- c(summary(tmpouta)$table[1:6,1], 
                                   tmpouta$scale, tmpouta$loglik[2]) 
                  parxxxse[j2,] <- c(summary(tmpouta)$table[1:6,2])         
                  aovtmp[[j2]] <- summary(tmpouta)
                }
                if (chkdat==1) {
                  tmpouta <- survReg(Surv(time=clogtmp, time2=indcen, 
                                          type='left') ~ wavest + tmpv1 + tmpv2 
                                     +tmpv3, dist='gaussian')
                  parxxx[j2,] <- c(summary(tmpouta)$table[1:5,1], 0, 
                                   tmpouta$scale, tmpouta$loglik[2]) 
                  parxxxse[j2,] <- c(summary(tmpouta)$table[1:5,2], 0)         
                  aovtmp[[j2]] <- summary(tmpouta)
                }  
              }
            }
            ssresxxx <- (-parxxx[,8])
            # leave out cases with negative coefficient for seasonal wave
            ssresxxx[parxxx[,2]<0] <- NA
            # add 2 to likelihood for double humps
            # Need to add statement about why we would want to do this
            ssresxxx[25:56] <- ssresxxx[25:56] + 2
            pckone <- order(ssresxxx)[1]
            nrtmp <- ntb[iblk] + cnttmp
            stpars2[nrtmp,] <- c(parxxx[pckone,], cmaxt, pckone)
            stpars2se[nrtmp,] <- parxxxse[pckone,]
            aovout2[[nrtmp]] <- aovtmp[[pckone]]
            # end check to see if data is sufficient to fit trend
          }
          # end loops for beginning and ending years of trend
        }
      }
      # plot results for 5 best trends in block
      pckxxx <- blkandtndinfo[,1]==yrstart & blkandtndinfo[,2]==yrend
      pckxxx2 <- !is.na(stpars2[,8])
      stparsblk <- stpars2[pckxxx & pckxxx2,]
      ibegblk <- blkandtndinfo[pckxxx & pckxxx2,3]
      iendblk <- blkandtndinfo[pckxxx & pckxxx2,4]
      lentmp <- length(ibegblk)
      lik3rd <- sort(stparsblk[,8])[lentmp-4]
      for (itmp in 1:lentmp)    {
        if(itmp>1 & stparsblk[itmp,8]>=lik3rd) {
          diflikx <- round(stparsblk[itmp,8] - stparsblk[1,8],2)
          pckone <- stparsblk[itmp,10]
          mod1 <- floor((pckone - 1) / 4) + 1
          hlife1 <- pckone - (mod1 - 1) * 4
          cmax1 <- stparsblk[itmp,9]
          ipkt <- floor(360 * tseas)
          ipkt[ipkt==0] <- 1
          wavexx <- compwaveconv123(cmax1, mod1, hlife1)
          wavest <- wavexx[ipkt]
          ipktpr <- floor(360 * tseaspr)
          ipktpr[ipktpr==0] <- 1
          wavestpr <- wavexx[ipktpr]
          cint1 <- stparsblk[itmp,1]
          cwave1 <- stparsblk[itmp,2]
          canom1 <- stparsblk[itmp,3]
          canom2 <- stparsblk[itmp,4]
          canom3 <- stparsblk[itmp,5]
          ctnd1 <- stparsblk[itmp,6]
          tmid <- ibegblk[itmp] + (iendblk[itmp] + 1 - ibegblk[itmp]) / 2
          hwd <- (iendblk[itmp] + 1 - ibegblk[itmp]) / 2
          tndlin1 <- tyr - tmid  
          tndlin1pr <- tyrpr - tmid 
          tndlin1[tndlin1<(-hwd)] <- -hwd
          tndlin1[tndlin1>hwd] <- hwd
          tndlin1pr[tndlin1pr<(-hwd)] <- -hwd
          tndlin1pr[tndlin1pr>hwd] <- hwd
          fittmppr <- (cint1 + cwave1 * wavestpr + canom1 * tmpv1pr + canom2 
                       * tmpv2pr + ctnd1 * tndlin1pr + canom3 * tmpv3pr)
          cadjx0 <- clog
          fitadjx0 <- fittmppr
          cadjx1 <- (clog - cwave1 * wavest - canom1 * tmpv1 - canom2 * tmpv2 
                     - canom3 * tmpv3)
          fitadjx1 <- cint1 + ctnd1 * tndlin1pr
          cresx0 <- clog - (cint1 + cwave1 * wavest + canom1 * tmpv1 + canom2
                            * tmpv2 + ctnd1 * tndlin1 + canom3 * tmpv3)
          for (jplt in 1:2)  {
          	if(jplt==1) {
              ytmp <- cadjx0
              ytmpxx <- fitadjx0
              ytmpxx2 <- fitadjx1
              ylow <- floor(min(c(cadjx0, fitadjx0)) - 0.25) 
              yup <- ceiling(max(c(cadjx0, fitadjx0)) + 1)
              ytck <- 0.02 * (yup - ylow)   
              xlow <- yrstart; 
              xup <- yrend + 1 
              xtck <- 0.012 * (xup - xlow)
            }
            if(jplt==2) {
              ytmp <- cresx0
              ylow <- floor(min(c(cresx0)) - 0.25) 
              yup <- ceiling(max(c(cresx0)) + 0.25)
              ytck <- 0.02 * (yup - ylow)   
              xlow <- yrstart
              xup <- yrend + 1
              xtck <- 0.012 * (xup - xlow)
            }
            plot(xaxs='i', yaxs='i', xaxt='n', yaxt='n', xlab='', ylab='', 
                 xlim=c(xlow,xup), ylim=c(ylow,yup))
            if(jplt==1) {
              lines(tyrpr, ytmpxx, type='l', col=6, lwd=0.5)
              lines(tyrpr, ytmpxx2, col=1, lwd=3)
            }
            if(jplt==2) {
              lines(c(xlow, xup),c(0, 0),type='l',col=1,lwd=3)
            }
            lines(tyr[centmp], ytmp[centmp], type='p', pch=1, cex=0.7, col=8)
            lines(tyr[!centmp], ytmp[!centmp], type='p', pch=16, cex=0.65, 
                  col=8)
            for (j in seq(xlow, xup - 1, 1)) {
              mtext(side=1, line=0.5, at=j + 0.5, cex=0.75, as.character(j))
              lines(c(j, j), c(ylow, ylow + ytck), lwd=1)
              lines(c(j, j), c(yup, yup - ytck), lwd=1)
            }
            for (j in seq(ylow, yup, 1)) {
              mtext(side=2, line=0.5, at=j, adj=1, cex=0.75, as.character(j))
              lines(y=c(j, j), x=c(xlow, xlow + xtck), lwd=1)
              lines(y=c(j, j), x=c(xup, xup - xtck), lwd=1)
            }
            if(jplt==1)  {
              text(xlow + 0.5 * (xup - xlow), yup - 2.5 * ytck, adj=0, 
                   cex=0.65, 'uncensored concentration')
              lines(xlow + 0.47 * (xup - xlow), yup - 2.5 * ytck, type='p', 
                    pch=16, cex=0.65, col=8)
              text(xlow + 0.5 * (xup - xlow), yup - 5 * ytck, adj=0, cex=.065,
                   'censored concentration')
              lines(xlow + 0.47 * (xup - xlow), yup - 5 * ytck, type='p', 
                    pch=1, cex=0.65, col=8)
              text(xlow + 0.5 * (xup - xlow), yup - 7.5 * ytck, adj=0, 
                   cex=0.65, 'fitted concentration (Seawave-Q)')
              lines(c(xlow + 0.45 * (xup - xlow), xlow + 0.49 * (xup - xlow)),
                    c(yup - 7.5 * ytck, yup - 7.5 * ytck), type='l', lwd=1.0,
                    col=6)
              text(xlow + 0.5 * (xup - xlow), yup - 10 * ytck, adj=0, 
                   cex=0.65, 'trend')
              lines(c(xlow + 0.45 * (xup - xlow), xlow + 0.49 * (xup - xlow)),
                    c(yup - 10 * ytck, yup - 10 * ytck), type='l', lwd=3.0,
                    col=8)
              mtext(side=3, line=1, outer=T, adj=0.1, cex=0.8, chemname)
              mtext(side=3, line=1, outer=T, adj=0.4, cex=0.8, stlab)
              mtext(side=3, line=1, outer=T, adj=0.7, cex=0.8, 
                    paste(as.character(ibegblk[itmp]), '-', 
                          as.character(iendblk[itmp])))
              mtext(side=3, line=1, outer=T, adj=0.9, cex=0.8, 
                    paste('lnlikdif=', as.character(diflikx)))
              mtext(side=2, outer=F, line=4, cex=.7, 
                    'Concentration, as base-10 logarithm\nof micrograms per 
                    liter')
            }
            if(jplt==2) { 
              mtext(side=2, outer=F, line=3, cex=0.7, 'Residual')
            }
          }
          # end check to see if model results are plotted
        }
      }
      # end check for adequate data in the block
    }
    # end loop for blocks
  }
  stpars2 <- round(stpars2, 8)
  stpars2se <- round(stpars2se, 8)
  stparsout <- data.frame(blkandtndinfo, stpars2, stpars2se)
  names(stparsout) <- c('blkst', 'blkend', 'tndst', 'tndend', 'int', 'wave',
                        'sa100', 'sa10', 'sa1', 'tnd', 'scl', 'lik', 'cmax',
                        'modno', 'intse', 'wavese', 'sa100se', 'sa10se', 
                        'sa1se', 'tndse')
  stparsout
}

