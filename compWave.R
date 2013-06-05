#' Function to compute seasonal wave for pesticide analysis

#' This function is part of the the SEAWAVE-Q model --- a 
#' parametric regression model specifically designed for 
#' analyzing seasonal- and flow-related variability and 
#' trends in pesticide concentrations.  The development of 
#' the seasonal wave is described in Vecchia and others (2008)
#' and has been used in studies of pesticide concentrations
#' in surface water (Ryberg and others, 2010; Sullivan and 
#' others, 2009).

#' @name compWave
#' @title Compute seasonal wave for pesticide anlaysis
#' @author Aldo V. Vecchia \email{avecchia@@usgs.gov}
#' @param cmax is time of maximum concentration
#' @param jmod is model choice, numeric from 1 to 14.  The choice
#' specifies the instantaneous input rate for the approximately 
#' one-month interval beginning at time (k - 1) ⁄ 12,
#' k = 1, 2, 3, ... ,12.
#' @param hlife is the halflife in months, number 1 to 4.  This
#' controls the rate at which the pesticide is removed by processes 
#' such as degradation or runoff.
#' @keywords wave halflife
#' @export
#' @return a numeric vector representing representing intra-annual 
#' variability in concentration.
#' @examples
#' my.wave <- compWave(123,2,2)
#' head(my.wave)
#' tail(my.wave)
#' @references
#' Ryberg, K.R., Vecchia, A.V., Martin, J.D., Gilliom, R.J., 2010, Trends in 
#' pesticide concentrations in urban streams in the United States, 1992-2008: 
#' U.S. Geological Survey Scientific Investigations Report 2010-5139, 101 p. 
#' \url{http://pubs.usgs.gov/sir/2010/5139/}.
#'
#' Sullivan, D.J., Vecchia, A.V., Lorenz, D.L., Gilliom, R.J., Martin, J.D., 
#' 2009, Trends in pesticide concentrations in corn-belt streams, 1996-2006: 
#' U.S. Geological Survey Scientific Investigations Report 2009-5132, 75 p. 
#' \url{http://pubs.usgs.gov/sir/2009/5132/}.
#'
#' Vecchia, A.V., Martin, J.D., and Gilliiom, R.J., 2008, Modeling variability 
#' and trends in pesticide concentrations in streams: Journal of the American 
#' Water Resources Association, v. 44, no. 5, p. 1308-1324, 
#' \url{http://dx.doi.org/10.1111/j.1752-1688.2008.00225.x}.

compWave <- function(cmax,jmod,hlife) {
  txx <- seq(0,1,1/360)
  if (hlife==1) { 
  	phi <- 12
  	# approximate halflife is 12/phi or 1 month in first case
  } else if (hlife==2) { 
  	phi <- 6
  } else if (hlife==3) { 
  	phi <- 4
  } else if (hlife==4) {
  	phi <- 3
  } else {
    stop("Half life value of ",hlife, " is invalid, must be an integer 1 to 4.")
  }
 
  # wtx (dimensionless) is proportional to the instantaneous input rate
  # for the kth monthly time interval (Vecchia and others, 2008)
  # After considerable experimentation, the discrete parameter values
  # were defined such that each seasonal wave could be easily distinguished.
  if (jmod==1) { 
  	wtx <- c(0,0,0,0,0,1,0,0,0,0,0,0)
  	pkt <- 6/12
  } else if (jmod==2) {
  	wtx <- c(0,0,0,0,0,1,1,0,0,0,0,0)
  	pkt <- 7/12
  } else if (jmod==3) {
  	wtx <- c(0,0,0,0,0,1,1,1,0,0,0,0)
  	pkt <- 8/12
  } else if (jmod==4) {
  	wtx <- c(0,0,0,0,0,1,1,1,1,0,0,0)
  	pkt <- 9/12
  } else if (jmod==5) {
  	wtx <- c(0,0,0,1,1,1,1,1,1,0,0,0)
  	pkt <- 9/12
  } else if (jmod==6) {
  	wtx <- c(1,1,1,1,1,1,1,1,1,0,0,0)
  	pkt <- 9/12
  } else if (jmod==7) {
  	wtx <- c(.5,0,0,1,1,0,0,0,0,0,0,0.5)
  	pkt <- 5/12
  } else if (jmod==8) {
  	wtx <- c(0,0,0,1,1,0,0,0,0,0,0.5,0.5)
  	pkt <- 5/12
  } else if (jmod==9) {
  	wtx <- c(0,0,0,1,1,0,0,0,0,0.5,0.5,0)
  	pkt <- 5/12
  } else if (jmod==10) {
  	wtx <- c(0,0,0,1,1,0,0,0,0.5,0.5,0,0)
  	pkt <- 5/12
  } else if (jmod==11) {
  	wtx <- c(.5,0,0,0,1,0,0,0,0,0,0,0.5)
  	pkt <- 5/12
  } else if (jmod==12) {
  	wtx <- c(0,0,0,0,1,0,0,0,0,0,0.5,0.5)
  	pkt <- 5/12
  } else if (jmod==13) {
  	wtx <- c(0,0,0,0,1,0,0,0,0,0.5,0.5,0)
  	pkt <- 5/12
  } else if (jmod==14) {
  	wtx <- c(0,0,0,0,1,0,0,0,0.5,0.5,0,0)
  	pkt <- 5/12
  } else { 
    stop("jmod value of ",jmod," is invalid, must be an integer 1 to 14.")
  }
  # See Appendix of Vecchia and others (2008)
  del <- 1/12
  rho <- exp(-phi)
  z0xx <- rho^(txx)
  r12 <- rho^(-del*c(1:12))
  con <- wtx[1]*(r12[1]-1)
  for(k in 2:12) {
    con <- con+wtx[k]*(r12[k]-r12[k-1])
  }
  # equation A2, Vecchia and others (2008)
  # for all phi > 0 and wtx >= 0
  con <- rho/(1-rho)*con
  z0xx <- z0xx*con
  zmat <- matrix(nrow=length(txx),ncol=12)
  pckm <- matrix(nrow=length(txx),ncol=12)
  ntot <- length(txx)
  pckm[,1] <- c(txx<=del)
  for (k in 2:12) {
    pckm[,k] <- c(txx>(k-1)*del & txx<=k*del)
  }
  zmat[,1] <- rho^txx*(rho^(-replace(txx,txx>1/12,1/12))-1)
  for (k in 2:12) {
    ztmp <- rep(0,ntot)
    for (j in 1:(k-1)) {
      ztmp[pckm[,j]] <- 0
	}
    ztmp[pckm[,k]] <- 1-rho^(txx[pckm[,k]]-(k-1)*del)
    if(k<12) {  
      for (j in (k+1):12) {
	    ztmp[pckm[,j]] <- rho^(txx[pckm[,j]]-del*k)-rho^(txx[pckm[,j]]-(k-1)*del)
	  }
    }
    zmat[,k] <- ztmp
  }
  sst <- z0xx
  for (k in 1:12) { 
    sst <- sst+wtx[k]*zmat[,k]
  }
  sst <- sst/phi
  medxx <- (max(sst)+min(sst))/2
  rngxx <- 2*(max(sst)-medxx)
  sst <- (sst-medxx)/rngxx
  if(cmax<=pkt) { 
    txx2 <- (txx+1-pkt+cmax)
  } else if (cmax>pkt)  {
    txx2 <- txx-pkt+cmax
  }
  txx2[txx2>1] <- txx2[txx2>1]-1
  otmp <- order(txx2)
  sst <- sst[otmp]
  sst
}


  
