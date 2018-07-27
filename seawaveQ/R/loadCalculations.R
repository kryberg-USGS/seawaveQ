#' Function to calculate pesticide loads in kilograms per year.
#' 
#' Parameter load (mass) is the product of water-quality concentration 
#' (a mass per volume)and an associated streamflow rate (volume per time). This
#' function generates an annual time series of pesticide loads on either a
#' calendar year basis or a water year basis.
#' @name loadCoalculations
#' @title Fit seasonal wave and continuous ancillary data for trend 
#' analysis
#' @note In this load calculation function, daily pesticide concentration 
#' estimates provided by the \code{fitswavecav} function are corrected for 
#' retransformation bias (the concentration model is built on #' the base-10 
#' logarithm of concentration; therefore, a bias correction is required when 
#' transforming back to the original units) and then used to calculate daily 
#' loads. The bias correction is based on the quasi-maximum likelihood estimator 
#' (Cohn and others, 1989) that was developed for natural logarithms, with an 
#' adjustment for the base-10 logarithm of the concentration. To calculate 
#' loads, the bias-corrected concentration estimates are multiplied by daily 
#' streamflow and a constant, 0.892998605, which converts the load units 
#' (micrograms per liter * cubic feet per second) to kilograms per year. 
#' Daily loads are summed to annual values. See page 70 and equation 26 of 
#' Oelsner and others (2017) for further details regarding the load 
#' calculation and bias correction.
#' Users may modify this function to convert to units other than kilograms
#' per year.
#' @param dailyDat is the daily streamflow data in the form of a data.frame
#' with three columns representing a station ID, date, and streamflow
#' @param pestPredict is the continuous (daily) estimation of pesticide
#' concentrations for one or more pesticides at a single site. This should be
#' in the form of the fourth element of the list returned by \code{fitswavecav}.
#' @param modRes is the first element of the list returned by \code{fitswavecav} 
#' and include the scale parameter for one or more pesticide trend models at
#' a single site. The scale parameter is used in the bias correction.
#' @param yrtype allows one to calculate annual loads based on a calendar 
#' year or a water year, where a water year is the 12-month period October 1 
#' through September 30 designated by the calendar year in which it ends. A 
#' yrtype of 1 represents a calendar year and is the default because that
#' is the way the original model was developed. A yrtype of 2 represents a
#' water year. 
#' @keywords datagen ts
#' @return a data frame.
#' @format The data frame returned has one row for each pesticide-year at
#' a particular site and four columns. The general format is as follows: \cr
#' \tabular{lll}{
#'  pstaid \tab character \tab The station identification number \cr
#'  pcode \tab character \tab The parameter code for which a load was calculated\cr
#'  year or wyear \tab numeric \tab The year or water year for which a load was calculated \cr
#'  load \tab numeric \tab the load in kilograms per year \cr
#' }
#' @export
#' @author Karen R. Ryberg
#' @examples
#' data(swData)
#' modMoRivOmaha <- combineData(qwdat = qwMoRivOmaha, cqwdat = cqwMoRivOmaha)
#' myfit1 <- fitswavecav(cdat = modMoRivOmaha, cavdat = cqwMoRivOmaha, 
#' tanm = "myfit1", pnames = c("04035", "04037", "04041"), yrstart = 1995, 
#' yrend = 2003, tndbeg = 1995, tndend = 2003, iwcav = c("flowa30", "flowa1"), 
#' dcol = "dates", qwcols = c("R", "P"))
#' MoRivOmahaLoadsYr <- loadCalculations(cqwMoRivOmaha[,1:3], myfit1[[4]], 
#' myfit1[[1]])
#' MoRivOmahaLoadsYr
#' @references
#' Cohn, T.A., DeLong, L.L., Gilroy, E.J., Hirsch, R.M., and Wells, D.K., 1989, 
#' Estimating constituent loads: Water Resources Research, v. 25, no. 5, 
#' p. 937--942.
#'
#' Oelsner, G.P., Sprague, L.A., Murphy, J.C., Zuellig, R.E., Johnson, H.M., 
#' Ryberg, K.R., Falcone, J.A., Stets, E.G., Vecchia, A.V., Riskin, M.L., 
#' De Cicco, L.A., Mills, T.J., and Farmer, W.H., 2017, Water-quality trends in 
#' the Nation's rivers and streams, 1972--2012---Data preparation, statistical 
#' methods, and trend results (ver. 2.0, October 2017): U.S. Geological Survey 
#' Scientific Investigations Report 2017--5006, 136 p., 
#' \url{https://doi.org/10.3133/sir20175006}.
#' 
loadCalculations <- function(dailyDat, pestPredict, modRes, yrtype = 1) {
  # compute a decimal day that matches the one in seawaveQ results
  DecimalTime <- function(date) {
    yr <- year(date)
    mo <- month(date)
    da <- day(date)
    dectime <- yr + (mo - 1)/12 + (da - 0.5)/366
    dectime
  }
  dailyDat$dectime <- round(DecimalTime(dailyDat$dates), digits = 3)
  dailyDat$year <- year(dailyDat$dates)
  dailyDat$month <- month(dailyDat$dates)
  dimnames(dailyDat)[[2]][1] <- "pstaid"
  dimnames(dailyDat)[[2]][3] <- "Q"
  dailyDat$wyear <- dailyDat$year
  dailyDat$wyear[dailyDat$month > 9] <- dailyDat$year[dailyDat$month > 9] + 1
  pestPredict$dectime <- round(pestPredict$dectime, digits = 3)
  pestPredict <- melt(pestPredict, id = "dectime", na.rm = TRUE)
  dimnames(pestPredict)[[2]][2:3] <- c("pcode", "conc")
  mergedDat <- join(pestPredict, dailyDat, by = "dectime")
  modRes$pcode <- paste("P", modRes$pname, sep = "")
  mergedDat <- join(mergedDat, modRes[, c("scl", "pcode")], by = "pcode")
  o <- order(mergedDat$pstaid, mergedDat$pcode, mergedDat$dectime)
  mergedDat <- mergedDat[o, ]
  # bias correction for concentration
  # seawaveQ returns back transformed concentration values, but they are not
  # bias corrected
  mergedDat$biascorconc <- mergedDat$conc * exp(0.5*(log(10)*mergedDat$scl)^2)
  k <- 0.8929986
  # load in kilograms per year
  mergedDat$load <- mergedDat$biascorconc * mergedDat$Q * k
  mergedDat <- mergedDat[, c("pstaid", "pcode", "year", "wyear", "load")]
  if (yrtype == 1) {
    # calendar year load
    loads <- aggregate(load ~ pstaid + pcode + year, data = mergedDat, sum)
    o <- order(loads$pstaid, loads$pcode, loads$year)
  } else if (yrtype == 2) {
    # water year load
    loads <- aggregate(load ~ pstaid + pcode + wyear, data = mergedDat, sum)
    o <- order(loads$pstaid, loads$pcode, loads$wyear)
  } else {
    warning("yrtype argument must be a numeric 1 or 2")
  }    
  loads <- loads[o, ]
  loads
}
