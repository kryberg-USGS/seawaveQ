pdf("seasonalWaves3.pdf", width=6.5, height=9)
par(las=1, tcl=0.3, mfrow=c(7,4), mar=c(3, 3, 0.5, 1) + 0.1, 
    mgp=c(1.5, 0.25, 0))
for ( j in 1:7 ) {
  for ( k in 1:4 ) {
    if ( j==5 | j==6 ) {
      lpos <- "top"
    }
    else { lpos <- "topright"}
    swave <- compwaveconv(cmaxt=0.3, jmod=j, hlife=k, mclass=1)
    plot(seq(0, 1, 1/360), swave, typ="l", xaxs='i', yaxs='i', 
         ylim=c(-0.6, 0.6), cex.axis=0.6, cex.lab=0.6, 
         ylab="Seasonal wave", xlab="Decimal seasons")
    jmodt<- paste("jmod = ", j, sep="")
    hlifet <- paste("hlife = ", k, sep="")
    cmaxt <- paste("cmaxt = ", 0.3, sep="")
    legend(lpos, c(jmodt, hlifet, cmaxt), cex=0.5, bty="n")
  }
}
for ( j in 8:14 ) {
  for ( k in 1:4 ) {
    if ( j==12 | j==13 | j==14 ) {
      lpos <- "top"
    }
    else { lpos <- "topright"}
    swave <- compwaveconv(cmaxt=0.3, jmod=j, hlife=k, mclass=1)
    plot(seq(0, 1, 1/360), swave, typ="l", xaxs='i', yaxs='i', 
         ylim=c(-0.6, 0.6), cex.axis=0.6, cex.lab=0.6, 
         ylab="Seasonal wave", xlab="Decimal seasons")
    jmodt<- paste("jmod = ", j, sep="")
    hlifet <- paste("hlife = ", k, sep="")
    cmaxt <- paste("cmaxt = ", 0.3, sep="")    
    legend(lpos, c(jmodt, hlifet, cmaxt), cex=0.5, bty="n")
  }
}
dev.off()

pdf("seasonalWaves6.pdf", width=6.5, height=9)
par(las=1, tcl=0.3, mfrow=c(7,4), mar=c(3, 3, 0.5, 1) + 0.1, 
    mgp=c(1.5, 0.25, 0))
for ( j in 1:7 ) {
  for ( k in 1:4 ) {
    if ( j==5 | j==6 ) {
      lpos <- "bottom"
    }
    else { lpos <- "topright"}
    swave <- compwaveconv(cmax=0.6, jmod=j, hlife=k, mclass=1)
    plot(seq(0, 1, 1/360), swave, typ="l", xaxs='i', yaxs='i', 
         ylim=c(-0.6, 0.6), cex.axis=0.6, cex.lab=0.6, 
         ylab="Seasonal wave", xlab="Decimal seasons")
    jmodt<- paste("jmod = ", j, sep="")
    hlifet <- paste("hlife = ", k, sep="")
    cmaxt <- paste("cmaxt = ", 0.6, sep="")
    legend(lpos, c(jmodt, hlifet, cmaxt), cex=0.5, bty="n")
  }
}
for ( j in 8:14 ) {
  for ( k in 1:4 ) {
    if ( j==14 ) {
      lpos <- "topleft"
    }
    else { lpos <- "topright"}
    swave <- compwaveconv(cmax=0.6, jmod=j, hlife=k, mclass=1)
    plot(seq(0, 1, 1/360), swave, typ="l", xaxs='i', yaxs='i', 
         ylim=c(-0.6, 0.6), cex.axis=0.6, cex.lab=0.6, 
         ylab="Seasonal wave", xlab="Decimal seasons")
    jmodt<- paste("jmod = ", j, sep="")
    hlifet <- paste("hlife = ", k, sep="")
    cmaxt <- paste("cmaxt = ", 0.6, sep="")    
    legend(lpos, c(jmodt, hlifet, cmaxt), cex=0.5, bty="n")
  }
}
dev.off()