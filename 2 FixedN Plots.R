# ------------------------------------------------------------------------------
# ------------------- PLOT FUNCTIONS FOR FIXED-N DESIGN ------------------------
# ------------------------------------------------------------------------------

###################### Distribution of Bayes Factors ###########################

#' @import dplyr

#' @param sim.tomaxn An element of fixed.simresults
#' @param samplesize Fixed n at which BF distribution is evaluated
#' @param method Method(s) to compute Bayes Factors ("default", "informed"), combine as vector to plot both methods
#' @param boundary Critical BF boundaries for H0 and H1
#' @param xlim limits on x axis
#' @param bw Black/white color scheme? (default:FALSE)
#' @param plot.H0 show colors in reversed order (high BF are red)

plot.exp.BF.dist <- function(sim.tomaxn, samplesize, true.ES, method = "default", boundary = c(1/6, 6), xlim = c(1/10, 1000), bw = FALSE, plot.H0 = FALSE){
  
  # Define arguments
  match.arg(method, c("default", "informed"))
  if (length(boundary) == 1) boundary <- sort(1/boundary, boundary)
  
  # Convert boundaries and xlim to log scale
  logBoundary <- log(boundary)
  logXlim <- log(xlim)
  
  # Convert sim.tomaxn input to a dataset to make it usable for dplyr
  sim <- as.data.frame(sim.tomaxn)
  
  # Reduce dataset to relevant method and sample size
  if (method == "default"){
    logBF <- sim %>% filter(n == samplesize) %>% select(logBF.default) %>% .[,1]
  } else {
    logBF <- sim %>% filter(n == samplesize) %>% select(logBF.informed) %>% .[,1]
  }
  
  # Compute densities of Bayes Factors
  dens <- density(logBF, from = logXlim[1], to = logXlim[2])
  
  # Normalize densities to max = 1
  maxDens <- max(dens$y)
  dens$y <- dens$y/maxDens
  
  
# --------------------------- Start the plot -----------------------------------
  
  # main title
  
  if (method == "default") {
    main <- bquote(paste("Distribution of Default Bayes Factors (DGP: ", delta*" = ", .(true.ES), ")"))
  } else {
    main <- bquote(paste("Distribution of Informed Bayes Factors (DGP: ", delta*" = ", .(true.ES), ")"))
  }
  
  # Automatic labeling of x axis
  xaxis.at <- c(-log(30), -log(10), -log(3), log(1), log(3), log(10), log(30))
  xaxis.labels <- c("1/30", "1/10", "1/3", "1", "3", "10", "30")
  i <- 2
  repeat {
    if (logXlim[2] >= log(10^i)) {
      xaxis.at <- c(xaxis.at, log(10^i))
      xaxis.labels <- c(xaxis.labels, as.character(10^i))
      i <- i+1
    } else {break;}
  }
  
  # Set up the plot
  par(mar = c(5, 4, 6, 2) + 0.1)
  plot(NA,
       xlim=logXlim,
       ylim=c(0, 1),
       xlab="",
       ylab="",
       bty="n",
       axes=FALSE)
  
  # Draw the x axis
  axis(1, at = xaxis.at[inside(xaxis.at, c(-Inf, logXlim[2]))],  labels=xaxis.labels[inside(xaxis.at, c(-Inf, logXlim[2]))], las=1, cex.axis=1.5)	
  
  # Set the title
  title(main, line = 4)
  
  # Draw boundary lines and line for BF = 1 
  abline(v=logBoundary, lty="dashed")
  abline(v=log(1), lty="dotted")
  
  # Get the axis ranges, draw y-axis with arrow
  u <- par("usr")
  points(u[1], u[4], pch=17, xpd = TRUE)
  lines(c(u[1], u[1]), c(u[3], u[4]), xpd = TRUE)
  
  lines(dens)
  
  # Color the plot
  if (bw==FALSE) {
    colors <- c("green4", "orange1", "red3")
  } else {
    colors <- c("grey80", "grey50", "grey10")
  }
  
  if (plot.H0==TRUE) {colors <- rev(colors)}
  
  drawpoly(dens, -Inf, logBoundary[1], col=scales::alpha(colors[3], 0.4))	
  if (length(logBoundary) == 2) {
    drawpoly(dens, logBoundary[1], logBoundary[2], col=scales::alpha(colors[2], 0.4))
    drawpoly(dens, logBoundary[2], Inf, col=scales::alpha(colors[1], 0.4))
  } else {
    drawpoly(dens, logBoundary[1], Inf, col=scales::alpha(colors[1], 0.4))
  }
  
  # Write percentages above the Distribution Areas
  
  PH0 <- round(length(which(logBF < logBoundary[1])==TRUE)/length(logBF)*100, 1)
  Pinc <- round(length(which(logBF > logBoundary[1] & 
                               logBF < logBoundary[2])==TRUE)/length(logBF)*100, 1)
  PH1 <- round(length(which(logBF > logBoundary[2])==TRUE)/length(logBF)*100, 1)
  
  text(x = (logXlim[1]+logBoundary[1])/2, y = 1.1, labels = paste(PH0, "%", sep = ""), cex = 1.5, xpd = NA)
  text(x = 0, y = 1.1, labels = paste(Pinc, "%"), cex = 1.5, xpd = NA)
  text(x = (logXlim[2]+logBoundary[2])/2, y = 1.1, labels = paste(PH1, "%", sep = ""), cex = 1.5, xpd = NA)
  
  
  # Axis labeling
  mtext("Bayes Factor", side = 1, cex = 1.5, line = 3)
  mtext("Density", side = 2, cex = 1.5, line = 1, las = 3)
}


########################### Combined Plot ######################################

combine.plot.exp.BF.dist <- function(method, res.H1, res.H0, true.ES.H1, samplesize, boundary){
  
  nrowmatrix <- length(method)
  
  par(mfrow = c(nrowmatrix, 2))
  
  if("Default" %in% method){
    
    plot.exp.BF.dist(res.H1, samplesize, true.ES = true.ES.H1, "default", boundary, c(1/100, 1000), FALSE, FALSE)
    plot.exp.BF.dist(res.H0, samplesize, true.ES = 0, "default", boundary, c(1/100, 1000), FALSE, TRUE)
    
  }
  
  if("Informed" %in% method){
    
    plot.exp.BF.dist(res.H1, samplesize, true.ES = true.ES.H1, "informed", boundary, c(1/100, 1000), FALSE, FALSE)
    plot.exp.BF.dist(res.H0, samplesize, true.ES = 0, "informed", boundary, c(1/100, 1000), FALSE, TRUE)
    
  }
}
