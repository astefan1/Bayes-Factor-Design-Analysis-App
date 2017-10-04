# ------------------------------------------------------------------------------
# ------------------ SUMMARY PLOT WITH TRAJECTORIES (a al Felix) ---------------
# ------------------------------------------------------------------------------

inside <- function(x, R) Vectorize({x >= R[1] & x <= R[2]})

#' @import dplyr
#' @import TeachingDemos
#' @import scales
#' @import sfsmisc
#'
#' @param SequentialBF.obsub 
#' @param boundary 
#' @param method 
#' @param n.trajectories 
#' @param forH1
#'
#' @return
#' @export
#'
#' @examples
summaryplot.sequentialBFDA <- function(SequentialBF.obsub, boundary = 10, method = "default", n.trajectories = 60, forH1=TRUE){
  
  # Define Simulation object
  sim <- SequentialBF.obsub$sim
  
  # Define function arguments
  method <- match.arg(method, c("default", "informed"))
  
  if (method == "default"){
    names(sim)[names(sim) == "logBF.default"] <- "logBF"
  } else {
    names(sim)[names(sim) == "logBF.informed"] <- "logBF"
  }
  
  # Define boundary vectors
  if (length(boundary) == 1) boundary <- sort(c(boundary, 1/boundary))
  logBoundary <- log(boundary)
  
  # Define n.max and n.min for computation and display
  n.max <- max(sim$n)
  n.min <- min(sim$n)
  
  indices <- BFDA.analyze(SequentialBF.obsub,
                          method=method,
                          boundary=boundary,
                          n.min=n.min,
                          n.max=n.max,
                          verbose=FALSE)
  
  # Select trajectories to display (proportional, reproducible through set.seed)
  set.seed(0xBEEF)
  seltraj <- c(as.character(sample(indices$upper.hit.ids,
                                   size=round(indices$upper.hit.frac*n.trajectories))),
       as.character(sample(indices$lower.hit.ids,
                           size=round(indices$lower.hit.frac*n.trajectories))),
       as.character(sample(indices$n.max.hit.ids,
                           size=round(indices$n.max.hit.frac*n.trajectories))))
  demo1 <- sim %>% filter(id %in% seltraj)
  
  # Delete all points after first boundary was hit
  unNA <- function(x) {x[is.na(x)]=Inf; return(x)}
  demo2 <- demo1 %>% group_by(id) %>% 
    mutate(firstbreak = unNA(which(logBF >= logBoundary[2] | logBF <= logBoundary[1])[1])) %>% 
    filter(row_number() <= firstbreak) %>% ungroup()
  
  # "Land" all final points on the boundary
  demo2$logBF[demo2$logBF > logBoundary[2]] <- logBoundary[2]
  demo2$logBF[demo2$logBF < logBoundary[1]] <- logBoundary[1]
  
  final_point_boundary <- demo2 %>% group_by(id) %>% filter(n == max(n), logBF==logBoundary[1] | logBF==logBoundary[2])
  final_point_n.max <- demo2 %>% filter(n == n.max, logBF<logBoundary[2] & logBF > logBoundary[1])
  
  # Define xlim and ylim
  ylim <- c(max(min(sim$logBF), logBoundary[1])*1.7, min(max(sim$logBF), logBoundary[2])*1.7)*1.1
  xlim <- c(n.min, max(demo2$n))
  
  # Compute densities & normalize densities, so that the areas sum to one
  
  # Compute upper density. Weigh by frequency -> all three densities sum up to 1
  if (!is.null(indices$d.top)) {
    d.top.area <- integrate.xy(indices$d.top$x, indices$d.top$y)
    dens.top <- as.data.frame(indices$d.top[c("x", "y")])
    dens.top <- dens.top[dens.top$y > 0.0001, ]
    # normalize density to area 1
    dens.top$y <- dens.top$y/d.top.area		
    dens.top$y <- dens.top$y*indices$upper.hit.frac	# weigh density
    dens.top <- dens.top[dens.top$x < n.max, ]
    #sum(diff(dens.top$x)*dens.top$y)
  } else {dens.top <- NULL}
  
  # Compute lower density
  if (!is.null(indices$d.bottom)) {
    d.bottom.area <- integrate.xy(indices$d.bottom$x, indices$d.bottom$y)
    dens.bottom <- as.data.frame(indices$d.bottom[c("x", "y")])
    dens.bottom <- dens.bottom[dens.bottom$y > 0.0001, ]
    # normalize density to area 1
    dens.bottom$y <- dens.bottom$y/d.bottom.area
    dens.bottom$y <- dens.bottom$y*indices$lower.hit.frac # weigh density
    dens.bottom <- dens.bottom[dens.bottom$x < n.max, ]
    #sum(diff(dens.bottom$x)*dens.bottom$y)
  } else {dens.bottom <- NULL}
  
  # Compute right density: Distribution of logBF	
  if (!is.null(indices$d.right)) {
    d.right.area <- integrate.xy(indices$d.right$x, indices$d.right$y)
    dens.right <- as.data.frame(indices$d.right[c("x", "y")])
    dens.right <- dens.right[dens.right$y > 0.0001, ]
    # normalize density to area 1
    dens.right$y <- dens.right$y/d.right.area
    dens.right$y <- dens.right$y*indices$n.max.hit.frac	# weigh density
    #sum(diff(dens.right$x)*dens.right$y)
  } else {dens.right <- NULL}
  
  # automatic labeling of y-axis
  yaxis.at <- c(-log(30), -log(10), -log(3), log(1), log(3), log(10), log(30))
  yaxis.labels <- c("1/30", "1/10", "1/3", "1", "3", "10", "30")
  i <- 2
  repeat {
    if (ylim[2] >= log(10^i)) {
      yaxis.at <- c(yaxis.at, log(10^i))
      yaxis.labels <- c(yaxis.labels, as.character(10^i))
      i <- i+1
    } else {break;}
  }
  
  ## ======================================================================
  ## The plot
  ## ======================================================================
  
  # positions for the category labels
  labels.y <- c(4, 2.86, 1.7, .55, -.55, -1.7, -2.85, -4)
  
  par(mai=c(5.7, 1, 1, 1), xpd=NA)
  plot(NA,
       xlim=c(xlim[1], xlim[2]),
       ylim=ylim,
       xlab="",
       ylab="",
       bty="n",
       axes=FALSE)
  
  # nice labels
  text(labels = "Sample Size per Group", y = ylim[1]+log(1/300), x = (xlim[2]-10)/2+10, cex=1.5)						# xlab
  mtext(expression(Bayes~Factor~(BF[10])), side=2, line=3, cex=1.5, font = 2)	# ylab
  
  # axes
  # set scale ticks
  ticks <- round(axTicks(1, c(10, xlim[2], par("xaxp")[3])))
  axis(1, at = c(ticks), pos = log(1/100*0.5))
  axis(2, at = log(c(1/100, 1/30, 1/10, 1/3, 1, 3, 10, 30, 100)),  labels=c("1/100", "1/30", "1/10", "1/3", "1", "3", "10", "30", "100"), las=2)	
  
  # draw the demo trajectories
  for (i in unique(demo2$id)) {
    lines(demo2$n[demo2$id==i], demo2$logBF[demo2$id==i], col=scales::alpha("grey30", 0.5))
  }
  
  # BF-boundaries
  if (all(is.finite(boundary))) {
    lines(x=c(xlim[1], xlim[2]), y=c(logBoundary[1], logBoundary[1]), lty="solid", lwd=1.5)
    lines(x=c(xlim[1], xlim[2]), y=c(logBoundary[2], logBoundary[2]), lty="solid", lwd=1.5)
  }
  
  ## Annotation: horizontal lines at BF categories
  for (y in c(c(-log(c(30, 10, 3)), 0, log(c(3, 10, 30))))) {
    if (inside(y, ylim))
      lines(x=c(xlim[1], xlim[2]), y=rep(y, 2), lty="dotted", col="grey20")
  }	
  
  # ---------------------------------------------------------------------
  #  Draw densities
  
  
  # ---------------------------------------------------------------------
  #  Compute amplification factors: 
  
  # get aspect ratio so that densities are correctly scaled
  # TODO: Unnecessary?
  w <- par("pin")[1]/diff(par("usr")[1:2])
  h <- par("pin")[2]/diff(par("usr")[3:4])
  asp <- w/h
  
  
  if (!is.null(c(dens.bottom$y, dens.top$y))) {
    if (max(c(dens.top$y, dens.bottom$y)) > 0.05) {dens.amp <- 30}
    else if (max(c(dens.top$y, dens.bottom$y)) > 0.01) {dens.amp <- 100}
    else if (max(c(dens.top$y, dens.bottom$y)) > 0.001) {dens.amp <- 300}
  } else {
    dens.amp <- 600
  }
  
  
  # Upper density
  if (!is.null(dens.top)) {
    poly.top <- rbind(data.frame(x=dens.top$x[1], y=0), dens.top, data.frame(x=xlim[2], y=0))
    polygon(poly.top$x[poly.top$x <= xlim[2]], (poly.top$y[poly.top$x <= xlim[2]])*dens.amp + logBoundary[2], col="grey80")
    lines(dens.top$x[dens.top$x <= xlim[2]], (dens.top$y[dens.top$x <= xlim[2]])*dens.amp + logBoundary[2], col="grey30")
  }
  
  # Lower density
  if (!is.null(dens.bottom)) {
    poly.bottom <- rbind(data.frame(x=dens.bottom$x[1], y=0), dens.bottom, data.frame(x=max(dens.bottom$x), y=0))
    polygon(poly.bottom$x[poly.bottom$x <= xlim[2]], -(poly.bottom$y[poly.bottom$x <= xlim[2]])*dens.amp + logBoundary[1], col="grey80")
    lines(dens.bottom$x[dens.bottom$x <= xlim[2]], -(dens.bottom$y[dens.bottom$x <= xlim[2]])*dens.amp + logBoundary[1], col="grey30")
  }
  
  
  # add final points at boundary hit
  points(final_point_boundary$n, final_point_boundary$logBF, pch=16, cex=1)
  
  
  if (!is.null(dens.right) & indices$n.max.hit.frac > 0.005) {
    xmax <- n.max + dens.right.offset*(strheight("X")/asp) + max(dens.right$y)*dens.amp + strwidth("XXXXXX")
  } else {
    xmax <- n.max + strheight("X")/asp
  }
  
  par(xpd=NA)	# allow drawing outside the plot
  if (inside(labels.y[8], ylim))
    text(xlim[2] + ((ticks[2]-ticks[1])/5), labels.y[8], bquote(Very~strong~H[.(ifelse(forH1==TRUE,0,1))]), adj=c(0, 0.5), cex=1)
  if (inside(labels.y[7], ylim))
    text(xlim[2] + ((ticks[2]-ticks[1])/5), labels.y[7], bquote(Strong~H[.(ifelse(forH1==TRUE,0,1))]), adj=c(0, 0.5), cex=1)
  if (inside(labels.y[6], ylim))
    text(xlim[2] + ((ticks[2]-ticks[1])/5), labels.y[6], bquote(Moderate~H[.(ifelse(forH1==TRUE,0,1))]), adj=c(0, 0.5), cex=1)
  if (inside(labels.y[5], ylim))
    text(xlim[2] + ((ticks[2]-ticks[1])/5), labels.y[5], bquote(Anecdotal~H[.(ifelse(forH1==TRUE,0,1))]), adj=c(0, 0.5), cex=1)
  if (inside(labels.y[1], ylim))
    text(xlim[2] + ((ticks[2]-ticks[1])/5), labels.y[1], bquote(Very~strong~H[.(ifelse(forH1==TRUE,1,0))]), adj=c(0, 0.5), cex=1)
  if (inside(labels.y[2], ylim))
    text(xlim[2] + ((ticks[2]-ticks[1])/5), labels.y[2], bquote(Strong~H[.(ifelse(forH1==TRUE,1,0))]), adj=c(0, 0.5), cex=1)
  if (inside(labels.y[3], ylim))
    text(xlim[2] + ((ticks[2]-ticks[1])/5), labels.y[3], bquote(Moderate~H[.(ifelse(forH1==TRUE,1,0))]), adj=c(0, 0.5), cex=1)
  if (inside(labels.y[4], ylim))
    text(xlim[2] + ((ticks[2]-ticks[1])/5), labels.y[4], bquote(Anecdotal~H[.(ifelse(forH1==TRUE,1,0))]), adj=c(0, 0.5), cex=1)
  par(xpd=TRUE)
  
  # Title
  if (method == "default"){
    title("Summary Plot for Default Method", cex.main = 1.5, line = 4)
  } else {
    title("Summary Plot for Informed Method", cex.main = 1.5, line = 4)
  }
  
  
  # Write labels of stopping percentages
  TeachingDemos::shadowtext(col="black", bg="white", x=xlim[1], y=logBoundary[2]+max(poly.top$y[poly.top$x <= xlim[2]]*dens.amp), label=bquote(paste(.(round(indices$upper.hit.frac*100)), "% arrived at ", H[1], " boundary")), cex=1, adj=c(-0.1, -0.3))
  TeachingDemos::shadowtext(col="black", bg="white", x=xlim[1], y=log(1/30), label=bquote(paste(.(round(indices$lower.hit.frac*100)), "% arrived at ", H[0], " boundary")), cex=1, adj=c(-0.1, 1.3))
  
  if (indices$n.max.hit.frac > 0.005 & all(is.finite(boundary))) {		
    if (n.max.label.position == "dynamic") {
      # find maximum of dens.right; center the label on this point
      Y <- dens.right$x[which.max(dens.right$y)]
    } else {
      Y <- log(1)
    }
    
    TeachingDemos::shadowtext(col="black", bg="white", x=n.max, y=Y, label=bquote(paste(.(round(indices$n.max.hit.frac*100)), "% stopped at ", n[max], " boundary")), cex=1, srt=90, adj=c(0.5, 1.3))
  }
}
