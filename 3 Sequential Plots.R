# -----------------------------------------------------------------------------------
# -------------------- EFFICIENCY PLOT (CLEAN R GRAPH) ------------------------------
# -----------------------------------------------------------------------------------

# NOTE: This script contaings the functions efficiencyplot() and cleanhist().
# efficiencyplot() plots BF vs. mean/median N, cleanhist() plots a clean histogram
# of N or any other variable.

#' @param efficiency efficiency element (s. 1d Analysis)
#' @param true.ES true effect size (ususally last numbers of efficiency element)
#' @param median.default should median N for default method be drawn? (TRUE/FALSE)
#' @param median.informed should median N for informed method be drawn (TRUE/FALSE)
#' @param hinges for which method should upper/lower hinges be drawn ("none", "default", "informed")
#' @param whiskers for which method should upper/lower whiskers be drawn ("none", "default", "informed")


efficiencyplot <- function(efficiency, true.ES, median.default = TRUE, median.informed = FALSE, Nquartiles = c("default", "informed", "none"), NCIs = c("default", "informed", "none")){
  
  efficiency <- as.data.frame(efficiency)
  
  # Check if either median.default or median.informed is true
  nopriorselected <- median.default == FALSE & median.informed == FALSE
  if (nopriorselected == TRUE){
    
    par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
        font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
    plot(x=1,y=1, bty = "n", axes = FALSE, ann = FALSE, pch = ".", xlim = c(3, 30), ylim = c(0, 500))
    axis(side = 1, at = c(3, 10, 15, 20, 25, 30))
    axis(side = 2, at = c(10, 500))
    mtext("Boundary", 1, line = 2.5, cex = 1.5, font = 2)
    mtext("Median N per Group", 2, line = 4, cex = 1.5, font = 2, las = 0)
    text(3, 250, "Please select at least one prior distribution of effect sizes!", adj = 0, cex = 1.5)
    title(main = "")
    
  }
  
  # Set plot parameters
  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
      font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
  
  # Define maximum of y axis
  if (Nquartiles == "none" & NCIs == "none" & nopriorselected == FALSE){
    ymax <- 1.06 * max(max(efficiency$median.default), max(efficiency$median.informed))
  } else if ((NCIs == Nquartiles & Nquartiles == "default") | (NCIs == "none" & Nquartiles == "default") | (NCIs == "default" & Nquartiles == "none")){
    ymax <- max(efficiency$`quant.95%.default`)
  } else if ((NCIs == "default" & Nquartiles == "informed") | (NCIs == "informed" & Nquartiles == "default")){
    ymax <- max(max(efficiency$`quant.95%.default`), max(efficiency$`quant.95%.informed`))
  } else {
    ymax <- max(efficiency$`quant.95%.informed`)
  }

  # Define scale of y axis
  ylim <- c(10, ymax)
  yticks <- pretty(ylim, n = 7)
  
  # Labeling (Default and informed Medians)
  max.mediandefault <- max(efficiency$median.default)
  max.medianinformed <- max(efficiency$median.informed)
  
  if (abs(max.mediandefault-max.medianinformed) < (yticks[2]-yticks[1])){
    if (max.mediandefault > max.medianinformed){
      ylabel.default <- 1.06 * max.mediandefault
      ylabel.informed <- 0.94 * max.medianinformed
    } else {
      ylabel.default <- 0.94 * max.mediandefault
      ylabel.informed <- 1.06 * max.medianinformed
    }
  } else {
    ylabel.default <- max.mediandefault
    ylabel.informed <- max.medianinformed
  }
  
  # Here starts the actual plot
  
  # Start with drawing the line for the default median (if selected)
  if (median.default == TRUE){
    
    plot(efficiency$boundary2, efficiency$median.default,
         xlim = c(0, 40), ylim = c(10, ymax), type = "l", lwd = 2,
         lty = 1, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    axis(side = 1, at = c(3, 10, 15, 20, 25, 30))
    axis(side = 2, at = yticks)
    
    text(x = 30, y = ylabel.default, labels = "Median N Default", lwd = 2, pos = 4,
         cex = 1.2)
  }
  
  # Draw informed median (if selected)
  if (median.informed == TRUE){
    
    if(median.default == TRUE){
      par(new = TRUE)
    }
    
    plot(efficiency$boundary2, efficiency$median.informed,
         xlim = c(0, 40), ylim = c(10, ymax), type = "l", lwd = 2,
         lty = 3, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    axis(side = 1, at = c(3, 10, 15, 20, 25, 30))
    axis(side = 2, at = yticks)
    
    text(x = 30, y = ylabel.informed, labels = "Median N Informed", lwd = 2, pos = 4,
         cex = 1.2)
    
  }
  
  # Axis labeling
  mtext("Boundary", 1, line = 2.5, cex = 1.5, font = 2)
  mtext("Median N per Group", 2, line = 4, cex = 1.5, font = 2, las = 0)
  title(main = bquote(paste("Expected N per group (Data Generating Process: ", delta*" = ", .(true.ES), ")")))
  
  # Draw quartiles (if selected)
  if (Nquartiles == "default" & nopriorselected == FALSE){
    polygon(x = c(efficiency$boundary2, rev(efficiency$boundary2)),
            y = c(efficiency$`quant.25%.default`, rev(efficiency$`quant.75%.default`)),
            col = alpha("grey", 0.5), border = NA)
  } else if (Nquartiles == "informed" & nopriorselected == FALSE){
    polygon(x = c(efficiency$boundary2, rev(efficiency$boundary2)),
            y = c(efficiency$`quant.25%.informed`, rev(efficiency$`quant.75%.informed`)),
            col = alpha("grey", 0.5), border = NA)
  } 
  
  # Draw whiskers (if selected)
  if (NCIs == "default" & nopriorselected == FALSE){
    polygon(x = c(efficiency$boundary2, rev(efficiency$boundary2)),
            y = c(efficiency$`quant.5%.default`, rev(efficiency$`quant.95%.default`)),
            col = alpha("grey", 0.3), border = NA)
  } else if (NCIs == "informed" & nopriorselected == FALSE){
    polygon(x = c(efficiency$boundary2, rev(efficiency$boundary2)),
            y = c(efficiency$`quant.5%.informed`, rev(efficiency$`quant.95%.informed`)),
            col = alpha("grey", 0.3), border = NA)
  }
  

}

# ----------------------------------------------------------------------------------
# ---------------------------- CLEAN HISTOGRAM -------------------------------------
# ----------------------------------------------------------------------------------

#' @param x variable for which histogram should be drawn
#' @param true.ES true effect size

cleanhist <- function(x, true.ES){
  
  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1,
      mgp = c(3.5, 1, 0), cex.lab = 1.5 ,
      font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
  
  hist(x,
       main = bquote(paste("Distribution of N (DGP: ", delta*" = ", .(true.ES), ")")),
       xlab = "",
       ylab = "",
       col = "grey",
       breaks = 20)
  
  mtext("Sample Size", side = 1, line = 2.5, cex = 1.5, font = 2)
  mtext("Frequency", side = 2, line = 4, cex = 1.5, font = 2, las = 0)
  
  rug(jitter(x), lwd = 0.2)
  
  box(which = "figure", lwd = 2)
  
}

#' @param true.ES true effect size (if effect exists)
#' @param n.default.H1 vector of n under condition default/H1
#' @param n.default.H0 vector of n under condition default/H0
#' @param n.informed.H1 vector of n under condition informed/H1
#' @param n.informed.H0 vector of n under condition informed/H0

cleanhist.combine <- function(true.ES, n.default.H1 = NA, n.default.H0 = NA, n.informed.H1 = NA, n.informed.H0 = NA, default = TRUE, informed = TRUE, H0 = TRUE, H1 = TRUE){
  
  # Define matrix
  
  ifelse(xor(default, informed),
         ncolmatrix <- 1,
         ncolmatrix <- 2)
  
  ifelse(xor(H0, H1),
         nrowmatrix <- 1,
         nrowmatrix <- 2)

  par(mfrow = c(nrowmatrix, ncolmatrix))
  
  # Draw plots into matrix
  
  if (default == TRUE & H1 == TRUE){
    cleanhist(n.default.H1, true.ES)
    title("Prior on Effect Size: Default", line = 0, font.main = 1)
  }
  
  if (default == TRUE & H0 == TRUE){
    cleanhist(n.default.H0, true.ES = 0)
    title("Prior on Effect Size: Default", line = 0, font.main = 1)
  }
  
  if (informed == TRUE & H1 == TRUE){
    cleanhist(n.informed.H1, true.ES)
    title("Prior on Effect Size: Informed", line = 0, font.main = 1)
  }
  
  if (informed == TRUE & H0 == TRUE){
    cleanhist(n.informed.H0, true.ES = 0) 
    title("Prior on Effect Size: Informed", line = 0, font.main = 1)
  }
}

# ----------------------------------------------------------------------------------
# -------------------------- DECISION ERROR PLOTS ----------------------------------
# ----------------------------------------------------------------------------------


plot.FNrate <- function(efficiency, true.ES, default = TRUE, informed = TRUE){
  
  efficiency <- as.data.frame(efficiency)
  
  # Set plot size
  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
      font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
  
  # Set ymax
  if (default == TRUE & informed == TRUE){
    ymax <- max(max(efficiency$lower.hit.default), max(efficiency$lower.hit.informed))
  } else if (default == TRUE) {
    ymax <- max(efficiency$lower.hit.default)
  } else {
    ymax <- max(efficiency$lower.hit.informed)
  }
  
  # Here starts the actual plot
  
  # Start with drawing the default line (if selected)
  if (default == TRUE){
    
    plot(efficiency$boundary2, efficiency$lower.hit.default,
         type = "l", lwd = 2, lty = 1, xlab = "",ylab = "",
         xaxt = "n", ylim = c(0, ymax))
    axis(side = 1, at = c(3, 10, 15, 20, 25, 30))
    
  }
  
  # Draw informed median (if selected)
  if (informed == TRUE){
    
    if(default == TRUE){
      par(new = TRUE)
    }
    
    plot(efficiency$boundary2, efficiency$lower.hit.informed,
         type = "l", lwd = 2, lty = 3, xlab = "", ylab = "",
         xaxt = "n", ylim = c(0, ymax))
    axis(side = 1, at = c(3, 10, 15, 20, 25, 30))
    
  }
  
  # Axis labeling
  mtext("Boundary", 1, line = 2.5, cex = 1.5, font = 2)
  mtext("FN Evidence in %", 2, line = 3.8, cex = 1.5, font = 2, las = 0)
  title(main = bquote(paste("False Negative Evidence for ", delta*" = ", .(true.ES))))
  
  if (default == TRUE & informed == TRUE){
    legend("topright", legend = c("Default", "Informed"), lty = c("solid", "dotted"), bty = "n")
  } else if (default == TRUE) {
    legend("topright", legend = "Default", lty = "solid", bty = "n")
  } else {
    legend("topright", legend = "Informed", lty = "dotted", bty = "n")
  }
  
}


plot.FPrate <- function(efficiency.0, default = TRUE, informed = TRUE){
  
  efficiency.0 <- as.data.frame(efficiency.0)
  
  # Set plot size
  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
      font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
  
  # Set ymax
  if (default == TRUE & informed == TRUE){
    ymax <- max(max(efficiency.0$upper.hit.default), max(efficiency.0$upper.hit.informed))
  } else if (default == TRUE) {
    ymax <- max(efficiency.0$upper.hit.default)
  } else {
    ymax <- max(efficiency.0$upper.hit.informed)
  }
  
  # Here starts the actual plot
  
  # Start with drawing the default line (if selected)
  if (default == TRUE){
    
    plot(efficiency.0$boundary2, efficiency.0$upper.hit.default,
         type = "l", lwd = 2, lty = 1, xlab = "",ylab = "",
         xaxt = "n", ylim = c(0, ymax))
    axis(side = 1, at = c(3, 10, 15, 20, 25, 30))
    
  }
  
  # Draw informed median (if selected)
  if (informed == TRUE){
    
    if(default == TRUE){
      par(new = TRUE)
    }
    
    plot(efficiency.0$boundary2, efficiency.0$upper.hit.informed,
         type = "l", lwd = 2, lty = 3, xlab = "", ylab = "",
         xaxt = "n", ylim = c(0, ymax))
    axis(side = 1, at = c(3, 10, 15, 20, 25, 30))
    
  }
  
  # Axis labeling
  mtext("Boundary", 1, line = 2.5, cex = 1.5, font = 2)
  mtext("FN Evidence in %", 2, line = 3.8, cex = 1.5, font = 2, las = 0)
  title(main = paste0("False Positive Evidence"))
  
  if (default == TRUE & informed == TRUE){
    legend("topright", legend = c("Default", "Informed"), lty = c("solid", "dotted"), bty = "n")
  } else if (default == TRUE) {
    legend("topright", legend = "Default", lty = "solid", bty = "n")
  } else {
    legend("topright", legend = "Informed", lty = "dotted", bty = "n")
  }
  
}

combinedplot.errorrates <- function(efficiency, efficiency.0, true.ES, default = TRUE, informed = TRUE, H0 = TRUE, H1 = TRUE){
  
  efficiency <- as.data.frame(efficiency)
  
  if (H0 == TRUE & H1 == TRUE){
    par(mfrow = c(1,2))
  }
  
  if (H0 == TRUE & H1 == TRUE){
    plot.FNrate(efficiency, true.ES, default=default, informed=informed)
    plot.FPrate(efficiency.0, default=default, informed=informed)
  } else if (H0 == TRUE){
    plot.FPrate(efficiency.0, default=default, informed=informed)
  } else {
    plot.FNrate(efficiency, true.ES, default=default, informed=informed)
  }
}

# ----------------------------------------------------------------------------------
# ------------------------------- Violin Plot --------------------------------------
# ----------------------------------------------------------------------------------

#' @import ggplot2
#' 
#' @param ndistlist data frame from which data are extracted (ndists object)
#' @param true.ES true ES
#' @param bound symmetric decision boundary (positive integer) 
#' @param default Should default design be plotted
#' @param informed Should informed design be plotted
#' @param H1 Should data for H1 as DGP be plotted
#' @param H0 Should data for H0 as DGP be plotted

cleanviolinplot <- function(ndistslist, true.ES, bound, default = TRUE, informed = TRUE, H1 = TRUE, H0 = TRUE){
  
  if((default == FALSE & informed == FALSE) | (H0 == FALSE & H1 == FALSE)){
    return()
  }
  
  # Get the data
  
  if(default == TRUE & H1 == TRUE){
    default.H1 <- ndistslist[[paste0("ndists.", true.ES)]][[paste0("default.", bound)]]
  } else {default.H1 <- c()}
  
  if(default == TRUE & H0 == TRUE){
    default.H0 <- ndistslist[[paste0("ndists.", 0)]][[paste0("default.", bound)]]
  } else {default.H0 <- c()}
  
  if(informed == TRUE & H1 == TRUE){
    informed.H1 <- ndistslist[[paste0("ndists.", true.ES)]][[paste0("informed.", bound)]]
  } else {informed.H1 <- c()}
  
  if(informed == TRUE & H0 == TRUE){
    informed.H0 <- ndistslist[[paste0("ndists.", 0)]][[paste0("informed.", bound)]]
  } else {informed.H0 <- c()}
  
  dat.long <- c(default.H1, default.H0, informed.H1, informed.H0)
  category <- c(rep("Default Prior, DGP: H1", length(default.H1)),
                rep("Default Prior, DGP: H0", length(default.H0)),
                rep("Informed Prior, DGP: H1", length(informed.H1)),
                rep("Informed Prior, DGP: H0", length(informed.H0))
  )
  
  violinplotdata <- data.frame(dat.long, category)
  
  yticks <- pretty(log(violinplotdata$dat.long))
  if (H1 == FALSE){
    plottitle <- bquote(paste("Distribution of N if H0 is true (DGP:", delta*" = 0)"))
  } else {
    plottitle <- bquote(paste("Distribution of N (DGP for H1: ", delta*" = ", .(true.ES), ")"))
  }
  
  
  # Create the plot
  
  p <- ggplot(violinplotdata, aes(x=category, y=log(dat.long))) +
    
    geom_jitter(size = 1, shape = 20, stroke = 1,
                position = position_jitter(width=0.05, height = 0),
                color = "grey") +
    geom_violin(trim = FALSE, size = 0.75, width = 0.3, scale = "width", fill = "transparent") +
    stat_boxplot(geom = "errorbar", size = 0.75, width = 0.1) +
    geom_boxplot(size = 0.75, width = 0.2, outlier.shape = NA, fill = "transparent") +
    geom_violin(trim = FALSE, size = 0.75, width = 0.3,
                fill = "transparent", scale = "width") +
    
    theme_light() +
    theme(plot.title = element_text(size=18, hjust = 0.5),
          axis.text.x = element_text(size=13),
          axis.title = element_text(size=17),
          axis.text = element_text(size=13)) +
    labs(x = "", y = "Sample Size") +
    ggtitle(plottitle) +
    scale_y_continuous(breaks = yticks, labels = as.character(round(exp(yticks), 0)))
  
  
  return(p)
 

}

# ----------------------------------------------------------------------------------
# ----------------------------------- Boxplot --------------------------------------
# ----------------------------------------------------------------------------------

#' @import ggplot2
#' 
#' @param ndistlist data frame from which data are extracted (ndists object)
#' @param true.ES true ES
#' @param bound symmetric decision boundary (positive integer) 
#' @param default Should default design be plotted
#' @param informed Should informed design be plotted
#' @param H1 Should data for H1 as DGP be plotted
#' @param H0 Should data for H0 as DGP be plotted


cleanboxplot <- function(ndistslist, true.ES, bound, default = TRUE, informed = TRUE, H1 = TRUE, H0 = TRUE){
    
    if((default == FALSE & informed == FALSE) | (H0 == FALSE & H1 == FALSE)){
      return()
    }
    
    if(default == TRUE & H1 == TRUE){
      default.H1 <- ndistslist[[paste0("ndists.", true.ES)]][[paste0("default.", bound)]]
    } else {default.H1 <- c()}
    
    if(default == TRUE & H0 == TRUE){
      default.H0 <- ndistslist[[paste0("ndists.", 0)]][[paste0("default.", bound)]]
    } else {default.H0 <- c()}
    
    if(informed == TRUE & H1 == TRUE){
      informed.H1 <- ndistslist[[paste0("ndists.", true.ES)]][[paste0("informed.", bound)]]
    } else {informed.H1 <- c()}
    
    if(informed == TRUE & H0 == TRUE){
      informed.H0 <- ndistslist[[paste0("ndists.", 0)]][[paste0("informed.", bound)]]
    } else {informed.H0 <- c()}
    
    dat.long <- c(default.H1, default.H0, informed.H1, informed.H0)
    category <- c(rep("Default Prior, DGP: H1", length(default.H1)),
                  rep("Default Prior, DGP: H0", length(default.H0)),
                  rep("Informed Prior, DGP: H1", length(informed.H1)),
                  rep("Informed Prior, DGP: H0", length(informed.H0))
    )
    
    bpdata <- data.frame(dat.long, category)
    
    yticks <- pretty(log(bpdata$dat.long))
    
    if (H1 == FALSE){
      plottitle <- bquote(paste("Distribution of N if H0 is true (DGP: ", delta*" = 0)"))
    } else {
      plottitle <- bquote(paste("Distribution of N (DGP for H1: ", delta*" = ", .(true.ES), ")"))
    }
    
    
    p <- ggplot(bpdata, aes(x = category, y = log(dat.long))) +
      
      stat_boxplot(geom ='errorbar',size = 0.75, width = 0.1) +
      geom_boxplot(width = 0.2, size = 0.75) +
      
      theme_light() +
      theme(plot.title = element_text(size=18, hjust = 0.5),
            axis.text.x = element_text(size=13),
            axis.title = element_text(size=17),
            axis.text = element_text(size=13)) +
      labs(x = "", y = "Sample Size") +
      ggtitle(plottitle) +
      scale_y_continuous(breaks = yticks, labels = as.character(round(exp(yticks), 0)))
    
    return(p)
}
  
  
  