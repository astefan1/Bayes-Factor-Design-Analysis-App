# ------------------------------------------------------------------------------
# ---------------- ANALYSIS FUNCTIONS FOR FIXED-N DESIGN -----------------------
# ------------------------------------------------------------------------------

############################# Errorrates #######################################

#' @import dplyr

#' @param sim.tomaxn A simulated dataset (corresponding to res-datasets)
#' @param method "Default" and/or "Informed" method to compute Bayes factor
#' @param boundary Selected evidence boundary for decisions (symmetric boundary -> integer)
#' @param samplesize Sample size (number from 10:200 by 1 or 220:500 by 20)
#' @param errortype 1 (type I, false positive) or 2 (type II, false negative)

FPFN.fixed <- function(sim.tomaxn, method, boundary, samplesize, errortype){
  
  # Define arguments, objects, and new functions
  method <- match.arg(method, c("Default", "Informed"), several.ok = TRUE)
  
  logBoundary <- log(boundary)
  
  lowerbound <- function(X){length(which(X <= -logBoundary))/10000}
  upperbound <- function(X){length(which(X >= logBoundary))/10000}
  
  sim <- as.data.frame(sim.tomaxn)
  
  # Filter only relevant information from simulation object
  sim.red <- sim %>% filter(n == samplesize)
  
  # Compute the percentages of trials which reached lower or upper bound
  lower.hit <- apply(sim.red[,4:5], 2, lowerbound)
  upper.hit <- apply(sim.red[,4:5], 2, upperbound)
  
  # Summarize results
  conc.evidence <- rbind(upper.hit, lower.hit)
  rownames(conc.evidence) <- c("False Positive Evidence", "False Negative Evidence")
  colnames(conc.evidence) <- c("Default Prior on ES", "Informed Prior on ES")
  

  # Show only relevant results (depending on selected method and errortype)
  if(length(method) == 2) {
    selectcols <- c(1,2)
  } else if ("Default" %in% method){
    selectcols <- 1
  } else {
    selectcols <- 2
  }
  
  errorrates <- conc.evidence[errortype, selectcols]

  return(errorrates)
  
}

#################### Sample size for expected BF ###############################

#' @import dplyr

#' @param sim.tomaxn A simulated dataset (corresponding to res-datasets)
#' @param evidencestrength An (absolute) Bayes factor as measure of evidence strength
#' @param prob Probability with which evidence strength should be obtained (number from 0:1)

NforBF <- function(sim.tomaxn, evidencestrength, prob, H0 = FALSE){
  
  # Define objects
  logEv <- log(evidencestrength)
  
  sim <- as.data.frame(sim.tomaxn)
  
  # Conduct analysis
  
  if(H0 == FALSE){
    
  analysis <- sim %>%
    do(data.frame(evstrength.default = abs(.$logBF.default),
                  evstrength.informed = abs(.$logBF.informed),
                  n = .$n)) %>% 
    group_by(n) %>% # group by 
    do(data.frame(prob.quant.default = quantile(.$evstrength.default, probs = 1-prob),
                  prob.quant.informed = quantile(.$evstrength.informed, probs = 1-prob))) %>%
    ungroup() %>%
    as.data.frame()
  
  nforBF.default <- analysis[which(analysis$prob.quant.default >= logEv), 1][1]
  nforBF.informed <- analysis[which(analysis$prob.quant.informed >= logEv),1][1]
  
  } else {
    analysis <- sim %>%
      do(data.frame(evstrength.default = .$logBF.default,
                    evstrength.informed = .$logBF.informed,
                    n = .$n)) %>% 
      group_by(n) %>% # group by 
      do(data.frame(prob.quant.default = quantile(.$evstrength.default, probs = prob),
                    prob.quant.informed = quantile(.$evstrength.informed, probs = prob))) %>%
      ungroup() %>%
      as.data.frame()
    
    nforBF.default <- analysis[which(analysis$prob.quant.default <= -logEv), 1][1]
    nforBF.informed <- analysis[which(analysis$prob.quant.informed <= -logEv),1][1]
  }
  
  if(H0 == FALSE){
    bound <- "larger than "
  } else {
    bound <- "smaller than 1/"
  }
  
  verbal.default <- paste0("you will need at least <strong>",
                           nforBF.default,
                           " observations per group </strong>to obtain a Bayes factor <strong>",
                           bound,
                           evidencestrength,
                           "</strong> with a probability of <strong>p = ",
                           prob,
                           "</strong>.")
  
  verbal.informed <- paste0("you will need at least <strong>",
                            nforBF.informed,
                            " observations per group</strong> to obtain a Bayes factor <strong>",
                            bound,
                            evidencestrength,
                            "</strong> with a probability of <strong>p = ",
                            prob,
                            "<strong>.")
  
  if(is.na(nforBF.default)){verbal.default <- paste0("you will need <strong>more than 500 observations per group</strong> to obtain a Bayes factor <strong>",
                                                     bound,
                                                     evidencestrength,
                                                     "</strong> with a probability of <strong>p = ",
                                                     prob,
                                                     "</strong>.")}
  if(is.na(nforBF.informed)){verbal.informed <- paste0("you will need <strong>more than 500 observations per group</strong> to obtain a Bayes factor <strong>",
                                                       bound,
                                                       evidencestrength,
                                                       "</strong> with a probability of <strong>p = ",
                                                       prob,
                                                       "</strong>.")}
  
  verbal <- c(verbal.default, verbal.informed)
  
  return(verbal)
  
}
