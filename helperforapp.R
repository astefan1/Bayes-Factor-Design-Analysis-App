# HELPER FOR UI DESIGN (CHOICEVALUES, CHOICEVALUES FOR PLOT)

methodselection <- function(method){
  
  if (length(method) == 0) {method <- NA}
  
  if(length(method) == 2){
    
    choicenames.hinges <- list("Do not display hinges", "Display for default prior", "Display for informed prior")
    choicenames.whiskers <- list("Do not display whiskers", "Display for default prior", "Display for informed prior")
    choicevalues <- list("none", "default", "informed")
    
  } else if ("Default" %in% method){
    
    choicenames.hinges <- list("Do not display hinges", "Display hinges for default prior")
    choicenames.whiskers <- list("Do not display whiskers", "Display for default prior")
    choicevalues <- list("none", "default")
    
  } else if ("Informed" %in% method){
    
    choicenames.hinges <- list("Do not display hinges", "Display hinges for informed prior")
    choicenames.whiskers <- list("Do not display whiskers", "Display for informed prior")
    choicevalues <- list("none", "informed")
    
  } else if (is.na(method)){
    
    choicenames.hinges <- list("Do not display hinges")
    choicenames.whiskers <- list("Do not display whiskers")
    choicevalues <- list("none")
  }
  
  choices <- list(choicenames.hinges, choicenames.whiskers, choicevalues)
  return(choices)
}

# HELPER FOR SUBSETTING

subset.method <- function(df, true.ES, default = TRUE, informed = TRUE, H0 = FALSE){
  
  df <- as.data.frame(df)
  
  if(default == TRUE & informed == FALSE){
    df.subset <- select(df, ends_with("default"), boundary2)
  } else if (default == FALSE & informed == TRUE){
    df.subset <- select(df, ends_with("informed"), boundary2)
  } else if (default == TRUE & informed == TRUE) {
    df.subset <- df
  } else {
    df.subset <- NA
  }

  
  if (H0 == TRUE){
      colnames(df.subset) <- paste0(colnames(df.subset), ".H0")
  }
  return(df.subset)
}

# HELPERS FOR PLOTS

inside <- function(x, R) Vectorize({x >= R[1] & x <= R[2]})

drawpoly <- function(dens, from, to, ...) {
  poly <- data.frame(x=dens$x, y=dens$y)
  poly <- poly %>% filter(x>from & x<to)
  poly <- rbind(c(x=min(poly$x), y=0), poly, c(x=max(poly$x), 0))
  polygon(poly$x, poly$y, ...)
}


