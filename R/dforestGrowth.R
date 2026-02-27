# Create deforest function to include differential equations in different stages of life cycle 
# T = time 
# C = forest size
# Params are established (r, g, canopy_thresh, carrying capacity K)
dforestgrowth <- function(Time, C, parms) {
  # If forest still below threshold (50 kgC)
  if (C < parms$canopy_thresh) {
    # Grow exponentially (r*current forest size)
    dC <- parms$r * C
    
  } else {
    # If forest is >= threshold 
    dC <- parms$g * (1 - C / parms$K)
    # Linear growth towards K 
  }
  
  return(list(dC)) # Output is a list according to deSolve
}