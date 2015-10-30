# Title: inbreed function
#
# Author: Michael Koontz
# Email: mikoontz@gmail.com
#
# Date Created: 20150401
# Last Updated: 20151021

### Intention: Uses model from Crow and Kimura (1970; pg 102 and 269) to determine theoretical loss of heterozygosity (relative to that of initial population size) through generations. Takes in a matrix of population sizes. Calculates the inbreeding coefficient (F) through time representing the probability that two alleles are identical by descent (common ancestry). IMPORTANT NOTE: This version assumes no immigration

# Note that this calculation generates the Ft value by NOT assuming self fertilization is possible. 

# N should be a matrix of population sizes.

# Optional input F0 describes the inbreeding coefficient of the large external population. Perhaps, through the course of dozens of generations in the lab, this large population is somewhat inbred as well.

# Update to fix bug where N[t]==1 would return an NaN due to dividing by 0 and then propagate through the rest of the time series
inbreed_noMigrants <- function(N, F0=0)
{
  Fst <- matrix(F0, nrow=nrow(N), ncol=length(N)+1)
  
  for (ft in 3:ncol(Fst))
  {
    t <- ft-1 # Time from perspective of population is offset by 1 generation
    Fst[, ft] <- Fst[, ft-1] + ( (1-2*Fst[, ft-1]+Fst[, ft-2]) / (2*N[, t]) )
  }
  
  # Remove the first column (because it represents the Fst of the generation that produced the large external population from which the first generation of founders is derived)
  Fst <- Fst[, -1]
  
  # For simplicity's sake, redeclare Fst values for extinct populations as NA (rather than -Inf or NaN)
  
  Fst[N==0] <- NA
  
  return(Fst)
}

