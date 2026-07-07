## testing fDOM correction methods

#temperature correction from Watras et al. 2011-----
#using average of value reported by Watras et al. 2011, also used in Akie et al. 2024
fdom_temp <- function(fdom, temp, rho=-0.0115){
   fdom_tc <- fdom / (1 + rho*(temp - 25))
   return(fdom_tc)
}

#equations from Fleck et al. 2026 -----
  #using fits based on 7 best sensor combinations, does pretty well
  fdom_inverse_poly <- function(fdom_tc, turb, x=0.96, y=0.0097, z=9.6e-6){
   corr_factor <- x + (y * turb) + (z * turb^2)

   fdom_corr <- corr_factor * fdom_tc

   return(fdom_corr)
  }

  #does poorly at higher turbidities
  fdom_2p_exponential <- function(fdom_tc, turb, b=0.995, c=-0.0062){
    fdom_corr <- b * exp(c*turb)
    return(fdom_corr)
  }

  #does better at higher turbidites, based on avg of 11 sensors
  fdom_5p_exponential <- function(fdom_tc, turb, a=0.053, b=0.647, c=-0.0047, d=0.354, e=-0.0224){
    fdom_corr <- fdom_tc * (a + (b*exp(c*turb)) + (d*exp(e*turb)))
    return(fdom_corr)
  }

#equations from downing et al. 2012 ------
  fdom_1p_exponential <- function(fdom_tc, turb, a=-0.003){
    fdom_corr <- fdom_tc / exp(a * turb)
    return(fdom_corr)
  }


#workflow
  #select method
  #show parameter boxes with default
  #show citation for default parameters small

  #update data
  #flagging
