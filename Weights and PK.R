# 
# Weighting Formula and PK Function
#
# LE Model by AE & KP 7/7/2020
# 

rm(list = ls())


WM <- function(x,w){
  
  ans <-sum(x*w)/sum(w)
  return(ans)
}

# x = mean
# w = weight


WSD <- function(x,wm,w){
  
M           <- as.numeric(length(w[w > 0]))
numerator   <- sum(w*(x- wm)^2)
denominator <- (M - 1)/M * (sum(wm))
ans         <- numerator / denominator

return(ans)                            
}

# x = mean
# wm = weighted mean
# M = number of non zero weights


Simple.Serum.PK <- function(DP,kP,Vd){
  
  CP <- DP/(kP * Vd)
  
  return(CP)
}


Simple.Dose.PK <- function(CP,kP,Vd){
  
  DP <- CP*kP*Vd
  
  return(DP)
}

# DP = dose (ng/kg bw/day) # notice its per kg BODYWEIGHT
# CP = serum conc. ng/mL
# Vd = volume distribution (mL/kg bw)
# kP = elimination rate (day -1)


# Test
WM(1:5,2:6)
WSD(1:5,0:4,2:6)
Simple.Serum.PK(5,.0008,170)
Simple.Dose.PK(10,.00039,200)
