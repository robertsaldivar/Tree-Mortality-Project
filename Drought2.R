#' Drought Submodel
#'
#'
#'@param SPEI The Standardised Precipitation-Evapotranspiration Index The range for the input is -3 to 3
#'@param WD This the mean wood density. The default is set for Quercus virginiana
#'@param SLA The Surface mean Leaf Area. The default is set for Quercus virginiana
#'@return log Mortality (log trees/Trees*Year)
#'
#'References for 
#'
#' models mortality data (proportion of standing dead trees and annual rates). Models only for adult trees not seedlings or saplings
#' 
#'  for the input data I am currently using the balance data from the SPEI package

library(SPEI)


drought_function <- function(input_data, WD = 0.946, SLA = 7.39, coeff1 = -1.28, coeff2 = 0.38, coeff3 = -0.41) {
  
  
  SPEI = SPEI::spei(data = input_data, scale = 12)
  
  SPEI = as.data.frame(SPEI$fitted)
  
  log_mortality_drought = coeff1*SPEI + coeff2*SPEI*WD + coeff3*SPEI*SLA
  
  drought_mortality = exp(log_mortality_drought)
  
  drought_mortality_rate = drought_mortality/100
  
  #ifelse((drought_mortality_rate >= 1), 0.99, drought_mortality_rate)
  
  return(drought_mortality_rate)
}
