#' Drought Submodel
#'
#'
#'@param SPEI The Standardised Precipitation-Evapotranspiration Index
#'@param WD This the mean wood density. The default is set for Quercus gambelii
#'@param SLA The Surface mean Leaf Area. The default is set for Quercus gambelii
#'
#'References for 
#'
#' models mortality data (proportion of standing dead trees and annual rates). Models only for adult trees not seedlings or saplings


drought_function <- function(SPEI, WD = 0.62, SLA = 13.89, SPEI_coeff = -1.28, WD_coeff = 0.38, SLA_coeff = -0.41) {
  mortality_drought = SPEI_coeff*SPEI + SPEI_coeff*SPEI*WD_coeff*WD + SPEI_coeff*SPEI*SLA_coeff*SLA
  
  return(mortality_drought)
}