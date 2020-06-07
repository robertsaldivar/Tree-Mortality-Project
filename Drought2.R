#' Drought Submodel
#'
#'
#'@param input_data The input_data is the climatic water balance.
#'@param WD This the mean wood density. The default is set for Quercus virginiana
#'@param SLA The mean Surface Leaf Area. The default is set for Quercus virginiana
#'@param Coeff1 This is the coefficienct that is SPEI part of the equation. THe default is set to -1.28 but the potential range of the parameter is -1.28 to -0.52
#'@param Coeff2 This is the coefficienct that is SPEI*WD part of the equation. THe default is set to 0.38 but the potential range of the parameter is -0.2 to 0.97
#'@param Coeff3 This is the coefficienct that is SPEI*SLA part of the equation. THe default is set to -0.41 but the potential range of the parameter is -0.95 to 0.13
#'@return mortality rate
#'@references Greenwood, Sarah, Paloma Ruiz-Benito, Jordi Martínez-Vilalta, Francisco Lloret, Thomas Kitzberger, Craig D. Allen, Rod Fensham, et al. 2017. “Tree Mortality across Biomes Is Promoted by Drought Intensity, Lower Wood Density and Higher Specific Leaf Area.” Edited by Jerome Chave. Ecology Letters 20 (4): 539–53. https://doi.org/10.1111/ele.12748.
#'
#'


library(SPEI)


drought_function <- function(input_data, WD = 0.946, SLA = 7.39, coeff1 = -1.28, coeff2 = 0.38, coeff3 = -0.41) {
  
  
  SPEI = SPEI::spei(data = input_data, scale = 12)
  
  SPEI = as.data.frame(SPEI$fitted)
  
  log_mortality_drought = coeff1*SPEI + coeff2*SPEI*WD + coeff3*SPEI*SLA
  
  
  drought_mortality = exp(log_mortality_drought)
  
  drought_mortality_rate = drought_mortality/100
  
  
  return(drought_mortality_rate)
}
