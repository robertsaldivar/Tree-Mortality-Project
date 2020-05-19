#' Drought Submodel
#'
#'

drought_function <- function(SPEI, WD, SLA, SPEI_coeff = -1.28, WD_coeff = 0.38, SLA_coeff = -0.41) {
  mortality_drought = SPEI_coeff*SPEI + SPEI_coeff*SPEI*WD_coeff*WD + SPEI_coeff*SPEI*SLA_coeff*SLA
  
  return(mortality_drought)
}