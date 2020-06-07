#' Grazing Submodel
#' 
#' @param r growth rate of trees
#' @param tree_K trees carrying capacity
#' @param tree_n number of trees from last timestep
#' @param herb_n number of herbivores from last timestep
#' @param biomassscaling the data frame containing biomass conversion factors for the tree 
#' population
#' @param alphascaling the data frame containing alpha values that represent the effect of the 
#' herbivore population on the tree population
#' @param alpha2 the alpha value representing the effect of the tree population on the herbivore 
#' population
#' @param herbcc the carrying capacity of the herbivore population
#' @references Kang, Yun, Dieter Armbruster, and Yang Kuang. 2008. “Dynamics of a Plant–Herbivore Model.” Journal of Biological Dynamics 2 (2): 89–101. https://doi.org/10.1080/17513750801956313.


# The plant and herbivore equations below are based off equations from Kang et al. 2008

# The following assumptions are made in this model: 
## - entire canopy of individual tree is accessible to the herbivore at all times
## - herbivores are always present (at all time steps), they are not seasonal grazers
## - trees do not lose leaves seasonally, able to be eaten by herbivores at all time steps
## - alpha value is based on biological relationship between herbivores (deer in this case) 
##   and trees (oaks in this case)
## - herbivore carrying capacity is set to a user-selected value. If the herbivore population
##   exceeds this number, the herbivore population is set to the carrying capacity value 
##   (resembling natural conditions where a location cannot support any additional animals on 
##   the landscape). Growth rate does not slow as this value is approached.


# 2. use equations that give total biomass (and total amount) of tree + herb populations

grazingsubmodel = function(r=0.7, herb_n, tree_n, tree_K=200000, biomassscaling, alphascaling, alpha2 = 0.2, herbcc = 400){
  
  
  population_df <- data.frame("age_class" = c(1:10), "tree_n" = t(tree_n), "herb_n" = herb_n)
  #for some reason this names the column that's supposed to be "tree_n" "X1" instead
  colnames(population_df) = c("age_class","tree_n","herb_n")
  
  
  # Converting from tree population size to biomass, using scaling factors that are specific to 
  # each age class
  tree_biomass = population_df$tree_n / biomassscaling$scaling
  
  # Converting from herbivore population size to biomass, assuming one herbivore contains 5 g C 
  # per individual
  herb_biomass = population_df$herb_n * 5
  
  
  tree_biomass_n1 = tree_biomass*exp(r*(1-tree_biomass/tree_K)-alphascaling$alpha*(herb_biomass)) 
  
  
  # We'll treat the herbivore biomass as the sum of the tree biomass values across all age classes
  herbivore_biomass_n1 = alpha2*sum(tree_biomass) * exp(r * (1 - (sum(tree_biomass)/tree_K)))
  
  if (herbivore_biomass_n1 > herbcc) {
    herbivore_biomass_n1 = herbcc
  }                                                
  
  
  # Then convert total tree population biomass, for each age class, back into amount of individuals,
  # at each time step. 
  tree_individuals = tree_biomass_n1 * biomassscaling$scaling
  
  
  #convert herbivores back to number of individuals
  newherb_n = sum(herbivore_biomass_n1)/5
  
  return(list(tree_individuals,newherb_n)) 
}