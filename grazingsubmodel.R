#' Grazing Submodel
#' 
#' @param r growth rate of trees
#' @param tree_K trees carrying capacity
#' @param tree_n number of trees from last timestep
#' @param herb_n number of herbivores from last timestep


# The plant and herbivore equations below are based off equations from Kang et al. 2008 

# The following assumptions are made in this model: 
## - entire canopy of individual tree is accessible to the herbivore at all times
## - herbivores are always present (at all time steps), they are not seasonal grazers
## - trees do not lose leaves seasonally, able to be eaten by herbivores at all time steps
## - alpha value is based on biological relationship between herbivores (deer in this case) 
##   and trees (oaks in this case)


# 2. use equations that give total biomass (and total amount) of tree + herb populations

grazingsubmodel = function(r=0.7, herb_n, tree_n, tree_K=2000, biomassscaling, alphascaling){

###leftover from testing
#r=0.7
#tree_K=2000
###  
  # Need population values for age classes in a data frame
  
  
  population_df <- data.frame("age_class" = c(1:10), "tree_n" = t(tree_n), "herb_n" = herb_n)
  #for some reason this names the column that's supposed to be "tree_n" "X1" instead
  colnames(population_df) = c("age_class","tree_n","herb_n")
  
  
  # Converting from tree population size to biomass, using scaling factors that are specific to 
  # each age class
  tree_biomass = population_df$tree_n / biomassscaling$scaling
  
  # Converting from herbivore population size to biomass, assuming one herbivore contains 5 g C 
  # per individual
  herb_biomass = population_df$herb_n * 5
  
  
  # Equations from paper:
  #x_t = P_t/Pmax
  #y_t = H_t/Pmax
  #xn+1 = 
  
  
  tree_biomass_n1 = tree_biomass*exp(r*(1-tree_biomass/tree_K)-alphascaling$alpha*(herb_biomass/tree_K)) #there was an /tree_K missing in the herbivore biomass part
  
  #conceptually this is not doing what we want it to. It is changing the herbivore biomass by including the herbivory AND
  #natural growth rate for EACH 'age bin' of herbivores, even though there aren't any age bins of herbivores, just 10 different
  #instances of the same herbivores, solely for the purpose of allowing them to prey on the trees in each age bin
  
  #in other words, it's accounting for the natural growth rate of the same [herb_n] herbivores ten different times
  
  #for now we'll go with it though, and treat the herbivore biomamss as the sume of this value
  herbivore_biomass_n1 = tree_biomass * exp(r * (1 - ((tree_biomass)/tree_K))) * (1 - exp(-alphascaling$alpha 
                                                                                          * herb_biomass/tree_K))
  
  
  # Then convert total tree population biomass, for each age class, back into amount of individuals, at
  # each time step. 
  
  tree_individuals = tree_biomass_n1 * biomassscaling$scaling
  #we can return this value directly
  
  
  #convert herbivores back to number of individuals in case we want to plot them
  newherb_n = sum(herbivore_biomass_n1)/5
  
  return(list(tree_individuals,newherb_n)) 
}