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

grazingsubmodel = function(r=0.7, herb_n=10, tree_n=20, tree_K=2000, biomassscaling, alphascaling){
  
  
  # Need population values for age classes in a data frame
  # Number of herbivores is the same regardless of the trees' age class. Assuming there is a 
  # overall herbivore population of 100 individuals, we will divide them up evenly among the 10
  # age classes so that 10 herbivores are present to eat any of the trees in any age class
   population_df <- data.frame("age_class" = c(1:10), "tree_n" = tree_n, "herb_n" = herb_n)

  
  # biomass scaling data frame
  ## We also need a biomass scaling data frame, which has age class-specific scaling factors. 
  ## For example, each individual in age class 1 has an average biomass (g C) of 0.5, etc. This 
  ## will help us convert between # of individuals and amount of biomass.
  biomassscaling <- data.frame("age_class" = c(1:10), "scaling" = c(1/2, 1/4, 1/5, 1/6, 1/7,
                                                                    1/8, 1/9, 1/10, 1/10, 1/10))
 
  # alpha data frame
  ## Alpha values represents the "feeding rate" or "effect" of herbivores on trees, in each age 
  ## class. Because most full grown herbivores (deer) can likely eat the leaves of an entire 
  ## individual in age classes 1 and 2, we will assign higher alpha values to the smaller age 
  ## classes and lower alpha values to the larger age classes
  alphascaling <- data.frame("age_class" = c(1:10), "alpha" = c(0.09, 0.08, 0.07, 0.06, 0.06, 0.06, 
                                                                0.05, 0.04, 0.03, 0.01))
  
  
  # Converting from tree population size to biomass, using scaling factors that are specific to 
  # each age class
  tree_biomass = population_df$tree_n / biomassscaling$scaling
  
  # Converting from herbivore population size to biomass, assuming one herbivore contains 5 g C 
  # per individual
  herb_biomass = population_df$herb_n * 5

      
  # Equations from paper:
    
  tree_biomass_n1 = tree_biomass * exp((r * (1 - ((tree_biomass)/tree_K)) - (alphascaling$alpha * herb_biomass)))
  
  herbivore_biomass_n1 = tree_biomass * exp(r * (1 - ((tree_biomass)/tree_K))) * (1 - exp(-alphascaling$alpha 
  * herb_biomass))
  
  
  # Then convert total tree population biomass, for each age class, back into amount of individuals, at
  # each time step. 
    
    tree_individuals = tree_biomass_n1 * biomassscaling$scaling
    
  # Finally, convert number of individuals into "amount of individuals that died". Comparing with
  # the initial population size, tree_n.
    amount_died = tree_individuals - tree_n
    
  # If the difference between the calculated population size and initial population size is 
  # positive or zero, then it is assumed no deaths have occurred (amont_died = 0). Otherwise,
  # the difference should reflect the number of deaths.
      amount_died = ifelse(amount_died >= 0, 0, abs(amount_died))
    
    
    # error checking for the case when tree_n = 0 (which would make grazing_died_proportion 
    # undefined). We'll assign an NA in that case.
    tree_n = ifelse(tree_n == 0, return(NA), tree_n)
    
    
    grazing_died_proportion = amount_died/tree_n
    
  
  return(grazing_died_proportion) 
}