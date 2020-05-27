#' Grazing Submodel
#' 
#' @param r growth rate of trees
#' @param alpha alpha, effect between herbivores and trees
#' @param tree_K trees carrying capacity
#' @param tree_n number of trees from last timestep
#' @param herb_n number of herbivores from last timestep


# The survivability equations below are based off equations from Kang et al. 2008 

# The following assumptions are made in this model: 
## - entire canopy of individual tree is accessible to the herbivore at all times
## - herbivores are always present (at all time steps), they are not seasonal grazers
## - trees do not lose leaves seasonally, able to be eaten by herbivores at all time steps
## - alpha value determined based on biological understanding of relationship between 
##   herbivores (deer in this case) and trees (oaks in this case)


# 2. use equations that give total biomass (and total amount) of tree + herb populations

grazingsubmodel = function(alpha, r, herb_n, tree_n, tree_K){
  
  # Number of time steps 
  nstep = 100 # years
  
  # Need population values for age classes in a data frame
  # Number of herbivores is the same regardless of the trees' age class. Assuming there is a 
  # overall herbivore population of 100 individuals, we will divide them up evenly among the 10
  # age classes so that 10 herbivores are present to eat any of the trees in any age class
  population_df <- data.frame("age_class" = c(1:10), "tree_n" = NA, "herb_n" = c(10, 10, 10, 10,
                                                                                  10, 10, 10, 10,
                                                                                  10, 10))
  
  # We also need a biomass scaling data frame, which has age class-specific scaling factors. 
  # For example, each individual in age class 1 has an average biomass (g C) of 10, etc. This 
  # will help us convert between # of individuals and amount of biomass.
  biomassscaling <- data.frame("age_class" = c(1:10), "scaling" = c(1/10, 1/20, 1/30, 1/40, 1/50,
                                                                     1/60, 1/70, 1/80, 1/90, 1/100))
 
  # Converting from population size to biomass
  tree_n = population_df$population / biomassscaling$scaling
  

  # Defining our other variables:
  ## Alpha values represent "the total amount of the biomass that an herbivore consumes", 
  ## essentially the "feeding rate" or effect of herbivores on trees. Because most full grown 
  ## herbivores (deer) can likely eat the leaves of an entire individual in age classes 1 and 2, we
  ## will assign the alpha value to be approximately 20 g C
  alpha = 20
  ## r is constant growth rate of total tree population
  r = 0.03
  ## tree_K is carrying capacity of total tree population
  tree_K = 400
  
  # Create data frame to store results from for loop below
  
  output_df <- data.frame("year" = c(1:100), "tree_biomass_n1" = NA, "herbivore_biomass_n1" = NA)
      
  # Then apply the biomass equations to each age class
  ## starts at 2 because we have to use the initial values of timestep 1 to calculate all
  ## subsequent values
  for (i in 2:nstep) {
    
  output_df$tree_biomass_n1[i] = tree_n[i] * exp(r * (1 - ((tree_n[i])/tree_K)) - (alpha * herb_n[i]))
  
  # tree_nplus1 = alpha[i] * herb_n * tree_n if we want number that died/mortality rate
  
  output_df$herbivore_biomass_n1[i] = tree_n[i] * exp(r * (1 - ((tree_n[i])/tree_K))) * (1 - exp(-alpha 
  * herb_n[i]))
  
  }
  
  # Then convert total tree population biomass, for each age class, back into amount of individuals, at
  # each time step. Finally, convert number of individuals into "amount of individuals that died"
  
  desired_length = 100
  tree_individuals = vector(mode = "list", length = desired_length)
  
  amount_died = vector(mode = "list", length = desired_length)
  
  for (i in 2:nstep) {
    
    tree_individuals[i] = output_df$tree_biomass_n1[i] * biomassscaling$scaling[i]
    
    amount_died[i] = c(0, diff(tree_individuals[i]))
    
    if(amount_died[i] >= 0){
      amount_died[i] = 0}
    else{
      if(amount_died[i] < 0){
        amount_died[i] = abs(amount_died[i])
      }
    }
    
  # tried to convert here from # of individuals in next time step to # that have died by calculating 
  # the difference between the current class and the previous class. If it is a positive number,
  # no net loss has occurred. If it is a negative number, a certain number of trees have died
  
  return(list(amount_died)) 
  }
}