#' Grazing Submodel
#' 
#' @param t time (days)
#' @param r growth rate of trees
#' @param alpha alpha, effect between herbivores and trees
#' @param tree_K trees carrying capacity
#' @param tree_n number of trees from last timestep
#' @param herb_n number of herbivores from last timestep


# Two options for this submodel: 
# 1. use equations that give survivability for each age class (factor in age class) for trees + herbs

# The survivability equations below are based off equations from Kang et al. 2008 

# The following assumptions are made in this model: 
## - entire canopy of individual tree is accessible to the herbivore at all times
## - herbivores are always present (at all time steps), they are not seasonal grazers
## - trees do not lose leaves seasonally, able to be eaten by herbivores at all time steps
## - alpha value determined for each age class were based on biological understanding that younger
##   individuals usually produce more palatable leaves (are more preferred by herbivores than 
##   older trees are)


grazingsubmodel = function(survivability, initialpop, nstep, alpha, herb_n, trees_n) {
  
  # the number of age classes
  nclasses = 10
  
  #initialize the Leslie matrix
  leslie_matrix = matrix(nrow=nclasses, ncol=nclasses)
  leslie_matrix[,] = 0.0
  leslie_matrix[1,] = survivability
  
  for (i in 1:(nclasses-1)) {
    leslie_matrix[i+1,i] = survivability[i]
   
     # survivability for trees
    survivability_trees[i] = survivability[i] - (alpha_herb * herb_n)
    
    # survivability for herbivores
    survivability_herbs[i] = survivability[i] - (alpha_tree * trees_n)
    
     }
  leslie_matrix[nclasses,nclasses] = survivability[nclasses]
  
  # create matrices to store population structure
  pop.structure_trees = matrix(nrow=nclasses, ncol=nstep)
  
  pop.structure_trees[,1] = initialpop
  
  
  pop.structure_herbs = matrix(nrow=nclasses, ncol=nstep)
  
  pop.structure_herbs[,1] = initialpop
  
  
  for (i in 2:nstep) {
  
    pop.structure_trees[,i] = leslie_matrix %*% pop.structure_trees[,i-1]
    pop.structure_herbs[,i] = leslie_matrix %*% pop.structure_herbs[,i-1]
  }
  
  return(list(c(pop.structure_trees, pop.structure_herbs)))
}


## above shouldnt be over whole nstep time
## output get mortality fraction/rate





# 2. use equations that give total biomass (and total amount) of tree + herb populations

grazingsubmodel = function(alpha, r, herb_n, tree_n, tree_K){
  
  # Number of time steps 
  nstep = 100 # years
  
  # Need population values for age classes in a data frame
  # Number of herbivores is the same regardless of the trees' age class. Assuming there is a 
  # overall herbivore population of 100 individuals, we will divide them up evenly among the 10
  # age classes so that 10 herbivores are present to eat any of the trees in any age class
  population_df <- data.frame("age_class" = c(1:10), "tree_n" = c(), "herb_n" = c(10, 10, 10, 10,
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
  
  # Then convert total population biomass, for each age class, back into amount of individuals.
  # Finally, convert number of individuals into "amount of individuals that died"
  
  for (i in 1:nstep) {
    
    tree_individuals = output_df$tree_biomass_n1[i] * biomassscaling$scaling[i]
    
    amount_died = tree_individuals[i+1] - tree_individuals[i]
   
    if(amount_died[i] >= 0) amount_died[i] = 0
    if(amount_died[i] < 0) amount_died[i] = abs(amount_died[i])
    
  }

  # tried to convert here from # of individuals in next time step to # that have died by calculating 
  # the difference between the current class and the previous class. If it is a positive number,
  # no net loss has occurred. If it is a negative number, a certain number of trees have died
  
  return(list(amount_died))
}
