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

grazingsubmodel = function(herb_n, tree_n, herb_d = 0.03, herb_eff = 0.1, alpha = c(0.0020,0.0010,0.005,0,0,0,0,0,0,0), biomassscaling = c(1/10, 1/20, 1/30, 1/40, 1/50,1/60, 1/70, 1/80, 1/90, 1/100)){
  ##input tree_n is the data frame with the population in each age class
  ##input herb_n is the total herbivore population
  
  ##Alpha is the coefficient for consumption of biomass based on interaction
  
  ## herb_d is death rate of the herbivore population
  
  ## herb_eff is the efficiency by which herbivores convert consumed biomass into herbivore biomass
  
  ## We also need a biomass scaling data frame, which has age class-specific scaling factors. 
  ## For example, each individual in age class 1 has an average biomass (g C) of 10, etc. This 
  ## will help us convert between # of individuals and amount of biomass.

###left over from testing - delete later 
#herb_n = 200
#tree_n = pops[1,]
#herb_d = 0.03
#herb_eff = 0.1
#alpha = c(20,10,5,0,0,0,0,0,0,0)
#biomassscaling = c(1/10, 1/20, 1/30, 1/40, 1/50,1/60, 1/70, 1/80, 1/90, 1/100)
###



  # Converting from population size to biomass
  tree_bm = tree_n / biomassscaling
  

  treedata = tree_n
  # Then apply the biomass equations to each age class
  for (i in 1:length(tree_n)){
    #tree biomass death from herbivory - it is a NEGATIVE NUMBER
    treedata[i] = - (alpha[i] * herb_n*tree_bm[i])
    
  }
  
  
  #now get the new herb biomass: existing + growth from herbivory - deaths
  #prevent herbivore biomass from growing to more than 2x of herb_n in a single timestep
  newherb = min(2*herb_n,herb_n + herb_eff*sum(-treedata) - herb_d*herb_n)
  
  
  # Then convert total tree population biomass, for each age class, back into amount of individuals.
  deadtrees = tree_n
  
  
    
  for (i in 1:length(tree_n)){
    #the number of trees that died in each age group
    deadtrees[i] = treedata[i] * biomassscaling[i]
  }    
  
  # Create data frame to store results from for loop below
  
    return(list(deadtrees,newherb)) 
  
}