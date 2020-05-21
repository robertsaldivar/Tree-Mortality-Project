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

# Alpha values for the effect of herbivores on trees, for each of the age classes
alpha_herb_1 =
alpha_herb_2 =
alpha_herb_3 =
alpha_herb_4 =
alpha_herb_5 =
alpha_herb_6 =
alpha_herb_7 =
alpha_herb_8 =
alpha_herb_9 =
alpha_herb_10 =
  
# Alpha values for the effect of trees on herbivores, for each of the age classes

alpha_tree_1 =
alpha_tree_2 =
alpha_tree_3 =
alpha_tree_4 =
alpha_tree_5 =
alpha_tree_6 =
alpha_tree_7 =
alpha_tree_8 =
alpha_tree_9 =
alpha_tree_10 =

# The survivability equations below are based off equations from Kang et al. 2008 

grazingsubmodel = function(survivability, initialpop, nstep) {
  
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
    survivability_herbs[i] = survivability[i] - (alpha_trees * trees_n)
    
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








# 2. use equations that give total biomass (and total amount) of tree + herb populations

# Alpha values for the effect of herbivores on trees and vice versa:
alpha_herb = 
alpha_tree = 

grazingsubmodel = function( ){
  
  tree_nplus1 = tree_n * exp(r * (1 - (tree_n/tree_K)) - (alpha * herb_n)
 
  herbivore_nplus1 = tree_n * exp(r * (1 - (tree_n/tree_K))) * (1 - exp(-alpha * herb_n))
  
  return(list(c(tree_nplus1, herbivore_nplus1)))
}


# NOTES: 
# which format should the output of this submodel be? Would need to convert total biomass to 
# contributions to the population sizes

