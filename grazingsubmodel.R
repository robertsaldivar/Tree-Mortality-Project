#' Grazing Submodel
#' 
#' @param t time (days)
#' @param a proportionality constant
#' @param tree_survive number of offspring of unparasitized host (??) surviving to next timestep
#' @param tree_last number of trees from last timestep
#' @param herb_last number of herbivores from last timestep

# optional?: parameter c describes the number of parasitoids that hatch from an infected host



# This will use the Nicholson-Bailey Model (for host-parasitoid interactions) as applied to
# plant-herbivore interactions (where an individual tree is the "host" and an individual ungulate
# is the "parasitoid" that grazes on the tree)

# The following equations represent tree (tree_now) and herbivore (herb_now) populations at 
# the (t + 1) timestep


tree_pop = tree_survive * tree_last * exp(-a * herb_last)
  
herb_pop = c * tree_last * (1 - exp(-a * herb_last))
  
  

# Running the function across the desired timesteps

grazingsubmodel = function(){
  
  tree_pop[i] = tree_survive * tree_last[t - 1] * exp(-a * herb_last[i])
 
  herbivore_pop[i] = tree_last[t - 1] * (1 - exp(-a * herb_last[i]))
  
  return(list(c(tree_pop, herbivore_pop)))
}


# NOTES: 
# output should be amount that has died (population losses) in order to represent mortality

