#' Grazing Submodel
#' 
#' @param t time (days)
#' @param a proportionality constant
#' @param tree_survive number of offspring of unparasitized host (??) surviving to next timestep
#' @param tree_last number of trees from last timestep
#' @param herb_last number of herbivores from last timestep
#' 

# This will use the Nicholson-Bailey Model (for host-parasitoid interactions) as applied to
# plant-herbivore interactions (where an individual tree is the "host" and an individual ungulate
# is the "parasitoid" that grazes on the tree)

# The following equations represent tree (tree_now) and herbivore (herb_now) populations at 
# the (t + 1) timestep


tree_now = tree_survive * tree_last * exp(-a * herb_last)
  
herb_now = tree_last * (1 - exp(-a * herb_last))
  
  

# Running the function across the desired timesteps

for(i in 1:length(t))){
  
  tree_now[i] = tree_survive * tree_last[t - 1] * exp(-a * herb_last[i])
  herbivore_now[i] = tree_last[t - 1] * (1 - exp(-a * herb_last[i]))
  
  return(list(c(tree_now, herbivore_now)))
}

# then results will need to be stored in a data frame (here or in markdown??)