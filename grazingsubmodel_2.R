# 1. use equations that give survivability for each age class (factor in age class) for trees + herbs

grazing_submodel = function(survivability, initialpop, nstep, alpha, herb_n, trees_n) {

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
