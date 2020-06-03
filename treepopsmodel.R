
# Model of tree population (by age class) over time


library(SPEI)
library(tidyverse)
#source("age_fxns.R") #get the age growth and mortality functions - currently not actually needed!
source("Drought2.R") #get the drought functions
source("grazingsubmodel.R")

treepopsmodel = function(t = 44, timestep = 1, initialpops = c(1000,200,188,170,153,120,80,37,15,6),herb_n = 100,WD,SLA,coeff1,coeff2,coeff3){
###ENTER THE DESIRED INPUTS HERE

 #pops in each age bin (age bins are 0-9,10-19,20-29, etc, with last being 90-99)
lpops = length(initialpops) #the NUMBER of different age bins
 #total population of herbivores at start


###For drought calculation: needs precipitation data in MONTHS for the entire time range AND the [timestep] years prior to the start of that range
#the FIRST value in this (i.e. index = 1) should be from [timestep] years before the intended start time

#by default we'll use the testing data available from the SPEI library
data(balance)
PMinusPET = balance$tampa
#there isn't enough data by default for 100 years






###For grazing calculation: needs the INITIAL herbivore POPULATION COUNT


####Written by Kaili:
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
###^Written by Kaili  

#initialize results matrix
pops = data.frame(t(initialpops))
colnames(pops) = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99")
pops[2:(t+1),]=0
#initialize the age matrix
#age_matrix = matrix(nrow = 2, ncol = length(initialpops)) #only really need 2 rows:
  #1st row is for fertility rates
  #2nd row is for death rates
  #the columns represent the age bins
#age_matrix[]  = 0
#age_matrix[1,] = fert_rates
#age_matrix[2,] = mort_rates
#for the desired time length:
for(i in 1:t) { #run 1 timestep of the model
  #at the start of a timestep the survivability matrix is:
  # Fertility row:  [whatever the fertility rate set above is]
  # Death rows:     0 0 0 0 0
  #                 0 0 0 0 0
  #                 ...
  
  #we will need to reset the death rows to just the age-related death fractions
  #age_matrix[2,]=mort_rates
  
  #get the herbivore related deaths/rates [what should be returned is either:
  
  
  grazingoutput = grazingsubmodel(herb_n = herb_n,tree_n = pops[i,], biomassscaling = biomassscaling, alphascaling = alphascaling)
  herb_n = grazingoutput[[2]]
  newpops_g = grazingoutput[[1]]
  
  ######Drought related mortality
  ###FOR ME: get JUST THE MOST RECENT 24 MONTHS of P minus PET
  #each +1 to i is +12 to the drought index we want
  #on 1st timestep we want from 1 to 12, which is i to 12i
  #on 2nd timestep we want from 13 to 24, which is 12(i-1)+1 to 12i
  
  
  startindex = 12*(i-1)+1
  endindex = 12*i
  d_input = PMinusPET[startindex:(endindex+100)]
  
  
  #get the drought related deaths/rates [looks like it's a rate - % of standing dead trees, which I take to mean % of trees that DIE of drought at the timestep]
  dm_temp = drought_function(d_input) #this is currently broken - I half expect something happened with the library
  
  droughtmort = dm_temp[12*timestep,]*pops[i,] #if drought mortality % is over 100% for some reason, set it to 100%
  
  
  newpops = newpops_g - droughtmort
  #no newpops bin can be less than 0
  for(i2 in 1:length(newpops)){
    if(newpops[i2] < 0){
      newpops[i2] = 0
    }
    
  }
  
  #deathrow_update = matrix(nrow = 1, ncol = lpops)
  #for(i2 in 1:lpops){
  #deathrow_update[i2] = min(1,(age_matrix[2,i2] + droughtmort))
  #}
  
  
  #get the dpops value
  #dpops = matrix(nrow = 1, ncol = lpops)
  #dpops[1,]=0
  
  #newpops = dpops
  #prevbinageup = 0
  #currentbinageup = 0
  #for(i2 in 1:lpops){
  #for ANY bin, get the deaths during the timestep:
  #    dpops[i2] =  g_mort[[i2]]-age_matrix[2,i2]*pops[i,i2]
  #herbivory deaths [returned as a negative number already]
  #the fraction of the bin's pops that died due to drought and other causes
  
  #of the survivors, 1/10*timestep will advance to the next age bin
  #currentbinageup = 0.1*timestep*pops[i,i2]-dpops[i2]
  
  
  #components that differ between the seedling bin and the other bins  
  #if(i2 == 1){ #if we are doing the seedlings, add up all the seedling production from each age range
  #  newseedlings = sum(pops[i,i2]*age_matrix[1,])
  #  dpops[i2] = dpops[i2] +  newseedlings - currentbinageup
  
  #} else{ #otherwise, add in age up from prior age bin
  #  dpops[i2] = dpops[i2]+prevbinageup - currentbinageup
  #}
  #at this point we can assign the current bin ageup amount to the prev bin value for use on the next iteration
  #prevbinageup = currentbinageup
  
  #now that we have the change, get the new pops in the bin (capped at minimum of 0) 
  #newpops[i2] = max(0,(pops[i,i2]+dpops[i2]))
  
  #}
  
  #add this to the next row in pops
  pops[(i+1),] = newpops
  
}

#what we actually want is to return a single number or a series of numbers

#if the trees do NOT survive to the end:
survived_to = 0
if(sum(pops[length(pops),]) == 0) {
  #then we want to know the last year in which there were living trees
  i = length(pops)
  survivors = 0
  while(survivors == 0){
    survivors = sum(pops[i,])
    i = i-1
  }
  survived_to = i
  #and the surviving tree pops is zero:
  survivors = 0
} else{ #otherwise,
  #the trees survived to length(pops)
  survived_to = length(pops)
  #and the surviving tree pops is the sum of the age bins
  survivors = sum(pops[length(pops),])
}

return(list(survived_to,survivors))
}