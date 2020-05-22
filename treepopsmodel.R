

#stores the population in a set of bins representing different age ranges
#currently I am assuming the bins are 0-2,2-10,10-20,...90-100,100+ [for a total of 12 bins]
  #the last bin is basically a 'kill bin;' age-related mortality is set to 100% for this bin by default, meaning the model
  #causes all trees over age 100 to die immediately


###5/19/20 - let's start with it being simpler, just 10 bins for each of the decades between 0 and 100

#at each timestep

#in all of the below submodels, 'dpops' is the change in pops for each age bin JUST FROM THAT SUBMODEL
#so the 'dpops' from the age-related growth is a num with length equal to the number of bins containing the pop gain
#in each bin due to seedling production and age incrementing

#age-related new seedlings
age_growth = function(pops,maturityindex=2,ageranges=c(2,8,10,10,10,10,10,10,10,10,10),reproduction_coeff=0.3){
  #pops is the most recent timestep's age distribution
  #maturityindex is the index of the first bin in pops that is considered mature
  #reproduction_coeff is the parameter coefficient representing new seedling production from mature individuals
  #agerange is the length in years of each age bin, except for the last one
  #get the new seedlings as function of mature ones
  
  
  mature = sum(pops[maturityindex:length(pops)])
  
  newseedlings = reproduction_coeff*mature
  
  
  
  #now increment the age
  
  dpops = pops
  
  for(i in 1:length(pops)){
    #for each age range, increment the pops by 1 year
    
    #for now that'll be handled by just moving 1/(length of age range in years) of the age range's pops to the next age range
    
    #for now also assuming each age range is exactly 10 years and that there are 10 of them
    dpops[i] = 0
    
    #get the dpops for each age range
    
    
    #+newseedlings for lowest age range
    dpops[i] = ifelse(i>1,(1/ageranges[i-1])*pops[i-1],newseedlings)
    }
    #print(dpops)
  
  return(dpops)
}



#age-related mortality
age_mortality = function(pops,agedeathcoeffs = c(0.8,0,0,0,0,0,0.01,0.1,0.25,0.5,0.8,1)){
  #pops is the most recent timestep's age distribution
  #agedeathcoeffs is the parameter coefficients for the age impact on mortality at each age range (so a set of 12 numbers)
  #right now these are the percentage of pops in the age range that die
  dpops = pops
  for(i in 1:length(pops)){
    dpops[i] = -1*agedeathcoeffs[i]*pops[i] #dpops is actual CHANGE so amount that die is negative
  }
  return(dpops)
}

#drought index determination
drought_index = function(temp,precip,cdlength,coeffs=1.5,droughtcutoff=(300^1.5)/293.15){
  #temp is the current temperature in celsius
  #precip is the precipitation in [mm?]
  #coeffs is a dataframe with any coefficients needed
  #cdlength is the number of timesteps that the current drought has lasted
  #droughtcutoff is the value of the drought index BELOW which there is considered to be a drought
  
  ###currently just a shell for now - add stuff based on the Palmer Index or something else
  
  #so that it runs I've made it do something
  #there's more drought if:
    #temp is higher
    #precip is lower
  
  temp = temp+273.15 #converting to Kelvin gives it a value that is similar order of magnitude to precip values in mm, and
  #also prevents temperature values from being less than 1
  di = (precip^coeffs)/temp
  
  #this value is now a number which is higher at higher precip and at lower temperature, so a LOWER di means there is
  #'more drought'
  
  
  #is there a drought currently? if yes, add 1 to cdlength; if no, set cdlength to zero
  if(di < droughtcutoff){
    cdlength = cdlength+1  
  }
  if(di>= droughtcutoff){
    cdlength = 0
  }
  

  
  drought_stuff = list(di,cdlength)
  return(drought_stuff)
  
}

#drought-related mortality
drought_mortality = function(pops,di,cdlength,droughtrisk=0.1,drought_coeffs=c(0.2,0.001,0.001,0.001,0.001,0.001,0.01,0.05,0.15,0.3,0.6,1)){
  #pops is the most recent timestep's age distribution
  #di is the drought index value
  #cdlength is the number of timesteps the current drought has lasted (including this one)
  #droughtrisk is a parameter or variable representing drought vulnerability of the site (ex. a site with less ability
  #for the soil to retain water will be more vulnerable)
    #currently assuming that
  #drought_coeffs is a data frame of coefficient parameters
  
  dpops = pops
  
  #currently this is a shell but I've set it to do SOMETHING
  
  #drought mortality is worse for: [I don't know if these are accurate]
  #very young individuals (so 1st bin = higher)
  #older individuals (last few bins = higher)
  #stronger drought (greater if di is higher)
  #longer drought (greater if cdlength is higher)
  
  for(i in 1:length(dpops)){
    #the drought mortality for the bin is: bin pop * z, where z is a value between 0 and 1 representing the % that die of
    #drought
    
    #so we need to convert f(di,cdlength,droughtrisk,[any coefficients]) into a value from 0 to 1, regardless of what
    #the actual value of the function is.
    
    #should be a function which approaches 1 as input approaches infinity, and with a value of zero if the input is zero
    #1 - 1/x should work, but it will crap out if the x is zero exactly.
    
    #first we calculate x: [SIMPLE FOR NOW]
    x = di*cdlength*droughtrisk*drought_coeffs[i] #multiplying by cdlength means that if there is no drought then x = 0
    #now we calculate z:
    z=0
    if(x > 0){
      z = 1-(1/x)
    }
     #z approaches 100% as x gets larger, so if x is 0 (i.e. there is no drought) there
    #should be NO mortality
    #now we calculate the number of drought deaths for the bin
    dpops[i] = -1*pops[i]*z #dpops is actual CHANGE so these deaths are negative numbers

  }
  return(dpops)
}

#grazing-related mortality (and herbivore pop adjustment)
grazing_mortality = function(pops,herbivorepop,grazingdeathcoeffs = c(0.8,0.01,0,0,0,0,0,0,0,0,0,0),herbivore_eff=0.3,herbivore_mort=0.3){
  #pops is the most recent timestep's age distribution
  #herbivorepop is the herbivore pop from the most recent timestep (updated here)
  #grazingdeathcoeffs is the grazing impact coefficient for each age range - it's the equivalent of the 'alpha' in the LV
  #model of predator-prey interaction
  #herbivore_eff is a paramater that represents efficiency of the herbivore in converting prey into more herbivores
  #herbivore_mort is a parameter that represents the death rate of herbivores
  
  #first, calculate the grazing-related mortality for each age bin (the equivalent of the "alpha*prey*pred" term in the LV
  #model)
  
  dpops = pops
  
  for(i in 1:length(pops)){
    dpops[i] = -1*grazingdeathcoeffs[i]*herbivorepop*pops[i]
  }
  #print(dpops)
  
  #next, modify the herbivore pop
  totalpopeaten = sum(dpops) #this is how much herbivores ate during the timestep being modeled right now
  dherb = herbivore_eff*totalpopeaten - herbivorepop*herbivore_mort #basically the LV model for the predator
  
  l = list(dpops,dherb)
  
  return(l)
}




#########

#main model shell


h = 10000
p = c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000)
tm = 0
pr = 0

for(i in 1:(12*100)){
  tm[i]=23
  pr[i]=330
}
#treepopmodel_default = function(pops_i,herbpop_i,temps,precips,dl_i=0,t = 12*100){
  #uses default coeffs for all submodels
  
  #inputs are initial values for state conditions
  #pops_i = initial age distribution
  #herbpop_i = initial herbivore population
  #dl_i = length in timesteps that drought has been occurring prior to the start of the model
  #temps = temperatures at each timestep
  #precips = precip at each timestep
  
  #t is the time for the model to run in timesteps. Currently I'm assuming 1 timestep is 1 month and setting a default of
  #100 years to run
  pops_i = p
  herbpop_i = h
  temps = tm
  precips = pr
  t = 12*100
  dl_i = 0
  #create the age distribution over time dataframe
  pops = as.data.frame(t(pops_i))  #column i in the dataframe is the ith timestep
  colnames(pops) = c("0-2","3-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100","100+")
  
  
  #track mortality
  mortality = pops_i
  
  #create the variable for the CURRENT drought length, etc.
  cdl = dl_i
  herbpop = herbpop_i #do we want to keep track of this over time or only store the current value for it?
  
  
  for(i in 1:t){ #do the model steps at each timestep
    #first, organisms living at the start of the timestep reproduce:
    #get the age growth:
    cpops = pops[i,]
    dpops_agegrowth = age_growth(cpops)
    
    
    
    #get the age mortality:
    dpops_agemortality = age_mortality(cpops)
    
    #get the drought index value
    d_output = drought_index(temps[i],precips[i],cdlength=cdl)
    di = d_output[[1]][1]
    cdl = d_output[[2]][1]
    #get drought mortality
    dpops_droughtmortality = drought_mortality(cpops,di,cdl)
    
    #get grazing mortality and new herbivores
    g_output = grazing_mortality(cpops,herbpop)
    dpops_grazingmortality = g_output[[1]][]
    herbpop = herbpop + g_output[[2]][1] #update herbivore pop based on output of this
    
    #modify the pop values in each age bin
    
    mort = dpops_agemortality+dpops_droughtmortality+dpops_grazingmortality #this is the mortality during the CURRENT timestep
    #prevent mortality rates from bring bin pops below zero
    for(i2 in 1:length(cpops)){
      if(cpops[i2]+mort[i2] < dpops_agegrowth[i2]){
        mort[i2] = cpops[i2] + dpops_agegrowth[i2]
      }
    }
    
    
    pops[i+1,] = pops[i,]+dpops_agegrowth+mort #this is the population at the start of the NEXT timestep
    
    
  }
  
  l = list(pops,mortality)
  return(l)
  
#}
h = 1000
p = c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000)
tm = 0
pr = 0

for(i in 1:12*100){
  tm[i]=23
  pr[i]=330
}
#pops_test = treepopmodel_default(p,h,tm,pr)

#naomi's comments on our project

#she likes the question
#'friendly amendment' - be more general than mortality, look at productivity (or population based on the way we're doing it)
#'
#'#drought indicies out there
#' ex. Palmer Drought Severity Index - relates AET and PET, uses temp and precip
#' give some adjustment based on length of time that there has been a drought!
#' the "soil type" could be made into a more general "vulnerability" parameter
#' 
#' So drought would look like droughtlevel = PalmerIndex + VulnerabilityFudgeFactor
#' And then droughtmortality = f(droughtlevel)
#' 
#' younger and older individuals more vulnerable to drought
#' 
#' Model herbivore pops as well? Predator-prey model.
#' 
#' She suggests starting with the drought impact between now and monday