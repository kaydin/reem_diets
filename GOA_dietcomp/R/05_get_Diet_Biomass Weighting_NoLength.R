#####
# Weight diet proportions (by pred biomass by strata from AFSC bottom trawl survey
#future step add region in specification for region (currently hardcoded to GOA-need to combine GOA and EBS and AI stratum files)

library(tidyverse)

###obtain diet proportions (g prey/g pred (of length bin) /year/ strata) source ("03_dietcomp_GOA_Lbin.r")

# function to calculate diet proportion weighted by predator biomass by length by strata
get_dietcomp_biomassweighted_NoLength <- function(diet_tables = list(
  ppfile   = ppfile  ,
  pplookfile = pplookfile,
  preylook_col = preylook_col,
  LWparamfile = LWparamfile,
  GOA_stratafile = GOA_stratafile 
),                             
predators = 8857040901) #NODC code for YFS or use "NODC" as defined in "02_dataprep_NoLength.r"
{

  # Get diet proportions (g prey/g pred  / strata) table  by running "get_dietcomp_NoLength_function.r" function
  dietprop.strata <- get_dietcomp_NoLength(predators = predators)
  head(dietprop.strata)

#obtain biomass/species/strata (source ("GOA Rpath/GOA_biomass/R/04_BiomassCalc_Stratum_Length2.r"))
biomstrat=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_EGOA.A.csv")
head(biomstrat)
biomstrat2=biomstrat[-1,] #remove first row of NAs

#read in strata table info (stratum ID, NPFMC ID, depth category)
stratafile <-read.csv("../lookups/GOA_strata_area2021.csv")

##Check if need to rename Species/species names to match in diet and biomass database
#levels(dietprop.strata$species)
#levels(biomstrat$Species)

#join new pred biomass table with diet proportion table  
# calculate new diet proportion weighted by pred biomass summed at stratum level
##aggregate diet proportion 

dietprop.strata2<-dietprop.strata %>%
  right_join(biomstrat2, by=c("STRAT"="stratum", "species"="Species")) %>%    #double check is right_join correct (stratum/STRAT won't join with left_join and full_join)
  mutate(preyBiom.Str=dietprop*new.stratum_biomass) %>%    #convertr diet proportion to prey biomass (mt)  per stratum (predator biomass at stratum level is mt)
  group_by(STRAT, species, sp_prey) %>%                        #sum across Lbins at strata level 
  mutate (PredBiom.Str=sum(new.stratum_biomass)) %>%   #prey biomass  per stratum
  mutate (PreyBiom.Str=sum(preyBiom.Str))   #prey (mt) consumed by pred  per stratum

head(dietprop.strata2)

#dietprop.strata2$PreyBiom.Str
#dietprop.strata2$PredBiom.Str
#levels(dietprop.strata2$species)

return(dietprop.strata2)
}  #end function

#test function is working
x <- get_dietcomp_biomassweighted_NoLength(predators = c(8857040901)) #NODC code for YFS or use "NODC" as defined in "02_dataprep_NoLength.r"
head(x)


