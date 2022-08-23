#####
# Weight diet proportions (by pred biomass by strata from AFSC bottom trawl survey

library(tidyverse)

###############
###STEP 1. obtain diet proportions (g prey/g pred (of length bin) /year/ strata) 
###############

#source("03_get_Dietcomp_NoLength.R") # if haven't run it already
dietprop.strata=o_vals
head(dietprop.strata)

###############
# STEP 2. obtain biomass by species by strata (source ("GOA Rpath/GOA_biomass/R/04_BiomassCalc_Stratum_Length2.r"))
###############
#this file is read in "01_Diet_dataprep_NoLength.r"

head(biomstrat)
biomstrat2=biomstrat[-1,] #remove first row of NAs

#check
biomstrat2[which(biomstrat2$Species=="Big skate"),]
levels(biomstrat2$Species)

##############
#STEP 3. read in strata table info (stratum ID, NPFMC ID, depth category)
#############

stratafile <-read.csv("../lookups/GOA_strata_area2021.csv")

#############
#STEP 4. Check if need to rename Species/species names to match in diet and biomass database
#############

levels(dietprop.strata$species)
levels(biomstrat$Species)

#############
#STEP 5. join new pred biomass table with diet proportion table  
#############

# calculate new diet proportion weighted by pred biomass summed at stratum level
# aggregate diet proportion 

dietprop.strata2<-dietprop.strata %>%
  right_join(biomstrat2, by=c("STRAT"="stratum", "species"="Species")) %>%    #double check is right_join correct (stratum/STRAT won't join with left_join and full_join)
  mutate(preyBiom.Str=dietprop*new.stratum_biomass) %>%    #convertr diet proportion to prey biomass (mt)  per stratum (predator biomass at stratum level is mt)
  group_by(STRAT, species, sp_prey) %>%                        #sum across Lbins at strata level 
  mutate (PredBiom.Str=sum(new.stratum_biomass, na.rm = TRUE)) %>%   #prey biomass  per stratum
  mutate (PreyBiom.Str=sum(preyBiom.Str, na.rm = TRUE))   #prey (mt) consumed by pred  per stratum

#############
#STEP 6. Check Output file
#############

head(dietprop.strata2)

dietprop.strata2%>%
  filter(NODC==8826010109)%>%   #dusky rockfish
  select(PredBiom.Str, PreyBiom.Str)

biomstrat2%>%
  filter(NODC==8713040103)

levels(dietprop.strata2$species)

