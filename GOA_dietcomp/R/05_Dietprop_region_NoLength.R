#Calculate diet proportions weighted by predator biomass by region larger than stratum (No length categories)
#produce in pred/prey matrix for Ecopath

##################
#STEP 1. get diet proportion weighted by pred biomass  by stratum (using "get_dietprop_biomassweighted_NoLength" function from "04_get_diet_biomass_weighted_NoLength.r")
##################

#source("04_get_Diet_Biomass_Weighting_NoLength.r") #if haven't run already
dietprop.biomstrat= dietprop.strata2
head(dietprop.biomstrat)

dietprop.biomstrat= dietprop.strata2

### #aggregate to WGOA (INPFC=Kodiak, Shumagin, Chirikof)
diet.WGOA<-dietprop.biomstrat%>%
  left_join(GOAstrata, by=c("STRAT"="Stratum"))%>%
  filter (INPFC_area=="Kodiak" |INPFC_area== "Shumagin"|INPFC_area== "Chirikof") %>%  #WGOA INPFC regions
  group_by(species,sp_prey) %>%
  summarize (PreyBiom.WGOA = sum(PreyBiom.Str), 
             PredBiom.WGOA= sum(PredBiom.Str), 
             dietprop.WGOA = PreyBiom.WGOA/PredBiom.WGOA) 

#check
diet.WGOA%>%
  filter(NODC.code==8857040901)%>% #Big Skate;  8713040103)%>%
select(PredBiom.WGOA, PreyBiom.WGOA)


### #aggregate to EGOA (INPFC=Yakutat & Southeastern)
diet.EGOA<-dietprop.biomstrat%>%
  left_join(GOAstrata, by=c("STRAT"="Stratum"))%>%
  filter (INPFC_area=="Yakutat" |INPFC_area== "Southeastern") %>%  #EGOA INPFC regions
  group_by(species,sp_prey) %>%
  summarize (PreyBiom.EGOA = sum(PreyBiom.Str), 
             PredBiom.EGOA= sum(PredBiom.Str), 
             dietprop.EGOA = PreyBiom.EGOA/PredBiom.EGOA) 
#check
diet.EGOA%>%
  filter(NODC.code==8857040901)%>% #Big Skate 8713040103)%>%
  select(PredBiom.EGOA, PreyBiom.EGOA)

head(diet.WGOA)
head(diet.EGOA)

## make diet proportion matrix for Ecopath (pred=columns, prey=rows) EGOA and WGOA
diet.WGOA.EwE_noLbin<-diet.WGOA %>%
  select(species, sp_prey,dietprop.WGOA ) %>%
  spread(key=species, value=dietprop.WGOA)     #PRED are columns, prey are rows


head(diet.WGOA.EwE_noLbin)


#write.csv(diet.WGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.WGOA.EwE.DemersalShelfRF.csv")
#write.csv(diet.WGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.WGOA.EwE.otherskates.csv")
#write.csv(diet.WGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.WGOA.EwE.shallowFF.csv")
#write.csv(diet.WGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.WGOA.EwE.slopeRF.csv")
#write.csv(diet.WGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.WGOA.EwE.other.csv")


diet.EGOA.EwE_noLbin<-diet.EGOA %>%
  select(species, sp_prey,dietprop.EGOA ) %>%
  spread(key=species, value=dietprop.EGOA)     #PRED are columns, prey are rows

head(diet.EGOA.EwE_noLbin)

#write.csv(diet.EGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.EGOA.EwE.DemersalShelfRF.csv")
#write.csv(diet.EGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.EGOA.EwE.otherskates.csv")
#write.csv(diet.EGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.EGOA.EwE.shallowFF.csv")
#write.csv(diet.EGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.EGOA.EwE.slopeRF.csv")
#write.csv(diet.EGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.EGOA.EwE.other.csv")


#BOOM




#############start
#need all pred species so can join tables by species name in this file
#create loop to read in all   FunGrpA, FunGrpB, FunGRpC into biomass by strat calculations and then into diet calcs
#then run biomass by stratum function for FunGrpB, FunGRpC... and run Diet_biomas_Weight for FunGrpB, FunGrpC.. then join? (bind_rows)
#Predators list entered as species in 01_Diet_dataprepr and then 04_get_Dietcomp (linked to NODC)
#divide prey species by juv and adult for ecopoath


