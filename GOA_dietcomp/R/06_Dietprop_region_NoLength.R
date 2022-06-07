#Calculate diet proportions weighted by predator biomass by region larger than stratum (No length categories)
#produce in pred/prey matrix for Ecopath

#get diet proportion weighted by pred biomass  by stratum (using "get_dietprop_biomassweighted_NoLength" function from "04_get_diet_biomass_weighted_NoLength.r")

#specify here which predators to calculate

predators= c(8857040901)  #NODC code for YFS (or use "NODC")
#future step add region in specification for region (currently hardcoded to GOA-need to combine GOA and EBS and AI stratum files)

dietprop.biomstrat <- get_dietcomp_biomassweighted_NoLength(predators = predators) #c(8857040901)) # = predators ) 
head(dietprop.biomstrat)

### #aggregate to WGOA (INPFC=Kodiak, Shumagin, Chirikof)
diet.WGOA.stanza<-dietprop.biomstrat%>%
  left_join(GOAstrata, by=c("STRAT"="Stratum"))%>%
  filter (INPFC_area=="Kodiak" |INPFC_area== "Shumagin"|INPFC_area== "Chirikof") %>%
  group_by(species,sp_prey) %>%
  summarize (PreyBiom.WGOA = sum(PreyBiom.Str), 
             PredBiom.WGOA= sum(PredBiom.Str), 
             dietprop.WGOA = PreyBiom.WGOA/PredBiom.WGOA) 

### #aggregate to EGOA (INPFC=Yakutat & Southeastern)
diet.EGOA.stanza<-dietprop.biomstrat%>%
  left_join(GOAstrata, by=c("STRAT"="Stratum"))%>%
  filter (INPFC_area=="Yakutat" |INPFC_area== "Southeastern") %>%
  group_by(species,sp_prey) %>%
  summarize (PreyBiom.EGOA = sum(PreyBiom.Str), 
             PredBiom.EGOA= sum(PredBiom.Str), 
             dietprop.EGOA = PreyBiom.EGOA/PredBiom.EGOA) 


head(diet.WGOA.stanza)
head(diet.EGOA.stanza)

## make diet proportion matrix for Ecopath (pred=columns, prey=rows) EGOA and WGOA
diet.WGOA.EwE_noLbin<-diet.WGOA.stanza %>%
  select(species, sp_prey,dietprop.WGOA ) %>%
  spread(key=species, value=dietprop.WGOA)     #PRED are columns, prey are rows

#write.csv(diet.WGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.WGOA.EwE.A.csv")

diet.EGOA.EwE_noLbin<-diet.EGOA.stanza %>%
  select(species, sp_prey,dietprop.EGOA ) %>%
  spread(key=species, value=dietprop.EGOA)     #PRED are columns, prey are rows

#write.csv(diet.EGOA.EwE_noLbin, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.EGOA.EwE.A.csv")

#BOOM


diet.WGOA.EwE_noLbin$`N. Rock sole`

#############start
#03_dietprop nolength - levels(o_vals$species)-shows all species but levels (diet_prop_strata)-shows only YFS
#need all pred species so can join tables by species name in this file
#then run biomass by stratum function for FunGrpB, FunGRpC... and run Diet_biomas_Weight for FunGrpB, FunGrpC.. then join? (bind_rows)


