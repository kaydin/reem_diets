
#Calculate diet proportions weighted by predator biomass by length category (adult/juvenile) by region larger than stratum
#produce in pred/prey matrix for Ecopath

#get diet proportion weighted by pred biomass by length category (juv/adult) by stratum (using "get_dietprop_biomassweighted_bylength" function from "04_get_diet_biomass_weighted_ByLength.r")

#specify here which predators to calculate
#predators=predators #defined in "01_Diet_dataprep_Length.r" #c("W.pollock") #predators
#future step add region in specification for cuntion (currently hardcoded to GOA-need to combine GOA and EBS and AI stratum files)

##old function code
#dietprop.biomlengstrat <- get_dietprop_biomassweighted_bylength(predators = predators ) #P.cod, Arrowtooth
#head(dietprop.biomlengstrat)

#source("04_get_Diet_Biomass_Weighting_Length.R") #instead of function
dietprop.biomlengstrat=dietprop.strata3

################
### WGOA
###############
### #aggregate to WGOA (INPFC=Kodiak, Shumagin, Chirikof)
diet.WGOA.stanza<-dietprop.biomlengstrat%>%
  left_join(GOAstrata, by=c("STRAT"="Stratum"))%>%
  filter (INPFC_area=="Kodiak" |INPFC_area== "Shumagin"|INPFC_area== "Chirikof") %>%
  group_by(PRED,Stanza,sp_prey) %>%
  summarize (PreyBiom.JuvAdult.WGOA = sum(PreyBiom.JuvAdult.Str), 
             PredBiom.JuvAdult.WGOA= sum(PredBiom.JuvAdult.Str), 
             dietprop.JuvAdult.WGOA = PreyBiom.JuvAdult.WGOA/PredBiom.JuvAdult.WGOA)%>%
   unite(ECOPATH_PRED2,PRED, Stanza,sep = "_") #create new column "ECOPATH_PRED2" by joining species names (PRED) and adult/juv (STANZA)

#check
head(diet.WGOA.stanza)
unique(diet.WGOA.stanza$ECOPATH_PRED2) #check the final 'unite' line worked
dietprop.biomlengstrat[which(dietprop.biomlengstrat$Stanza=="NA"),]

## make diet proportion matrix for Ecopath (pred=columns, prey=rows) WGOA
diet.WGOA.stanza.EwE<-diet.WGOA.stanza %>%
  select(ECOPATH_PRED2, sp_prey,dietprop.JuvAdult.WGOA ) %>%
  spread(key=ECOPATH_PRED2, value=dietprop.JuvAdult.WGOA)     #PRED are columns, prey are rows

head(diet.WGOA.stanza.EwE)

#write.csv(diet.WGOA.stanza.EwE, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.WGOA.stanza.EwE.csv")

################
### EGOA
###############
#aggregate to EGOA (INPFC=Yakutat & Southeastern)
diet.EGOA.stanza<-dietprop.biomlengstrat%>%
  left_join(GOAstrata, by=c("STRAT"="Stratum"))%>%
  filter (INPFC_area=="Yakutat" |INPFC_area== "Southeastern") %>%
  group_by(PRED,Stanza,sp_prey) %>%
  summarize (PreyBiom.JuvAdult.EGOA = sum(PreyBiom.JuvAdult.Str), 
             PredBiom.JuvAdult.EGOA= sum(PredBiom.JuvAdult.Str), 
             dietprop.JuvAdult.EGOA = PreyBiom.JuvAdult.EGOA/PredBiom.JuvAdult.EGOA) %>%
  unite(ECOPATH_PRED2,PRED, Stanza,sep = "_") #create new column "ECOPATH_PRED2" by joining species names (PRED) and adult/juv (STANZA)

head(diet.EGOA.stanza)


## make diet proportion matrix for Ecopath (pred=columns, prey=rows) EGOA 
diet.EGOA.stanza.EwE<-diet.EGOA.stanza %>%
  select(ECOPATH_PRED2, sp_prey,dietprop.JuvAdult.EGOA ) %>%
  spread(key=ECOPATH_PRED2, value=dietprop.JuvAdult.EGOA)     #PRED are columns, prey are rows

head(diet.EGOA.stanza.EwE)
#write.csv(diet.EGOA.stanza.EwE, "C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/diet.EGOA.stanza.EwE.csv")


#BOOM





