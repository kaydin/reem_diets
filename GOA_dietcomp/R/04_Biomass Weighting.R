#####
# Weight diet proportions (by pred by length bin by year) by strata from AFSC bottom trawl survey

library(tidyverse)

###obtain diet proportions (g prey/g pred (of length bin) /year/ strata) source ("02_dietcomp_GOA.r")
##Method1
#dietprop.strata=read.csv("../results/out_diets_allGOA.csv")
#head(dietprop.strata)

##Method2
#source("03b_dietcomp_GOA4.r") # to get diet prop by strata #this takes a while to run
dietprop.strata=read.csv("../results/out_diets_allGOA2.csv")
head(dietprop.strata)

#obtain biomass/species/strata (source ("GOA Rpath/GOA_biomass/R/05_BiomassCalc_Stratum.r"))
biomstrat=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA Rpath/GOA_biomass/results/SumStrata_biomass_GOA_1984_2021.csv")
head(biomstrat)

#read in strata table info (stratum ID, NPFMC ID, depth category)
stratafile <-read.csv("../lookups/GOA_strata_area2021.csv")

#create new temporary table to populate with biomass summed across multiple NODC codes for given pred
db.temp<-dietprop.strata %>%
  select(PRED, STRAT, YY)%>%
  mutate(PredBiom=NA)%>%
  mutate(INPFC_AREA=NA)%>%
  mutate(Depth=NA)%>%
  remove_rownames()%>%
  distinct()

###begin loop to get biomass for predator group (sometimes multiple NODC codes) by strata by year
#predators list of predator species from 03_dietcomp_GOA4.r
#stratlist of strata from 03_dietcomp_GOA4.r
#yearlist of years from 03_dietcomp_GOA4.r

i=1 #initialize temp table to store results
for (PRED in predators){    
  # Pick the predator 
  ThisPred <- preds[[PRED]] 
  biomstrat2 <- biomstrat[biomstrat$NODC %in% ThisPred$nodc,]  
  
  for (STRAT in stratlist){
    stratcode=STRAT
    biomstrat3<- biomstrat2[biomstrat2$stratum %in% stratcode,]
      
      for (YY in yearlist){
        yearcode=YY
        biomstrat4<-biomstrat3[biomstrat3$year %in% yearcode,]
        
         if(length(biomstrat4$year)>0) {
           ThisPredBiom=sum(biomstrat4$SumStratum_biomass) #sum pred biomass across NODC codes for same pred species, within stratum and year
           INPFC=biomstrat4$INPFC_AREA
           } else { 
             ThisPredBiom=NA 
             INPFC=NA
            }#End Ifelse statement
             
        #save new pred biomass with pred name
        db.temp$PRED[i]=PRED
        db.temp$YY[i]=yearcode
        db.temp$STRAT[i]=stratcode
        db.temp$PredBiom[i]=ThisPredBiom
        db.temp$INPFC_AREA[i]=as.character(INPFC)
        db.temp$Depth[i]=as.character(stratafile$Depth[which(stratafile$Stratum==stratcode)])
        
        i=i+1 # (next row in table)
        
      } # end of yearlist
} #end of stratblock      
}  # end predator loop 

db.temp

#join new pred biomass table with diet proportion table  
dietprop.strata2<-dietprop.strata %>%
  full_join(db.temp, by=c("PRED", "YY", "STRAT")) 

# calculate new diet proportion weighted by pred biomass summed at stratum level
dietprop.strata2<-dietprop.strata2 %>%
  mutate(dietpropWtbiom=dietprop*PredBiom)  

#### #final diet proportion table weighted by biomass by strata
dietprop.strata2
 
#### #aggregate to INPFC & Depth category areas: sum prey_sp (g_prey/g_pred/stratum/yr) across pred, year, prey species, INPFC to 
dietprop.INPFC<-dietprop.strata2%>%
  group_by(PRED,sp_prey, YY, INPFC_AREA, Depth) %>%
  summarise(dietpropWt.INPFC.Depth = sum(dietpropWtbiom), .groups = "keep")

#### Divide by Area of INPFC/Depth region as used in EwE
source("../Rpath/GOA Rpath/GOA_area_calc/GOA_area_calc.R") # area (km2) by super strata ( by region (Shumagin, Cherikof, Kodiak) by depth strata (shelf, gully, slope)
GOAareaEwE

#join area (km2) by NPFMC region and by depth category (slope, shelf, gully)
dietprop.INPFC.km2<-dietprop.INPFC %>%
  left_join(GOAareaEwE, by = c("Depth", "INPFC_AREA"="INPFC_area")) 


#calculate spatial by dividing weighted diet proportion at NPFMC/Depth level by area of NPFMC/Depth region (g_prey/g_pred/km2/yr)
dietprop.INPFC.km2<-dietprop.INPFC %>%
  mutate(gprey.gpred.km2=dietpropWt.INPFC.Depth/sum.km2) #divide superstrata by area




