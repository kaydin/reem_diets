
#calculate biomass by superstrata #need to add age classes (<20cm? or ontogenetic age classes from diet code?)

#setwd("C:/Users/bridget.ferriss/work/Rpath/GAP_design_based_indices/R")
#source ("./GAP_design_based_indices/R/01_cleanup_data.R")
#source ("./GAP_design_based_indices/R/02_get_cpue.R")
#source ("./GAP_design_based_indices/R/03_get_biomass_stratum.R")

#setwd("C:/Users/bridget.ferriss/work/Rpath/GOA Rpath/Biomass")
stratalist=read.csv(here::here( "data","goa_strata.csv")) #this path set in 01_cleanup_data.R

#example to check working (POP)
 x <- get_biomass_stratum(speciescode = 30060,survey_area = "GOA") #these are RACE species codes (not NODC)
 head(x)

library(tidyverse)

#list of species & species codes
 FunGrp=read.csv(here::here( "data","ModelSpeciesList_GAP_Biomass.csv"))
 head(FunGrp)
 
 #aggregate RACE codes by species name
 library(dplyr)
 FunGrp2 =FunGrp %>%
   group_by(Species) %>% 
   summarise(across(RACE, list))
 
 # Predators to calculate (all must be on above list)
 predators <- FunGrp2$Species
 
 #create empty biomass by functional group and year table
b.tot=matrix(nrow=1, ncol=6)
colnames(b.tot)=c("year", "stratum", "INPFC_AREA", "SumStratum_biomass.temp", "speciescode", "species")

 for (PRED in 1:length(predators)) { #each functional group
  
    for (i in 1:length(FunGrp2$RACE[[PRED]])) { #each species code within a functional group (sometimes more than 1)
   
      # Pick the func group and run through all multiple codes for that group (if >1)
    RACEcode <- FunGrp2$RACE[[PRED]][i]
    biom <- get_biomass_stratum(speciescode = RACEcode,survey_area = "GOA") 
    
    #merge catch data (biom) with stata info  (stratalist)
    biom2=biom %>%
      inner_join(stratalist, by = c("stratum" = "STRATUM")) %>%
      select(survey, year, stratum, INPFC_AREA, stratum_biomass) %>%
      group_by(year,INPFC_AREA, stratum) %>%
      add_column(RACE=RACEcode)%>%
      add_column(species=FunGrp2$Species[[PRED]])
    
    #prep table to sum biomass within stratum
    biom2=biom2 %>%rename (SumStratum_biomass.temp=stratum_biomass) 
    
    #sum up biomass for each functional group across multiple RACE codes
    if (i==1) { #only 1 code for species group 
      b.sp=biom2 %>%
        select (year, stratum, INPFC_AREA, SumStratum_biomass.temp,RACE,species) %>%
        rename (SumStratum_biomass=SumStratum_biomass.temp) } else { #if more than 1 code for species group
          
          #save biomass summed for this pred for this stratum in table
          b.sp=b.sp %>%
           left_join(biom2) %>%
           mutate (new_SumStratum_biomass=SumStratum_biomass+SumStratum_biomass.temp) %>%
           select (year, stratum, INPFC_AREA, new_SumStratum_biomass,RACE,species) %>%
           rename (SumStratum_biomass=new_SumStratum_biomass) #new name=old name
      } #end if statement
   } #end functional group loop
   
   if (PRED==1) { #save and move to next predator species
     b.tot=b.sp } else {
   b.tot=bind_rows(b.tot,b.sp) 
   b.sp=NA #clear species specific biomass table for next species
     } #end if statement
   
 } #end total biomass calc loop


### Final biomass by species by year by strata table
head(b.tot)

#add extra columns of species IDs (Ecopath species group names, latin names, codes)
b.tot2=b.tot %>%
  inner_join(FunGrp, by = c("species"="Species")) 

#save results write to csv file  
write.csv(b.tot2,"../results/SumStrata_biomass_GOA_1984_2021.csv",row.names=F) 
