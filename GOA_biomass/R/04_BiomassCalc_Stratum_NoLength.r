
#calculate biomass by superstrata 

#setwd("C:/Users/bridget.ferriss/work/Rpath/GAP_design_based_indices/R")
#source ("./GAP_design_based_indices/R/01_cleanup_data.R")
#source ("./GAP_design_based_indices/R/02_get_cpue.R")
#source ("./GAP_design_based_indices/R/03_get_biomass_stratum.R")

#setwd("C:/Users/bridget.ferriss/work/Rpath/GOA Rpath/Biomass")
#stratalist=read.csv(here::here( "data","goa_strata.csv")) #this path set in 01_cleanup_data.R
stratalist=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/goa_strata.csv")

#example to check working (POP)
 #x <- get_biomass_stratum(speciescode = 30060,survey_area = "GOA") #these are RACE species codes (not NODC)
# head(x)

library(tidyverse)
 library(dplyr)
 
#list of species & species codes to calcualte biomass (species with no stanzas/ no length bins) 
#FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass.csv") #includes all species and program runs out of memory to calculate
#not enough memory to run all species togethr so divide up cvs file for FunGrp
#ModelSpeciesList_GAP_Biomass_EGOA.B_otherskates.csv
#ModelSpeciesList_GAP_Biomass_EGOA.A.csv
#FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.A.csv")
 head(FunGrp)
 
 
 
 # RACE codes of predators to calculate biomass (all must be on above list)
 RACE <- FunGrp$RACE
 
# create empty dataframe to fill 
 final_table=data.frame(matrix(nrow=1, ncol=9, NA))
 colnames(final_table)=c("survey", "RACE", "stratum", "haul_count", "catch_count", "mean_wgt_cpue", "mean_num_cpue", "stratum_biomass", "stratum_pop") #remove year

 #Start loop through each RACE code to calculate biomass by length bin by stratum - OK if multiple RACE codes for same species - solve this later in code
      for (i in 1:length(RACE)) { 
        
      # Pick the func group and run through biomass estimates for that species (maybe multiple RACE codes for that species)
    RACEcode <- RACE[i]    #test: RACEcode=30060
  
  # call function to calculate biomass per length bins per stratum for that speciescode
    biomass_stratum <- get_biomass_stratum_noLength(speciescode = RACEcode,survey_area = "GOA")  #from 03_get_biomass_stratum_length.R 
     
    biomass_stratum<-biomass_stratum %>%
     select (survey, stratum, RACE, haul_count, catch_count, mean_wgt_cpue, mean_num_cpue, stratum_biomass, stratum_pop) #remove year
     
  #add to final table of all results       
 final_table<-final_table %>%
       bind_rows(final_table, biomass_stratum)
       
    } #end loop for each RACE code
     
#combine final biomass table with other Strata info file and Model Species List_GAP_Biomass.csv file (species names)  
 final_table2<- final_table %>%
       left_join(stratalist, by = c("stratum" = "STRATUM")) %>%
        left_join (FunGrp, by = "RACE")
       
 # sum biomass by  stratum  by species across multiple RACE codes (if have >1)
 Biomass_Strata_noLbin_EGOA.A<-final_table2 %>%
   group_by(survey, Species, stratum) %>%
   mutate(new.stratum_biomass=sum(stratum_biomass)) %>%
   ungroup()  %>%
   distinct(Species, stratum, .keep_all=TRUE) %>%
   select (survey, Species, RACE, stratum, haul_count, catch_count, mean_wgt_cpue, mean_num_cpue, new.stratum_biomass, stratum_pop,INPFC_AREA, MAX_DEPTH, REGULATORY_AREA_NAME, NODC, NAME, ECOPATH_PRED) #remove year

  
#write.csv(Biomass_Strata_noLbin_EGOA.A,"C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_EGOA.A.csv",row.names=F) 
 #write.csv(Biomass_Strata_noLbin_EGOA.A,"C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_DemersalShelfRF.csv",row.names=F) 
 #write.csv(Biomass_Strata_noLbin_EGOA.A,"C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_otherskates.csv",row.names=F)
 #write.csv(Biomass_Strata_noLbin_EGOA.A,"C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_shallowFF.csv",row.names=F)
 #write.csv(Biomass_Strata_noLbin_EGOA.A,"C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_slopeRF.csv",row.names=F)
 #write.csv(Biomass_Strata_noLbin_EGOA.A,"C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_other.csv",row.names=F)
 
 head(Biomass_Strata_noLbin_EGOA.A)
 

 
