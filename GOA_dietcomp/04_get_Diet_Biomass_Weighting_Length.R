#####
# Weight diet proportions (by pred by length bin by year) by strata from AFSC bottom trawl survey
#future step add region in specification for cuntion (currently hardcoded to GOA-need to combine GOA and EBS and AI stratum files)

library(tidyverse)

###############
###STEP 1. obtain diet proportions (g prey/g pred (of length bin) /year/ strata) 
###############

#source("03_get_Dietcomp_Length.R") # if haven't run it yet
dietprop.strata=o_vals
head(dietprop.strata)

###############
# STEP2. obtain biomass by species by strata (source ("GOA Rpath/GOA_biomass/R/04_BiomassCalc_Stratum_Length2.r"))
###############

biomstratLbin=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Strata_Lbin.csv")
head(biomstratLbin)
biomstratLbin2=biomstratLbin[-1,] #remove first row of NAs

#check
biomstratLbin2[which(biomstratLbin2$Species=="P. Cod"),]
levels(biomstratLbin2$Species)

##############
# STEP 3. read in strata table info (stratum ID, NPFMC ID, depth category)
#############

stratafile <-read.csv("../lookups/GOA_strata_area2021.csv")

#match length bins from RACE and diet data - create new column of Lbins in biomass table to match diet table
#biomstratLbin2$length
#biomstratLbin3$length_cm
#biomstratLbin3$Lbin

biomstratLbin3 <- biomstratLbin2 %>%
   mutate(length_cm=length/10) %>%    #note biomass (from RACE legnths are in mm, diet lengths from food habtis are in cm 
   mutate(Lbin = case_when((Species == "POP" & length_cm <20) ~ 20, 
                                         (Species == "POP" & length_cm >19) ~ 999, 
                                         (Species == "W. Pollock" & length_cm <25) ~ 25, 
                                         (Species == "W. Pollock" & length_cm <40) ~ 40, 
                                         (Species == "W. Pollock" & length_cm <55) ~ 55,
                                         (Species == "W. Pollock" & length_cm >54) ~ 999,
                                         (Species == "Sablefish" & length_cm <20) ~ 20, 
                                         (Species == "Sablefish" & length_cm >19) ~ 999, 
                                         (Species == "Rex Sole" & length_cm <20) ~ 20, 
                                         (Species == "Rex Sole" & length_cm >19) ~ 999,  
                                         (Species == "P. Halibut" & length_cm <20) ~ 20, 
                                         (Species == "P. Halibut" & length_cm <50) ~ 50,
                                         (Species == "P. Halibut" & length_cm <70) ~ 70, 
                                         (Species == "P. Halibut" & length_cm >69) ~ 999, 
                                         (Species == "P. Cod" & length_cm <20) ~ 20, 
                                         (Species == "P. Cod" & length_cm <60) ~ 60, 
                                         (Species == "P. Cod" & length_cm <85) ~ 85,
                                         (Species == "P. Cod" & length_cm >84) ~ 999, 
                                         (Species == "Arrowtooth" & length_cm <20) ~ 20, 
                                         (Species == "Arrowtooth" & length_cm <50) ~ 50, 
                                         (Species == "Arrowtooth" & length_cm >49) ~ 999, 
                                         (Species == "FH. Sole" & length_cm <20) ~20,
                                         (Species == "FH. Sole" & length_cm >19) ~999,
                                         TRUE ~ 0)) 

#convert LL notation [0,20) to Lbin notation to match biomass dataset]
dietprop.strata2<- dietprop.strata %>%
  mutate(Lbin= case_when((PRED == "P.ocean perch" & LL =="[0,20)") ~20, 
                         (PRED == "P.ocean perch" & LL =="[20,999)") ~ 999, 
                         (PRED == "W.pollock" & LL =="[0,25)") ~ 25, 
                         (PRED == "W.pollock" & LL =="[25,40)") ~ 40, 
                         (PRED == "W.pollock" & LL =="[40,55)") ~ 55,
                         (PRED == "W.pollock" & LL =="[55,999)") ~ 999,
                         (PRED == "Sablefish" & LL =="[0,20)") ~ 20, 
                         (PRED == "Sablefish" & LL =="[20,999)") ~ 999, 
                         (PRED == "Rex Sole" & LL =="[0,20)") ~ 20, 
                         (PRED == "Rex Sole" & LL =="[20,999)") ~ 999,  
                         (PRED == "P.halibut" & LL =="[0,20)") ~ 20, 
                         (PRED == "P.halibut" & LL =="[20,50)") ~ 50,
                         (PRED == "P.halibut" & LL =="[50,70)") ~ 70, 
                         (PRED == "P.halibut" & LL =="[70,999)") ~ 999, 
                         (PRED == "P.cod" & LL =="[0,20)") ~ 20, 
                         (PRED == "P.cod" & LL =="[20,60)") ~ 60, 
                         (PRED == "P.cod" & LL =="[60,85)") ~ 85,
                         (PRED == "P.cod" & LL =="[85,999)") ~ 999, 
                         (PRED == "Arrowtooth" & LL =="[0,20)") ~ 20, 
                         (PRED == "Arrowtooth" & LL =="[20,50)") ~ 50, 
                         (PRED == "Arrowtooth" & LL =="[50,999)")~ 999,
                         (PRED == "FH. Sole" & LL =="[0,20)") ~20, 
                         (PRED == "FH. Sole" & LL =="[20,999)") ~ 999, 
                         TRUE ~ 0)) 

#assign length bins to juv and adult stanzas
biomstratLbin4<- biomstratLbin3 %>%
  mutate(Stanza = case_when((Species == "POP" & Lbin ==20)~"juv",
                           (Species == "POP" & Lbin >20) ~"adult", 
                           (Species == "W. Pollock" & Lbin ==25) ~"juv", 
                           (Species == "W. Pollock" & Lbin >25) ~ "adult",  
                           (Species == "Sablefish" & Lbin ==20) ~"juv", 
                           (Species == "Sablefish" & Lbin >20) ~ "adult", 
                           (Species == "Rex Sole" & Lbin ==20) ~ "juv", 
                           (Species == "Rex Sole" & Lbin >20) ~ "adult",  
                           (Species == "P. Halibut" & Lbin ==20) ~ "juv",
                           (Species == "P. Halibut" & Lbin >20) ~ "adult", 
                           (Species == "P. Cod" & Lbin ==20) ~ "juv", 
                           (Species == "P. Cod" & Lbin >20) ~ "adult", 
                           (Species == "FH. Sole" & Lbin ==20) ~"juv",
                           (Species == "FH. Sole" & Lbin >20) ~"adult",
                           (Species == "Arrowtooth" & Lbin ==20) ~ "juv", 
                           (Species == "Arrowtooth" & Lbin >20) ~ "adult"))
                           #, 
                          # TRUE ~ 0))                          #doesn't work if include this like in previous case_whens - maybe becasue text?
biomstratLbin4$Stanza

           
##############
# STEP 4. rename PRED/species names to match in diet and biomass database
##############

biomstratLbin5<-biomstratLbin4%>%
  mutate(Species = recode(Species, "W. Pollock" = "W.pollock"),
         Species = recode(Species, "P. Cod" = "P.cod"),
         Species = recode(Species, "POP" = "P.ocean perch"),
         Species = recode(Species, "P. Halibut" = "P.halibut")) 

##############
# STEP 5. join new pred biomass by length table with diet proportion table  
##############
# calculate new diet proportion weighted by pred biomass summed at stratum level by length bin
##aggregate diet proportion by juv and adult size classes

dietprop.strata3<-dietprop.strata2 %>%
  left_join(biomstratLbin5, by=c("STRAT"="stratum", "PRED"="Species",  "Lbin")) %>%    #double check is right_join correct (stratum/STRAT won't join with left_join and full_join)
  mutate(preyBiom.Lbin.Str=dietprop*new.stratum_biomass.Lbin) %>%    #convertr diet proportion to prey biomass (mt) per Lbin per stratum (predator biomass at stratum level is mt)
  group_by(STRAT, PRED, Stanza, sp_prey) %>%                        #sum across Lbins at strata level into juv/adult
  mutate (PredBiom.JuvAdult.Str=sum(new.stratum_biomass.Lbin, na.rm = TRUE)) %>%   #prey biomass (Juv/Adult) per stratum
  mutate (PreyBiom.JuvAdult.Str=sum(preyBiom.Lbin.Str, na.rm = TRUE))   #prey (mt) consumed by pred (Juv/Adult) per stratum

#############
# STEP 6.  Check output
############
head(dietprop.strata3)
dietprop.strata3$Stanza









