
#{r environ}
#setwd() if needed here
library("tidyverse")
library("janitor") 
library("lubridate")

# User supplied stuff
REGION    <- "GOA"
SUBMODEL  <- "WGOA"
#source("./user.R")

# Load functions - note: reload these if you change user inputs above.
source("R/REEM_fooddata_functions.R")


#########################################################
#{r load_clean_data, eval=FALSE}

# REEM.loadclean.RACE loads all files with a .csv extension that are in the 
# directory indicated by path and names them after the file.  Any csv file
# in the directory will be loaded.
REEM.loadclean.RACE(path = "data/local_racebase")

# Above RACE data is all regions.  For food habits and strata, only the
# region-specific data is loaded (re-run from here if switching regions).
# REEM.loadclean.diets [region]_predprey and _predlength data.
REEM.loadclean.diets(region=REGION, path="data/local_reem_data")

# Load the table of combined survey strata.  
raw_strata <- read.csv("lookups/combined_BTS_strata.csv") %>% janitor::clean_names()
region_strata <- raw_strata %>% filter(survey == REGION)
model_strata  <- raw_strata %>% filter(submodel == SUBMODEL)

# Load length/weight regression parameters (currently from pre-made file, could
# re-generate here)  
LWparam=read.csv("lookups/LengthWeightRegressionParams.csv") %>%
  filter(EwEReg == SUBMODEL)  

## Raw data should be loaded into the Workspace at this point

# Cleanup-type data manipulations
# convert to area in km given width in m
haul <- haul %>% 
  mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width)) 
# Simple tidy up function for the dates in the cruise data  
cruisedat <- tidy.cruise.dates(cruise) 
# Add length/weight regressions to length table
lengthdat<-length %>%
  filter(region == REGION)%>%
  group_by (cruise, haul, species_code)%>%
  mutate(NumSamp_SpHaul=sum(frequency))%>%
  ungroup()%>%
  left_join(LWparam, by=c("species_code"="sp_code"))%>%
  mutate(PropLength_Num = frequency/NumSamp_SpHaul)%>%
  mutate (weight_Lbin=LW.intercept_a*length^LW.slope_b) #weight of fish (sp, haul) in given length bin
# KYA - not sure if the below are needed  
#lengthhaul <- lengthdat %>%
#  left_join(haul,by=c("hauljoin"="hauljoin"))
#length_sp <- lengthhaul %>%
#  filter()  

####################################################################
preylookfile <- "./lookups/Alaska_PreyLookup_MASTER.csv" 
preylook_col <- "ECOPATH_PREY"    
preylookup   <- read.csv(preylookfile) %>%
                mutate(prey_guild = .data[[preylook_col]])

stratbin_col <- "subarea"
stratbins    <- raw_strata %>%
                mutate(stratum_bin = .data[[stratbin_col]])

## Create a crosstab table, with one line per predator, and prey weights (twt) summed
## by prey_guild into columns.  It groups by all columns before the prey_nodc column.

PP_table <- PP_data %>%
            left_join(preylookup, by=c("prey_nodc"="NODC_CODE")) %>%
            left_join(stratbins, by=c("region"="survey","stratum"="stratum"))

## Create a crosstab table, with one line per predator, and prey weights (twt) summed
## by prey_guild into columns.  It groups by all columns before the prey_nodc column.
#PP_crosstab <- PP_data %>%
#          left_join(preylookup, by=c("prey_nodc"="NODC_CODE")) %>%
#          left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
#          #group_by(across(1:(which(names(PP_data)=="prey_nodc")-1)), stratum_bin, prey_guild) %>%
#          group_by(across(1:(which(names(PP_data)=="prey_nodc")-1)), submodel, subarea, prey_guild) %>%
#          tally(wt=twt) %>%
#          spread(prey_guild, n, fill=0)
  
  htest <- haul %>% 
           filter(abundance_haul == "Y") %>% 
           mutate(year=floor(cruise/100)) %>% 
           left_join(stratbins, by=c("region"="survey", "stratum"="stratum")) %>%
           group_by(region,stratum_bin,year) %>% 
           tally()
  

## Operations after this need to work on the table separately for each predator
## (so as to use different length binds)
  
pred_params <- list(
    "W.pollock"  = list(nodc="8791030701", A_L=0.00553096, B_L=3.044172,   LCLASS=c(0,10,25,40,55,999) ),
    "P.cod"      = list(nodc="8791030401", A_L=0.00411781, B_L=3.25325765, LCLASS=c(0,10,30,60,85,999) ),
    "Arrowtooth" = list(nodc=c("8857040100", "8857040102"), # ATF includes Atheresthes sp. unid
                        A_L=0.00443866, B_L=3.19894001, LCLASS=c(0,10,30,50,999) ),
    "P.halibut"  = list(nodc="8857041901", A_L=0.01093251, B_L=3.24,       LCLASS=c(0,10,50,70,999) )
    )

predators <- c("W.pollock")

PRED <- predators[1]
ppar <- pred_params[[PRED]] 

# The crosstab is made after a predator is selected (this will drop any prey columns with 0
# totals for that predator)
pred_crosstab <- PP_table %>% 
           filter(pred_nodc %in% ppar$nodc) %>%
           group_by(across(1:(which(names(PP_data)=="prey_nodc")-1)), submodel, subarea, prey_guild) %>%
           tally(wt=twt) %>%
           spread(prey_guild, n, fill=0) #%>% select(-"<NA>") <NA> showing up means a prey code is missing?

# The field used (e.g. subarea) should be the group_by column immediately before
# prey_guild on the above lookup
preycross <- data.frame(pred_crosstab) %>% 
             select((which(names(pred_crosstab)=="subarea")+1):last_col())

# Adding predator_specific columns (moving so prey columns are last on table)
# and apply any final filters (like month)
preddat <- pred_crosstab %>%
  mutate(lbin = cut(pred_len, ppar$LCLASS, right=F)) %>%
  mutate(bodywt = ppar$A_L * pred_len^ppar$B_L) %>%
  relocate(any_of(c("lbin","bodywt")), .after=pred_len) %>%
  filter(month %in% 5:8)



  

#Species-specific additions  
#LCLASS=c(0,10,25,40,55,999)
#lbin  mutate(lbin = cut(pred_len, LCLASS, right=F)) %>%  

  
  
  
