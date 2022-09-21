
# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library("tidyverse")
library("janitor") 
library("lubridate")

####################################################################

source("R/REEM_fooddata_functions.R")

food <- list()
food[["BS"]]  <- REEM.loadclean.diets(region="BS", path="data/local_reem_data")
food[["GOA"]] <- REEM.loadclean.diets(region="GOA", path="data/local_reem_data")
food[["AI"]]  <- REEM.loadclean.diets(region="AI", path="data/local_reem_data")

preylook_col <- "ecopath_prey"    
stratbin_col <- "strat_groups"
raw_strata    <- read.clean.csv("lookups/combined_BTS_strata.csv")     
raw_preynames <- read.clean.csv("lookups/Alaska_PreyLookup_MASTER.csv") 
raw_years     <- data.frame(year=1985:2021, year_group=1985:2021) 

pred_params <- list(
    "W.pollock"  = list(nodc="8791030701", race="21740", A_L=0.00553096, B_L=3.044172,   LCLASS=c(0,10,25,40,55,999) ),
    "P.cod"      = list(nodc="8791030401", race="21720", A_L=0.00411781, B_L=3.25325765, LCLASS=c(0,10,30,60,85,999) ),
    "Arrowtooth" = list(nodc="8857040102", race="10110", A_L=0.00443866, B_L=3.19894001, LCLASS=c(0,10,30,50,999) ),
    "P.halibut"  = list(nodc="8857041901", race="10120", A_L=0.01093251, B_L=3.24,       LCLASS=c(0,10,50,70,999) )
    )

strat.props.bs <- predprey_tables(predator="P.cod", model="EBS", ppdat=food[["BS"]], months=5:8)
strat.props.goa <- predprey_tables(predator="Arrowtooth", model="WGOA", ppdat=food[["GOA"]], months=5:8)



#################################################################################
REEM.loadclean.RACE(path = "data/local_racebase")

# convert to area in km given width in m
  haul <- haul %>% mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width)) 
# Simple tidy up function for the dates in the cruise data  
  cruisedat <- tidy.cruise.dates(cruise) 

source("R/REEM_fooddata_functions.R")
biomass_stratum <- get_biomass_stratum_noLength(speciescode = pred_params[["P.cod"]]$race, survey_area = "GOA")




#################################################################################
htest <- haul %>% 
  filter(abundance_haul == "Y") %>% 
  mutate(year=floor(cruise/100)) %>% 
  left_join(stratbins, by=c("region"="survey", "stratum"="stratum")) %>%
  group_by(region,stratum_bin,year) %>% 
  tally()


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
#region_strata <- raw_strata %>% filter(survey == REGION)
#model_strata  <- raw_strata %>% filter(submodel == SUBMODEL)


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

#Species-specific additions  
#LCLASS=c(0,10,25,40,55,999)
#lbin  mutate(lbin = cut(pred_len, LCLASS, right=F)) %>%  

  
  
  
