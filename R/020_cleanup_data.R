# Step 1: Get RACEBASE data -----------------------------------------------
# Where to put the cleaned/wrangled ---------------------------------------
#setwd("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/R")

library(tidyverse)

# Load Oracle data (takes a while) ----------------------------------------
# This local folder contains csv files of all the RACEBASE tables.

#bio_path <- here::here("GOA_Biomass")

#all_strata <- read.csv("data/goa_strata.csv")

# Get stratum areas from GOA_STRATA table----------------------------------
raw_strata <- read.csv("data/combined_BTS_strata.csv") %>%
                janitor::clean_names() %>%
                filter(survey == main_survey)
                
# Add area swept to each haul ---------------------------------------------
haul <- haul %>%
  mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width)) # bc distance in km and width in m

# This should match the EFFORT column in AI/CPUE

# Get cruise info from RACE.DATA ------------------------------------------

cruisedat <- cruise %>%
  dplyr::select(survey_name, region,
                cruisejoin,
                agency_name) 
cruisedat$start_date <- as.Date(cruise$start_date)
cruisedat$year <- lubridate::year(cruisedat$start_date)

##BF add
#Length data - initial calculations to determine biomass by length bins; this step is proportion of number of fish in each legnth in by species by haul
#convert length in length bins in hauls to weight in length bins in hauls
#get parameters from length/weight regression (00_LengthWEight_Regression.R) in (C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/R) generates csv file
#W =a*L^b

LWparam=read.csv("data/LengthWeightRegressionParams.csv") %>%
  filter(EwEReg == LW_EwE ) #,row.names=F) 

lengthdat<-length %>%
  filter(region == LW_EwE)%>%
  group_by (cruise, haul, species_code)%>%
  mutate(NumSamp_SpHaul=sum(frequency))%>%
  ungroup()%>%
  left_join(LWparam, by=c("species_code"="sp_code"))%>%
  mutate(PropLength_Num = frequency/NumSamp_SpHaul)%>%
  mutate (weight_Lbin=LW.intercept_a*length^LW.slope_b) #weight of fish (sp, haul) in given length bin
 






lengthhaul <- lengthdat %>%
  left_join(haul,by=c("hauljoin"="hauljoin"))


length_sp <- lengthhaul %>%
  filter()


