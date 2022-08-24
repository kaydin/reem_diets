# Step 1: Get RACEBASE data -----------------------------------------------
# Where to put the cleaned/wrangled ---------------------------------------
#setwd("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/R")

library(tidyverse)

# Load Oracle data (takes a while) ----------------------------------------
# This local folder contains csv files of all the RACEBASE tables.

bio_path <- here::here("GOA_Biomass")

a <- list.files(paste(bio_path,"data/local_racebase",sep="/"), pattern = "\\.csv")

for (i in 1:length(a)) {
  b <- read.csv(file = paste(bio_path,"data/local_racebase", a[i], sep="/"))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = a[i]), value = b)
}


# Get stratum areas from GOA_STRATA table----------------------------------
goa_ai_strata <- read.csv(paste(bio_path,"data/goa_strata.csv",sep="/"))

goa_strata <- goa_ai_strata %>%
  janitor::clean_names() %>%
  filter(survey == "GOA")

ai_strata <- goa_ai_strata %>%
  janitor::clean_names() %>%
  filter(survey == "AI")

# Add area swept to each haul ---------------------------------------------
head(haul)
nrow(haul)
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

LWparam=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/results/LengthWeightRegressionParams.csv") #,row.names=F) 
head(LWparam)

head(length)

lengthdat<-length %>%
  group_by (cruise, haul, species_code)%>%
  mutate(NumSamp_SpHaul=sum(frequency))%>%
  ungroup()%>%
  left_join(LWparam, by=c("species_code"="sp_code"))%>%
  mutate(PropLength_Num = frequency/NumSamp_SpHaul)%>%
  mutate (weight_Lbin=LW.intercept_a*length^LW.slope_b) #weight of fish (sp, haul) in given length bin
 
head(lengthdat)




