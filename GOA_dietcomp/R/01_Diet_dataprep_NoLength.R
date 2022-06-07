
library(tidyverse)

setwd("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/R")

ppfile       <- "../data/GOA_raw.csv.gz"                    # raw Pred/prey diet file (RACEBASE query result) #BF added "../data/" and BF added ".gz"
pplookfile   <- "../lookups/Alaska_PreyLookup_MASTER.csv"  # lookup file for grouping prey #BF added "../lookups/"
preylook_col <- "ECOPATH_PREY"                      # Column name in pplookfile to use to group prey
LWparamfile      <- "../results/LengthWeightRegressionParams.csv"  #table of length weight parameters calculated by "00_LengthWeightRegression.r" from BTS specimens
GOA_stratafile <-"../lookups/GOA_strata_area2021.csv" #produced by GOA_area_calc.R
min_sample   <- 5                               # Minimum sample size for using diet data

### Length weight regression parameters
# List of predator-specific values of Length/ Weight Regression (W =a*L^b)  
# run script that calculates species-specific parameters in regression from Racebase (groundfish bottom trawl survey) 
# script queries racebase data so is updated when new survey data are enterred

#source("00_LengthWeightRegression.r") (if need updating)
LWparam=read.csv(LWparamfile)
head(LWparam) #output is a table with NODC and RACE species codes, species name, common name, LW regression intercept (a), and LW regression slope (b) (only calculated for species with min sample size=5)

#read in list of predators to calculate diet (no stanzas or length bins)
FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.A.csv")
#FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.B_otherskates.csv")
head(FunGrp)

##################
# RACE codes of predators with juv and adult stanzas (multiple length bins) to calculate biomass by length bin (all must be on above list)
RACE <- FunGrp$RACE
NODC <- FunGrp$NODC
Species <- FunGrp$Species

#save Length-Weight Regression parameters (W=aL^b) to associated NODC code
FunGrp2<-FunGrp %>%
  left_join(LWparam, by = c("NODC"="NODC.species.code"))

head(FunGrp2)

# Years to output
#yearlist <- 1985:2019
#yearlist <- 1991:2013

##Select which INPFC regions to calculate diet for (and list strata codes within each INPFC region)
## Group strata as desired
GOAstrata=read.csv(GOA_stratafile)

#from EBS code - strat in EBS are strata but GOA should be individual stratum
#stratblock_GOA <- list(
#  "Shumagin"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Shumagin")], 
#  "Chirikof"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Chirikof")], 
#  "Kodiak"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Kodiak")],
#  "Yakutat"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Yakutat")],
#  "Southeastern" =GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Southeastern")] 
#)

#list of all strata in GOA to calculate diet
stratlist=GOAstrata$Stratum

# Read in lookup and raw data and ensure 10-digit nodc numbers are read as text keys
preylooktable <- read.csv(pplookfile)
preylook <- preylooktable[,preylook_col]
names(preylook)<-sprintf("%010.0f",preylooktable$NODC_CODE)
fullprey<-unique(preylooktable[,preylook_col])

rawdat   <- read.csv(ppfile)
rawdat$PREY_NODC <-sprintf("%010.0f",rawdat$PREY_NODC)
rawdat$PRED_NODC <-sprintf("%010.0f",rawdat$PRED_NODC)  
rawdat$preyguild <- preylook[rawdat$PREY_NODC]

