#select species to calculate diet 
#select region (WGOA or EGOA) - this determines what LWparams to use

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

#################
## STEP 1. Read length-weight parameters and select which EwE Region will be calculating diet becasue difference LWparam values for each region #source("00_LengthWeightRegression.r") (if need updating)
#################

LWparam.allreg=read.csv(LWparamfile) #if calculating direct from RACE data
#LWparam=read.csv(LWparam_precalc) #using Szymon's values and other precalculated values
head(LWparam.allreg) #output is a table with NODC and RACE species codes, species name, common name, LW regression intercept (a), and LW regression slope (b) (only calculated for species with min sample size=5)

#select region for
LWparam<-LWparam.allreg%>%
  filter(EwEReg=="EGOA") #Choices: "WGOA", "EGOA", "BS", "AI"

#check
unique(LWparam$EwEReg)

#################
## STEP 2. DEfine predators of interest (for diet calculations by stanza or length)
#################

#read in list of predators to calculate diet (no stanzas or length bins)
#FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.B_DemersalShelfRF.csv") 
#FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.B_otherskates.csv")
#FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.B_shallowFF.csv")
#FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.B_slopeRF.csv")
#FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.B_other.csv")
head(FunGrp)

#
RACE <- FunGrp$RACE
NODC <- FunGrp$NODC
Species <- FunGrp$Species

#####################
## STEP. Read in biomass calculation results
#####################

#biomstrat=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_DemersalShelfRF.csv") 
#biomstrat=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_otherskates.csv")
#biomstrat=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_shallowFF.csv")
#biomstrat=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_slopeRF.csv")
#biomstrat=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/results/Biomass_Stratum_noLbin_other.csv")

###################
## STEP. save Length-Weight Regression parameters (W=aL^b) to associated NODC code
###################

FunGrp2<-FunGrp %>%
  left_join(LWparam, by = c("NODC"="NODCcode"))

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

