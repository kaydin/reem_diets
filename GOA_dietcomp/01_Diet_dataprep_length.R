
#Prepare data to run diet proportion function
#specify species to calculate diet (by length or stanza
#specify region (WGOA or EGOA )-this determines which length-weight parameters to use)

library(tidyverse)

setwd("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/R")

ppfile       <- "../data/GOA_raw.csv.gz"                    # raw Pred/prey diet file (RACEBASE query result) #BF added "../data/" and BF added ".gz"
pplookfile   <- "../lookups/Alaska_PreyLookup_MASTER.csv"  # lookup file for grouping prey #BF added "../lookups/"
preylook_col <- "ECOPATH_PREY"                      # Column name in pplookfile to use to group prey
LWparamfile      <- "../results/LengthWeightRegressionParams.csv"  #table of length weight parameters calculated by "00_LengthWeightRegression.r" from BTS specimens
GOA_stratafile <-"../lookups/GOA_strata_area2021.csv" #produced by GOA_area_calc.R
min_sample   <- 5                               # Minimum sample size for using diet data

#source("00_LengthWeightRegression.r") (if need updating)
#LWparam=read.csv(LWparamfile) #if calculating direct from RACE data
#LWparam=read.csv(LWparam_precalc) #using Szymon's values and other precalculated values
#head(LWparam) #output is a table with NODC and RACE species codes, species name, common name, LW regression intercept (a), and LW regression slope (b) (only calculated for species with min sample size=5)

#################
## STEP 1. Read length-weight parameters and select which EwE Region will be calculating diet becasue difference LWparam values for each region
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

POP=LWparam[which(LWparam[,1]=="8826010102"),] #Pacific ocean perch"),] #30060
ATF=LWparam[which(LWparam[,1]=="8857040100" | LWparam[,1]=="8857040102"),] #)  #10110),] #10110 # arrowtooth flounder
PLK=LWparam[which(LWparam[,1]=="8791030701"),]  #walleye pollock (juvenile)"),] #21741
#PLKjuv=LWparam[which(LWparam[,1]=="walleye pollock (adult)"),] #21742
COD=LWparam[which(LWparam[,1]=="8791030401"),] #"Pacific cod (adult)"),] #21722
#COD=LWparam[which(LWparam[,1]=="Pacific cod (juvenile)"),] #21721
HAL=LWparam[which(LWparam[,1]=="8857041901"),]
SBL=LWparam[which(LWparam[,1]=="8827020101"),]
REX=LWparam[which(LWparam[,1]=="8857040501"),]
FHS=LWparam[which(LWparam[,1]=="8857040601"),]

#save species-specific length-weight paramenters and length bins
preds <- list(
  "W.pollock"  = list(nodc=PLK$NODCcode, A_L=PLK$LW.intercept_a, B_L=PLK$LW.slope_b, LCLASS=c(0,25,40,55,999)), #c(0,10,25,40,55,999)
  "P.cod"      = list(nodc=COD$NODCcode, A_L=COD$LW.intercept_a, B_L=COD$LW.slope_b, LCLASS=c(0,20,60,85,999)), #c(0,10,30,60,85,999)
  "P.halibut"  = list(nodc=HAL$NODCcode, A_L=HAL$LW.intercept_a, B_L=HAL$LW.slope_b, LCLASS=c(0,20,50,70,999)), #c(0,10,50,70,999)
  "P.ocean perch"  = list(nodc=POP$NODCcode, A_L=POP$LW.intercept_a, B_L=POP$LW.slope_b, LCLASS=c(0,20,999)),
  "Rex Sole"  = list(nodc=REX$NODCcode, A_L=REX$LW.intercept_a, B_L=REX$LW.slope_b, LCLASS=c(0,20,999)),
  "FH. Sole" = list(nodc=FHS$NODCcode, A_L=FHS$LW.intercept_a, B_L=FHS$LW.slope_b, LCLASS=c(0,20,999)), 
  "Sablefish"= list(nodc=SBL$NODCcode, A_L=SBL$LW.intercept_a, B_L=SBL$LW.slope_b, LCLASS=c(0,20,999)), 
  "Arrowtooth" = list(nodc=ATF$NODCcode, A_L=ATF$LW.intercept_a, B_L=ATF$LW.slope_b,  LCLASS=c(0,20,50,999)) #c(0,10,30,50,999),#a and b parameters from Holsman 2015 A_L=0.00459, B_L=3.18,
)


# Predators to calculate (all must be on above list)
predators <- c("W.pollock","P.cod", "P.halibut","P.ocean perch", "Rex Sole", "FH. Sole","Sablefish", "Arrowtooth")   

# Years to output
#yearlist <- 1985:2019
#yearlist <- 1991:2013

##Select which INPFC regions to calculate diet for (and list strata codes within each INPFC region)
## Group strata as desired
GOAstrata=read.csv(GOA_stratafile)

##from EBS code - strat in EBS are strata but GOA should be individual stratum
#stratblock_GOA <- list(
#  "Shumagin"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Shumagin")], 
#  "Chirikof"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Chirikof")], 
#  "Kodiak"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Kodiak")],
#  "Yakutat"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Yakutat")],
#  "Southeastern" =GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Southeastern")] 
#)

#list of all individual strata in GOA to calculate diet
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
