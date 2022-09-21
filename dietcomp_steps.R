
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

####################################################################
preylookfile <- "./lookups/Alaska_PreyLookup_MASTER.csv" 

model.surveys = c(WGOA="GOA", EGOA="GOA", EBS="BS", NBS="BS") 

MODEL="EBS"
preylook_col <- "ECOPATH_PREY"    
stratbin_col <- "subarea"

REEM.loadclean.diets(region=model.surveys[MODEL], path="data/local_reem_data")
raw_preynames <- read.csv(preylookfile)

pred_params <- list(
    "W.pollock"  = list(nodc="8791030701", A_L=0.00553096, B_L=3.044172,   LCLASS=c(0,10,25,40,55,999) ),
    "P.cod"      = list(nodc="8791030401", A_L=0.00411781, B_L=3.25325765, LCLASS=c(0,10,30,60,85,999) ),
    "Arrowtooth" = list(nodc=c("8857040100", "8857040102"), # ATF includes Atheresthes sp. unid
                        A_L=0.00443866, B_L=3.19894001, LCLASS=c(0,10,30,50,999) ),
    "P.halibut"  = list(nodc="8857041901", A_L=0.01093251, B_L=3.24,       LCLASS=c(0,10,50,70,999) )
    )


predators <- c("P.cod")

## Everything below this line is for a SINGLE PREDATOR.  Calculating for multiple
## predators is currently not supported.
{  # Start of predator loop
  
PRED <- predators[1]
ppar <- pred_params[[PRED]] 

# The crosstab is made after a predator is selected (this will drop any prey columns with 0
# totals for that predator)

preylookup   <- read.csv(preylookfile) %>% mutate(prey_guild  = .data[[preylook_col]])
stratbins    <- raw_strata             %>% mutate(stratum_bin = .data[[stratbin_col]])

# Use year_group to pool years before summing, e.g. set year_group to "cold", "warm"
# as indexed by year.
yearblock    <- data.frame(year=1985:2021, year_group=1985:2021)

# Create a table with one line per pred/prey entry, add group lookups. 
# filter based on model, month, and predator.  
# Add L/W regressions and length bin.
pred_tab <- PP_data %>%
  # Add lookup tables
  left_join(preylookup, by=c("prey_nodc"="NODC_CODE")) %>%
  left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
  left_join(yearblock, by=c("year"="year")) %>%
  # First filter out all predators except the main PRED
  filter(submodel %in% MODEL)   %>%
  filter(pred_nodc %in% ppar$nodc) %>%
  filter(!is.na(year_group)) %>%
  filter(month %in% 5:8) %>%
  # Then add predator_specific data and make sure it's located before the prey_guild column
  #mutate(full = twt>0) %>% 
  mutate(lbin = as.character(cut(pred_len, ppar$LCLASS, right=F))) %>%
  mutate(bodywt = ppar$A_L * pred_len^ppar$B_L) %>%
  relocate(any_of(c("lbin","bodywt")), .before=prey_nodc) %>%
  # Make crosstab query grouping by all columns up to prey_nodc EXCEPT prey_nodc, then stratum and prey bins 
  group_by(across(c(1:prey_nodc,-prey_nodc)), stratum_bin, year_group, prey_guild) %>%
  summarize(prey_wt=sum(twt), .groups="keep")


# This creates one line per predator with stomach weight totals
pred_tots <- pred_tab %>%
  group_by(across(c(1:prey_guild,-prey_guild))) %>%
  summarize(tot_wt=sum(prey_wt), tot_sci=sum(prey_wt/bodywt), full=(prey_wt>0), prey_nguilds=n(), .groups="keep") %>%
  unique()

# For the stratum, year and length get totals
strat_tots <- pred_tots %>% 
  ungroup() %>%
  select(stratum_bin,year_group,lbin,full,tot_wt,tot_sci) %>%
  group_by(stratum_bin,year_group,lbin) %>%
  summarize(pred_N=n(), pred_full=sum(full), tot_wt=sum(tot_wt), tot_sci=sum(tot_sci), .groups="keep")


# Sum 
strat_dietprop <- pred_tab %>%
  ungroup() %>%
  mutate(prey_sci = prey_wt/bodywt) %>%
  select(stratum_bin,year_group,lbin,prey_guild,prey_wt,prey_sci) %>%  
  filter(prey_wt>0) %>%
  group_by(stratum_bin,year_group,lbin, prey_guild) %>%
  summarize(prey_N=n(), prey_wt=sum(prey_wt), prey_sci=sum(prey_sci), .groups="keep") %>%
  ungroup() %>%
  left_join(strat_tots, by=c("stratum_bin"="stratum_bin", "year_group"="year_group", "lbin"="lbin")) %>%
  relocate(any_of(c("pred_N","pred_full","tot_wt","tot_sci")), .before=prey_guild) %>%
  mutate(dietprop_wt = prey_wt/tot_wt) %>%
  mutate(dietprop_sci = prey_sci/tot_sci)



















pred_crosstab <- PP_data %>%
  # Add lookup tables
    left_join(preylookup, by=c("prey_nodc"="NODC_CODE")) %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
  # First filter out all predators except the main PRED
    filter(submodel %in% MODEL)   %>%
    filter(pred_nodc %in% ppar$nodc) %>%
  # Then add predator_specific data and make sure it's located before the prey_guild column
    mutate(lbin = as.character(cut(pred_len, ppar$LCLASS, right=F))) %>%
    mutate(bodywt = ppar$A_L * pred_len^ppar$B_L) %>%
    relocate(any_of(c("lbin","bodywt")), .before=prey_nodc) %>%
  # Make crosstab query grouping by all columns up to prey_nodc EXCEPT prey_nodc, then stratum and prey bins 
    group_by(across(c(1:prey_nodc,-prey_nodc)), stratum_bin, prey_guild) %>%
    tally(wt=twt) %>%
    spread(prey_guild, n, fill=0) #%>% select(-"<NA>") <NA> showing up means a prey code is missing?

if ("<NA>" %in% colnames(pred_crosstab)){
  print("warning, prey code goes to NA.  Removing NAs and check prey lookup")
  pred_crosstab <- pred_crosstab %>% select(-"<NA>")
}


## Add any final selection criteria here
SURV <- data.frame(pred_crosstab %>%
                   filter(month %in% 5:8))

## IMPORTANT:  The above crosstab will drop any prey categories that have 0's for 
## the chosen predator.  So each predator will have prey columns in a different order.
yearlist <- 1985:2021

# List out ranges for looping and adding
allstrat <- sort(unique(SURV$stratum_bin))
alllen   <- sort(as.character(unique(SURV$lbin)))
allyears <- yearlist
allprey  <- colnames(SURV)[ (which(names(SURV)=="stratum_bin")+1):ncol(SURV) ]
prey_guild <- colnames(pred_crosstab)[ (which(names(pred_crosstab)=="stratum_bin")+1):ncol(pred_crosstab) ]

min_sample   <- 5

o_vals <- NULL
for (STRAT in allstrat){
  cat(PRED,STRAT,"\n"); flush.console()
  for (LL in alllen){
    cat(LL,"\n"); flush.console()
    for (YY in allyears){
      cat(YY,"\n"); flush.console()
      
      SELPRED <- SURV[ SURV$stratum_bin %in% STRAT &
                         SURV$lbin %in%  LL    &
                         SURV$year %in%  YY   ,]  
      #SELPRED <- SURV %>%
      #           filter(stratum_bin==STRAT & lbin==LL & year==YY)
      #cat(nrow(SELPRED),"\n"); flush.console()
          
      if (nrow(SELPRED) >= min_sample){
        gear_temp <- mean(SELPRED$gear_temp,na.rm=T)
        surface_temp <- mean(SELPRED$surface_temp,na.rm=T)
        
        allfood  <- SELPRED[,allprey]
        goodprey <- as.matrix(allfood) #allfood[,colSums(allfood)>0] to remove colums with 0 for dirichlet  
        Nsamp    <- nrow(goodprey)
        prey_sp  <- colnames(goodprey)
        prey_N   <- colSums(goodprey>0) 
        prey_g   <- colSums(goodprey)
        spadd    <- 0.0 #spadd    <- DETECT*preywt_g/sum(preywt_g) for dirichlet
        SCI      <- t(t(goodprey)+spadd) / (SELPRED$bodywt)
        prey_SCI <- colSums(SCI)
        tot_g   <- sum(goodprey)
        tot_SCI <- sum(SCI)
        dietprop_g   <- colSums(goodprey)/tot_g
        dietprop_SCI <- colSums(SCI)/tot_SCI
        Nfull <- sum(rowSums(sampmat[IND,])>0)

        outvals <- data.frame(model,PRED,STRAT,YY,LL,Nsamp,Nfull,gear_temp,surface_temp,tot_g,tot_SCI,prey_guild,prey_N,prey_g,prey_SCI,dietprop_g,dietprop_SCI)#,cperfull,elog,e_sd)
        o_vals  <- rbind(o_vals,outvals)
      } # end of if sample size conditional 

    } # end of yearlist
  } # end of lenblock
} #end of stratblock      


}  # end predator loop 







                                   
# Still using a FOR loop to loop through all the strata/length combinations
# for each species.  For point estimates this could probably be converted to
# a tibble without loop, but leaving it in look form to allow for error
# estimation methods (bootstrap estimates) to be spliced in.


#################################################################################
htest <- haul %>% 
  filter(abundance_haul == "Y") %>% 
  mutate(year=floor(cruise/100)) %>% 
  left_join(stratbins, by=c("region"="survey", "stratum"="stratum")) %>%
  group_by(region,stratum_bin,year) %>% 
  tally()


  

#Species-specific additions  
#LCLASS=c(0,10,25,40,55,999)
#lbin  mutate(lbin = cut(pred_len, LCLASS, right=F)) %>%  

  
  
  
