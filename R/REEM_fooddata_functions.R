#source("R/function_get_cpue_byLength.R")
#source("R/function_get_cpue_NoLength.R")
#source("R/function_get_biomass_stratum_Length.R")
#source("R/function_get_biomass_stratum_NoLength.R")

source("R/GAP_get_cpue.R")
source("R/GAP_get_biomass_stratum.R")
#source("R/GAP_loadclean.R")
#############################################################
haul_summary <- function(model){

  this.model=model
  stratbins    <- strata_lookup # %>% mutate(stratum_bin = .data[[stratbin_col]])
  x <- haul %>% 
    filter(abundance_haul == "Y") %>% 
    mutate(year=floor(cruise/100)) %>% 
    left_join(stratbins, by=c("region"="survey", "stratum"="stratum")) %>%
    filter(model==this.model) %>%
    group_by(model,stratum_bin,year) %>% 
    summarize(stations=length(hauljoin),
              bottom_temp_mean  = mean(gear_temperature,na.rm=T),
              surface_temp_mean = mean(surface_temperature,na.rm=T),
              .groups="keep")
  return(x)
}

#############################################################
get_lw <- function(predator="P.cod", model="EBS", years=NULL, all.data=F){
  
  stratbins    <- strata_lookup     #%>% mutate(stratum_bin = .data[[stratbin_col]])
  preylookup    <- preynames_lookup #%>% mutate(prey_guild  = .data[[preylook_col]])
  ppar          <- pred_params[[predator]] 
  model_name    <- model    # renamed to avoid name confusion during lookup
  predator_name <- predator # renamed to avoid name confusion during lookup  
  
  speciescode = as.integer(pred_params[[predator]]$race)
  
  sp_specimen <- specimen %>%
    filter(species_code == speciescode)
  
  # KYA added
  #stratbins    <- strata_lookup    %>% mutate(stratum_bin = .data[[stratbin_col]])
  model_haul <- haul %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum"))
  
  x <- model_haul %>%
    left_join(cruisedat,
              by = c("cruisejoin", "region")
    ) %>%
    filter(abundance_haul == "Y" &
             model == model_name) %>% # KYA changed
    left_join(sp_specimen, by = "hauljoin") %>%
    dplyr::select(
      species_code,model,stratum_bin,region.x,#KYA added model, stratum_bin
      cruisejoin.x, vessel.x, haul.x, hauljoin,
      stratum, stationid,
      year,
      start_latitude, start_longitude, gear_depth, bottom_depth,
      gear_temperature, surface_temperature,
      AreaSwept_km2, specimenid, length, sex, weight, age
    ) %>%
    filter(!is.na(specimenid) & length>0 & weight>1) %>%  #remove fish W=1 (logW=0) in which weight is too hard to measure and throws off regression 
    dplyr::rename(
      Lat = start_latitude,
      Lon = start_longitude,
      cruisejoin = cruisejoin.x,
      vessel = vessel.x,
      haul   = haul.x,
      region = region.x,
      Bottom_temp = gear_temperature,
      Surface_temp = surface_temperature
    ) %>%
    mutate(predator = predator_name,
           length_cm = length/10) %>% relocate(predator) 
  
  
  if(!is.null(years)){
    x <- x %>% filter(year %in% years)
  }
  
  lw <- lm(log(weight) ~ log(length_cm), data=x)
  lw_a <- lw$coef[[1]]
  lw_b <- lw$coef[[2]]
  
  dat <- x %>%
    mutate(lbin = as.character(cut(length_cm, ppar$LCLASS, right=F)),
           log_lw_a = lw$coef[[1]], 
           lw_b = lw$coef[[2]], 
           log_diff = log(weight) - (lw_a + lw_b*log(length_cm)))
  
  if(all.data){
    return(dat)
  }
  else{
    return(list(lw_a=exp(lw_a),lw_b=lw_b))
  }
  
}

#############################################################

preylength_splits <- function(pred_nodc, prey_nodc, predcut, preycut, model, months=5:8){
  
  stratbins    <- strata_lookup    #%>% mutate(stratum_bin = .data[[stratbin_col]])  
  model_name    <- model    # renamed to avoid name confusion during lookup
  raw_pp <- preylengths
  this.pred_nodc <- pred_nodc
  this.prey_nodc <- prey_nodc
  
  allpred_tab <- raw_pp %>%
    # Add lookup tables
    #left_join(preylookup, by=c("prey_nodc"="nodc_code")) %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
    #left_join(yearblock, by=c("year"="year")) %>%
    relocate(stratum_bin) %>% relocate(model) %>%
    filter(model %in% model_name)   %>%
    filter(pred_nodc %in% this.pred_nodc) %>%
    filter(prey_nodc %in% this.prey_nodc) %>%
    #filter(!is.na(year)) %>%
    filter(month %in% months) %>%
    select(1:freq) %>%
    mutate(pred_lbin_cm = as.character(cut(pred_len, predcut, right=F)),
           prey_lbin_mm = as.character(cut(prey_size_mm, preycut, right=F)))
  
}

predprey_tables <- function(predator="P.cod", model="EBS", months=5:8, all.data=F){

  # Global variables
  stratbins    <- strata_lookup    # %>% mutate(stratum_bin = .data[[stratbin_col]])
  preylookup    <- preynames_lookup # %>% mutate(prey_guild  = .data[[preylook_col]])
  #yearblock    <- years_lookup
  ppar          <- pred_params[[predator]]
  raw_pp        <- predprey
  model_name    <- model    # renamed to avoid name confusion during lookup
  predator_name <- predator # renamed to avoid name confusion during lookup

  # Operations done on all predators before selection  
  allpred_tab <- raw_pp %>%
    # Add lookup tables
    left_join(preylookup, by=c("prey_nodc"="nodc_code")) %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
    #left_join(yearblock, by=c("year"="year")) %>%
    relocate(stratum_bin) %>% relocate(model) 
  
  # Select predator (and apply other filters like model and months), apply predator-specific values
  pred_tab <- allpred_tab %>%
    filter(model %in% model_name)   %>%
    filter(pred_nodc %in% ppar$nodc)  %>%
    #filter(!is.na(year)) %>%
    filter(month %in% months)   %>%
    mutate(predator = predator_name) %>% relocate(predator) %>%
    # Then add predator_specific data and make sure it's located before the prey_guild column
    #mutate(full = twt>0) %>% 
    mutate(lbin = as.character(cut(pred_len, ppar$LCLASS, right=F))) %>%
    mutate(bodywt = ppar$lw_a * pred_len^ppar$lw_b) %>%
    relocate(any_of(c("lbin","bodywt")), .before=prey_nodc) %>%
    # Group by all columns up to prey_nodc EXCEPT prey_nodc, then stratum and prey bins 
    group_by(across(c(1:prey_nodc,-prey_nodc)), stratum_bin, year, prey_guild) %>%
    summarize(prey_wt=sum(twt), .groups="keep")

  cat(nrow(pred_tab),"predprey records found, summarizing...\n"); flush.console()

  #assign("pred_tab_tmp", value = pred_tab, envir = .GlobalEnv)
  
  # This creates one line per predator with stomach weight totals
  pred_tots <- pred_tab %>%
    group_by(across(c(1:prey_guild,-prey_guild))) %>%
    summarize(tot_wt=sum(prey_wt), tot_sci=sum(prey_wt/bodywt), full=(prey_wt>0), prey_nguilds=n(), .groups="keep") %>%
    unique()

  # For the stratum, year and length get totals
  strat_tots <- pred_tots %>% 
    ungroup() %>%
    select(predator,model,stratum_bin,year,lbin,full,tot_wt,tot_sci) %>%
    group_by(predator,model,stratum_bin,year,lbin) %>%
    summarize(pred_n=n(), pred_full=sum(full), tot_wt=sum(tot_wt), tot_sci=sum(tot_sci), .groups="keep")

  # Sum 
  strat_dietprop <- pred_tab %>%
    ungroup() %>%  
    mutate(prey_sci = prey_wt/bodywt) %>%
    select(predator,model,stratum_bin,year,lbin,prey_guild,prey_wt,prey_sci) %>%  
    filter(prey_wt>0) %>%
    group_by(predator,model,stratum_bin,year,lbin, prey_guild) %>%
    summarize(prey_n=n(), prey_wt=sum(prey_wt), prey_sci=sum(prey_sci), .groups="keep") %>%
    ungroup() %>%
    left_join(strat_tots, by=c("predator"="predator","model"="model","stratum_bin"="stratum_bin", "year"="year", "lbin"="lbin")) %>%
    relocate(any_of(c("pred_n","pred_full","tot_wt","tot_sci")), .before=prey_guild) %>%
    mutate(dietprop_wt = prey_wt/tot_wt) %>%
    mutate(dietprop_sci = prey_sci/tot_sci)

  # No longer using the crosstab version - saving here for future reference (may need tweaks)
  #pred_crosstab <- PP_data %>%
  #  # Add lookup tables
  #  left_join(preylookup, by=c("prey_nodc"="NODC_CODE")) %>%
  #  left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
  #  # First filter out all predators except the main PRED
  #  filter(submodel %in% MODEL)   %>%
  #  filter(pred_nodc %in% ppar$nodc) %>%
  #  # Then add predator_specific data and make sure it's located before the prey_guild column
  #  mutate(lbin = as.character(cut(pred_len, ppar$LCLASS, right=F))) %>%
  #  mutate(bodywt = ppar$A_L * pred_len^ppar$B_L) %>%
  #  relocate(any_of(c("lbin","bodywt")), .before=prey_nodc) %>%
  #  # Make crosstab query grouping by all columns up to prey_nodc EXCEPT prey_nodc, then stratum and prey bins 
  #  group_by(across(c(1:prey_nodc,-prey_nodc)), stratum_bin, prey_guild) %>%
  #  tally(wt=twt) %>%
  #  spread(prey_guild, n, fill=0) #%>% select(-"<NA>") <NA> showing up means a prey code is missing?
  if (all.data){  
    return(list(predprey_table = data.frame(pred_tab),
              pred_totals    = data.frame(pred_tots), 
              strat_dietprop = data.frame(strat_dietprop)))
  } else {
    return(data.frame(strat_dietprop))
  }

}

##################################################################
#
read.clean.csv <- function(filename){
  return(read.csv(filename) %>% janitor::clean_names())
}

##################################################################

fc_T_eq2<-function(TT, cpars){
  CTM_CT0        <- cpars$C_TM - cpars$C_T0    
  CTMoverCTM_CT0 <- cpars$C_TM / CTM_CT0
  Z = log(cpars$C_Q) * CTM_CT0
  Y = log(cpars$C_Q) * (CTM_CT0 + 2.0)  			 
  X_C <- ( Z*Z * (1.0 + ( (1.0 + 40.0/Y) ** 0.5)) ** 2.0)/400.0 
  Vc       <- CTMoverCTM_CT0 - TT / CTM_CT0
  Fc_T     <- (Vc ^ X_C ) * exp(X_C * (1.0 - Vc))

  return(Fc_T)
}
  
  
##################################################################
# Load and name-clean REEM diet files.  This loads 
# region must be one of 'BS', 'AI', or 'GOA',.
REEM.loadclean.diets<- function(data_path = "data/local_reem_data"){
  
  tname <- "predprey"
  fname <- paste(data_path, paste("predprey.csv",sep=""), sep="/")
  cat("loading and cleaning", tname, "from", fname, "\n"); flush.console()
  b <- read.csv(file = fname)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(tname, value = b, envir = .GlobalEnv)
  
  tname <- "preylengths"
  fname <- paste(data_path, paste("preylengths.csv",sep=""), sep="/")
  cat("loading and cleaning", tname, "from", fname, "\n"); flush.console()
  b <- read.csv(file = fname)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(tname, value = b, envir = .GlobalEnv)    
  #return(list(PP_data=PP_data,PL_data=PL_data))
  
}

##########################################################################
REEM.loadclean.lookups<-function(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                                 stratum_bin_column    = "strat_groups",
                                 preynames_lookup_file = "lookups/Alaska_PreyLookup_MASTER.csv",
                                 prey_guild_column     = "ecopath_prey"){
  
  strata_lookup    <<- read.clean.csv(strata_lookup_file)    %>% mutate(stratum_bin = .data[[stratum_bin_column]])
  preynames_lookup <<- read.clean.csv(preynames_lookup_file) %>% mutate(prey_guild  = .data[[prey_guild_column]])
  
  #assign("strata_lookup",    value = strata_,    envir = .GlobalEnv)  
  #assign("preynames_lookup", value = read.clean.csv(preynames_lookup_file), envir = .GlobalEnv)    
}

##################################################################
# Load and name-clean REEM diet files.  This loads 
# region must be one of 'BS', 'AI', or 'GOA',.
REEM.loadclean.diets.old<- function(region=REGION, path="data/local_reem_data"){
  
  tname <- "PP_data"
  fname <- paste(path, paste(region, "_predprey.csv",sep=""), sep="/")
  cat("loading and cleaning", tname, "from", fname, "\n"); flush.console()
  b <- read.csv(file = fname)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  PP_data <- b #assign(tname, value = b, envir = .GlobalEnv)
  
  tname <- "PL_data"
  fname <- paste(path, paste(region, "_preylengths.csv",sep=""), sep="/")
  cat("loading and cleaning", tname, "from", fname, "\n"); flush.console()
  b <- read.csv(file = fname)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  PL_data <- b #assign(tname, value = b, envir = .GlobalEnv)    
  return(list(PP_data=PP_data,PL_data=PL_data))
}

##################################################################
##################################################################
#Load and name-clean racebase files
REEM.loadclean.RACE <- function(path="data/local_racebase"){

# Read in all csv files in given directory into tables, cleaning variable names
  a <- list.files(path, pattern = "\\.csv")
  for (i in 1:length(a)) {
    tname <- gsub(pattern = "\\.csv", replacement = "", x = a[i])
    fname <- paste(path, a[i], sep="/")
    cat("loading and cleaning", tname, "from", fname, "\n"); flush.console()
    b <- read.csv(file = fname)
    b <- janitor::clean_names(b)
    if (names(b)[1] %in% "x1") {
      b$x1 <- NULL
    }
    # KYA Note - assign is a global assigment (to environment)
    assign(tname, value = b, envir = .GlobalEnv)
  }
  
# Additions from GAP script. note <<- assignment to export variables to global environment.
  
    # Add area swept in km2 to hauls
    haul <- haul %>% 
      dplyr::mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width)) 
    haul <<- haul
    
    # Tidy cruise date format
    cruisedat <- cruise %>% 
      dplyr::select(survey_name, region, cruisejoin, agency_name) 
    cruisedat$start_date <- as.Date(cruise$start_date)
    cruisedat$year <- lubridate::year(cruisedat$start_date)
    cruisedat <<- cruisedat

}

