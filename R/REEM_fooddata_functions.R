source("R/function_get_cpue_byLength.R")
source("R/function_get_cpue_NoLength.R")
source("R/function_get_biomass_stratum_Length.R")
source("R/function_get_biomass_stratum_NoLength.R")
#############################################################

predprey_tables <- function(predator="P.cod", model="EBS", ppdat=food[["BS"]], months=5:8){

  preylookup   <- raw_preynames %>% mutate(prey_guild  = .data[[preylook_col]])
  stratbins    <- raw_strata    %>% mutate(stratum_bin = .data[[stratbin_col]])
  yearblock    <- raw_years
  ppar <- pred_params[[predator]]
  
  pred_tab <- ppdat$PP_data %>%
    # Add lookup tables
    left_join(preylookup, by=c("prey_nodc"="nodc_code")) %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
    left_join(yearblock, by=c("year"="year")) %>%
    mutate(predator = predator) %>% relocate(predator) %>%
    mutate(model = model, .after=predator) %>%
    # First filter out all predators except the main PRED
    filter(submodel %in% model)   %>%
    filter(pred_nodc %in% ppar$nodc)  %>%
    filter(!is.na(year_group)) %>%
    filter(month %in% months)   %>%
    # Then add predator_specific data and make sure it's located before the prey_guild column
    #mutate(full = twt>0) %>% 
    mutate(lbin = as.character(cut(pred_len, ppar$LCLASS, right=F))) %>%
    mutate(bodywt = ppar$A_L * pred_len^ppar$B_L) %>%
    relocate(any_of(c("lbin","bodywt")), .before=prey_nodc) %>%
    # Make crosstab query grouping by all columns up to prey_nodc EXCEPT prey_nodc, then stratum and prey bins 
    group_by(across(c(1:prey_nodc,-prey_nodc)), stratum_bin, year_group, prey_guild) %>%
    summarize(prey_wt=sum(twt), .groups="keep")

  cat(nrow(pred_tab),"predprey records found, summarizing...\n"); flush.console()

  # This creates one line per predator with stomach weight totals
  pred_tots <- pred_tab %>%
    group_by(across(c(1:prey_guild,-prey_guild))) %>%
    summarize(tot_wt=sum(prey_wt), tot_sci=sum(prey_wt/bodywt), full=(prey_wt>0), prey_nguilds=n(), .groups="keep") %>%
    unique()

  # For the stratum, year and length get totals
  strat_tots <- pred_tots %>% 
    ungroup() %>%
    select(predator,model,stratum_bin,year_group,lbin,full,tot_wt,tot_sci) %>%
    group_by(predator,model,stratum_bin,year_group,lbin) %>%
    summarize(pred_n=n(), pred_full=sum(full), tot_wt=sum(tot_wt), tot_sci=sum(tot_sci), .groups="keep")

  # Sum 
  strat_dietprop <- pred_tab %>%
    ungroup() %>%  
    mutate(prey_sci = prey_wt/bodywt) %>%
    select(predator,model,stratum_bin,year_group,lbin,prey_guild,prey_wt,prey_sci) %>%  
    filter(prey_wt>0) %>%
    group_by(predator,model,stratum_bin,year_group,lbin, prey_guild) %>%
    summarize(prey_n=n(), prey_wt=sum(prey_wt), prey_sci=sum(prey_sci), .groups="keep") %>%
    ungroup() %>%
    left_join(strat_tots, by=c("predator"="predator","model"="model","stratum_bin"="stratum_bin", "year_group"="year_group", "lbin"="lbin")) %>%
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
    
  return(list(predprey_table = data.frame(pred_tab),
              pred_totals    = data.frame(pred_tots), 
              strat_dietprop = data.frame(strat_dietprop)))

}

#############################################################
# Minor date function
tidy.cruise.dates <-function(cruise){
  cdat <- cruise %>% dplyr::select(survey_name, region, cruisejoin, agency_name) 
  cdat$start_date <- as.Date(cruise$start_date)
  cdat$year <- lubridate::year(cdat$start_date)
  return(cdat)
}

##################################################################
#Load and name-clean racebase files
REEM.loadclean.RACE <- function(path="data/local_racebase"){

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
}

##################################################################
#
read.clean.csv <- function(filename){
  return(read.csv(filename) %>% janitor::clean_names())
}

##################################################################
# Load and name-clean REEM diet files.  This loads 
# region must be one of 'BS', 'AI', or 'GOA',.
REEM.loadclean.diets<- function(region=REGION, path="data/local_reem_data"){
  
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