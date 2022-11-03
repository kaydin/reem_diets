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

  #lengthdat <- length %>%
  #  filter(region == REGION)%>%
  #  group_by (cruise, haul, species_code)%>%
  #  mutate(NumSamp_SpHaul=sum(frequency))%>%
  #  ungroup()%>%
  #  left_join(LWparam, by=c("species_code"="sp_code"))%>%
  #  mutate(PropLength_Num = frequency/NumSamp_SpHaul)%>%
  #  mutate (weight_Lbin=LW.intercept_a*length^LW.slope_b) #weight of fish (sp, haul) in given length bin
  #lengthdat <<- lengthdat

}


