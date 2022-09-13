source("R/function_get_cpue_byLength.R")
source("R/function_get_cpue_NoLength.R")
source("R/function_get_biomass_stratum_Length.R")
source("R/function_get_biomass_stratum_NoLength.R")

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
  assign(tname, value = b, envir = .GlobalEnv)
  
  tname <- "PL_data"
  fname <- paste(path, paste(region, "_preylengths.csv",sep=""), sep="/")
  cat("loading and cleaning", tname, "from", fname, "\n"); flush.console()
  b <- read.csv(file = fname)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(tname, value = b, envir = .GlobalEnv)  

}

#' ##################################################################
#' #From 03_get_biomass_stratum_length
#' #
#' # get_biomass_stratum
#' 
#' #' Calculate index of total biomass per stratum
#' #'
#' #' @param racebase_tables a list of the cruisedat, haul, and catch tables from RACEBASE (all regions, all years)
#' #' @param speciescode five-digit numeric species code from the GAP species guides
#' #' @param survey_area a character code for the survey area, either "GOA" or "AI"
#' #' @param vulnerability the vulnerability of the species to the survey (defaults to 1)
#' #' @param strata a dataframe with the number and areas of the strata. Defaults to the strata table from RACEBASE.
#' #'
#' #' @return a dataframe containing the mean weighted cpue, 
#' #' @export
#' #'
#' #' @examples
#' #' x <- get_biomass_stratum(speciescode = 30060,survey_area = "GOA")
#' #' head(x)
#' get_biomass_stratum_Length <- function(racebase_tables = list(
#'   cruisedat = cruisedat,
#'   haul = haul,
#'   catch = catch,
#'   lengthdat = lengthdat #BF added length file
#'   ),
#'   speciescode = 21741, #speciescode_list$sp_code, #30060, # POP
#'   survey_area = "GOA",
#'   vulnerability = 1,
#'   strata # = switch(survey_area,  "GOA" = goa_strata,  "AI" = ai_strata)
#' ) {
#'   At <- sum(strata$area)
#'   
#'   # Get cpue table
#'   x <- get_cpue_Length(
#'     racebase_tables = racebase_tables,
#'     survey_area = survey_area,
#'     speciescode = speciescode
#'   )
#'   
#'   # Total CPUE for species, year, stratum
#'   # no RACEBASE equivalent (building block of BIOMASS_STRATUM)
#'   
#'   x2 <- x %>%
#'     #group_by(year, stratum, length) %>%  #includes year
#'     group_by(stratum, length) %>%  
#'     dplyr::summarize(
#'       haul_count = length(unique(stationid)), # number of total abundance hauls
#'       mean_wgtLbin_cpue = mean(WgtLBin_CPUE_km2), #Length step - average biomass (CPUE-/km2) per length bin across hauls
#'       var_wgtLbin_cpue = ifelse(haul_count <= 1, NA, var(WgtLBin_CPUE_km2) / haul_count),
#'       mean_numLbin_cpue =mean(NumLBin_CPUE_km2),
#'       var_numLbin_cpue = ifelse(haul_count <= 1, NA, var(NumLBin_CPUE_km2) / haul_count),
#'       catch_count = length(which(Catch_KG > 0)) # number of hauls with nonzero catch
#'     ) %>%
#'     dplyr::ungroup() %>%
#'     select(
#'       stratum,length,
#'       haul_count, catch_count,
#'       mean_wgtLbin_cpue, var_wgtLbin_cpue,
#'       mean_numLbin_cpue, var_numLbin_cpue #year
#'     )
#'   
#'   if (all(x2$catch_count <= x2$haul_count)) {
#'     print("Number of hauls with positive catches is realistic.")
#'   }
#'   
#'   # RACEBASE equivalent table: BIOMASS_STRATUM
#'   biomass_stratum.Lbin <- x2 %>%
#'     dplyr::left_join(strata) %>%
#'     rowwise() %>% # for applying ifelse() by row
#'     mutate(
#'       stratum_biomass.Lbin = area * mean_wgtLbin_cpue / vulnerability * 0.001, # kg --> mt
#'       stratum_ratio.Lbin = area / At,
#'       biomass_var.Lbin = area^2 * var_wgtLbin_cpue * 1e-6, # kg--> mt, square it because it's variance
#'       qt_size = ifelse(haul_count <= 1, 0,
#'                        qt(p = 0.025, df = haul_count - 1, lower.tail = F)
#'       ),
#'       min_biomass.Lbin = stratum_biomass.Lbin - qt_size * sqrt(biomass_var.Lbin),
#'       max_biomass.Lbin = stratum_biomass.Lbin + qt_size * sqrt(biomass_var.Lbin),
#'       stratum_pop.Lbin = area * mean_numLbin_cpue, # not sure why this calculation would be different from the stratum_biomass
#'       pop_var.Lbin = area^2 * var_numLbin_cpue,
#'       min_pop.Lbin = stratum_pop.Lbin - qt_size * sqrt(pop_var.Lbin),
#'       max_pop.Lbin = stratum_pop.Lbin + qt_size * sqrt(pop_var.Lbin)
#'     ) %>%
#'     mutate(
#'       min_biomass.Lbin = ifelse(min_biomass.Lbin < 0, 0, min_biomass.Lbin),
#'       min_pop.Lbin = ifelse(min_pop.Lbin < 0, 0, min_pop.Lbin)
#'     ) %>% # set low CI to zero if it's negative
#'     add_column(RACE=speciescode) %>%
#'     select(survey, RACE, stratum, stratum_ratio.Lbin, haul_count, catch_count, length, mean_wgtLbin_cpue, var_wgtLbin_cpue, mean_numLbin_cpue, var_numLbin_cpue, stratum_biomass.Lbin, biomass_var.Lbin, min_biomass.Lbin, max_biomass.Lbin, stratum_pop.Lbin, pop_var.Lbin, min_pop.Lbin, max_pop.Lbin, area) %>% #removed year
#'     mutate(
#'       Ni = area / 0.01,
#'       fi = (Ni * (Ni - haul_count)) / haul_count
#'     )
#'   
#'   return(biomass_stratum.Lbin)
#' }
#' 
#' 
#' ##################################################################
#' # From 02_getCPUE by length (modified for cod/crab 025 version)
#' #
#' get_cpue_Length <- function(racebase_tables = list(
#'   cruisedat = cruisedat,
#'   haul = haul,
#'   catch = catch,
#'   lengthdat = lengthdat 
#'   ),                             #BF added length file
#'   speciescode = 21740, #speciescode_list$sp_code, # 21741, #predators, # POP
#'   survey_area = "GOA")  {   
#' 
#'   # survey_area is called region in RACEBASE
#'   cruisedat <- racebase_tables$cruisedat
#'   haul <- racebase_tables$haul
#'   catch <- racebase_tables$catch
#'   lengthdat <- racebase_tables$lengthdat #BF added
#'   
#'   sp_catch <- catch %>%
#'     filter(species_code %in% speciescode)
#'   
#'   dat <- haul %>%
#'     left_join(cruisedat, by = c("cruisejoin", "region")) %>%
#'     filter(abundance_haul == "Y" & region == region) %>%
#'     left_join(sp_catch, by = "hauljoin") %>% 
#'     replace_na(list( weight = 0, number_fish = 0)) %>%
#'     left_join(lengthdat, by = c("hauljoin", "species_code")) %>% 
#'     
#'     dplyr::select(
#'       region,species_code,
#'       cruisejoin.x, vessel.x, haul.x, hauljoin,
#'       #haul_type, performance, duration,
#'       stratum, stationid,
#'       distance_fished, weight, year,
#'       frequency,sex,
#'       weight, number_fish, length, NumSamp_SpHaul, PropLength_Num, weight_Lbin,
#'       start_latitude, start_longitude,
#'       gear_temperature, surface_temperature,
#'       AreaSwept_km2
#'     ) %>%
#'     dplyr::rename(
#'       Lat = start_latitude,
#'       Lon = start_longitude,
#'       Catch_KG = weight,
#'       Vessel = vessel.x,
#'       fish_length = length,
#'       gear_temp = gear_temperature,
#'       Surface_temp = surface_temperature
#'     )
#'   # %>%
#'   # filter(year == survey_yr)
#'   #x <- dat
#'   x <- dat %>%
#'     mutate(
#'       WGTCPUE = Catch_KG / AreaSwept_km2,
#'       NUMCPUE = number_fish / AreaSwept_km2,
#'       survey = region
#'     ) %>%
#'     mutate (
#'       NumLBin_CPUE_km2 = PropLength_Num*NUMCPUE  # Length step: convert proportion of number fish in each length bin to numberCPUE in each length bin (#fish/km2)
#'     )%>%
#'     mutate (WgtLBin_CPUE_km2 = NumLBin_CPUE_km2*weight_Lbin) %>% # Length step: calc weight for all fish in Lbin using proportion of CPUE (number fish in Lbin *weight)
#'     #replace_na(list(species_code = speciescode)) %>% #THIS DOESN"T WORK
#'     #select(
#'     #  year, survey, Vessel, haul.x, hauljoin, stationid, performance,
#'     #  stratum, distance_fished,
#'     #  species_code, Catch_KG, number_fish,
#'     #  AreaSwept_km2, WGTCPUE, NUMCPUE, length, NumLBin_CPUE_km2, WgtLBin_CPUE_km2
#'     #) %>%
#'     arrange(year) %>%
#'     dplyr::rename(haul_num=year,
#'                   num_cpue=NUMCPUE,
#'                   haul_join=hauljoin
#'     )
#'   
#'   return(x)
#' }
