# 00_download_data_from_oracle

##############################
# Takes an open RODBC channel, an SQL query text, and a filename (including path),
# runs the query over the channel, and writes the results to the csv file. 
query.to.csv <- function(channel, query, file){
  a <- RODBC::sqlQuery(channel, query)
  cat("saving", nrow(a), "obs. of", ncol(a), "variables to", file, ".\n");  flush.console()
  write.csv(x=a, file, row.names = FALSE)
}


##############################
# Loops through simple name of tables (e.g. HAUL or CATCH) and queries them 
# specifically from RACEBASE. tables using query.to.csv function.
racebase.download <- function(channel, tables="HAUL", path="./data/local_racebase"){
  
  # Setup folders for local files -------------------------------------------
  if(!file.exists(path)) dir.create(path, recursive = TRUE)
  
  for (tab in tables){
    cat("Loading",tab,"... "); flush.console()
    query.to.csv(channel = channel,
                 query   = paste("SELECT * FROM RACEBASE.", tab , sep=""),
                 file    = paste(path,"/", tolower(tab), ".csv", sep=""))
  }
}  


################################
#
#
reem.download.bts.predprey <- function(channel, region="GOA", path="data/local_reem_data"){

  if(!file.exists(path)) dir.create(path, recursive = TRUE)
  cat("Loading", region, "predprey ... "); flush.console()
  
  query_text <- paste(
      "SELECT",
      "NPRED.nodc as PRED_NODC, NPRED.name as PRED_NAME,",
      "HH.REGION, HH.VESSEL, HH.CRUISE, HH.HAUL, HH.STRATUM, HH.YEAR, HH.MONTH, HH.DAY,",
      "HH.GEAR_TEMP, HH.GEAR_DEPTH, HH.BOTTOM_DEPTH, HH.SURFACE_TEMP, HH.START_HOUR,",
      "HH.HAULJOIN, HH.STATIONID, HH.CRUISE_TYPE,HH.RLAT, HH.RLONG,",
      "PP.PREDJOIN, PP.PRED_LEN,",
      "NPREY.nodc as PREY_NODC, NPREY.name as PREY_NAME,", #"NPREY.ECOPATH_Prey,", 
      "sum(PP.PREY_TWT) as TWT, sum(PP.PREY_CNT) as CNT", 
      "FROM",
      "foodlab.predprey PP, foodlab.haul HH, foodlab.nodc NPRED, foodlab.nodc NPREY",
      "WHERE",
      "(PP.VESSEL = HH.VESSEL AND PP.CRUISE = HH.CRUISE AND PP.HAUL = HH.HAUL)", 
      "AND (PP.PRED_NODC = NPRED.nodc AND PP.PREY_NODC = NPREY.nodc)", 
      "AND (HH.REGION=", paste("'", region ,"')",sep=''),
      "AND (HH.CRUISE_TYPE='Race_Groundfish')",
      #"AND (HH.YEAR <= 2015)",
      #"AND (HH.MONTH >= 5 AND HH.MONTH <= 8)",
      "GROUP BY",
      "PP.PREDJOIN, NPRED.nodc, NPRED.name, HH.REGION,",
      "HH.VESSEL, HH.CRUISE, HH.HAUL, HH.STRATUM, HH.YEAR, HH.MONTH, HH.DAY,",
      "HH.GEAR_TEMP, HH.GEAR_DEPTH, HH.BOTTOM_DEPTH, HH.SURFACE_TEMP,",
      "HH.START_HOUR, HH.HAULJOIN, HH.STATIONID, HH.CRUISE_TYPE, HH.RLAT,",
      "HH.RLONG, PP.PRED_LEN,",
      "NPREY.nodc, NPREY.name", #"NPREY.ECOPATH_Prey",
      "",
      sep=' ')
  
      query.to.csv(channel = channel,
               query   = query_text,
               file    = paste(path,"/", region, "_predprey.csv", sep=""))
}

################################
#
#
reem.download.bts.preylenths <-function(channel, region="GOA", path=("data/local_reem_data")){

  if(!file.exists(path)) dir.create(path, recursive = TRUE)  
  cat("Loading", region, "preylengths ... "); flush.console()
  
  query_text <- paste(
    "SELECT",
    "NPRED.nodc as PRED_NODC, NPRED.name as PRED_NAME,",
    "HH.REGION, HH.VESSEL, HH.CRUISE, HH.HAUL, HH.STRATUM, HH.YEAR, HH.MONTH, HH.DAY,",
    "HH.GEAR_TEMP, HH.GEAR_DEPTH, HH.BOTTOM_DEPTH, HH.SURFACE_TEMP, HH.START_HOUR,",
    "HH.HAULJOIN, HH.STATIONID, HH.CRUISE_TYPE,HH.RLAT, HH.RLONG,",
    "PP.PREDJOIN, PP.PRED_LEN,",
    "NPREY.nodc as PREY_NODC, NPREY.name as PREY_NAME,", #"NPREY.ECOPATH_Prey,", 
    "PP.PREY_SZ1 as Prey_Size_mm, COUNT(PP.PREY_NODC) AS Freq", 
    "FROM",
    "foodlab.preylen PP, foodlab.haul HH, nodc NPRED, nodc NPREY", 
    "WHERE", 
    #"(HH.REGION='BS' OR HH.REGION='AI' OR HH.REGION='GOA')", 
    "(PP.VESSEL=HH.VESSEL AND PP.CRUISE=HH.CRUISE AND PP.HAUL=HH.HAUL)", 
    "AND PP.PRED_NODC=NPRED.nodc AND PP.PREY_NODC = NPREY.nodc",
    "AND (HH.REGION=", paste("'", region ,"')",sep=''),
    "AND (HH.CRUISE_TYPE='Race_Groundfish')",
    "GROUP BY",
    "PP.PREDJOIN, NPRED.nodc, NPRED.name, HH.REGION,",
    "HH.VESSEL, HH.CRUISE, HH.HAUL, HH.STRATUM, HH.YEAR, HH.MONTH, HH.DAY,",
    "HH.GEAR_TEMP, HH.GEAR_DEPTH, HH.BOTTOM_DEPTH, HH.SURFACE_TEMP,",
    "HH.START_HOUR, HH.HAULJOIN, HH.STATIONID, HH.CRUISE_TYPE, HH.RLAT,",
    "HH.RLONG, PP.PRED_LEN,",
    "NPREY.nodc, NPREY.name,", #"NPREY.ECOPATH_Prey",
    "PP.PREY_SZ1",
    sep=' ')
  
    query.to.csv(channel = channel,
               query   = query_text,
               file    = paste(path,"/", region, "_preylengths.csv", sep=""))
  
}

###############################################################################
# Collects username and password (using secure getPass package) and returns
# an open oracle channel.  Need to be on VPN with appropriate ODBC drivers
# installed for it to work.
oracle.connect <- function(sql_schema){
  
  # Define RODBC connection to ORACLE
  get.connected <- function(schema = sql_schema, username, password) { #BF changed from AFSC to AFSCP1 - that's how it's set up on my computer
    (echo <- FALSE)
    if (!hasArg(username)) {username <- getPass(msg = "Enter your ORACLE Username: ")}
    if (!hasArg(password)) {password <- getPass(msg = "Enter your ORACLE Password: ")
    }
    channel <- RODBC::odbcConnect(paste(schema), paste(username), paste(password), believeNRows = FALSE)
  }
  
  # Execute the connection
  suppressWarnings(channel <- get.connected())
  while (channel == -1) {
    cat("Unable to connect. Username or password may be incorrect. Please re-enter.\n\n")
    suppressWarnings(channel <- get.connected())
  }
  if (class(channel) == "RODBC") {
    cat("\nSuccessfully connected to Oracle!\n\n")
  }
  
  return(channel)
}


##################################################################
# Wrapper to close an Oracle connection.
oracle.disconnect <- function(channel){
  chan <- odbcClose(channel)
  return(chan)
}


# OLD STUFF
# ##################DOWNLOAD TABLES##################################
# # RACEBASE ----------------------------------------------------------------
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.CATCH")
# write.csv(x=a, "./data/local_racebase/catch.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.HAUL")
# write.csv(x=a, "./data/local_racebase/haul.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.LENGTH")
# write.csv(x=a, "./data/local_racebase/length.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIMEN")
# write.csv(x=a, "./data/local_racebase/specimen.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.STRATUM")
# write.csv(x=a, "./data/local_racebase/stratum.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.STATIONS")
# write.csv(x=a, "./data/local_racebase/stations.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIES")
# write.csv(x=a, "./data/local_racebase/species.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.CRUISE")
# write.csv(x=a, "./data/local_racebase/cruise.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIES_CLASSIFICATION")
# write.csv(x=a, "./data/local_racebase/species_classification.csv", row.names = FALSE)
# 
# #BF added this line to get length data by haul to calculate biomass by species by length bin
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SIZECOMP_TOTAL  ")
# write.csv(x=a, "./data/local_racebase/ SIZECOMP_TOTAL.csv", row.names = FALSE)
# 
# 
# # RACE_DATA ---------------------------------------------------------------
# if(!file.exists("data/local_race_data")) dir.create("data/local_race_data", recursive = TRUE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.HAULS")
# write.csv(x=a, "./data/local_race_data/hauls.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.RACE_SPECIES_CODES")
# write.csv(x=a, "./data/local_race_data/race_species_codes.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.VESSELS")
# write.csv(x=a, "./data/local_race_data/vessels.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.TAXONOMIC_RANKS")
# write.csv(x=a, "./data/local_race_data/taxonomic_ranks.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.SPECIES_TAXONOMICS")
# write.csv(x=a, "./data/local_race_data/species_taxonomic.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACE_DATA.V_CRUISES")
# write.csv(x=a, "./data/local_race_data/cruises.csv", row.names = FALSE)
# 
# 
# 
# # ADFG --------------------------------------------------------------------
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.LENGTH_ADFG")
# write.csv(x=a, "./data/length_ADFG.csv", row.names = FALSE)
# 
# a<-RODBC::sqlQuery(channel, "SELECT * FROM RACEBASE.SPECIMEN_ADFG")
# write.csv(x=a, "./data/specimen_ADFG.csv", row.names = FALSE)
# 
# odbcClose(channel)
# cat("\nDisconnected from Oracle.\n\n")

