# 00_download_data_from_oracle

##############################
# Takes an open RODBC channel, an SQL query text, and a filename (including path),
# runs the query over the channel, and writes the results to the csv file. 
query.to.csv <- function(channel, query, file){
  a <- RODBC::sqlQuery(channel, query)
  cat(tab, "Saving", nrow(a), "obs. of", ncol(a), "variables to", fout, ".\n");  flush.console()
  write.csv(x=a, fout, row.names = FALSE)
}


##############################
racebase.download <- function(channel, tables="HAUL", path="./data/local_racebase"){
  
  # Setup folders for local files -------------------------------------------
  if(!file.exists(path)) dir.create(path, recursive = TRUE)

  for (tab in tables){
    query.to.csv(channel = channel,
                 query   = paste("SELECT * FROM RACEBASE.", tab , sep=""),
                 file    = paste(path,"/", tolower(tab), ".csv", sep=""))
  }
}  

# GOA ---------------------------------------------------------------------


################################
reem.download.bts.predprey <- function(channel, region="GOA", path="./data/local_reem"){
  
  query_text <-
    
  
}


a<-RODBC::sqlQuery(channel, "SELECT * FROM GOA.GOA_STRATA")
write.csv(x=a, "./data/goa_strata.csv", row.names = FALSE)


# NODC (Food Habits) ------------------------------------------------------
a<-RODBC::sqlQuery(channel, "SELECT * FROM FOODLAB.NODC")
write.csv(x=a, "./data/local_reem_data/nodc.csv", row.names = FALSE)


###############################################################################
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

