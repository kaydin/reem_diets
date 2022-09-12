# 00_download_data_from_oracle

# Setup folders for local files -------------------------------------------
if(!file.exists("data/local_reem_data")) dir.create("data/local_reem_data", recursive = TRUE)

##################DOWNLOAD TABLES##################################
# GOA ---------------------------------------------------------------------

a<-RODBC::sqlQuery(oracle.channel, "SELECT * FROM GOA.GOA_STRATA")
write.csv(x=a, "./data/goa_strata.csv", row.names = FALSE)


# NODC (Food Habits) ------------------------------------------------------
a<-RODBC::sqlQuery(oracle.channel, "SELECT * FROM FOODLAB.NODC")
write.csv(x=a, "./data/local_reem_data/nodc.csv", row.names = FALSE)

regions <- c("AI","BS","GOA")

RAWDAT  <- list()

for (RR in regions){
  qtest <- paste(
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
    "AND (HH.REGION=", paste("'", RR ,"')",sep=''),
    "AND (HH.CRUISE_TYPE='Race_Groundfish')",
    #"AND (HH.YEAR <= 2015)",
    "AND (HH.MONTH >= 5 AND HH.MONTH <= 8)",
    "GROUP BY",
    "PP.PREDJOIN, NPRED.nodc, NPRED.name, HH.REGION,",
    "HH.VESSEL, HH.CRUISE, HH.HAUL, HH.STRATUM, HH.YEAR, HH.MONTH, HH.DAY,",
    "HH.GEAR_TEMP, HH.GEAR_DEPTH, HH.BOTTOM_DEPTH, HH.SURFACE_TEMP,",
    "HH.START_HOUR, HH.HAULJOIN, HH.STATIONID, HH.CRUISE_TYPE, HH.RLAT,",
    "HH.RLONG, PP.PRED_LEN,",
    "NPREY.nodc, NPREY.name", #"NPREY.ECOPATH_Prey",
    "",
    sep=' ')
  
  RAWDAT[[RR]] <- RODBC::sqlQuery(oracle.channel, qtest)
  #write.csv(x=RAWDAT[[RR]], file=gzfile(paste(RR,"_predprey.csv.gz",sep='')), row.names=F)
  write.csv(x=RAWDAT[[RR]], file=paste("./data/local_reem_data/",RR,"_predprey.csv",sep=''), row.names=F)
}

qtest <- paste(
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
  "(HH.REGION='BS' OR HH.REGION='AI' OR HH.REGION='GOA')", 
  "AND (PP.VESSEL=HH.VESSEL AND PP.CRUISE=HH.CRUISE AND PP.HAUL=HH.HAUL)", 
  "AND PP.PRED_NODC=NPRED.nodc AND PP.PREY_NODC = NPREY.nodc", 
  "GROUP BY",
  "PP.PREDJOIN, NPRED.nodc, NPRED.name, HH.REGION,",
  "HH.VESSEL, HH.CRUISE, HH.HAUL, HH.STRATUM, HH.YEAR, HH.MONTH, HH.DAY,",
  "HH.GEAR_TEMP, HH.GEAR_DEPTH, HH.BOTTOM_DEPTH, HH.SURFACE_TEMP,",
  "HH.START_HOUR, HH.HAULJOIN, HH.STATIONID, HH.CRUISE_TYPE, HH.RLAT,",
  "HH.RLONG, PP.PRED_LEN,",
  "NPREY.nodc, NPREY.name,", #"NPREY.ECOPATH_Prey",
  "PP.PREY_SZ1",
  sep=' ')

PLdat <- RODBC::sqlQuery(oracle.channel, qtest)
write.csv(x=PLdat, file="./data/local_reem_data/predprey_lengths.csv", row.names=F)  



