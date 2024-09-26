library("RODBC")
library("getPass")
source("R/REEM_download_functions.R")

# code assumes everything runs from main repo directory
# (subdirectories should include /data and /R").

# oracle.connect input argument is the connection name in installed odbc driver,
# may differ by user (e.g. "AFSC" or "AFSCP1" etc.).  This function should bring
# up a username and password interface to log in to Oracle.
channel <- oracle.connect("AFSC")  

# Download clean version of food habits data (filters so race surveys only)
#reem.download.rawfood(channel, path="data/local_reem_data")
reem.download.bts.predprey(channel, path="data/local_reem_data")
reem.download.bts.preylengths(channel, path="data/local_reem_data")

oracle.disconnect(channel)


# Here, we download tables from RACEBASE (Oracle tables with names prefixed by RACEBASE
# like RACEBACE.HAUL) and save them as .csvs in the given path directory.  This does *not*
# put the data in the workspace.  The list of tables can be changed (e.g. to refresh a
# single table only).  These queries select all table fields (no data manipulations).

library("RODBC")
library("getPass")
source("R/REEM_download_functions.R")

channel <- oracle.connect("AFSC")  

racebase.tables   <- c("CATCH", "HAUL", "LENGTH", "SPECIMEN", "STRATUM", "STATIONS",
                       "SPECIES", "CRUISE", "SPECIES_CLASSIFICATION") #"SIZECOMP_TOTAL"
racebase.download(channel, tables=racebase.tables, path="data/local_racebase")

race_data.tables <- c("V_CRUISES")
race_data.download(channel, tables=race_data.tables, path="data/local_racebase")

oracle.disconnect(channel)
