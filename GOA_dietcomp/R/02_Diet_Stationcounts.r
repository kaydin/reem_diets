# Data files (bottom-trawl public format, from RACE)
  datdir   <- "../data/bts_data" #BF added "../"
  rawfiles <- c("goa1984_1987.csv", "goa1990_1999.csv",
                "goa2001_2005.csv",  "goa2007_2013_update.csv",
                "goa2015_updated.csv", "goa2017.csv",
	   		        "goa2019.csv", "goa2021.csv")
    
# Load by-station data  #BF added a GOA section here
  GOA_RAW  <- NULL		
  for (f in rawfiles){cat(f,"\n"); flush.console(); 
    GOA_RAW <- rbind(GOA_RAW,read.csv(paste(datdir,f,sep="/"),stringsAsFactors=F))
  }
  
  hauls <- unique(GOA_RAW[,c("LATITUDE","LONGITUDE","STATION","STRATUM","YEAR","DATETIME",
                                             "BOT_DEPTH","BOT_TEMP","SURF_TEMP","VESSEL","CRUISE","HAUL")])
  
  
  
 #BF EBS ## Load by-station data
  #BF EBS # EBS_RAW  <- NULL		
  #BF EBS # for (f in rawfiles){cat(f,"\n"); flush.console(); 
  #BF EBS #    EBS_RAW <- rbind(EBS_RAW,read.csv(paste(datdir,f,sep="/"),stringsAsFactors=F))
  #BF EBS #  }

  #BF EBS #hauls <- unique(EBS_RAW[,c("LATITUDE","LONGITUDE","STATION","STRATUM","YEAR","DATETIME",
  #BF EBS #                 "BOT_DEPTH","BOT_TEMP","SURF_TEMP","VESSEL","CRUISE","HAUL")])




