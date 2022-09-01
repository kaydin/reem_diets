#Calculate area of WGOA super strata (region and depth category) for biomass and diet calculations
#area (km2) and labels from bottom trawl survey strata data
#EGOA = Yakutat and Southeastern  (Area 640 and 650)
#WGOA = Chirikof, Kodiak, Shumagin areas (630, 620, part of 610)
#depth categories: shelf (50-200m) gully (100-200+) slope (200-1000m)

library(tidyverse)

stratafile <-"../lookups/GOA_strata_area2021.csv"

#strata & aggregate into region
strat_table <- read.csv(stratafile)

head(strat_table)
# Group strata as desired

#Full GOA
GOA=strat_table  %>%
    group_by(INPFC_area) %>%
    summarise(sum.km2 = sum(Area_km2), n = n())

Area.GOA.km2=sum(GOA$sum.km2)

#WGOA (remove Southeastern and Yakutat regions)
WGOA=strat_table  %>%
    group_by(INPFC_area) %>%
    filter(INPFC_area!="Southeastern" & INPFC_area!="Yakutat") %>%
    summarise(sum.km2 = sum(Area_km2), n = n())

Area.WGOA.km2=sum(WGOA$sum.km2)

#EGOA (only Southeastern and Yakutat regions )
Area.EGOA.km2=Area.GOA.km2-Area.WGOA.km2

#GOA by EwE regions (INPFC region & Depth (shelf/gully/slope); 6 total in WGOA); km2 and percent 
GOAareaEwE=strat_table  %>%
    group_by(INPFC_area,Depth) %>%
    summarise(sum.km2 = sum(Area_km2), n = n()) %>%
    mutate(percent_total=sum.km2/Area.GOA.km2*100) 



