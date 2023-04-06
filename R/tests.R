
library("tidyverse")
setwd("lookups/lookup_merge")
t1 <- read.csv("Alaska_PreyLookup_MASTER_mar2023.csv")
t2 <- read.csv("nodc_REEM_mar2023.csv")
t3 <- read.csv("ModelSpeciesList_GAP_Biomass.csv") 

t2$RACE[t2$RACE==0]<-NA
t3$RACE[t3$RACE==0]<-NA
# Result 4/4/23 t3 incomplete - t2 has everything that t3 has so t3 unneeded
# > which(!(t3$RACE %in% t2$RACE))
# integer(0)

nodc_combo <- t2 %>% full_join(t1, by=c("NODC"="NODC_CODE"),keep=T)
nodc_combo[is.na(nodc_combo$NODC),] # Checks for missing in oracle version
nodc_combo[is.na(nodc_combo$NODC_CODE),] # Checks for missing in local version
# Result 4/4/23 is the following were in the Oracle but not local master:
# NODC RACE NORPAC                                 NAME 
# 1262 5708010198   NA     NA Benthoctopus sibiricus (Octopodidae)     
# 1263 6183060299   NA     NA      Pagurus setosus (setose hermit)     

write.csv(nodc_combo,"nodc_combo.csv",row.names=F)

r0 <- read.csv("species_RACEmar2023.csv")
r_ebs <- read.csv("EBS_RACE_fit_lookup_11-2022.csv")
r_goa <- read.csv("GOA_RACE_fit_lookup_agg3.csv")
r_ai  <- read.csv("AI_RACE_fit_lookup_agg.csv")

# r_ebs and r_goa and r_ai are subsets based on the complete list in the
# for-guild provided-by-RACE biomass lists for each system.

r_ebs[which(!(r_ebs$RACE %in% r0$SPECIES_CODE)),]
# Codes in the r_ebs that are no longer in the r0 list
# RACE   SCIENTIFIC                 COMMON ECOPATH_Num ECOPATH_Name EXPLORE_Name
# 718 78001      (blank)    cuttlefish unident.          77      octopus      Octopi 
# 851 90000 Foraminifera foraminiferan unident.         106      sponges      Sponges

r_goa[which(!(r_goa$RACE %in% r0$SPECIES_CODE)),]
# RACE      SCIENTIFIC         COMMON ECOPATH_Num ECOPATH_Name
# 1261 91045 Coelosphaeridae ginseng sponge         101      Sponges
# 1344 99986     Tetilla sp.        (blank)         101      Sponges

r_ai[which(!(r_ai$RACE %in% r0$SPECIES_CODE)),]
# <0 rows> (or 0-length row.names)

colnames(r_ebs) <- paste(colnames(r_ebs),"ebs",sep=".")
colnames(r_goa) <- paste(colnames(r_goa),"goa",sep=".")
colnames(r_ai) <- paste(colnames(r_ai),"ai",sep=".")

rtest1 <- r0     %>% full_join(r_ebs,by=c("SPECIES_CODE"="RACE.ebs"),keep=T)
rtest1$SPECIES_CODE <- ifelse(!is.na(rtest1$SPECIES_CODE),rtest1$SPECIES_CODE,rtest1$RACE.ebs)

rtest2 <- rtest1 %>% full_join(r_goa,by=c("SPECIES_CODE"="RACE.goa"),keep=T)
rtest2$SPECIES_CODE <- ifelse(!is.na(rtest2$SPECIES_CODE),rtest2$SPECIES_CODE,rtest2$RACE.goa)

rtest3 <- rtest2 %>% full_join(r_ai, by=c("SPECIES_CODE"="RACE.ai" ),keep=T)
rtest3$SPECIES_CODE <- ifelse(!is.na(rtest3$SPECIES_CODE),rtest3$SPECIES_CODE,rtest3$RACE.ai)

taxons <- read.csv("species_classification_RACEmar2023.csv") %>%
  select(SPECIES_CODE,PHYLUM_TAXON,CLASS_TAXON,ORDER_TAXON,FAMILY_TAXON,GENUS_TAXON,SPECIES_TAXON)

race_combo <- left_join(rtest3,taxons,by="SPECIES_CODE")

write.csv(race_combo,"race_combo.csv",row.names=F)

race_nodc <- race_combo %>% full_join(nodc_combo,by=c("SPECIES_CODE"="RACE"),keep=T)
write.csv(race_nodc,"race_nodc_raw.csv",row.names=F)


all_wgoa <- get_cpue_all(model="WGOA") %>% group_by(species_code) %>% summarize(wgoa.wt=sum(wgtcpue))
all_egoa <- get_cpue_all(model="EGOA") %>% group_by(species_code) %>% summarize(egoa.wt=sum(wgtcpue))

goa.look <- race_nodc %>% 
  select(SPECIES_CODE, SPECIES_NAME, COMMON_NAME, ECOPATH_Name.goa, EGOA, 
         PHYLUM_TAXON, CLASS_TAXON, ORDER_TAXON, FAMILY_TAXON, GENUS_TAXON, SPECIES_TAXON) %>% 
  left_join(all_wgoa,by=c("SPECIES_CODE"="species_code")) %>%
  left_join(all_egoa,by=c("SPECIES_CODE"="species_code")) %>%
  filter(!is.na(SPECIES_CODE))

goa.look$wgoa.wt[is.na(goa.look$wgoa.wt)] <-0
goa.look$egoa.wt[is.na(goa.look$egoa.wt)] <-0
write.csv(goa.look,"goa_bio_lookup.csv",row.names=F)

write.csv(race_nodc,"race_nodc_raw.csv",row.names=F)

