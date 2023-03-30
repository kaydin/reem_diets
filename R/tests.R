
library("tidyverse")
setwd("lookups/lookup_merge")
t1 <- read.csv("Alaska_PreyLookup_MASTER_mar2023.csv")
t2 <- read.csv("nodc_REEM_mar2023.csv")

test <- t2 %>% full_join(t1, by=c("NODC"="NODC_CODE"),keep=T)

test[is.na(test$NODC),] # Checks for missing in oracle version
test[is.na(test$NODC_CODE),] # Checks for missing in local version


r0 <- read.csv("species_RACEmar2023.csv")
r_ebs <- read.csv("EBS_RACE_fit_lookup_11-2022.csv")
r_goa <- read.csv("GOA_RACE_fit_lookup_agg3.csv")

rtest1 <- r0 %>% full_join(r_ebs,by=c("SPECIES_CODE"="RACE"),keep=T)
rtest2 <- rtest1 %>% full_join(r_goa,by=c("SPECIES_CODE"="RACE"),keep=T)
test$RACE[test$RACE==0]=NA

test3 <- rtest2 %>% full_join(test,by=c("SPECIES_CODE"="RACE"),keep=T)

