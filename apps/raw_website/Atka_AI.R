library("tidyverse")
library("janitor") 
#library("lubridate")

# Tables were first using downloaded using the following sql queries to Oracle
# haul: "SELECT * from foodlab.haul"
# predprey: "SELECT * from foodlab.predprey"
# preylen "SELECT * from foodlab.preylen"
# nodc: "SELECT * from foodlab.nodc"

source("R/REEM_fooddata_functions.R")

REEM.loadclean.diets(data_path = "data/local_reem_data")

PP <- janitor::clean_names(read.csv("data/local_reem_data/foodlab_predprey.csv"),case="sentence",sep_out="_")
PL <- janitor::clean_names(read.csv("data/local_reem_data/foodlab_preylen.csv"),case="sentence",sep_out="_")
NN <- janitor::clean_names(read.csv("data/local_reem_data/foodlab_nodc.csv"),case="sentence",sep_out="_")
HH <- janitor::clean_names(read.csv("data/local_reem_data/foodlab_haul.csv"),case="sentence",sep_out="_")

BT <- PP %>%
  select(Hauljoin, Predjoin, Pred_nodc, 
  Prey_nodc, Prey_cnt, Prey_twt, Pred_stomwt,
  Pred_len, Pred_full, Pred_wt, Pred_lh,
  Pred_dig, Prey_lh, Pred_sex, Prey_parts) %>%
  left_join(NN %>% select(Nodc, Name), by=c("Pred_nodc"="Nodc")) %>%
  rename(Pred_name=Name) %>% relocate(Pred_name, .after=Pred_nodc) %>%
  left_join(NN %>% select(Nodc, Name), by=c("Prey_nodc"="Nodc")) %>%
  rename(Prey_name=Name) %>% relocate(Prey_name, .after=Prey_nodc) %>%
  left_join(HH %>% select(Hauljoin, Vessel, Cruise, Haul, Cruise_type,
                          Rlat, Rlong, Year, Month, Day,
                          Region, Gear_depth, Bottom_depth,
                          Start_hour, Surface_temp, Gear_temp,
                          Inpfc_area, Stationid, Start_date, Stratum),
                          by="Hauljoin") %>%
  rename(INPFC_area=Inpfc_area)


## AI Atka data for Jane Sullivan
AI_Atka <- BT %>%
  filter(Region=="AI" & Pred_nodc==8827010501 & Cruise_type=="Race_Groundfish")
write.csv(AI_Atka,"AI_Atka_2025_07_24.csv",row.names=F)

## BS Pollock, cod, atf, halibut for Maurice
codes <- c(8791030701, 8791030401, 8857040102, 8857041901)
maurice <- BT %>%
  filter(Region=="BS" & Pred_nodc %in% codes & Cruise_type=="Race_Groundfish")
write.csv(maurice, "BS_preds_forMaurice_2025_07_25.csv", row.names=F)

## Eelpouts
eels <- BT %>%
  filter(Region=="BS" & Pred_nodc >= 8793000000 & Pred_nodc <= 8793010801 & 
           Cruise_type=="Race_Groundfish") 

eelsum <- eels %>% 
  mutate(chion=ifelse(Prey_nodc>=6187010300 & Prey_nodc<=6187010302,1,0),
         chion_len=Pred_len * chion, chion_wt=Prey_twt * chion) %>%
  group_by(Pred_name) %>%
  summarize(n_samples=n(), chion_n=sum(chion), chion_FreqOccur=chion_n/n_samples,
            totpreywt_g=sum(Prey_twt), totchionwt_g=sum(chion_wt), propdiet_bywt=totchionwt_g/totpreywt_g,
            minlen_cm=min(Pred_len),maxlen_cm=max(Pred_len),
            min_chioneater_cm=min(ifelse(chion,chion_len,9999)), max_chioneater_cm=max(chion_len), 
            .groups="keep") %>%
  filter(n_samples>10)

write.csv(eelsum,"eelpouts_eating_chion.csv",row.names=F)

## Greenland turbot prey

turbot <- BT %>%
  filter(Cruise_type=="Race_Groundfish" & Prey_nodc==8857041801)

# Maurice found a bug
test <- PP %>%
  filter(Hauljoin==11111135)

test2 <- PL %>%
  filter(Hauljoin==11111135)

write.csv(test,"DupPPRecord2013.csv",row.names=F)
write.csv(test2,"DupPLRecord2013.csv",row.names=F)

write.csv(maurice, "BS_preds_forMaurice_2025_07_25.csv", row.names=F)
