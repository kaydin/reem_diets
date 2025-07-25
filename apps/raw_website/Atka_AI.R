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

AI_Atka <- BT %>%
  filter(Region=="AI" & Pred_nodc==8827010501 & Cruise_type=="Race_Groundfish")
write.csv(AI_Atka,"AI_Atka_2025_07_24.csv",row.names=F)

codes <- c(8791030701, 8791030401, 8857040102, 8857041901)
maurice <- BT %>%
  filter(Region=="BS" & Pred_nodc %in% codes & Cruise_type=="Race_Groundfish")



