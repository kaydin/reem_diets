
# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library("tidyverse")
library("janitor") 
library("lubridate")

####################################################################
#outfiles   <- "apps/ESR_ESP_diets/ESR_Fall2024_EBSfix_"
outfiles    <- "apps/ESR_ESP_diets/allsystems_May2025_"
output.csv <- function(df,fname){write.csv(df,paste(outfiles,fname,".csv",sep=""),row.names=F)}

source("R/REEM_fooddata_functions.R")

# Load Race data and perform some preliminary cleanups/calculations
REEM.loadclean.RACE(path = "data/local_racebase")

# Load diet data and perform some preliminary cleanups/calculations
REEM.loadclean.diets(data_path = "data/local_reem_data")

# Load lookup tables and create summary lookups - re-run this line 
# if lookups change.
REEM.loadclean.lookups(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                       stratum_bin_column    = "strat_groups",
                       preynames_lookup_file = "lookups/Alaska_PreyLookup_MASTER.csv",
                       prey_guild_column     = "ecopath_prey_chion")

predlist <- read.clean.csv("lookups/Alaska_Predators_GOA.csv")

race_lookup <- read.clean.csv("lookups/goa_race_lookup_apr_04_2023.csv") %>% 
  mutate(race_group  = .data[["final_goa"]])

prey_guild_lookup <- read.clean.csv("apps/ESR_ESP_diets/dcat_EBSmerge.csv")


# To use a fixed proportion of body weight (biomass), set CA = desired daily proportion of body weight
# (or Ecopath annual QB/365) and CB=0, C_TM=100, C_T0=-100, C_Q=1.000000001
# If you're using this for diet proportions not total consumption, can simply
# use 1.0 for CA.

source("R/REEM_fooddata_functions.R")

pred_tot_combined <- NULL
for (this.model in c("EBS","NBS","AI","WGOA","EGOA")){
  pred_tot_combined <- rbind(pred_tot_combined, predprey_tables_all(model=this.model))
}

output.csv(pred_tot_combined,"pred_tot_combined")
#write.csv(pred_tot_combined,"pred_tot_combined_PEEC2024.csv",row.names=F)

######################
# Diets
diet_combined         <- NULL
diet_strat_combined   <- NULL
biomass_cons_combined <- NULL
test_bio_combined     <- NULL
juv_adu_combined      <- NULL

source("R/REEM_fooddata_functions.R")

bio_cons_domain_combined  <- NULL
diet_cons_domain_combined <- NULL

for (this.model in c("EBS","NBS")){
#for (this.model in c("WGOA","EBS","WGOA","NBS","AI")){ #for (this.model in c("EBS","NBS","AI","WGOA","EGOA")){
#this.model <- "WGOA"
  domain_summary <- haul_domain_summary(this.model)

  preds      <- predlist %>% filter(model==this.model)
  pred_names <- unique(preds$predator)
  pred_params=list()
  for (p in pred_names){
    pdat <- as.list(preds[preds$predator==p,])
    pred_params[[p]] <- pdat
    pred_params[[p]]$LCLASS <- sort(unique(c(0,pdat$juv_cm, pdat$adu_1, pdat$adu_2, pdat$adu_3,999)))
    pred_params[[p]]$jsize  <- paste("[",pred_params[[p]]$LCLASS[1],",",pred_params[[p]]$LCLASS[2],")",sep='')
    pred_params[[p]]$lw_b   <- pdat$b_l_mm_g
    pred_params[[p]]$lw_a   <- pdat$a_l_mm_g*(10^pdat$b_l_mm_g)
    pred_params[[p]]$lw_a_mm<- pdat$a_l_mm_g
    pred_params[[p]]$bioen  <- list(CA=pdat$ca, CB=pdat$cb, C_TM=pdat$c_tm, C_T0=pdat$c_t0, C_Q=pdat$c_q)
    pred_params[[p]]$vonb   <- list(linf_mm = pdat$vb_linf_mm, k=pdat$vb_k, t0=pdat$vb_t0, 
                                    winf_g = pdat$a_l_mm_g * pdat$vb_linf_mm ^ pdat$b_l_mm_g,
                                    h = 3 * pdat$vb_k * ((pdat$a_l_mm_g * pdat$vb_linf_mm ^ pdat$b_l_mm_g)^(1/3)) )
  }
  
  for (p in pred_names){ #p <- "Walleye_pollock"
  
  # Consumption at the haul level
    bio_cons_by_haul <- get_cpue_length_cons(predator=p, model=this.model)
  # Consumption summed to domain and lbin
    bio_cons_domain <- bio_cons_by_haul %>%
      group_by(year,model,stratum_bin,species_name,lbin) %>%
      summarize(sum_wlcpue_t_km2       = sum(WgtLBin_CPUE_kg_km2)/1000,
                sum_cons_bioen_t_km2   = sum(cons_kg_km2_bioen)/1000,
                sum_cons_vonb_t_km2    = sum(cons_vonb_kg_km2)/1000,
                .groups="keep") %>%
      left_join(domain_summary,by=c("model","stratum_bin","year")) %>%
      mutate(mean_wlcpue_t_km2  = sum_wlcpue_t_km2     / stations,
             mean_bioen_t_km2   = sum_cons_bioen_t_km2 / stations,
             mean_vonb_t_km2    = sum_cons_vonb_t_km2  / stations,
             tot_wlcpue_tons    = mean_wlcpue_t_km2 * area,
             tot_bioen_tons     = mean_bioen_t_km2  * area,
             tot_vonb_tons      = mean_vonb_t_km2 * area)
    
    min_sample <- 1
    diet <- predprey_tables(predator=p, model=this.model) %>%
      filter(pred_full >= min_sample)
    
    len_diet <- bio_cons_domain %>%
      left_join(diet, by=c("species_name"="predator", "model", "stratum_bin", "year", "lbin")) %>%
      replace_na(list(prey_guild="MISSING", dietprop_wt=1.0,dietprop_sci=1.0)) %>%
      mutate(preycons_sci_bioen_tons = dietprop_sci * tot_bioen_tons,
             preycons_sci_vonb_tons  = dietprop_sci * tot_vonb_tons) #%>%
      #left_join(prey_guild_lookup, by = c("prey_guild"="nodc_preycat"))

    if (nrow(bio_cons_domain)>0){
      bio_cons_domain_combined <- rbind(bio_cons_domain_combined, bio_cons_domain)
    }
    if (nrow(len_diet)>0){
      diet_cons_domain_combined <- rbind(diet_cons_domain_combined, len_diet)
    }    
  }
}  
  
    output.csv(bio_cons_domain_combined,"bio_cons_domain")
    output.csv(diet_cons_domain_combined,"diet_cons_domain")

######
# ESP cod calculations - EBS and NBS
    cod <- diet_cons_domain_combined %>% 
      filter(species_name=="Pacific_cod") %>%
      select(year:tot_sci, prey_guild, preycons_sci_bioen_tons) %>%
      pivot_wider(names_from=prey_guild, values_from=preycons_sci_bioen_tons, values_fill = 0)
    
    names(cod) <- make_clean_names(names(cod))
      
    codcrab <- cod %>%
        select(year:missing, "opilio",	"bairdi", "hybrid_chion", "misc_chion", "unid_chion") %>%
        mutate(opilio_extrap = ifelse(opilio+bairdi>0, opilio + unid_chion*opilio/(opilio+bairdi), 0),
               bairdi_extrap = ifelse(opilio+bairdi>0, bairdi + unid_chion*bairdi/(opilio+bairdi), 0),
               chion_unknown = ifelse(opilio+bairdi>0, 0, unid_chion),
               pred_n = ifelse(is.na(pred_n), 0, pred_n))
    
    codcrab_sum <- codcrab %>%
      group_by(model, year) %>%
      summarize(pred_n        = sum(pred_n), 
                opilio_extrap = sum(opilio_extrap),
                bairdi_extrap = sum(bairdi_extrap),
                chion_unknown = sum(chion_unknown),
                .groups="keep")

    write.csv(codcrab_sum,"EBS_NBS_codcrab_sum.csv",row.names=F)
    
    
    
#################################################################################        
  for (p in pred_names){
    
    juv_adu_lencons  <- get_stratum_length_cons(predator=p, model=this.model)
    strat_dietcons <- add_diets_to_strata_length_cons(juv_adu_lencons, predator=p, model=this.model, min_sample=5) %>%
      mutate( jcat = ifelse(lbin==pred_params[[p]]$jsize,"juv","adu"))

    domain_summary <- haul_domain_summary("WGOA")
    
    test_bio <- juv_adu_lencons %>%
      group_by(year,model,stratum_bin,species_name,hauljoin) %>%
      summarize(wt_tot = mean(tot_wtcpue_t_km2),
                wl_tot = sum(tot_wlcpue_t_km2),.groups="keep")
    
    # Get sums of consumption and biomass without distributing to prey type
    strat_bio_cons_sum <- juv_adu_lencons %>%
      group_by(year,model,stratum_bin,species_name,lbin) %>%
      summarize(strat_cons_bioen_t_km2    = mean(tot_cons_t_km2_bioen),
                strat_cons_vonb_t_km2     = mean(tot_cons_vonb_rel),
                strat_bio_t_km2           = mean(tot_wlcpue_t_km2),
                .groups="keep") %>%
      left_join(strat_areas,by=c("model","stratum_bin")) %>%
      mutate(cons_bioen_tons = strat_cons_bioen_t_km2 * area,
             cons_vonb_tons  = strat_cons_vonb_t_km2 * area,
             biomass_tons    = strat_bio_t_km2 * area)
    
    
    strat_dietprops <- strat_dietcons %>%
      group_by(year, model, species_name, jcat, prey_guild) %>%
      summarize(cons_tot_prey = sum(cons_rel_vonb), .groups="keep") %>%
      group_by(year, model, species_name, jcat) %>%
      mutate(cons_tot = sum(cons_tot_prey)) %>%
      ungroup() %>%
      mutate(diet_prop = cons_tot_prey/cons_tot) 
    
    strat_dietcons <- strat_dietcons %>%
      left_join(prey_guild_lookup, by = c("prey_guild"="nodc_preycat"))
    
    if (nrow(juv_adu_lencons)>0){
      juv_adu_combined <- rbind (juv_adu_combined,juv_adu_lencons)
    }
    if (nrow(test_bio)>0){
      test_bio_combined <- rbind(test_bio_combined, test_bio)
    }
    if(nrow(strat_bio_cons_sum)>0){
      biomass_cons_combined <- rbind(biomass_cons_combined,strat_bio_cons_sum)  
    }  
    if(nrow(strat_dietcons)>0){  
      diet_strat_combined <- rbind(diet_strat_combined,strat_dietcons)
    }
    if(nrow(strat_dietprops)>0){  
      diet_combined <- rbind(diet_combined,strat_dietprops)
    }
  } # pred_names loop
} # this.model loop


#guild_lookup <- read.clean.csv("apps/ESR_ESP_diets/dcat_EBSmerge.csv")

#diet_strat_guild <- diet_strat_combined %>%
#  left_join(guild_lookup, by = c("prey_guild"="nodc_preycat"))

#output.csv(diet_strat_guild,"diet_strat_guild")
output.csv(diet_strat_combined,"diet_strat_combined")
output.csv(diet_combined,"diet_combined")
output.csv(biomass_cons_combined,"biomass_cons")
output.csv(juv_adu_combined,"juv_adu_combined")
#write.csv(diet_strat_combined,"diet_strat_combined_ESR_2024.csv",row.names=F)
#write.csv(diet_combined,"diet_combined_ESR_2024.csv",row.names=F)


# testing vonB
par(mfrow=c(3,3))
body_wt <- seq(1,5000)
for (p in pred_names){
  print(pred_params[[p]]$vonb$h)
  plot(body_wt, pred_params[[p]]$vonb$h * (body_wt ^ (2.0/3.0)))
  title(p)
}
  


#output.csv(diet_strat_guild,"diet_strat_guild")

#################################################
### BERING SEA CRAB LENGTH PLOTS
pl_dat <- preylengths %>% 
  left_join(v_cruises%>%select(region,vessel_id,cruise,survey_name,survey_definition_id),
                 by=c("region","vessel"="vessel_id","cruise")) %>%
  rename(survey_id = survey_definition_id) %>%
  filter(survey_id==SURVEY_IDS["EBS"]) %>%
  filter(pred_nodc==8791030401 & prey_nodc %in% c("6187010300", "6187010301")  )

library(ggridges)
library(ggplot2)
plens <- do.call("c",mapply(rep, pl_dat$prey_size_mm, pl_dat$freq))
years <- do.call("c",mapply(rep, pl_dat$year,         pl_dat$freq))

plens <- c(plens,0,0)
years <- c(years,2004,2020)
len_dat <- data.frame(plens,years)


png("Crab_size.png",1600,2400)
ldat <- len_dat
ggplot(ldat, aes(x = plens, y = as.factor(years), fill="#5072B2")) +
  geom_density_ridges() +
  theme_ridges() + 
  xlab("Crab carapace width (mm)") +
  ylab("") +
  labs(title="Snow crab size in EBS Pacific cod stomachs")+
  xlim(0,80) +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5,size=48,face="bold"),
        plot.title = element_text(hjust = 0.5,size=64,face="bold"),
        axis.text=element_text(size=42),
        )
dev.off()

png("Crab_size_recent.png",1600,2400)
ldat <- len_dat[len_dat$years>2004,]
ggplot(ldat, aes(x = plens, y = as.factor(years), fill="#5072B2")) +
  geom_density_ridges() +
  theme_ridges() + 
  xlab("Crab carapace width (mm)") +
  ylab("") +
  labs(title="Snow crab size in EBS Pacific cod stomachs")+
  xlim(0,80) +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5,size=48,face="bold"),
        plot.title = element_text(hjust = 0.5,size=64,face="bold"),
        axis.text=element_text(size=42),
  )
dev.off()

##############################################################################

# bio_combined <-NULL
# 
# for (this.model in c("EGOA","WGOA")){
# 
# # Biomass pools (no size structure) biomass  
#   stratsum <- get_cpue_all(model=this.model) %>%
#     group_by(year, model, race_group, stratum) %>%
#     summarize(wgtcpue = sum(wgtcpue),
#               numcpue = sum(numcpue),.groups="keep") %>%
#     left_join(haul_stratum_summary(this.model),by=c("year","model","stratum")) %>%
#     mutate(bio_t_km2 = wgtcpue/1000/stations,
#            bio_tons  = bio_t_km2 * area)
#   
#   model_area <- sum(strata_lookup$area[strata_lookup$model==this.model])
#   
#   bio_totals <- stratsum %>%
#     group_by(year, model, race_group) %>%
#     summarize(bio_tons = sum(bio_tons),
#               bio_tkm2 = bio_tons/model_area, .groups="keep")
#   
# 
# # Split pools - loading juv/adu parameters 
#   preds      <- predlist %>% filter(model==this.model)
#   pred_names <- unique(preds$predator)
#   pred_params=list()
#   for (p in pred_names){
#     pdat <- as.list(preds[preds$predator==p,])
#     pred_params[[p]] <- pdat
#     pred_params[[p]]$LCLASS <- sort(unique(c(0,pdat$juv_cm, pdat$adu_1, pdat$adu_2, pdat$adu_3,999)))
#     pred_params[[p]]$jsize  <- paste("[",pred_params[[p]]$LCLASS[1],",",pred_params[[p]]$LCLASS[2],")",sep='')
#     pred_params[[p]]$lw_b   <- pdat$b_l_mm_g
#     pred_params[[p]]$lw_a   <- pdat$a_l_mm_g*(10^pdat$b_l_mm_g)  
#     pred_params[[p]]$bioen  <- list(CA=pdat$ca, CB=pdat$cb, C_TM=pdat$c_tm, C_T0=pdat$c_t0, C_Q=pdat$c_q)
#   }
# 
#   
# # Getting biomass-at-length and converting to split pool biomass  
#   juv_combined <- NULL
#   for (p in pred_names){
#     #p <- pred_names[1]
#     bio_pred <- bio_totals %>%
#       filter(race_group==p)
#     
#     juv_adu_lencons  <- get_stratum_length_cons(predator=p, model=this.model) %>%
#     group_by(year, model, species_name, stratum, lbin) %>%
#       summarize(strat_bio_sum = sum(tot_wlcpue_t_km2), .groups="keep") %>%
#       left_join(haul_stratum_summary(this.model),by=c("year","model","stratum")) %>%
#       mutate(bio_t_km2 = strat_bio_sum/stations,
#              bio_tons  = bio_t_km2 * area,
#              jcat      = ifelse(lbin==pred_params[[p]]$jsize,"juv","adu")) 
#     
#     juv_proportions <- juv_adu_lencons %>%
#       group_by(year,model,species_name,jcat) %>%
#       summarize(bio_tons = sum(bio_tons), .groups="keep") %>%
#       pivot_wider(names_from=jcat, values_from=bio_tons) %>%
#       mutate(juv_bio_prop = juv/(juv+adu))
#     
#     juv_combined <- rbind(juv_combined,juv_proportions)
#   } # end of pred_names loop
#   
#   bio_with_juvs <- bio_totals %>%
#     left_join(juv_combined,by=c("year","model","race_group"="species_name")) %>%
#     select(-c(juv,adu)) %>%
#     mutate(juv_bio_prop  = replace_na(juv_bio_prop,0.0),
#            adu_bio_tkm2  = (1.0-juv_bio_prop) * bio_tkm2,
#            juv_bio_tkm2  = juv_bio_prop       * bio_tkm2)
#   
#   
#     bio_combined <- rbind(bio_combined,bio_with_juvs)      
#   
#       
# }
# 
# write.csv(bio_combined,"goa_bio_combined_juvadu.csv",row.names=F)



## BELOW THIS IS OLD  ...?
##################################################################
# Sankey testing 2024
  sample <- diet_strat_combined %>%
     filter(species_name=="Pacific_cod")


##################################################################
##################################################################

model_area <- sum(strat_areas$area[strat_areas$model==this.model])

preds      <- predlist %>% filter(model==this.model)
pred_names <- unique(preds$predator)
pred_params=list()
for (p in pred_names){
  pdat <- as.list(preds[preds$predator==p,])
  pred_params[[p]] <- pdat
  pred_params[[p]]$LCLASS <- sort(unique(c(0,pdat$juv_cm, pdat$adu_1, pdat$adu_2, pdat$adu_3,999)))
  pred_params[[p]]$jsize  <- paste("[",pred_params[[p]]$LCLASS[1],",",pred_params[[p]]$LCLASS[2],")",sep='')
  pred_params[[p]]$lw_b   <- pdat$b_l_mm_g
  pred_params[[p]]$lw_a   <- pdat$a_l_mm_g*(10^pdat$b_l_mm_g)  
  pred_params[[p]]$bioen  <- list(CA=pdat$ca, CB=pdat$cb, C_TM=pdat$c_tm, C_T0=pdat$c_t0, C_Q=pdat$c_q)
}

combined_cons <- NULL
combined_bio  <- NULL
combined_sbio <- NULL
for (this.pred in pred_names){
  strat_lencons  <- get_stratum_length_cons(predator=this.pred, model=this.model)

  strat_dietcons <- add_diets_to_strata_length_cons(strat_lencons, predator=this.pred, model=this.model)
  combined_cons <- rbind(combined_cons,strat_dietcons)

  strat_bio <- strat_lencons %>%     
    group_by(year,model,stratum_bin,species_name,lbin) %>%
    summarize(strat_bio_t_km2 = mean(tot_wlcpue_t_km2), .groups="keep") %>%
    left_join(strat_areas,by=c("model","stratum_bin")) %>%
    mutate(bio_tons = strat_bio_t_km2 * area,
           jcat     = ifelse(lbin==pred_params[[this.pred]]$jsize,"juv","adu"))    
  
  combined_bio  <- rbind(combined_bio,strat_bio)    
  #test <- get_biomass_stratum(predator=this.pred,model=this.model)
  #combined_sbio <- rbind(combined_sbio, test)
}

bio_sum <- combined_bio %>%     
  group_by(year, model, species_name, jcat) %>%
  summarize(bio_tons = sum(bio_tons), .groups="keep") %>%
  mutate(model_area = model_area,
         bio_tkm2   = bio_tons/model_area)


test <- get_cpue_length_cons(predator=this.pred, model=this.model)



#}

write.csv(combined_cons,paste(this.model,"_stratcons.csv",sep=""),row.names=F)


prey_params <- list(
  "W.pollock"  = list(
                   nodc="8791030701", race="21740", LCLASS=c(0,10,25,999)
                 ),
  "Opilio"     = list(
                   nodc=c("6187010300", "6187010301"), LCLASS=c(0,30,95,999),
                   prey_a=0.000267, prey_b=3.097253
                 )
               )                        
                        
this.prey<-"W.Pollock"
this.pred<-"W.Pollock"

this.model <- "EGOA"
preylen_freqs <- preylength_splits(
  pred_params[[this.pred]]$nodc, 
  pred_params[[this.prey]]$nodc, 
  pred_params[[this.pred]]$LCLASS, 
  c(0,250,999),
  model="EBS")


preylen_freqs <- preylength_splits(
                          pred_params[[this.pred]]$nodc, 
                          c("6187010300", "6187010301"), # Opilio and Unid Chion
                          pred_params[[this.pred]]$LCLASS, 
                          c(0,30,95,999),
                          model="EBS") %>%
        mutate(prey_wt_g  =  prey_a * prey_size_mm^prey_b,
               prey_sum_g = freq * prey_wt_g) %>%
        group_by(model,year,pred_lbin_cm,prey_lbin_mm) %>%
        summarize(nprey=sum(freq),
                  wtprey=sum(prey_sum_g),
                  .groups="keep") %>%
        group_by(model,year,pred_lbin_cm) %>%
        mutate(tot_n=sum(nprey),
               tot_w=sum(wtprey)) %>%
        ungroup() %>%
        mutate(n_prop  = nprey/tot_n,
               wt_prop = wtprey/tot_w)

diet_prey <- diet_sum %>%
  filter(prey_guild=="Opilio") %>%
  left_join(preylen_freqs, by=c("model","year","lbin"="pred_lbin_cm")) %>%
  mutate(cons_prey_tons_day = cons_tons_day * wt_prop)

write.csv(diet_prey, "diet_prey.csv",row.names=F)

#test.new <- fc_T_eq2(seq(-1,30,0.2), bioen_pars[["P.cod.new"]])
#test.old <- fc_T_eq2(seq(-1,30,0.2), bioen_pars[["P.cod.old"]])

#end cody query
######################################
######################################

