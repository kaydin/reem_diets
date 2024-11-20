
# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library(tidyverse)
library(janitor) 
library(lubridate)
library(ggridges)
library(ggplot2)
library(gridExtra)

####################################################################
outfiles    <- "apps/ESR_ESP_diets/george_"
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
                       prey_guild_column     = "ecopath_prey")

predlist <- read.clean.csv("lookups/Alaska_Predators_GOA.csv")

race_lookup <- read.clean.csv("lookups/goa_race_lookup_apr_04_2023.csv") %>% 
  mutate(race_group  = .data[["final_goa"]])

prey_guild_lookup <- read.clean.csv("apps/ESR_ESP_diets/dcat_EBSmerge.csv")


# To use a fixed proportion of body weight (biomass), set CA = desired daily proportion of body weight
# (or Ecopath annual QB/365) and CB=0, C_TM=100, C_T0=-100, C_Q=1.000000001
# If you're using this for diet proportions not total consumption, can simply
# use 1.0 for CA.

source("R/REEM_fooddata_functions.R")

# Diets
diet_combined         <- NULL
diet_strat_combined   <- NULL
biomass_cons_combined <- NULL
test_bio_combined     <- NULL
juv_adu_combined      <- NULL


bio_cons_domain_combined  <- NULL
diet_cons_domain_combined <- NULL
# NOTE MIN_SAMPLE for George
min_sample <- 5

#for (this.model in c("EBS")){ #for (this.model in c("EBS","NBS","AI","WGOA","EGOA")){
  this.model <- "EBS"
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
  
  p <- "Walleye_pollock"
  pred_params[[p]]$LCLASS <- c(0,12,25,40,999,9999)  
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
    
    diet <- predprey_tables(predator=p, model=this.model) %>%
      filter(pred_full >= min_sample)
    
    len_diet <- bio_cons_domain %>%
      left_join(diet, by=c("species_name"="predator", "model", "stratum_bin", "year", "lbin")) %>%
      replace_na(list(prey_guild="MISSING", dietprop_wt=1.0,dietprop_sci=1.0)) %>%
      mutate(preycons_sci_bioen_tons = dietprop_sci * tot_bioen_tons,
             preycons_sci_vonb_tons  = dietprop_sci * tot_vonb_tons) %>%
      left_join(prey_guild_lookup, by = c("prey_guild"="nodc_preycat"))
    
    if (nrow(bio_cons_domain)>0){
      bio_cons_domain_combined <- rbind(bio_cons_domain_combined, bio_cons_domain)
    }
    if (nrow(len_diet)>0){
      diet_cons_domain_combined <- rbind(diet_cons_domain_combined, len_diet)
    }    

#output.csv(bio_cons_domain_combined,"bio_cons_domain")
#output.csv(diet_cons_domain_combined,"diet_cons_domain")

dat <- diet_cons_domain_combined %>%
  filter(year>=1987 & year<=2019) %>%
  filter(!is.na(walleye_pollock_ebs)) %>%
  filter(prey_guild != "MISSING") %>%
  filter(!(stratum_bin %in% c("NW_inner","SE_inner","NW_corner"))) %>%
  mutate(prey_type = 
    ifelse(walleye_pollock_ebs=="Shrimp","Other plankton",walleye_pollock_ebs)) 

totcons <- dat %>%
  group_by(year,model,species_name,lbin) %>%
  summarize(tot_bioen = sum(preycons_sci_bioen_tons),
            tot_vonb  = sum(preycons_sci_vonb_tons), 
            .groups="keep")

pcons <- dat %>%
  group_by(year,model,species_name,lbin,prey_type) %>%
  summarize(p_tot_bioen = sum(preycons_sci_bioen_tons),
            p_tot_vonb  = sum(preycons_sci_vonb_tons), 
            .groups="keep") %>% 
  left_join(totcons,by=c("year","model","species_name","lbin")) %>%
  mutate(bioen_prop = p_tot_bioen/tot_bioen,
         vonb_prop  = p_tot_vonb/tot_vonb)
  
samp_size <- dat %>%
  group_by(year,stratum_bin,lbin) %>%
  summarize(pred_n = mean(pred_n),.groups="keep") %>%
  group_by(year,lbin) %>%
  summarize(tot_n=sum(pred_n),.groups="keep")%>%
  filter(lbin!="[0,12)") %>%
  spread(lbin,tot_n,fill=0)

write.csv(samp_size,"geoge_fig7_samples.csv",row.names=F)


lsizes <- c("[12,25)", "[25,40)", "[40,999)")
lnames <- c("Pollock fork length 12-25 cm","Pollock fork length 25-40 cm","Pollock fork length 40+ cm")

#X11(width=8,height=3)
#par(mfrow=c(3,1))

# The loop didn't work for some wierd reason, doing i=1,2,3 by hand
i<-1# for(i in 1:3){
   png(paste("pdiet",lsizes[i],"george.png",sep="_"),width=8,height=3,units="in",res=600,type="cairo-png")

  pdat <- pcons %>%
    filter(lbin==lsizes[i])
  
ord <- c("Copepods", "Euphausiids", "Amphipods", "Other plankton", 
         "Other fish", "Benthos", "Walleye pollock")
ggplot(pdat, aes(x=as.factor(year), y=bioen_prop,group=factor(prey_type,levels=ord))) +
  geom_line(aes(color=factor(prey_type,levels=ord)), size=1.5) + 
  #geom_point(aes(shape=factor(prey_type,levels=ord),color=factor(prey_type,levels=ord))) +
  theme_classic() +
  labs(color="Prey Type",shape="Prey Type") +
  scale_y_continuous(labels = scales::percent) +
  xlab("") + 
  ylab("Percent in diet") +  
  ggtitle(lnames[i]) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=10,face="bold"),
        axis.title.y = element_text(hjust = 0.5,size=12,face="bold"),
        axis.text.y = element_text(hjust = 0.5,size=10),
        plot.title = element_text(face="bold"),
        legend.text = element_text(size=12),
        legend.title=  element_text(size=12,face="bold"))

#dev.copy(png,"test.png")

dev.off()

#}  


####################################################################

# For the record, functions to generate data assuming reem data
# files and code exist locally.
source("R/REEM_fooddata_functions.R")

raw_preylen <- read.clean.csv("data/local_reem_data/foodlab_preylen.csv")
raw_haul    <- read.clean.csv("data/local_reem_data/foodlab_haul.csv")

pdat <- raw_preylen %>%
  left_join(raw_haul,by=c("vessel","cruise","haul","year"))

poldat <- pdat %>%
  filter(region=="BS" & pred_nodc==8791030701 & prey_nodc==8791030701) %>%
  select(-type,-sta_id_let,-sta_id_num,-pred_lh,-pred_sex) %>%
  mutate( prey_sz_cat = floor(prey_sz1/10)*10 + 5,
          prey_wt_mm  = 5.63E-06	* (prey_sz1 ^ 3.0434),
          pred_cat    = cut(pred_len,c(0,12,25,40,999),right=F)
  ) 

save(poldat,file="poldat.Rdata")

####################
# Plot generation

load("poldat.Rdata")

ldat <- poldat %>%
  filter(cruise_type %in% c("Race_Groundfish","Race_Other")) %>%
  filter(year>=1981 & year<= 2019) %>%
  filter(month>=5   & month<=9)


Nprey <- nrow(ldat)
Npred <- length(unique(paste(ldat$vessel,ldat$cruise,ldat$haul,ldat$pred_specn)))

lsizes <- c("[12,25)", "[25,40)", "[40,999)")
lnames <- c("Pollock fork length 12-25 cm","Pollock fork length 25-40 cm","Pollock fork length 40+ cm")


# Why don't loops work for these graphs, check some other time
i <- 3
lfdat <- ldat %>% filter(pred_cat==lsizes[i])

Nprey <- nrow(lfdat)
Npred <- length(unique(paste(lfdat$vessel,lfdat$cruise,lfdat$haul,lfdat$pred_specn)))

p1 <- ggplot(lfdat, aes(x = prey_sz_cat, y = factor(month.abb[month],levels=month.abb))) +
  geom_density_ridges(bandwidth=10, fill="#308272") +
  theme_ridges() + 
  xlab("Prey standard length (mm)") +
  ylab("") +
  labs(title=lnames[i])+
  xlim(0,250) +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5,size=12),
        plot.title = element_text(hjust = 0.5,size=12,face="bold"),
        axis.text=element_text(size=12),
  )

# convert prey weights to high resolution to then distribute by weight
# multiplying weights by 100 to give integer values for making vectors
w_tiny       = 1+floor(lfdat$prey_wt_mm * 100)
month_factor = factor(month.abb[lfdat$month],levels=month.abb)
wcount <- do.call("c",mapply(rep, lfdat$prey_sz1, w_tiny))
wmons  <- do.call("c",mapply(rep, month_factor,  w_tiny))

wdat <- data.frame(wcount,wmons)  

p2<- ggplot(wdat, aes(x = wcount, y = wmons)) +
  geom_density_ridges(bandwidth=10,fill="#308272") +
  theme_ridges() + 
  xlab("Prey standard length (mm)") +
  ylab("") +
  labs(title="")+
  xlim(0,250) +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5,size=12),
        plot.title = element_text(hjust = 0.5,size=12,face="bold"),
        axis.text=element_text(size=12),
  )

png(paste("plens",lsizes[i],"george.png",sep="_"),width=8,height=3,units="in",res=600,type="cairo-png")
#X11(width=8,height=3)
grid.arrange(p1,p2,ncol=2)
dev.off()
cat(lsizes[i],Npred,Nprey,"\n")

