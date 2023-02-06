
# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library("tidyverse")
library("janitor") 
library("lubridate")

####################################################################

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
                       prey_guild_column     = "ssc_2023")

  prednames_lookup_file <- "lookups/EBS_RACE_fit_lookup_11-2022.csv"
  pred_guild_column     <- "ecopath_name"
  prednames.lookup <- read.clean.csv(prednames_lookup_file) %>% mutate(pred_guild  = .data[[pred_guild_column]])


pred_params <- list(
  "P.cod"        = list(nodc="8791030401", race="21720", LCLASS=c(0,10,30,60,999),  #NOTE FOR SSC one LC removed
                        bioen=list(ref="cod2015", CA=0.041, CB= -0.122, C_TM=21, C_T0=13.7, C_Q=2.41)),
  "W.pollock"    = list(nodc="8791030701", race="21740", LCLASS=c(0,10,25,40,999), #NOTE FOR SSC one LC removed
                        bioen=list(ref="poll2015", CA=0.119, CB=-0.46, C_TM=15, C_T0=10, C_Q=2.6)),
  "Arrowtooth"   = list(nodc="8857040102", race="10110", LCLASS=c(0,10,30,50,999),
                        bioen=list(ref="atf2015", CA=0.125, CB=-0.199, C_TM=26, C_T0=20.512, C_Q=2.497)),
  "P.halibut"    = list(nodc="8857041901", race="10120", LCLASS=c(0,10,50,70,999),
                        bioen=list(ref="hal2018", CA=0.0625, CB=-0.1076, C_TM=18, C_T0=12.97, C_Q=3.084))
)

# Consumption scaling (temperature-dependent) currently uses Wisconsin
# model eq 2 parameters for Cmax (per day) and temperature corrections.  
# To use a fixed proportion of body weight (biomass), set CA = desired daily proportion of body weight
# (or Ecopath annual QB/365) and CB=0, C_TM=100, C_T0=-100, C_Q=1.000000001
# If you're using this for diet proportions not total consumption, can simply
# use 1.0 for CA.
#pred_params[["P.cod"]]$bioen = list(ref="P.cod.old", CA=0.041,     CB= -0.122,  C_TM=21,  C_T0=13.7,  C_Q=2.41)
#pred_params[["P.cod"]]$bioen = list(ref="cod.qb"   , CA=1.39/365,  CB=0,  C_TM=100,  C_T0=-100,  C_Q=1.000000001)


##############################################################################
## GEORGE ANALYSIS 2022
library(maps)
source("R/REEM_fooddata_functions.R")


DAT <- get_cpue_all(model="NBS") %>%
  left_join(prednames.lookup, by=c("species_code"="race"))

haul_sums <- DAT %>%
  group_by(year,stratum_bin,Lat,Lon,Bottom_temp,Surface_temp,hauljoin,pred_guild) %>%
  summarize(wtcpue_t_km2=sum(wgtcpue)/1000, .groups="keep")

m <- (65-63)/(195-191)
b <-     63    -m*191
haul_sums <- haul_sums %>%
  mutate(stratum_bin=ifelse(stratum_bin=="NBS71"& Lat>=m*Lon%%360+b,"NBS72",stratum_bin))

stratum_means <- haul_sums %>%
  group_by(year,stratum_bin,pred_guild) %>%
  summarize(mean_wtcpue_t_km2 = mean(wtcpue_t_km2),
            mean_bottom_temp  = mean(Bottom_temp, na.rm=TRUE),
            mean_surface_temp = mean(Surface_temp,na.rm=TRUE),
            .groups="keep")

write.csv(stratum_means,"results/nbs_guild_means2.csv",row.names=F)

png("results/NBScons.png",width=2400,height=2400,type="cairo-png")
#par(mfrow=c(2,3))
for (yy in c(2021)){
  plot(haul_sums$Lon%%360,haul_sums$Lat,type="n",xlim=c(184,200),ylim=c(60,66),xlab="",ylab="",cex.axis=2)
  map(add = TRUE, col = "black", fill=TRUE, wrap=c(0,360))
#text(haul_sums$Lon%%360,haul_sums$Lat,haul_sums$stratum_bin)

stratlist <- c("NBS81","NBS70","NBS71","NBS72")
stratcols <- c("red","blue","green","purple")
for (c in 1:length(stratlist)){
  dat <- haul_sums[haul_sums$stratum_bin==stratlist[c] & haul_sums$year==yy,]
  points(dat$Lon%%360,dat$Lat,col=stratcols[c],pch=19,cex=8)
}

}
dev.off()


predprey <- predprey %>%
  mutate(stratum=ifelse(stratum==71 & region=="BS" & rlat>=m*rlong%%360+b, 72, stratum))
predprey$stratum <- as.numeric(predprey$stratum)

haul <- haul %>%
  mutate(stratum=ifelse(stratum==71 & region=="BS" & start_latitude>=m*start_longitude%%360+b, 72, stratum))
haul$stratum <- as.numeric(haul$stratum)

strata_lookup <- rbind(strata_lookup,c("BS","NBS","NBS72",72,82594.000/2,rep(NA,10),"NBS72"))
strata_lookup$area[strata_lookup$stratum==71 & strata_lookup$survey=="BS"]=82594.000/2
strata_lookup$area <- as.numeric(strata_lookup$area)
strata_lookup$stratum <- as.numeric(strata_lookup$stratum)
strat_areas <- strata_lookup %>%
  select(model,stratum_bin,area) %>%
  group_by(model, stratum_bin) %>%
  summarize(area=sum(area),.groups="keep")


predlist <- c("W.pollock","P.cod","Arrowtooth","P.halibut")
#this.pred  <- "W.pollock"
this.model <- "NBS"

combined_cons <- NULL
for (this.pred in predlist){
  # set up len-weight regression parameters and consumption weighting
  lwp <- get_lw(predator=this.pred, model=this.model, years=1982:2021, all.data=F) 
  pred_params[[this.pred]]$lw_a  = lwp$lw_a
  pred_params[[this.pred]]$lw_b  = lwp$lw_b
  strat_lencons  <- get_stratum_length_cons(predator=this.pred, model=this.model)
  strat_dietcons <- add_diets_to_strata_length_cons(strat_lencons, predator=this.pred, model=this.model, min_sample=5)
  
  combined_cons <- rbind(combined_cons,strat_dietcons)
}

write.csv(combined_cons,paste(this.model,"_NBS_stratcons_min5full.csv",sep=""),row.names=F)

