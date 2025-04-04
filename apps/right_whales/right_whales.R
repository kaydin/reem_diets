
# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library("tidyverse")
library("janitor") 
library("lubridate")

source("R/REEM_fooddata_functions.R")

# Load diet data and perform some preliminary cleanups/calculations
REEM.loadclean.diets(data_path = "data/local_reem_data")

REEM.loadclean.lookups(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                       stratum_bin_column    = "strat_groups",
                       preynames_lookup_file = "lookups/Alaska_PreyLookup_MASTER.csv",
                       prey_guild_column     = "ecopath_prey")

### Make predator table.  
### this.model is only used to get predator parameters (a and b for body weight)
this.model <- "EBS"
this.species <- "Walleye_pollock"
predlist <- read.clean.csv("lookups/Alaska_Predators_GOA.csv")
preds      <- predlist %>% filter(model==this.model)
pred_names <- this.species #unique(preds$predator)
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


#diet <- predprey_tables(predator=this.species, model=this.model, all.data=T)
diet <- rbind(
  predprey_tables(predator=this.species, model="EBS", all.data=T)$predprey_table,
  predprey_tables(predator=this.species, model="NBS", all.data=T)$predprey_table,
  predprey_tables(predator=this.species, model="WGOA", all.data=T)$predprey_table,
  predprey_tables(predator=this.species, model="AI", all.data=T)$predprey_table
)
  #source("apps/right_whales/neighbors_to_logit.r")

################################
this.prey <- "Copepod"

ddat <- diet %>% 
  pivot_wider(names_from=prey_guild, values_from=prey_wt, values_fill=0) %>%
  mutate(prey_sci = .data[[this.prey]]/bodywt)

library(sf)
library(magrittr)
library(ggplot2)
# If Bering Survey Grid is needed
  #dsn <- "C:/Users/kerim.aydin/Work/src/bering-sea-spatial/arcmap/Bering_Sea_Spatial_Products_2022.gdb"
  #sel_layer <- sf::st_read(dsn = dsn, layer = "EBS_strata_Conner2022") #%>% st_transform(4326)

# Create a grid.  Transform to AK Albers projection (3338) to do boxes in meters.
grid_lats <- c(48,  48,   68,    68)
grid_lons <- c(170, -140, 170, -140)
grid_base <- st_as_sf(data.frame(grid_lons,grid_lats), coords=c("grid_lons","grid_lats"),
             crs=4326, agr="constant") %>% 
             st_transform(3338) %>%
             st_make_grid(cellsize = c(40000, 40000)) %>%
             st_sf(grid_id = 1:length(.))

grid_centroids <- st_centroid(grid_base) %>% st_transform(4326) %>% as.data.frame

sample_grid <- ddat %>% 
  select(predjoin, rlat, rlong) %>%                 # get lat lon of samples
  st_as_sf(coords=c("rlong","rlat"), crs=4326) %>%  # make lat/lon sf object
  st_transform(3338) %>%                            # convert to meters projection
  st_join(grid_base, join = st_intersects) %>%      # find matching grid cells
  as.data.frame
  
ddat_grid <- ddat %>%
  left_join(sample_grid, by="predjoin")

ddat_mean <- ddat_grid %>% 
  select(predjoin, grid_id, prey_sci) %>%
  group_by(grid_id) %>% 
  summarize(n=n(), prey_sci=mean(prey_sci)) %>%
  left_join(grid_centroids, by="grid_id")

ddat_filter <- ddat_mean %>% filter(n>=25)

#ggplot() + 
#  geom_sf(data = sel_layer) +
#  geom_sf(data=grid_50, fill = 'transparent', lwd = 0.3) +
#  geom_sf(data=points, col="red", size=1)

library(marmap)
#b = getNOAA.bathy(lon1 = 170, lon2 = -140, lat1 = 48, lat2 = 68, 
#                  resolution = 2, antimeridian = TRUE)
#save(b, file="apps/right_whales/NOAA_bathy.Rdata")
load("apps/right_whales/NOAA_bathy.Rdata")
bf = fortify.bathy(b) # convert to data frame

library(mapdata)
reg = map_data("world2Hires")
reg = subset(reg, region %in% c('USSR', 'USA'))


lons = c(180, 210)
lats = c(52, 66)

dot_scale = 500
#lat_layer <- sel_layer %>% st_transform(4326) %>% st_shift_longitude()
#lat_grid  <- grid_50   %>% st_transform(4326) %>% st_shift_longitude()
lat_grid  <- grid_base   %>% st_transform(4326) %>% st_shift_longitude()
lat_data  <- ddat_filter$`.` %>% st_shift_longitude()
dat_vals <- ddat_filter$prey_sci

ggplot()+
  
  # add 100m contour
  geom_contour(data = bf, aes(x=x, y=y, z=z),
               breaks=c(-50), size=c(0.3), colour="grey") +
  
  # add 200m contour
  geom_contour(data = bf,  aes(x=x, y=y, z=z),
               breaks=c(-200), size=c(0.6), colour="grey") +
  
  # add coastline
  geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
               fill = "darkgrey", color = NA) +

  #geom_sf(data=lat_layer, col="red", size=1) + 
  #geom_sf(data=lat_grid, fill = 'transparent', lwd = 0.3) +
  geom_sf(data=lat_data, col="red", size=dat_vals * dot_scale) +
 coord_sf(xlim = lons, ylim = lats)

