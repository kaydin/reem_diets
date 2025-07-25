library(sf)
library(ggplot2)

sdat <- st_read("apps/shapes/gf95_nmfs/gf95_nmfs.shp")

BS_areas  <- c(508, 509, 512, 513, 514, 516, 517, 518, 519, 521, 523, 524, 
               530, 531, 533, 534)
AI_areas  <- c(541, 542, 543)
GOA_areas <- c(610, 620, 621, 630, 631, 640, 650, 680) 
  
BSAI_FMP <- sdat[sdat$NMFS_AREA %in% c(BS_areas, AI_areas),]
GOA_FMP  <- sdat[sdat$NMFS_AREA %in% c(GOA_areas),]

ggplot() + 
  geom_sf(data = BSAI_FMP, size = 1.5, color = "black", fill = "cyan1") + 
  ggtitle("BSAI FMP") + 
  coord_sf()

ggplot() + 
  geom_sf(data = GOA_FMP, size = 1.5, color = "black", fill = "cyan1") + 
  ggtitle("GOA FMP") + 
  coord_sf()



