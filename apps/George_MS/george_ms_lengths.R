
# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library(tidyverse)
library(janitor) 
library(lubridate)
library(ggridges)
library(ggplot2)

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
  filter(cruise_type %in% c("Race_Groundfish","Race_Other"))

Nprey <- nrow(ldat)
Npred <- length(unique(paste(ldat$vessel,ldat$cruise,ldat$haul,ldat$pred_specn)))

ggplot(ldat, aes(x = prey_sz_cat, y = factor(month.abb[month],levels=month.abb), fill="#5072B2")) +
  geom_density_ridges() +
  theme_ridges() + 
  xlab("Prey standard length (mm)") +
  ylab("") +
  labs(title="")+
  xlim(0,200) +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5,size=12,face="bold"),
        plot.title = element_text(hjust = 0.5,size=12,face="bold"),
        axis.text=element_text(size=12),
  )

# convert prey weights to high resolution to then distribute by weight
# multiplying weights by 100 to give integer values for making vectors
w_tiny       = 1+floor(ldat$prey_wt_mm * 100)
month_factor = factor(month.abb[ldat$month],levels=month.abb)
wcount <- do.call("c",mapply(rep, ldat$prey_sz1, w_tiny))
wmons  <- do.call("c",mapply(rep, month_factor,  w_tiny))

wdat <- data.frame(wcount,wmons)  

ggplot(wdat, aes(x = wcount, y = wmons)) +
  geom_density_ridges(bandwidth=10,color="#5072B2",fill="#5072B2") +
  theme_ridges() + 
  xlab("Prey standard length (mm)") +
  ylab("") +
  labs(title="")+
  xlim(0,200) +
  theme(legend.position = "none",
        axis.title.x = element_text(hjust = 0.5,size=12,face="bold"),
        plot.title = element_text(hjust = 0.5,size=12,face="bold"),
        axis.text=element_text(size=12),
  )


