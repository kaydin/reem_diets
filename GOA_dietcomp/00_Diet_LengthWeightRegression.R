## regression of length-weight by species of individuals in the RACE samples-calculate slope and intercept parameters to be able to convert length to weight in diet calculations 
## use "specimen" table from RACEBASE GAP surveys
## go through 00, 01 R scripts from RACEBASE GAP surveys first
## W =a*L^b (power function) or log(W) = log(a) + b*log(L) (linear function)
##  y=log(W) and x=log(L), slope = b and intercept = log(a)
## example of backtranform:  log(W) = -10.84+2.91log(L) on the transformed scale and W =0.000020*L^2.91on original scale (a = e^intercept = e^−10.84=0.00002)
#RACEbase: length (mm) weight (g) so need to convert length from mm to cm

library(tidyverse)
library(ggplot2)

###############
#Step 1. read in data generated from query of racebase
###############

setwd("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/R")

specimen=read.csv("../data/local_racebase/specimen.csv") #length, weight
species=read.csv("../data/local_racebase/species.csv") #species codes
nodc <- read.csv("../data/local_nodc/nodc.csv")
haul<-read.csv("../data/local_racebase/haul.csv")
stratum<-read.csv("../data/local_racebase/stratum.csv")
goa_strata<-read.csv("../data/goa_strata.csv")

head(specimen)
length(specimen[,1])

head(species)

###############
#Step 2. join species codes (NODC with GAP) and regional information
###############

specimen1 <- nodc %>%
  select(NODC,RACE) %>%
  left_join(specimen, by = c('RACE' = 'SPECIES_CODE')) 

#join  with regional information 
specimen2 <- haul %>%
  select(HAULJOIN,STRATUM) %>%
  left_join(specimen1, by = c('HAULJOIN')) #get stratum 

specimen3<-goa_strata%>%
  select(STRATUM, INPFC_AREA) %>%
  left_join(specimen2, by = c('STRATUM')) #get INPFC_AREA

#check
head(specimen3)

###############
#Step 3. add new column with EwE regions (e.g., WGOA and EGOA), regression calculations (logL, logW), remove rows with NAs and low weights
###############

specimen4<-specimen3%>%
  mutate(EwERegion=case_when(INPFC_AREA=="Kodiak"|INPFC_AREA=="KODIAK"|INPFC_AREA=="Shumagin"|INPFC_AREA=="SHUMAGIN"|INPFC_AREA=="Chirikof"~ "WGOA",
    INPFC_AREA=="Yakutat"|INPFC_AREA=="Southeastern"~ "EGOA",
    INPFC_AREA=="BERING SEA"|INPFC_AREA=="Southern Bering Sea"~"BS",
    INPFC_AREA=="Central Aleutians"|INPFC_AREA=="Eastern Aleutians"|INPFC_AREA=="Western Aleutians"~ "AI"))%>% 
   mutate(LENGTH_cm=LENGTH/10)%>% #convert weight from mm (original in RACE) to cm for regression (length_cm by weight_g)
   mutate(logL=log(LENGTH_cm+1))%>%  #create new variables for regression calcualtions
   mutate(logW=log(WEIGHT+1))%>%   #specimen2$WEIGHT_cm   #create new variables for regression calcualtions
   filter(!is.na(WEIGHT) & !is.na(LENGTH))%>%  #remove all records with missing length or weight
   filter(WEIGHT>1) #remove fish W=1 (logW=0) in which weight is too hard to measure and throws off regression

#check
head(specimen4)
levels(specimen4$INPFC_AREA)
unique(specimen4$EwERegion)

###############
#Step 4. Length-Weight Regression calculations by species and region
###############

#prep for main loop
species_code_list=unique(specimen4[,5]) #list of RACE codes
EwERegion_list=unique(specimen4[,24]) #list of EwE Region
min_sample = 5 #minimum sample size required to calculate lenght-weight regression

LWparam<-NODCcode<-sp_code<-sp_Lname<-sp_Cname<-LW.intercept_a<-LW.slope_b<-EwEReg<-n<-NA #create empty dataframe
  
#calculate l/W regression and save results in table
for (CODE in species_code_list) {
  # subset data for given species
  sp_code=CODE
  NODCcode=nodc[which(nodc[,2]==sp_code),1]
  speciesLW <-specimen4%>%
    filter(RACE==sp_code) 

  for (REG in EwERegion_list) {
    #subset dataset to EwE Region (e.g., WGOA, EGOA, BS, AI)
    EwEReg=REG
    spec.reg.LW <-speciesLW%>%
      filter(EwERegion==EwEReg) 
    
    if (length(spec.reg.LW[,1])>=min_sample){   #only calculate for species with greater than min sample size
      # fit length-weight regression for given species
      lm1<-lm(logW~logL,data=spec.reg.LW)
  
  #saving data to table
 sp_Lname=as.character(species[which(species[,1]==sp_code),2]) #latin name
 sp_Cname=as.character(species[which(species[,1]==sp_code),3]) #common name
 LW.intercept_a=exp(signif(lm1$coef[[1]],5)) #L/W regression intercept (a)
 LW.slope_b=signif(lm1$coef[[2]],5) #L/W regression slope (b)
 n=length(spec.reg.LW[,1]) #sample size of given species and region (should be greater than min sample size)

 LWparam=rbind(LWparam,data.frame(NODCcode,sp_code,sp_Lname,sp_Cname,LW.intercept_a, LW.slope_b,EwEReg,n))
    } #end if 
  } #end Region
} #end Species

#save table in a file
#write.csv(LWparam,"../results/LengthWeightRegressionParams.csv",row.names=F) 

###############
#Step 5. Double check individual species 
###############

# Method_1 calculate for individual species
speciesLW <-subset(specimen4,RACE=="21720") #cod #30060" #POP 
head(speciesLW)
lm1<-lm(logW~logL,data=speciesLW )
summary(lm1)

ggplot(speciesLW, aes(x=logL, y=logW))+
  geom_point()+
  stat_smooth(method="lm", col="black")

# Method_2 - pull parameter values from table produces in loop above ;compare to summary(lm1 output ; b=exp(intercept))
Cod=which(LWparam[,2]=="21720") 
LWparam[Cod,]

