## regression of length-weight by species 
## use "specimen" table from RACEBASE GAP surveys
## go through 00, 01 R scripts from RACEBASE GAP surveys first
## W =a*L^b (power function) or log(W) = log(a) + b*log(L) (linear)
##  y=log(W) and x=log(L), slope = b and intercept = log(a)
## example of backtranform:  log(W) = -10.84+2.91log(L) on the transformed scale and W =0.000020*L^2.91on original scale (a = e^intercept = e^−10.84=0.00002)


library(tidyverse)
library(ggplot2)

#read in data generated from query of racebase
#specimen=read.csv("C:/Users/bridget.ferriss/work/Rpath/GitHub Food Habits/GAP_design_based_indices/data/local_racebase/specimen.csv") #length, weight
#species=read.csv("C:/Users/bridget.ferriss/work/Rpath/GitHub Food Habits/GAP_design_based_indices/data/local_racebase/species.csv") #species codes
#nodc <- read.csv("C:/Users/bridget.ferriss/work/Rpath/GitHub Food Habits/GAP_design_based_indices/data/local_nodc/nodc.csv")

setwd("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_dietcomp/R")

specimen=read.csv("../data/local_racebase/specimen.csv") #length, weight
species=read.csv("../data/local_racebase/species.csv") #species codes
nodc <- read.csv("../data/local_nodc/nodc.csv")

head(specimen)
length(specimen[,1])

head(species)

#join species codes (NODC with GAP)
specimen2 <- nodc %>%
  select(NODC,RACE) %>%
  left_join(specimen, by = c('RACE' = 'SPECIES_CODE'))   #species_code')) %>%
 # filter(NODC==8791030401)        # put in NODC code of your choice

head(specimen2)

#remove all records with missing length or weight
specimen2 <-subset(specimen2,!is.na(WEIGHT) & !is.na(LENGTH))
length(specimen2[,1])
head(specimen2)

#create new variables for regression
specimen2$logL<-log(specimen2$LENGTH+1)
specimen2$logW<-log(specimen2$WEIGHT+1)

#subset by species
m=length(species[,1])
LWparam= data.frame("NODC.species.code"= rep(NA,m), "RACE.species.code"=rep(NA,m), "species_name"= rep(NA,m), "common_name"= rep(NA,m), "intercept_a"= rep(NA,m), "slope_b"= rep(NA,m))

#calculate l/W regression and save results in table
for (i in 1:m) { 
  # subset data for given species
  sp_code=as.numeric(as.character((species[i,1]))) #select a species code
  NODCcode=nodc[which(nodc[,2]==sp_code),1]
  speciesLW <-subset(specimen2,RACE==sp_code) #subset dataset to that species only
  # fit length-weight regression for given species 
  if (length(speciesLW[,1]>4)) {   #only calculate for species min sample size=5
  lm1<-lm(logW~logL,data=speciesLW ) 
  #save results to table
  LWparam[i,1]=nodc[which(nodc[,2]==sp_code),1] #NODC code
  LWparam[i,2]=species[i,1]  #species code
  LWparam[i,3]=as.character(species[i,2])  #latin name
  LWparam[i,4]=as.character(species[i,3])  #common name
  LWparam[i,5]= exp(signif(lm1$coef[[1]],5)) #L/W regression intercept (a)
  LWparam[i,6]= signif(lm1$coef[[2]],5) #L/W regression slope (b)
  i=i+1
  } else {
    i=i+1
  } #end if
} #end for

#save table in a file
write.csv(LWparam,"../results/LengthWeightRegressionParams.csv",row.names=F) #BF add "GOA" and "../"

### double check individual species
# Method1 calculate for individual species
speciesLW <-subset(specimen2,RACE=="21720") #cod #30060") #POP 
head(speciesLW)
lm1<-lm(logW~logL,data=speciesLW )
summary(lm1)

ggplot(speciesLW, aes(x=logL, y=logW))+
  geom_point()+
  stat_smooth(method="lm", col="black")

# Method 2 - pull parameter values from table produces in loop above ;compare to summary(lm1 output ; b=exp(intercept))
Cod=which(LWparam[,2]=="21720") 
LWparam[Cod,]


#remove fish below a certain weight? consider for later

