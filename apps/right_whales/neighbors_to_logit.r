#
#setwd("c:/d/src/fitting/logit")

#dbase<- "BS_allPrey_Nov_10_11_R.csv"
#dbase<- "BS_allPrey_Dec_12_11_R.csv"
#dbase<- "BS_allPrey_Feb17_2011_R.csv"
#dbase <- "EBS_allPrey_Jan26_2011_R.csv"
#dbase   <- "BS_allPrey_May1_2015_out_R.csv"
#outfile <- "BS_allPrey_May1_2015_XiEta.csv"

#datlist <- read.csv(dbase)

dlon    <- diet$predprey_table$rlon %% 360 #datlist$Lon %% 360
dlat    <- diet$predprey_table$rlat #datlist$Lat
#dlon    <- datlist$Lon
#dlat    <- datlist$Lat

gbase <- "Bering_grid_withFeast.nc"
require(ncdf4)
#require(fields)

nearest<-function(lat_point=0,lon_point=0,lats=c(0,0),lons=c(0,0)){
	
    rad    <- 6371.01 # Average earth in km
    rconv  <- pi/180.0
    phi1 <- lats * rconv   #LAT IN Radians
    lam1 <- lons * rconv   #LON IN Radians	

    phi2 <- lat_point * rconv   #LAT IN Radians
    lam2 <- lon_point * rconv   #LON IN Radians
    sindel <- sin((phi1-phi2)/2.0)
    sinlam <- sin((lam1-lam2)/2.0)
    distr <- 2 * asin(sqrt(sindel*sindel + cos(phi1)*cos(phi2)*sinlam*sinlam)) * rad

    return(list(wmin=which.min(distr),dist=min(distr)))
}

ncfile<-nc_open("apps/right_whales/Bering_grid_withFEAST.nc")
  depths  <- ncvar_get(ncfile,"h")
  water   <- ncvar_get(ncfile,"mask_rho")
  stock   <- ncvar_get(ncfile,"mask_feast")
  domains <- ncvar_get(ncfile,"domain_feast")
  gridlat <- ncvar_get(ncfile,"lat_rho")
  gridlon <- ncvar_get(ncfile,"lon_rho")
nc_close(ncfile)


xx<-array(1:182,c(182,258))
ee<-t(array(1:258,c(258,182)))

lat_quad<- NULL
lon_quad<- NULL
for (i in c(17,33,49,65,81,97,113,129,145,161)){
    for (j in c(23,45,67,89,111,133,155,177,199,221)){
    lat_quad[length(lat_quad)+1] <-gridlat[i,j]
    lon_quad[length(lon_quad)+1] <-gridlon[i,j]         
    }
}
QQ<-length(lat_quad)
quad<-list()
for(q in 1:QQ){quad[[q]]<-list(lat=NULL,lon=NULL,xi=NULL,eta=NULL,dom=NULL,dep=NULL)}
quad$master_lat<-lat_quad
quad$master_lon<-lon_quad

sxi<-xx[water > 0]
seta<-ee[water > 0]
slat<-gridlat[water > 0]
slon<-gridlon[water > 0]
sdom<-domains[water > 0]
sdep<-depths[water > 0]

for (s in 1:length(slat)){
    out <- nearest(slat[s],slon[s],quad$master_lat,quad$master_lon)
    neighbor<-out$wmin
    quad[[neighbor]]$lat[length(quad[[neighbor]]$lat)+1] <- slat[s]
    quad[[neighbor]]$lon[length(quad[[neighbor]]$lon)+1] <- slon[s]            
    quad[[neighbor]]$xi[length(quad[[neighbor]]$xi)+1]   <- sxi[s]
    quad[[neighbor]]$eta[length(quad[[neighbor]]$eta)+1] <- seta[s]
    quad[[neighbor]]$dom[length(quad[[neighbor]]$dom)+1] <- sdom[s]
    quad[[neighbor]]$dep[length(quad[[neighbor]]$dep)+1] <- sdep[s]    
}


NN  <- length(dlat)
xi_rho  <- eta_rho <- lat_rho <- lon_rho <- rep(0,NN)
BSIERP_domain <- rep(0,NN)
ROMS_depth <- rep(0,NN)

for (s in 1:NN){
  if (s %% 1000 == 1){print(c(s,NN)); flush.console();}
  out<-nearest(dlat[s],dlon[s],quad$master_lat,quad$master_lon)
  quadloc<-out$wmin
  out<-nearest(dlat[s],dlon[s],quad[[quadloc]]$lat,quad[[quadloc]]$lon)
  neighbor<-out$wmin
  xi_rho[s]        <- quad[[quadloc]]$xi[neighbor]
  eta_rho[s]       <- quad[[quadloc]]$eta[neighbor]
  BSIERP_domain[s] <- quad[[quadloc]]$dom[neighbor]
  ROMS_depth[s]    <- quad[[quadloc]]$dep[neighbor]
  lat_rho[s]       <- quad[[quadloc]]$lat[neighbor]
  lon_rho[s]       <- quad[[quadloc]]$lon[neighbor]
}

#xi_eta<-data.frame(xi_rho,eta_rho)
#write.csv(xi_eta,"xi_eta.csv",row.names = FALSE)
#paste -d, xi_eta.csv BS_allPrey_Feb17_2011_R.csv > BS_out.csv
res<-data.frame(xi_rho,eta_rho,lat_rho,lon_rho,BSIERP_domain,ROMS_depth,diet$predprey_table)
#write.csv(res,outfile,row.names = FALSE)

