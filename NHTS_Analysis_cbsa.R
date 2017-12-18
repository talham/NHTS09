##reference for conversion
##https://blog.exploratory.io/creating-geojson-out-of-shapefile-in-r-40bc0005857d
library(rgdal)
library(rgeos)
library(spdplyr)
library(geojsonio)
library(rmapshaper)
library(dplyr)
library(tidyr)
library(sf)
library(RCurl)

##survey data can be obtained
##http://nhts.ornl.gov/download.shtml
##shapefiles can be downloaded 
##https://www.census.gov/geo/maps-data/data/cbf/cbf_nation.html
##http://www2.census.gov/geo/tiger/TIGER2007FE/fe_2007_us_cbsa.zip
##http://www2.census.gov/geo/tiger/TIGER2007FE/fe_2007_us_state.zip

##process consisted of the following
## processing was conducted in GIS software - QGIS - but can be conducted in R as well
## 1. clip area csa files with national boundary files - area boundary files include territorial
## waters like Lake Michigan 
## 2.  Calculate a difference layer - using QGIS - between state and urban area csa files
## to get the rural areas by state. 
## 3. convert shapefiles into json files for importing into D3. 
## R calculations are provided but actual calcuations were done in QGIS

##setup files
setwd("C:/Users/talha/Documents/Training/Datasets/Transportation/NHTS")
writedata=file.path(getwd(),"Data",fsep="/")

# url_shp_to_spdf <- function(URL) {
#   
#   wd <- getwd()
#   td <- tempdir()
#   setwd(td)
#   
#   temp <- tempfile(fileext = ".zip")
#   download.file(URL, temp)
#   unzip(temp)
#   
#   shp <- dir(tempdir(), "*.shp$")
#   lyr <- sub(".shp$", "", shp)
#   y <- lapply(X = lyr, FUN = function(x) readOGR(dsn=shp, layer=lyr,verbose=FALSE))
#   names(y) <- lyr
#   
#   unlink(dir(td))
#   setwd(wd)
#   return(y)
# }

 
# national=url_shp_to_spdf("http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_nation_20m.zip")
# national<-national$cb_2016_us_nation_20m
# states<-url_shp_to_spdf("http://www2.census.gov/geo/tiger/TIGER2007FE/fe_2007_us_state.zip")
# states<-states$fe_2007_us_state
# cbsa<-url_shp_to_spdf("http://www2.census.gov/geo/tiger/TIGER2007FE/fe_2007_us_cbsa.zip")
# cbsa<-cbsa$fe_2007_us_cbsa
# #step 1: clip the cbsa layer with the national layer to incorporate national boundary
# cbsa07<-as(gIntersection(national,cbsa,byid=TRUE),"SpatialPolygonsDataFrame")
# #step 2: clip the state state layer with the national layer to incorporate national boundary
# states_clipped<-as(gIntersection(national,states,byid=TRUE),"SpatialPolygonsDataFrame")
# #step 3: calculate rural areas as the difference between urban areas and states
# cb_state_diff<-as(gDifference(states_clipped,cbsa07,byid=TRUE),"SpatialPolygonsDataFrame")


#
#read in the file
#msa shapefiles
cbsa07 <- readOGR(dsn = "shapefiles/cbsa_nation_clipped",layer = "cbsa_nation_07_clipped", verbose = FALSE)
#difference files
cb_state_diff<-readOGR(dsn="shapefiles/cbsa_07_rural", layer="cbsa_rural_07",verbose=FALSE)
cb_nation<-readOGR(dsn = "shapefiles/cb_2016_us_nation_20m",layer = "cb_2016_us_nation_20m", verbose = FALSE)

#show the files
cbsa07
cb_state_diff
cb_nation
#convert errors
cbsa07$NAME<-as.character(cbsa07$NAME)
cbsa07$NAMELSAD<-as.character(cbsa07$NAMELSAD)
cbsa07$NAME[cbsa07$CBSAFP==32420]<-"Mayaguez, PR"
cbsa07$NAMELSAD[cbsa07$CBSAFP==32420]<-"Mayaguez, PR MSA"
cbsa07$NAME[cbsa07$CBSAFP==41900]<-"San German, PR"
cbsa07$NAMELSAD[cbsa07$CBSAFP==41900]<-"San German-Cabo, PR"
cbsa07$NAME[cbsa07$CBSAFP==10380]<-"Aguadilla-Isabela-San Sebastian, PR"
cbsa07$NAMELSAD[cbsa07$CBSAFP==10380]<-"Aguadilla-Isabela-San Sebastian, PR MSA"
cbsa07$NAME[cbsa07$CBSAFP==15860]<-"Canon City, CO"
cbsa07$NAME[cbsa07$CBSAFP==21580]<-"Espanola, NM"

#convert to geo_json
#cb_states_json<-geojson_json(cb_states)
cbsa07_json<-geojson_json(cbsa07)
cbsa07_json<-ms_simplify(cbsa07_json)
cb_nation_json<-geojson_json(cb_nation)
cb_state_diff_json<-geojson_json(cb_state_diff)
cb_state_diff_json<-ms_simplify(cb_state_diff_json)
geojson_write(cbsa07_json,file="shapefiles/cbsa07.json",overwrite=TRUE)
geojson_write(cb_state_diff_json,file="shapefiles/cbsa_rural_07.json",overwrite=TRUE)
geojson_write(cb_nation_json,file="shapefiles/cb_nation.json",overwrite=TRUE) 

############################################################
############################################################
temp <- tempfile()
download.file("http://nhts.ornl.gov/2009/download/Ascii.zip",temp)
vehdata<- read.csv(unz(temp, "Ascii/VEHV2PUB.CSV"))
TRPdata<- read.csv(unz(temp, "Ascii/DAYV2PUB.CSV"))
unlink(temp)


## Analysis of the Survey data
###### Analysis of the vehicle data
#vehicle data from the survey. By household and vehicle. Unique identifier
#is household + vehicle id
#calculate vehicle age by census area 
vehdata<-vehdata %>% mutate(w_age=VEHAGE*WTHHFIN)
vehage<-vehdata %>% filter(VEHAGE!=-9) %>% group_by(HH_CBSA) %>% summarize(tot_w_age=sum(w_age),tot_w=sum(WTHHFIN))
vehage<-vehage %>% mutate(avg_age=round(tot_w_age/tot_w,2))
vehage_s<-vehage  %>% filter(HH_CBSA!=-9&HH_CBSA!='XXXXX')
#calculate age for rural areas i.e. HH_CBSA==-1
vehage_st<-vehdata_cen10 %>% filter(VEHAGE!=-9&(HH_CBSA==-1|HH_CBSA!='XXXXX')) %>% group_by(HHSTFIPS) %>% summarize(tot_w_age=sum(w_age),tot_w=sum(WTHHFIN))
vehage_st<-vehage_st %>% mutate(avg_age=round(tot_w_age/tot_w,2))
#convert the FiPS State into character
vehage_st$HHSTFIPS<-as.character(vehage_st$HHSTFIPS)
vehage_st[vehage_st$HHSTFIPS==1,1]<-"01"
vehage_st[vehage_st$HHSTFIPS==2,1]<-"02"
vehage_st[vehage_st$HHSTFIPS==4,1]<-"04"
vehage_st[vehage_st$HHSTFIPS==5,1]<-"05"
vehage_st[vehage_st$HHSTFIPS==6,1]<-"06"
vehage_st[vehage_st$HHSTFIPS==8,1]<-"08"
vehage_st[vehage_st$HHSTFIPS==9,1]<-"09"
write.csv(vehage_s,file.path(writedata,"vehicle_age.csv"))
write.csv(vehage_st,file.path(writedata,"vehicle_age_rural.csv"))
#########################################
###### Analysis of the trip data
TRPdata<-TRPdata %>% mutate(strt_hr=floor(STRTTIME/100),end_hr=floor(ENDTIME/100))
trip_sum<-TRPdata %>% filter(HH_CBSA!=-9&HH_CBSA!='XXXXX'&WHYTRP90<=12) %>% group_by(HH_CBSA,WHYTRP90,TRAVDAY,strt_hr) %>% count(wt=WTTRDFIN)
trip_sum<-rename(trip_sum,counts=n)
trip_sum<-arrange(trip_sum,HH_CBSA,WHYTRP90,TRAVDAY,strt_hr)
trip_tot<-TRPdata %>% filter(HH_CBSA!=-9&HH_CBSA!='XXXXX'&WHYTRP90<=12) %>% group_by(HH_CBSA,WHYTRP90,TRAVDAY) %>% count(wt=WTTRDFIN)
trip_tot<-rename(trip_tot,tot=n)
trip_tot<-arrange(trip_tot,HH_CBSA,WHYTRP90,TRAVDAY)
trip_sum<-merge(trip_sum,trip_tot, by=c("HH_CBSA","WHYTRP90","TRAVDAY"),all.x=TRUE)
#trip_sum<-TRPdata %>% filter(HH_CBSA!=-9&HH_CBSA!='XXXXX'&WHYTRP90<=12) %>% group_by(HH_CBSA,WHYTRP90,TRAVDAY,strt_hr) %>% count(wt=WTTRDFIN)
trip_sum<-trip_sum %>% mutate(prop=round(100*(counts/tot),2))
trip_sum$WHYTRP90<-factor(trip_sum$WHYTRP90,levels=c(1,2,3,4,5,6,7,8,10,11),labels=c("work","work-related","Shopping","Other Fam / Business",
                                                                                       "School/Church","Medical","Vacation","Visit Friends","Other Social/Rec","Other"))
####rural areas
trip_st<-TRPdata %>% filter((HH_CBSA==-9|HH_CBSA=='XXXXX')&WHYTRP90<=12) %>% group_by(HHSTFIPS,WHYTRP90,TRAVDAY,strt_hr) %>% count(wt=WTTRDFIN)
trip_st<-rename(trip_st,counts=n)
trip_st<-arrange(trip_st,HHSTFIPS,WHYTRP90,TRAVDAY,strt_hr)
trip_st_tot<-TRPdata %>% filter((HH_CBSA==-9|HH_CBSA=='XXXXX')&WHYTRP90<=12) %>% group_by(HHSTFIPS,WHYTRP90,TRAVDAY) %>% count(wt=WTTRDFIN)
trip_st_tot<-rename(trip_st_tot,tot=n)
trip_st_tot<-arrange(trip_st_tot,HHSTFIPS,WHYTRP90,TRAVDAY)
trip_st<-merge(trip_st,trip_st_tot, by=c("HHSTFIPS","WHYTRP90","TRAVDAY"),all.x=TRUE)
trip_st<-trip_st %>% mutate(prop=round(100*(counts/tot),2))
#data manipulation for visualization
trip_st$HHSTFIPS<-as.character(trip_st$HHSTFIPS)
trip_st[trip_st$HHSTFIPS==1,1]<-"01"
trip_st[trip_st$HHSTFIPS==2,1]<-"02"
trip_st[trip_st$HHSTFIPS==3,1]<-"03"
trip_st[trip_st$HHSTFIPS==4,1]<-"04"
trip_st[trip_st$HHSTFIPS==5,1]<-"05"
trip_st[trip_st$HHSTFIPS==6,1]<-"06"
trip_st[trip_st$HHSTFIPS==7,1]<-"07"
trip_st[trip_st$HHSTFIPS==8,1]<-"08"
trip_st[trip_st$HHSTFIPS==9,1]<-"09"
trip_st$WHYTRP90<-factor(trip_st$WHYTRP90,levels=c(1,2,3,4,5,6,7,8,10,11),labels=c("work","work-related","Shopping","Other Fam / Business",
                                                                                     "School/Church","Medical","Vacation","Visit Friends","Other Social/Rec","Other"))
### write files drive
write.csv(trip_st,file.path(writedata,"trip_rural.csv"))
write.csv(trip_sum,file.path(writedata,"trip_urban.csv"))


