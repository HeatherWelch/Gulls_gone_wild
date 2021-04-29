## megan's code to create empty raster
source("/Users/heatherwelch/Dropbox/Gulls/heather_working/github_Gulls_gone_wild/general_functions/load_libraries.R")
library(raster)

x <- raster()
x2<-crop(x,  extent( -125, -121,36 ,40))
nrow(x2) <- 444
ncol(x2) <- 361
#values(x2) <-1:160284

r <- setValues(x2, sample(x = 0:500, size = ncell(x2), replace = T))
projected_raster <- projectRaster(r, crs = "+proj=utm +zone=10 +datum=WGS84 +units=m ")
res(projected_raster ) <- 1000 #1 km resolution 

r2 <- setValues(projected_raster, sample(x = 0:500, size = ncell(projected_raster), replace = T))

raster_back <- projectRaster(r2, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(raster_back)
map(add=T)
writeRaster(raster_back,"/Users/heatherwelch/Dropbox/Gulls/static_rasters/studyarea.grd")
#writeRaster()

## distance to shore ####
# need to split
cclme=st_read("/Users/heatherwelch/Dropbox/OLE/spatial_data/lme/lme.shp") 
studyarea=st_read("/Users/heatherwelch/Dropbox/Gulls/shapefiles/us_medium_shoreline/us_medium_shoreline.shp")
sa=as_Spatial(studyarea)

sa_offshore=mask(raster_back,cclme)
sa_inshore=mask(raster_back,cclme,inverse=T)
# test=merge(sa_offshore,sa_inshore) ## great it works!
test_inshore=raster::distance(sa_offshore)
test_inshore2=mask(test_inshore,cclme,inverse=T)*-1
test_offshore=raster::distance(sa_inshore)
test_offshore2=mask(test_offshore,cclme)
master=merge(test_offshore2,test_inshore2)
writeRaster(master,"/Users/heatherwelch/Dropbox/Gulls/static_rasters/dist_shore.grd")

## distance to dump ####
trash=read.csv("/Users/heatherwelch/Dropbox/Gulls/shapefiles/lmopdataca_reduced.csv") %>% 
  dplyr::select(Latitude,Longitude)
dat= data.frame(thing=c("Pier 96 recology","recology sunset","oakland airport"),
                Latitude=c(37.741734,37.709994,37.713622), 
                Longitude=c(-122.371846,-122.390314,-122.193076), 
                stringsAsFactors=FALSE) %>% dplyr::select(Latitude,Longitude) 

masterTrash=rbind(trash,dat)%>% .[complete.cases(.),]
coordinates(masterTrash)=~Longitude+Latitude

test=raster::distanceFromPoints(raster_back,masterTrash)
writeRaster(test,"/Users/heatherwelch/Dropbox/Gulls/static_rasters/dist_dump.grd")

## fast food? ####
## accessed from: https://data.world/datafiniti/fast-food-restaurants-across-america
## Datafiniti_Fast_Food_Restaurants_Jun19.csv
ff=read.csv("/Users/heatherwelch/Downloads/Datafiniti_Fast_Food_Restaurants_Jun19.csv") %>% 
  filter(longitude>(-125) & longitude<(-121)) %>% filter(latitude>(36) & latitude<(40))

ff_2=dplyr::select(ff,c(longitude,latitude)) 
coordinates(ff_2)=~longitude+latitude
test_ff=raster::distanceFromPoints(raster_back,ff_2)
writeRaster(test_ff,"/Users/heatherwelch/Dropbox/Gulls/static_rasters/dist_fastfood.grd")





####### dist 2 colony 
colony<-cbind(-123.000313, 37.698321)

dist2colony=raster::distanceFromPoints(raster_back,colony)

writeRaster(dist2colony,"~/Dropbox/Gulls/static_rasters/dist2colony.grd")
