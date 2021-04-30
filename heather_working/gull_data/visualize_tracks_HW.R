source("/Users/heatherwelch/Dropbox/Gulls/heather_working/github_Gulls_gone_wild/general_functions/load_libraries.R")

#data cleaning notes to SCOTT
#never_left_nest<-c("201322003", "201322006", "201322007",  "201322012",  "201522015" , "201722010" , "201922009", "201922013") 
#201322007 & 201322012 are same
# tag 201922030  was duplicated 


library(rgdal)
#setwd("~/Desktop/us_medium_shoreline/")
studyarea=st_read("/Users/heatherwelch/Dropbox/Gulls/shapefiles/us_medium_shoreline/us_medium_shoreline.shp")
bridges=st_read("/Users/heatherwelch/Dropbox/Gulls/shapefiles/bay_area_bridges/bayarea_bridges.shp")
trash=read.csv("/Users/heatherwelch/Dropbox/Gulls/shapefiles/lmopdataca_reduced.csv")
  


########################
require(lubridate)
#require(ggplot2)
library(maps)
#library(mapdata)  
require(fields)
require(class)
require(zoo)
require(spam)

colscatter <- function(data, no.colors, minval, maxval){
  o <- seq(maxval, minval, length = no.colors+1)
  o1 <- (o + (o[2]-o[1])/2)[1:(length(o)-1)]
  c <- rev(tim.colors(no.colors))
  na_ind <- is.na(data)
  test <- as.matrix(data)
  test_no_na <- test[na_ind==FALSE,]
  train <- as.matrix(o1)
  cl <- as.matrix(1:no.colors)
  colind <- knn(as.matrix(train), as.matrix(test_no_na), cl)
  v = NULL
  v$ind <- c[colind]
  test[na_ind==FALSE] <- v$ind
  v$ind <- test
  v$bar <- c
  return(v)
}

########## lOAD DATA 

wg<-read.csv("/Users/heatherwelch/Dropbox/Gulls/gull_data/wegu2013_19_AllUTC_Master_Clipped.csv")

metadata<-read.csv("/Users/heatherwelch/Dropbox/Gulls/gull_data/wegu_metaDat_UTC_withheader.csv")

FI_birds<-metadata$birdID[which(metadata$colony==2000)]
wg2<-wg[which(wg$birdID %in% FI_birds),]

tm<-strptime(wg2$UTC,format='%m/%d/%Y %H:%M:%S', tz="GMT")
pos<-as.integer(as.POSIXct(strptime(wg2$UTC,format='%m/%d/%Y %H:%M:%S', tz="GMT"), origin = "1970-01-01", tz = "GMT"))
year1<- year(tm)
month1<- month(tm)
day1<- day(tm)
hour1<-hour(tm)
tms <- cbind(year1, month1, day1, hour1, wg2, pos)

#####################
#ggplot(tms, aes(x=Longitude, y=Latitude, color = year1)) + geom_line(lwd=.5)+ facet_grid(~ year1) + borders("state", xlim=range(wg$Longitude), ylim= range(wg$Latitude))



########### REMOVE BIRDs that never left nest ################
never_left_nest<-c("201322003", "201322006", "201322007",  "201322012",  "201522015" , "201722010" , "201922009", "201922013") #201322007 & 201322012 are same
#double checking I can remove these. 
#tm_sub<-tms[which(tms$birdID==unique(never_left_nest)[y]),]
#plot(tm_sub$Lon, tm_sub$Lat, type="l", xlim=c(-123,-123.02), ylim=c( 37.65,37.75))
#rect(-123.011693,37.693962,-122.999311,   37.704797, lwd=1 )

tms<-tms[-which( (as.character(tms$birdID) %in% never_left_nest)==T) , ]
########################################################




####### remove duplicates  # tag 201922030  was duplicated   ########
#require(tidyverse)
#my_data <- as_tibble(tms)
#tms<-distinct(my_data, .keep_all = TRUE)
#tms<-as.data.frame(tms)
#detach("package:tidyverse", unload=TRUE)
#the above code is correct but tidyveryse is messing up adding mapst o plots so just do this anotherway 
tms<-tms[-which(duplicated(tms)==T),]

# plotting HW ####
# points( -122.371846, 37.741734, col="purple", cex=3)#pier 96 recology 
# points( -122.390314, 37.709994, col="purple", cex=3)#recology sunset
# points( -122.193076, 37.713622,  col="purple", cex=3)#waste management?? /water treatment plant/ oakland airport
# points( -123.000313, 37.698321, col="pink", cex=4, pch=20)#SouthFI 

# creating a df with the above information
dat= data.frame(thing=c("Pier 96 recology","recology sunset","oakland airport","SouthFI"),
                     lat=c(37.741734,37.709994,37.713622,37.698321), 
                     lon=c(-122.371846,-122.390314,-122.193076,-123.000313), 
                     stringsAsFactors=FALSE) 

library(scales)
library(ggnewscale)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
a=tms %>% dplyr::filter(birdID==201622008) %>% mutate(date=as.Date(UTC,format="%m/%d/%Y %H:%M:%S")) %>% 
  mutate(DateTime=parse_date_time(UTC,orders="%m/%d/%Y %H:%M:%S"))

ggplot()+geom_point(data=a,aes(x=Longitude,y=Latitude,color=DateTime),alpha=.5)+
  scale_color_gradientn(colors=myPalette(100),trans=time_trans())+
  ylim(min(a$Latitude),max(a$Latitude))+xlim(min(a$Longitude),max(a$Longitude))+
  theme_classic()+
  geom_sf(data=studyarea, color = "darkgrey",fill=NA,size=.2)+
  geom_sf(data=bridges, color = "black",fill=NA,size=.4)+
  ## adding gull specific places
  new_scale_color()+
  geom_point(data=dat,aes(x=lon,y=lat,color=thing))+
  geom_point(data=trash,aes(x=Longitude,y=Latitude),color="black")

## writing for shiny app
tmss=tms %>% mutate(date=as.Date(UTC,format="%m/%d/%Y %H:%M:%S")) %>% 
  mutate(DateTime=parse_date_time(UTC,orders="%m/%d/%Y %H:%M:%S"))
write.csv(tmss,"/Users/heatherwelch/Dropbox/Gulls/heather_working/github_Gulls_gone_wild/heather_working/gulls_gone_wild_app/data/gulls.csv")

#OTHER STUFF FROM MEGAN ###################################


#### all tracks 
plot(tms$Longitude, tms$Latitude, type="l", xlim=c(-124, -121))
map(add=T, lwd=2, col="gray")
############


################## by year plots 
par(mfrow=c(3,3))
for(y in 1:length(unique(year1))){

tm_sub<-tms[which(year1==unique(year1)[y]),]

plot(tm_sub$Longitude, tm_sub$Latitude, main= paste(unique(year1)[y], "n=", length(unique(tm_sub$birdID)) ,sep=" "), type="l", xlab="lat",ylab="lon")
map(add=T, lwd=2, col="gray")
}
######################################################





#################################### by individual 
gull_speed<-NULL

for(y in 1:length(unique(year1))){
	tm_sub<-tms[which(tms$year1==unique(year1)[y]),]
print(y)
for(x in 1:length(unique(tm_sub$birdID))){
	print(x)
	tm_sub1<-tm_sub[which(tm_sub$birdID==unique(tm_sub$birdID)[x]),]
	len<-length(tm_sub1$Latitude)
#speed<-euclidean_speed(tm_sub1$Latitude[-1], tm_sub1$Latitude[-len], tm_sub1$Longitude[-1], tm_sub1$Longitude[-len], tm_sub1$pos[-1], tm_sub1$pos[-len]) *1000

x1 = as.matrix(cbind(tm_sub1$Latitude,tm_sub1$Latitude))[-1,]
x2 = as.matrix(cbind(tm_sub1$Latitude,tm_sub1$Latitude))[-len,]
 dists = rdist.earth.vec(x2, x1)*1000
speed = dists/ diff(tm_sub1$pos)

xx<-which(speed > 100)
if(length(xx)>0){
print(speed[xx])
speed[xx]<-NA
tm_sub1$Latitude[xx]<-NA
tm_sub1$Longitude[xx]<-NA
}
gull_speed<-rbind(gull_speed, cbind(tm_sub1, c(speed, NA)))

########## PLOT 
setwd("~/Desktop/NOAA_ERD/Projects/WesternGulls/Plots/Individual_tracks/")
png(paste(unique(tm_sub1$birdID) ,".png", sep=""), width=800, height=700)
par(oma=c(0,0,0,5))

C <- colscatter(speed, 64, min(speed,na.rm=T), max(speed,na.rm=T))
plot(tm_sub1$Longitude, tm_sub1$Latitude, main= paste(unique(year1)[y], "ID=", unique(tm_sub1$birdID) ,sep=" "), type="l", xlab="lat",ylab="lon", 
xlim=c(-123.9,-121.3), ylim=c(36.95,38.32))
map(add=T, lwd=2, col="gray")
points( -123.000313, 37.698321, col="pink", cex=4, pch=20)#SouthFI 
points(tm_sub1$Longitude, tm_sub1$Latitude, pch=20, col=C$ind)

#Places birds go 
points( -122.371846, 37.741734, col="purple", cex=3)#pier 96 recology 
points( -122.390314, 37.709994, col="purple", cex=3)#recology sunset
points( -122.193076, 37.713622,  col="purple", cex=3)#waste management?? /water treatment plant/ oakland airport


zzz<- speed
image.plot(legend.only=T, col=rev(C$bar),legend.mar= 0,legend.lab ="Speed m/s",  zlim=c(range(zzz,na.rm=T)), legend.line=2.5)
dev.off()
}
}
########################################################################






