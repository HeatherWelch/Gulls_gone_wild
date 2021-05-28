## gulls on boats
source("/Users/heatherwelch/Dropbox/Gulls/heather_working/github_Gulls_gone_wild/general_functions/load_libraries.R")
library(glue)

# read in datasets
yr=2013
fordavid=read.csv("/Users/heatherwelch/Dropbox/Gulls/gull_data/WesternGull_tracks_forDavid.csv")%>% 
  mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
  mutate(year=year(DateTime)) %>% filter(year==yr)
ais_raw=read.csv(glue("/Users/heatherwelch/Dropbox/Gulls/sfbay_data/sfbay_ais_may_june_{yr}.csv"))%>% 
  mutate(time.GMT=substring(timestamp,first=1,last=19))%>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S"))
outdir="/Users/heatherwelch/Dropbox/Gulls/Plots/plots_05_26_21_reduced"#;dir.create(outdir)
studyarea=st_read("/Users/heatherwelch/Dropbox/Gulls/shapefiles/us_medium_shoreline/us_medium_shoreline.shp")
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

tracks=unique(fordavid$tripID)

library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(8)

foreach(i=1:length(tracks),.export = c("fordavid","yr","ais_raw","outdir","studyarea","myPalette"),.combine=rbind,.packages = c("glue","tidyverse","raster"),.verbose=T) %dopar%{
  print(tracks[i])
  gull=fordavid %>% filter(tripID==tracks[i])
  
  ais2=ais_raw %>% 
    filter(DateTime>=min(gull$DateTime)&DateTime<=max(gull$DateTime))%>% 
    filter(lon>=min(gull$Longitude) & lon<=max(gull$Longitude)) %>% 
    filter(lat>=min(gull$Latitude) & lat<=max(gull$Latitude)) %>% filter(lon<(-122.45))
  
  labels <- pretty(gull$DateTime, 5)
  
  ssvid1=unique(ais2$ssvid)
  # if(length(ssvid1)>0){
     for(ii in 1:length(ssvid1)){
    print(ssvid1[ii])
    ais=ais2 %>% filter(ssvid==ssvid1[ii])
     if(nrow(ais)>1){
    a=ggplot()+
      geom_point(data=gull,aes(x=Longitude,y=Latitude,color=DateTime),shape=2)+
      geom_path(data=gull,aes(x=Longitude,y=Latitude,color=DateTime))+
      geom_path(data=ais,aes(x=lon,y=lat,color=DateTime))+
      geom_point(data=ais,aes(x=lon,y=lat,color=DateTime))+
      scale_color_gradientn(colors=myPalette(100),breaks=labels, labels=labels)+
      geom_sf(data=studyarea)+
      ylim(min(gull$Latitude),max(gull$Latitude))+xlim(min(gull$Longitude),max(gull$Longitude))+
      theme_classic()+
      ggtitle(glue("Gulltrip {tracks[i]} ssvid {ssvid1[ii]}"))
    
    png(glue("{outdir}/vessels_gull_{tracks[i]}_{ssvid1[ii]}.png"),width=20,height=20,units='cm',res=400,type = "cairo")
    par(ps=10)
    par(mar=c(4,4,1,1))
    par(cex=1)
    # print({grid.arrange(a,b,ncol=2)})
    print({a})
    # gg_hm
    dev.off()
     }
     }
}
# }

## okay that was a lot of work. ## prox stuff
load("/Users/heatherwelch/Dropbox/Gulls/gull_data/gull_trips.Rdata")
proj=st_crs(studyarea)
install.packages("wildlifeDI")
library(wildlifeDI)
gull2=gull %>% mutate(X=Longitude,Y=Latitude)
coordinates(gull2)=~Longitude+Latitude
crs(gull2)=crs(gull_trips)
gull3=st_as_sf(gull2) %>% st_transform(.,crs=st_crs(bridges))
xy=gull3 %>% dplyr::select(X,Y)# %>% rename(X=Longitude,Y=Latitude)
xy=st_coordinates(gull3)
# coordinates(xy)=~Longitude+Latitude
id=gull %>% dplyr::select(tripID)
id="gull1"
date=gull$DateTime

testgull=as.ltraj(xy=xy,date=date,id=id)

ais2=ais %>% mutate(X=lon,Y=lat)
coordinates(ais2)=~lon+lat
crs(ais2)=crs(gull_trips)
ais3=st_as_sf(ais2) %>% st_transform(.,crs=st_crs(bridges))
xy=st_coordinates(ais3)
# xy=ais %>% dplyr::select(lon,lat)%>% rename(X=lon,Y=lat)
id=ais %>% dplyr::select(ssvid) %>% unique()
date=ais$DateTime
testais=as.ltraj(xy=xy,date=date,id=id)

Prox(testais, testgull, tc=500, dc=55000,local=T,GetSimultaneous=T)
gpplot(a)+geom_point(aes(x=))

###
# gull2=gull %>% mutate(X=Longitude,Y=Latitude)
# coordinates(gull2)=~Longitude+Latitude
# crs(gull2)=crs(gull_trips)
# gull3=st_as_sf(gull2) %>% st_transform(.,crs=st_crs(bridges))
xy=fordavid %>% dplyr::select(Longitude,Latitude) %>% rename(X=Longitude,Y=Latitude)
# xy=st_coordinates(gull3)
# coordinates(xy)=~Longitude+Latitude
id=fordavid$Bird_ID
burst=fordavid$tripID
# id="gull1"
date=fordavid$DateTime

testgull=as.ltraj(xy=xy,date=date,id=id,burst = burst)

# ais2=ais %>% mutate(X=lon,Y=lat)
# coordinates(ais2)=~lon+lat
# crs(ais2)=crs(gull_trips)
# ais3=st_as_sf(ais2) %>% st_transform(.,crs=st_crs(bridges))
# xy=st_coordinates(ais3)
aaa=ais_raw
ais_raw=ais_raw %>% mutate(id=glue("{ssvid}_{DateTime}")) %>% distinct(.,id,.keep_all = TRUE)
 xy=ais_raw %>% dplyr::select(lon,lat)%>% rename(X=lon,Y=lat)
id=ais_raw$ssvid
burst=ais_raw$seg_id
date=ais_raw$DateTime
testais=as.ltraj(xy=xy,date=date,id=id)

Prox(testais, testgull, tc=500, dc=55000,local=T,GetSimultaneous=T)
gpplot(a)+geom_point(aes(x=))

