source("/Users/heatherwelch/Dropbox/Gulls/heather_working/github_Gulls_gone_wild/general_functions/load_libraries.R")
library(glue)

a2016=read.csv("/Users/heatherwelch/Dropbox/Gulls/sfbay_data/sfbay_ais_may_june_2016.csv")
aa2016=a2016 %>% mutate(day=substring(timestamp,first=1,last=10))
aaa2016=aa2016 %>% filter(day=="2016-06-04")
studyarea=st_read("/Users/heatherwelch/Dropbox/Gulls/shapefiles/us_medium_shoreline/us_medium_shoreline.shp")
bridges=st_read("/Users/heatherwelch/Dropbox/Gulls/shapefiles/bay_area_bridges/bayarea_bridges.shp")

outdir="/Users/heatherwelch/Dropbox/Gulls/Plots/plots_04_28_21"

##DATA FROM VISUALIZE TRACKS HW .R
gull=tms %>% dplyr::filter(birdID==201622008) %>% mutate(date=as.Date(UTC,format="%m/%d/%Y %H:%M:%S")) %>% 
  mutate(DateTime=parse_date_time(UTC,orders="%m/%d/%Y %H:%M:%S")) %>% filter(date=="2016-06-04")  %>% 
  mutate(h=strftime(DateTime, format="%H"))%>% 
    filter(h>"02"& h<"08") 

load("/Users/heatherwelch/Dropbox/Gulls/gull_data/gull_trips.Rdata")
# gull2=gull_trips %>% as.data.frame()%>% dplyr::filter(ID=="201622008")%>% 
#   mutate(h=format(strptime(time1,"%H:%M:%S"),'%H'))

gull2=gull_trips %>% st_as_sf()%>% group_by(tripID)%>% st_cast("LINESTRING")
gull3=gull_trips%>% st_as_sf() %>% filter(ID=="201622008") %>% arrange(desc(DateTime)) %>% group_by(tripID) %>% summarise(do_union = FALSE)%>% st_cast("LINESTRING")
gull4=st_transform(gull3,crs=st_crs(bridges)) %>% mutate(row.id=seq(1:5))

ggplot()+
  geom_sf(data=gull4,aes(color=tripID))+
  geom_sf(data=bridges)
ggate=bridges %>% dplyr::filter(BRIDGE_NAM=="Golden Gate Bridge")
d=st_intersects(gull4,ggate,sparse = T) %>% as.data.frame() 
test=gull4 %>%left_join(.,d)

ggplot()+
  geom_sf(data=test,aes(color=tripID,linetype=as.character(col.id)))+
  geom_sf(data=bridges)

gulll1=gull_trips%>% st_as_sf() %>% dplyr::filter(tripID==2016220085)%>% dplyr::filter(date1=="2016-06-04") %>% 
  filter(DateTime<"2016-06-04 03:01:13 UTC")
ggplot()+
  geom_sf(data=gulll1,aes(color=DateTime))+
  geom_sf(data=bridges)

### new attempt bird id 2016220085####
fordavid=read.csv("/Users/heatherwelch/Dropbox/Gulls/gull_data/WesternGull_tracks_forDavid.csv")
gull1=fordavid %>% dplyr::filter(tripID==2016220085) %>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
  filter(DateTime>"2016-06-04 00:00:00") %>% filter(Longitude>=-122.65 & Longitude<=-122.475) %>% filter(Latitude>=37.72 & Latitude<=37.81766)
ggplot()+
  geom_point(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime))+
  geom_point(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime))+
  # scale_color_gradientn(colors=myPalette(100),trans=time_trans())+
  geom_sf(data=studyarea)+
  ylim(37.72,max(gull$Latitude))+xlim(-122.65,-122.475)
  
# a2016=read.csv("/Users/heatherwelch/Dropbox/Gulls/sfbay_data/sfbay_ais_may_june_2016.csv")
aa2016=a2016 %>% mutate(time.GMT=substring(timestamp,first=1,last=19))%>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
  filter(DateTime>=min(gull1$DateTime)&DateTime<=max(gull1$DateTime))%>% filter(lon>=-122.65 & lon<=-122.475) %>% filter(lat>=37.72 & lat<=37.81766)

labels <- pretty(gull1$DateTime, 5)

ssvid1=unique(aa2016$ssvid)
for(i in 1:length(ssvid1)){
  print(ssvid1[i])
  ais=aa2016 %>% filter(ssvid==ssvid1[i])
  
a=ggplot()+
  geom_point(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime),shape=2)+
  geom_path(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime))+
  geom_path(data=ais,aes(x=lon,y=lat,color=DateTime))+
  geom_point(data=ais,aes(x=lon,y=lat,color=DateTime))+
  scale_color_gradientn(colors=myPalette(100),breaks=labels, labels=labels)+
  geom_sf(data=studyarea)+
  ylim(37.72,max(gull1$Latitude))+xlim(-122.65,-122.475)+
  theme_classic()+
  ggtitle(glue("Gulltrip 2016220085 ssvid {ssvid1[i]}"))

png(glue("{outdir}/vessels_gull_2016-06-04_{ssvid1[i]}.png"),width=20,height=20,units='cm',res=400,type = "cairo")
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
# print({grid.arrange(a,b,ncol=2)})
print({a})
# gg_hm
dev.off()
}

## new attempt bird id is 201522008 ####
year="2015"
birdID=201522008

fordavid=read.csv("/Users/heatherwelch/Dropbox/Gulls/gull_data/WesternGull_tracks_forDavid.csv")
gull1=fordavid %>% dplyr::filter(Bird_ID==birdID) %>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
 filter(Longitude>=-122.75 & Longitude<=-122.65) %>% filter(Latitude<=37.72 & Latitude>=37.675)
date=median(gull1$DateTime) %>% substr(.,1,10)

ggplot()+
  geom_point(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime))+
  geom_sf(data=studyarea)+
  ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))

ais_raw=read.csv(glue("/Users/heatherwelch/Dropbox/Gulls/sfbay_data/sfbay_ais_may_june_{year}.csv"))
aais_raw=ais_raw %>% mutate(time.GMT=substring(timestamp,first=1,last=19))%>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
  filter(lon>=min(gull1$Longitude) & lon<=max(gull1$Longitude)) %>% filter(lat>=min(gull1$Latitude) & lat<=max(gull1$Latitude)) %>% 
  filter(DateTime>=min(gull1$DateTime)&DateTime<=max(gull1$DateTime))

ggplot()+
  geom_point(data=aais_raw,aes(x=lon,y=lat,color=DateTime))+
  geom_sf(data=studyarea)+
  ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))


labels <- pretty(gull1$DateTime, 5)

ssvid1=unique(aais_raw$ssvid)
for(i in 1:length(ssvid1)){
  print(ssvid1[i])
  ais=aais_raw %>% filter(ssvid==ssvid1[i])
  
  a=ggplot()+
    geom_point(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime),shape=2)+
    geom_path(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime))+
    # geom_path(data=ais,aes(x=lon,y=lat,color=DateTime))+
    geom_point(data=ais,aes(x=lon,y=lat,color=DateTime))+
    scale_color_gradientn(colors=myPalette(100),breaks=labels, labels=labels)+
    geom_sf(data=studyarea)+
    ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))+
    theme_classic()+
    ggtitle(glue("Bird_id {birdID} ssvid {ssvid1[i]}"))
  
  
  png(glue("{outdir}/vessels_gull_{birdID}_{date}_{ssvid1[i]}.png"),width=20,height=20,units='cm',res=400,type = "cairo")
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  # print({grid.arrange(a,b,ncol=2)})
  print({a})
  # gg_hm
  dev.off()
}

###new attempt bird id is 201922004 ####
year="2019"
birdID=201922004
fordavid=read.csv("/Users/heatherwelch/Dropbox/Gulls/gull_data/WesternGull_tracks_forDavid.csv")
gull1=fordavid %>% dplyr::filter(Bird_ID==birdID) %>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
  filter(DateTime<"2019-05-23 00:00:00")%>% filter(Latitude<=37.7 & Latitude>=37.5)#%>% filter(Longitude>=-122.75 & Longitude<=-122.65)
date=median(gull1$DateTime) %>% substr(.,1,10)

ggplot()+
  geom_point(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime))+
  geom_sf(data=studyarea)+
  ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))

ais_raw=read.csv(glue("/Users/heatherwelch/Dropbox/Gulls/sfbay_data/sfbay_ais_may_june_{year}.csv"))

aais_raw=ais_raw %>% mutate(time.GMT=substring(timestamp,first=1,last=19))%>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
  filter(lon>=min(gull1$Longitude) & lon<=max(gull1$Longitude)) %>% filter(lat>=min(gull1$Latitude) & lat<=max(gull1$Latitude)) %>% 
  filter(DateTime>=min(gull1$DateTime)&DateTime<=max(gull1$DateTime))

ggplot()+
  geom_point(data=aais_raw,aes(x=lon,y=lat,color=DateTime))+
  geom_sf(data=studyarea)+
  ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))


labels <- pretty(gull1$DateTime, 5)

ssvid1=unique(aais_raw$ssvid)
for(i in 1:length(ssvid1)){
  print(ssvid1[i])
  ais=aais_raw %>% filter(ssvid==ssvid1[i])
  
  a=ggplot()+
    geom_point(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime),shape=2)+
    geom_path(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime))+
    geom_path(data=ais,aes(x=lon,y=lat,color=DateTime))+
    geom_point(data=ais,aes(x=lon,y=lat,color=DateTime))+
    scale_color_gradientn(colors=myPalette(100),breaks=labels, labels=labels)+
    geom_sf(data=studyarea)+
    ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))+
    theme_classic()+
    ggtitle(glue("Bird_id {birdID} ssvid {ssvid1[i]}"))
  
  
  png(glue("{outdir}/vessels_gull_{birdID}_{date}_{ssvid1[i]}.png"),width=20,height=20,units='cm',res=400,type = "cairo")
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  # print({grid.arrange(a,b,ncol=2)})
  print({a})
  # gg_hm
  dev.off()
}

###new attempt bird id is 201422011 ####
year="2014"
birdID=201422011
fordavid=read.csv("/Users/heatherwelch/Dropbox/Gulls/gull_data/WesternGull_tracks_forDavid.csv")
gull1=fordavid %>% dplyr::filter(Bird_ID==birdID) %>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
  filter(DateTime<"2014-06-05 00:00:00")%>% filter(Latitude<=37.89 & Latitude>=37.7) %>% filter(Longitude>=-122.85 & Longitude<=-122.45)
date=median(gull1$DateTime) %>% substr(.,1,10)

ggplot()+
  geom_point(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime))+
  geom_sf(data=studyarea)+
  ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))

ais_raw=read.csv(glue("/Users/heatherwelch/Dropbox/Gulls/sfbay_data/sfbay_ais_may_june_{year}.csv"))

aais_raw=ais_raw %>% mutate(time.GMT=substring(timestamp,first=1,last=19))%>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
  filter(lon>=min(gull1$Longitude) & lon<=max(gull1$Longitude)) %>% filter(lat>=min(gull1$Latitude) & lat<=max(gull1$Latitude)) %>% 
  filter(DateTime>=min(gull1$DateTime)&DateTime<=max(gull1$DateTime))

ggplot()+
  geom_point(data=aais_raw,aes(x=lon,y=lat,color=DateTime))+
  geom_sf(data=studyarea)+
  ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))

labels <- pretty(gull1$DateTime, 5)

ssvid1=unique(aais_raw$ssvid)
for(i in 1:length(ssvid1)){
  print(ssvid1[i])
  ais=aais_raw %>% filter(ssvid==ssvid1[i])
  
  a=ggplot()+
    geom_point(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime),shape=2)+
    geom_path(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime))+
    geom_path(data=ais,aes(x=lon,y=lat,color=DateTime))+
    geom_point(data=ais,aes(x=lon,y=lat,color=DateTime))+
    scale_color_gradientn(colors=myPalette(100),breaks=labels, labels=labels)+
    geom_sf(data=studyarea)+
    ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))+
    theme_classic()+
    ggtitle(glue("Bird_id {birdID} ssvid {ssvid1[i]}"))
  
  
  png(glue("{outdir}/vessels_gull_{birdID}_{date}_{ssvid1[i]}.png"),width=20,height=20,units='cm',res=400,type = "cairo")
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  # print({grid.arrange(a,b,ncol=2)})
  print({a})
  # gg_hm
  dev.off()
}


####


 library(scales)
library(glue)
library(ggnewscale)
library(gridExtra)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
test=aaa2016 %>% mutate(dt=substring(timestamp,first=1,last=19)) %>%    
  mutate(DateTime=parse_date_time(dt,orders="%Y/%m/%d %H:%M:%S")) %>% mutate(h=strftime(DateTime, format="%H"))%>% 
  filter(h>"02"& h<"08") %>% filter(lat<=37.81 & lat>37.7)%>% filter(lon<=-122.475 & lon>-122.70)
#   mutate(date=as.Date(dt,format="%Y/%m/%d %H:%M:%S")) 
# %>% 
#   mutate(DateTime=parse_date_time(dt,orders="%m/%d/%Y %H:%M:%S")) 
#%>% filter(ssvid=="215439000") %>% arrange(timestamp)
ssvid2=unique(test$ssvid)

for(i in 1:length(ssvid)){
  print(ssvid2[i])
  test2=test %>% filter(ssvid==ssvid2[i])
a=ggplot()+
  geom_point(data=test2,aes(x=lon,y=lat,color=h))+
  geom_line(data=test2,aes(x=lon,y=lat,group=ssvid))+
  geom_line(data=gull,aes(x=Longitude,y=Latitude))+
  geom_point(data=gull,aes(x=Longitude,y=Latitude,color=h),shape=2)+
  # scale_color_gradientn(colors=myPalette(100),trans=time_trans())+
  theme_classic()+
   geom_sf(data=studyarea, color = "darkgrey",fill=NA,size=.2)+
  geom_sf(data=bridges, color = "black",fill=NA,size=.4)+
   # ylim(36.60,38.40)+xlim(-124.0,-121.3)+
  ylim(37.7,max(gull$Latitude))+xlim(-122.70,-122.475)+
  ggtitle(glue("Vessel {ssvid2[i]}"))

b=ggplot()+
  geom_path(data=gull2,aes(x=Longitude,y=Latitude,color=tripID))+
  geom_point(data=gull2,aes(x=Longitude,y=Latitude,color=tripID),shape=2)+
  # scale_color_gradientn(colors=myPalette(100),trans=time_trans())+
  # geom_point(data=gull,aes(x=Longitude,y=Latitude,color=h))+
  # scale_color_gradientn(colors=myPalette(100))+
  theme_classic()+
  geom_sf(data=studyarea, color = "darkgrey",fill=NA,size=.2)+
  geom_sf(data=bridges, color = "black",fill=NA,size=.4)+
  # ylim(36.60,38.40)+xlim(-124.0,-121.3)+
  ylim(37.7,max(gull$Latitude))+xlim(-122.70,-122.475)+
  ggtitle("Bird 201622008")

png(glue("{outdir}/vessels_gull_2016-06-04_{ssvid2[i]}.png"),width=20,height=20,units='cm',res=400,type = "cairo")
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
# print({grid.arrange(a,b,ncol=2)})
print({a})
# gg_hm
dev.off()
}




c=ggplot()+
  geom_point(data=test,aes(x=lon,y=lat,color=ssvid))+
  geom_point(data=gull,aes(x=Longitude,y=Latitude),color="red")+
  theme_classic()+
  geom_sf(data=studyarea, color = "darkgrey",fill=NA,size=.2)+
  geom_sf(data=bridges, color = "black",fill=NA,size=.4)+
  # ylim(36.60,38.40)+xlim(-124.0,-121.3)+
  ylim(min(gull$Latitude),max(gull$Latitude))+xlim(-122.75,-122.47)+
  ggtitle("Bird 201622008")
library(plotly)
ggplotly(c)
