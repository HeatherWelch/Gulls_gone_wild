## Alternative gull transport figure
source("/Users/heatherwelch/Dropbox/Gulls/heather_working/github_Gulls_gone_wild/general_functions/load_libraries.R")
library(glue)

year="2014"
birdID=201422011
fordavid=read.csv("/Users/heatherwelch/Dropbox/Gulls/gull_data/WesternGull_tracks_forDavid.csv")
gull1=fordavid %>% dplyr::filter(Bird_ID==birdID) %>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S"))
ais_raw=read.csv(glue("/Users/heatherwelch/Dropbox/Gulls/sfbay_data/sfbay_ais_may_june_{year}.csv"))
aais_raw=ais_raw %>% mutate(time.GMT=substring(timestamp,first=1,last=19))%>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
   filter(lon>=min(gull1$Longitude) & lon<=max(gull1$Longitude)) %>% filter(lat>=min(gull1$Latitude) & lat<=max(gull1$Latitude)) %>% 
  filter(DateTime>=min(gull1$DateTime) & DateTime<=max(gull1$DateTime))

ggplot()+
  geom_point(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime))+
  geom_sf(data=studyarea)+
  ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))

labels <- pretty(gull1$DateTime, 5)
ssvid1=unique(aais_raw$ssvid) %>% grep(paste("413980000|366962000"),.,value=T)
for(i in 1:length(ssvid1)){
  ais=aais_raw %>% filter(ssvid==413980000 | ssvid==366962000)
  
  a=ggplot()+
    geom_point(data=gull1,aes(x=Longitude,y=Latitude),color="red",shape=2)+
    geom_path(data=gull1,aes(x=Longitude,y=Latitude),color="blue")+
    geom_path(data=ais,aes(x=lon,y=lat,color=ssvid,group=ssvid))+
    geom_point(data=ais,aes(x=lon,y=lat,color=ssvid,group=ssvid))+
    scale_color_gradientn(colors=myPalette(100),breaks=labels, labels=labels)+
    geom_sf(data=studyarea)+
    ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))+
    theme_classic()+
    ggtitle(glue("Bird_id {birdID} ssvid {ssvid1[i]}"))
  
  library(gganimate)
  b=a+transition_reveal(along = DateTime)+
    labs(title = 'Date: {frame_along}')
  animate(b,
          fps = 3, # frames per second
          nframes = 200)
  anim_save(b,
            fps = 3,
            nframes = 200,
            file = "animatedpaths.gif")
  # a=ggplot()+
  #   geom_point(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime),shape=2)+
  #   geom_path(data=gull1,aes(x=Longitude,y=Latitude,color=DateTime))+
  #   geom_path(data=ais,aes(x=lon,y=lat,color=DateTime))+
  #   geom_point(data=ais,aes(x=lon,y=lat,color=DateTime))+
  #   scale_color_gradientn(colors=myPalette(100),breaks=labels, labels=labels)+
  #   geom_sf(data=studyarea)+
  #   ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))+
  #   theme_classic()+
  #   ggtitle(glue("Bird_id {birdID} ssvid {ssvid1[i]}"))
  
  png(glue("{outdir}/vessels_gull_{birdID}_{date}_{ssvid1[i]}.png"),width=20,height=20,units='cm',res=400,type = "cairo")
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  # print({grid.arrange(a,b,ncol=2)})
  print({a})
  # gg_hm
  dev.off()
  
}

  ## turning this into a figure ####
######################################### gulls and ship
  ais=aais_raw %>% filter(ssvid==413980000 | ssvid==366962000)
ais1=ais %>% filter(ssvid==413980000)
gull1_1=gull1 %>% filter(DateTime<=max(ais1$DateTime))
labels1 <- pretty(gull1_1$DateTime, 5)
  day1=ggplot()+
    geom_path(data=gull1,aes(x=Longitude,y=Latitude),color="lightgrey")+
    geom_point(data=gull1,aes(x=Longitude,y=Latitude),shape=24,size=2,fill="lightgrey",color="lightgrey")+
    geom_path(data=gull1_1,aes(x=Longitude,y=Latitude),color="black")+
    geom_path(data=ais1,aes(x=lon,y=lat,group=ssvid),color="black")+
    new_scale_color()+
    geom_sf(data=studyarea)+
    geom_point(data=ais1 ,aes(x=lon,y=lat,color=DateTime,group=ssvid),shape=21,size=5,fill="black")+
    geom_point(data=gull1_1,aes(x=Longitude,y=Latitude,color=DateTime),shape=24,size=2,fill="black")+
    scale_color_gradientn(colors=myPalette(100),breaks=labels1, labels=labels1)+
    scale_fill_gradientn(colors=myPalette(100),breaks=labels1, labels=labels1)+
    theme_classic()+
    theme(legend.position = c(.2,.8))+
    ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))
  
  ais2=ais %>% filter(ssvid==366962000)
  gull1_2=gull1 %>% filter(DateTime>=min(ais2$DateTime))
  labels2 <- pretty(gull1_2$DateTime, 5)
  day2=ggplot()+
    geom_path(data=gull1,aes(x=Longitude,y=Latitude),color="lightgrey")+
    geom_point(data=gull1,aes(x=Longitude,y=Latitude),shape=24,size=2,fill="lightgrey",color="lightgrey")+
    geom_path(data=gull1_2,aes(x=Longitude,y=Latitude),color="black")+
    geom_path(data=ais2,aes(x=lon,y=lat,group=ssvid),color="black")+
    theme_classic()+
    geom_sf(data=studyarea)+
    geom_point(data=ais2 ,aes(x=lon,y=lat,color=DateTime,group=ssvid),shape=21,size=5,fill="black")+
    geom_point(data=gull1_2,aes(x=Longitude,y=Latitude,color=DateTime),shape=24,size=2,fill="black")+
    scale_color_gradientn(colors=myPalette(100),breaks=labels2, labels=labels2)+
    scale_fill_gradientn(colors=myPalette(100),breaks=labels2, labels=labels2)+
    theme(legend.position = c(.2,.8))+
    ylim(min(gull1$Latitude),max(gull1$Latitude))+xlim(min(gull1$Longitude),max(gull1$Longitude))
  day2

  ######################################### gulls and roads
  birdID=201822014
  gull3= fordavid %>% dplyr::filter(Bird_ID==birdID) %>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
    filter(DateTime<"2018-05-24 12:00:00")
  labels2 <- pretty(gull3$DateTime, 5)
  ggplot()+
    geom_point(data=gull3,aes(x=Longitude,y=Latitude,color=DateTime))+
    geom_sf(data=studyarea)+
    ylim(min(gull3$Latitude),max(gull3$Latitude))+xlim(-122.5,max(gull3$Longitude))+
    scale_color_gradientn(colors=myPalette(100),breaks=labels2, labels=labels2)
  
  devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
  
  library(ggmap)
  ## instructions:https://www.littlemissdata.com/blog/maps 
  ## also have to set up billing accoung
  loc <- c(-122.5, min(gull3$Latitude), -121.29, 37.85)
  ggmap::register_google(key = "AIzaSyCXMbB64l_V6C79VCVLE-2BxMicRwasmA8")
  # ph_basemap <- get_map(location=c(lon = -122, lat = 37.6), zoom=11, maptype = 'roadmap', source = 'google')
  # ph_basemap2 <- get_map(location=c(lon = -122, lat = 37.6), zoom=5, maptype = 'roadmap', source = 'google')
  # ph_basemap3 <- get_map(location=c(lon = -122, lat = 37.6), zoom=8, maptype = 'roadmap', source = 'google')
  # ph_basemap4 <- get_map(location=c(lon = -122, lat = 37.6), zoom=10, maptype = 'roadmap', source = 'google')
  
  ph_basemap5 <- get_map(location=loc, maptype = 'roadmap', source = 'google')
  labels3 <- pretty(gull3$DateTime, 5)
  roads=ggmap(ph_basemap5)+
  geom_point(data=gull3,aes(x=Longitude,y=Latitude,color=DateTime), inherit.aes = FALSE,shape=24,size=2,fill="black")+
  scale_color_gradientn(colors=myPalette(100),breaks=labels3, labels=labels3)+
    xlab(NULL)+ylab(NULL)

  library(patchwork)
 
  day1+day2
