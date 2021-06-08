## gulls on boats
source("/Users/heatherwelch/Dropbox/Gulls/heather_working/github_Gulls_gone_wild/general_functions/load_libraries.R")
library(glue)
library(wildlifeDI)
library(adehabitatLT)

# read in datasets
yr=2019
fordavid=read.csv("/Users/heatherwelch/Dropbox/Gulls/gull_data/WesternGull_tracks_forDavid.csv")%>% 
  mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
  mutate(year=year(DateTime)) %>% filter(year==yr)
ais_raw=read.csv(glue("/Users/heatherwelch/Dropbox/Gulls/sfbay_data/sfbay_ais_may_june_{yr}.csv"))%>% 
  mutate(time.GMT=substring(timestamp,first=1,last=19))%>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S"))
# outdir="/Users/heatherwelch/Dropbox/Gulls/Plots/plots_05_26_21_reduced"#;dir.create(outdir)
outdir=("/Users/heatherwelch/Dropbox/Gulls/Plots/plots_06_07_21_2min_interp_thresh5min_0.1_interactions");dir.create(outdir)
studyarea=st_read("/Users/heatherwelch/Dropbox/Gulls/shapefiles/us_medium_shoreline/us_medium_shoreline.shp")
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

tracks=unique(fordavid$tripID)

library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(8)

# test=foreach(i=36:36,.export = c("fordavid","yr","ais_raw","outdir","studyarea","myPalette"),.combine=rbind,.packages = c("glue","tidyverse","raster","wildlifeDI","adehabitatLT"),.verbose=T) %dopar%{
 test=foreach(i=1:length(tracks),.export = c("fordavid","yr","ais_raw","outdir","studyarea","myPalette"),.combine=rbind,.packages = c("glue","tidyverse","raster","wildlifeDI","adehabitatLT"),.verbose=T) %dopar%{
  print(tracks[i])
  gull=fordavid %>% filter(tripID==tracks[i]) %>% mutate(row2=as.character(seq(1:nrow(.))))
  
  ais2=ais_raw %>% 
    filter(DateTime>=min(gull$DateTime)&DateTime<=max(gull$DateTime))%>% 
    filter(lon>=min(gull$Longitude) & lon<=max(gull$Longitude)) %>% 
    filter(lat>=min(gull$Latitude) & lat<=max(gull$Latitude)) %>% filter(lon<(-122.45))
  
  ## finding pairs
  xy=gull %>% dplyr::select(Longitude,Latitude) %>% rename(X=Longitude,Y=Latitude)
  id=tracks[i]
  date=gull$DateTime
  testgull=as.ltraj(xy=xy,date=date,id=id,
                    proj4string = CRS("+proj=utm +zone=10 +datum=WGS84"))
 
  
  labels <- pretty(gull$DateTime, 5)
  
   empty=list()
  ssvid1=unique(ais2$ssvid)
   if(length(ssvid1)>0){
     # for(ii in 30:30){
      for(ii in 1:length(ssvid1)){
    print(ssvid1[ii])
       
    ais=ais2 %>% filter(ssvid==ssvid1[ii]) %>% mutate(row1=as.character(seq(1:nrow(.))))
    xy=ais %>% dplyr::select(lon,lat) %>% rename(X=lon,Y=lat)
    # id=ais$Bird_ID
    id=ssvid1[ii]
    date=ais$DateTime
    testais=as.ltraj(xy=xy,date=date,id=id,
                     proj4string = CRS("+proj=utm +zone=10 +datum=WGS84"))
    testais <- redisltraj(testais, 120, type="time")
    ais_retimed=ld(testais) %>% rename(DateTime=date)%>% mutate(row1=as.character(seq(1:nrow(.))))
    
    proxx=Prox(testais, testgull, tc=10*60, local=T,GetSimultaneous=T)%>% mutate(gull=tracks[i],vessel=ssvid1[ii])
    proxx2=proxx %>% filter(prox<.01)
    
     if(nrow(proxx2)>1){
       proxx3=proxx2 %>% mutate(diffpoint = as.numeric(row2) - lag(as.numeric(row2), 1)) %>%  ## needs to be >3 to be new interaction
      mutate(new_interaction=case_when(diffpoint>3~glue("Start_{row1}"))) %>%mutate(new_interaction=as.character(new_interaction))
       proxx3$new_interaction[1]="Start_1"
       
       proxx4=proxx3 %>% tidyr::fill(.,new_interaction,.direction="down") %>%
         rename(Gull_datetime=date2,Vessel_datetime=date1,time_difference=dt,degrees_distance=prox,tripID=gull,ssvid=vessel)

       proxx5=proxx4  %>% 
         left_join(.,gull) %>% left_join(.,ais_retimed,by="row1") %>% 
         dplyr::select(-c(X,time.GMT,DateTime.x,DateTime.y,dx,dy,dist,dt,R2n,abs.angle,rel.angle,id,burst,pkey)) %>% 
         rename(Gull_Longitude=Longitude,Gull_Latitude=Latitude,Vessel_Longitude=x,Vessel_Latitude=y)
    
       empty[[length(empty)+1]]=proxx5
       inter=unique(proxx5$new_interaction)
       for(iii in 1:length(inter)){
       print(inter[iii])
         proxx6=proxx5 %>% filter(new_interaction==inter[iii])
       a=ggplot()+
      geom_point(data=proxx6,aes(x=Gull_Longitude,y=Gull_Latitude),shape=2,color="black",size=4)+
      geom_point(data=gull,aes(x=Longitude,y=Latitude,color=DateTime),shape=2)+
      geom_path(data=gull,aes(x=Longitude,y=Latitude,color=DateTime))+
      # geom_path(data=ais,aes(x=lon,y=lat,color=DateTime))+
      geom_path(data=ais_retimed,aes(x=x,y=y,color=DateTime))+
      # geom_point(data=proxx2,aes(x=lon,y=lat),color="black",size=4)+
      geom_point(data=proxx6,aes(x=Vessel_Longitude,y=Vessel_Latitude),color="black",size=4)+
      # geom_point(data=ais,aes(x=lon,y=lat,color=DateTime))+
      geom_point(data=ais_retimed,aes(x=x,y=y,color=DateTime))+
      scale_color_gradientn(colors=myPalette(100),breaks=labels, labels=labels)+
      geom_sf(data=studyarea)+
      ylim(min(gull$Latitude),max(gull$Latitude))+xlim(min(gull$Longitude),max(gull$Longitude))+
      theme_classic()+
      ggtitle(glue("Gulltrip {tracks[i]} ssvid {ssvid1[ii]} {inter[iii]}"))
    
    png(glue("{outdir}/vessels_gull_{tracks[i]}_{ssvid1[ii]}_{inter[iii]}.png"),width=20,height=20,units='cm',res=400,type = "cairo")
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
   }
   master=do.call("rbind",empty)
   return(master)
 }

 write.csv(test,glue("{outdir}/prox_{yr}_5min_.02"))
 
