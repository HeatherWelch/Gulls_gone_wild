## gulls on boats
source("/Users/heatherwelch/Dropbox/Gulls/heather_working/github_Gulls_gone_wild/general_functions/load_libraries.R")
library(glue)
library(wildlifeDI)
library(adehabitatLT)
library(rgeos)
library(spatialEco)

## shapes to remove gulls in bay ####
# bayArea=st_read("/Users/heatherwelch/Dropbox/Gulls/shapefiles/stanford-qh320kj0191-shapefile/qh320kj0191.shp") #%>% 
#   #arrange(desc(acres)) %>% .[1,]
# BA=as_Spatial(bayArea)
# 
# mat1=matrix(c(-122.6, 37.9,  ## define SST box
#               -122.6,38,
#               -122.2,38,
#               -122.2, 37.9,
#               -122.6, 37.9),
#             ncol=2,byrow = T)
# 
# p = Polygon(mat1)
# ps = Polygons(list(p),1)
# sps = SpatialPolygons(list(ps))
# 
# #2
# df2<- data.frame(xmin=-122.53, xmax=-122.2, ymin=37.8, ymax=37.9)
# mat1=matrix(c(-122.53, 37.8,  ## define SST box
#               -122.53,37.9,
#               -122.2,37.9,
#               -122.2, 37.8,
#               -122.53, 37.8),
#             ncol=2,byrow = T)
# 
# p = Polygon(mat1)
# ps = Polygons(list(p),1)
# sps2 = SpatialPolygons(list(ps))
# 
# #3
# df3<- data.frame(xmin=-122.44, xmax=-122.10, ymin=37.4, ymax=37.8)
# mat1=matrix(c(-122.44, 37.4,  ## define SST box
#               -122.44,37.8,
#               -122.10,37.8,
#               -122.10, 37.4,
#               -122.44, 37.4),
#             ncol=2,byrow = T)
# 
# p = Polygon(mat1)
# ps = Polygons(list(p),1)
# sps3 = SpatialPolygons(list(ps))
# 
# crs(sps)=crs(BA)
# crs(sps2)=crs(BA)
# crs(sps3)=crs(BA)


# read in datasets ####
yr=2014
fordavid=read.csv("/Users/heatherwelch/Dropbox/Gulls/gull_data/WesternGull_tracks_forDavid.csv")%>% 
  mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S")) %>% 
  mutate(year=year(DateTime)) %>% filter(year==yr)

# df <- data.frame(xmin=-122.6, xmax=-122.2, ymin=37.9, ymax=38)
# df2<- data.frame(xmin=-122.53, xmax=-122.2, ymin=37.8, ymax=37.9)
# df3<- data.frame(xmin=-122.44, xmax=-122.10, ymin=37.4, ymax=37.8)
# 
# fordavid=fordavid1 %>% mutate(inout=case_when((Longitude<=df$xmin | Latitude<=df$ymin) ~"in",
#                                           TRUE ~"out")) %>% filter(inout=="in") %>%
#   mutate(inout=case_when((Longitude<=df2$xmin | Latitude<=df2$ymin) ~"in",
#                                           TRUE ~"out")) %>% filter(inout=="in") %>%
#   mutate(inout=case_when((Longitude<=df3$xmin | Latitude<=df3$ymin) ~"in",
#                                           TRUE ~"out")) %>% filter(inout=="in")


# coordinates(fordavid1)=~Longitude+Latitude
# crs(fordavid1)=crs(BA)
# fordavid2=erase.point(fordavid1,BA)
# fordavid3=erase.point(fordavid2,sps)
# fordavid4=erase.point(fordavid3,sps2)
# fordavid5=erase.point(fordavid4,sps3)
# fordavid=as.data.frame(fordavid5) 


ais_raw=read.csv(glue("/Users/heatherwelch/Dropbox/Gulls/sfbay_data/sfbay_ais_may_june_{yr}.csv"))%>% 
  mutate(time.GMT=substring(timestamp,first=1,last=19))%>% mutate(DateTime=parse_date_time(time.GMT,orders="%Y/%m/%d %H:%M:%S"))
# outdir="/Users/heatherwelch/Dropbox/Gulls/Plots/plots_05_26_21_reduced"#;dir.create(outdir)
outdir=("/Users/heatherwelch/Dropbox/Gulls/Plots/plots_06_09_21_2min_interp_thresh5min_0.1_interactions10_halfmoonclip");dir.create(outdir)
studyarea=st_read("/Users/heatherwelch/Dropbox/Gulls/shapefiles/us_medium_shoreline/us_medium_shoreline.shp")
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

tracks=unique(fordavid$tripID)

library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(8)

# test=foreach(i=36:36,.export = c("fordavid","yr","ais_raw","outdir","studyarea","myPalette"),.combine=rbind,.packages = c("glue","tidyverse","raster","wildlifeDI","adehabitatLT"),.verbose=T) %dopar%{
 test=foreach(i=1:length(tracks),.export = c("fordavid","yr","ais_raw","outdir","studyarea","myPalette","bayArea"),.combine=rbind,.packages = c("glue","tidyverse","raster","wildlifeDI","adehabitatLT"),.verbose=T) %dopar%{
   df <- data.frame(xmin=-122.6, xmax=-122.2, ymin=37.9, ymax=38)
   df2<- data.frame(xmin=-122.53, xmax=-122.2, ymin=37.8, ymax=37.9)
   # df3<- data.frame(xmin=-122.44, xmax=-122.10, ymin=37.4, ymax=37.8)
   df3<- data.frame(xmin=-122.5, xmax=-122.10, ymin=37.49, ymax=37.8)
   
   print(tracks[i])
  gull=fordavid %>% filter(tripID==tracks[i]) %>% mutate(row2=as.character(seq(1:nrow(.))))
  
  ais2=ais_raw %>% 
    filter(DateTime>=(min(gull$DateTime))&DateTime<=(max(gull$DateTime)))%>% 
    filter(lon>=min(gull$Longitude) & lon<=max(gull$Longitude)) %>% 
    filter(lat>=min(gull$Latitude) & lat<=max(gull$Latitude)) 
  
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
    
    ## finding interactions and removing the ones in the bay
    proxx=Prox(testais, testgull, tc=10*60, local=T,GetSimultaneous=T)%>% mutate(gull=tracks[i],vessel=ssvid1[ii])%>%
      left_join(.,gull) %>% left_join(.,ais_retimed,by="row1") %>% 
        mutate(inout=case_when((Longitude<=df$xmin | Latitude<=df$ymin) ~"in",
                                   TRUE ~"out")) %>% filter(inout=="in") %>%
        mutate(inout=case_when((Longitude<=df2$xmin | Latitude<=df2$ymin) ~"in",
                               TRUE ~"out")) %>% filter(inout=="in") %>%
        mutate(inout=case_when((Longitude<=df3$xmin | Latitude<=df3$ymin) ~"in",
                               TRUE ~"out")) %>% filter(inout=="in") %>% 
      dplyr::select(date1,row1,date2,row2,dt.x,prox,gull,vessel) %>% rename(dt=dt.x)
    
    proxx2=proxx %>% filter(prox<.01)
    
     if(nrow(proxx2)>1){
       proxx3=proxx2 %>% mutate(diffpoint = as.numeric(row2) - lag(as.numeric(row2), 1)) %>%  ## needs to be >3 to be new interaction
      mutate(new_interaction=case_when(diffpoint>10~glue("Start_{row1}"))) %>%mutate(new_interaction=as.character(new_interaction))
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

 write.csv(test,glue("{outdir}/prox_{yr}_5min_.01_int10.csv"))
 
