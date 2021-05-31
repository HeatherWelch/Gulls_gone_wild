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
outdir=("/Users/heatherwelch/Dropbox/Gulls/Plots/plots_05_26_21_reduced0.2");dir.create(outdir)
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
  testgull=as.ltraj(xy=xy,date=date,id=id)
 
  
  labels <- pretty(gull$DateTime, 5)
  
   empty=list()
  ssvid1=unique(ais2$ssvid)
   if(length(ssvid1)>0){
     # for(ii in 30:30){
      for(ii in 1:length(ssvid1)){
    print(ssvid1[ii])
       
    ais=ais2 %>% filter(ssvid==ssvid1[ii]) %>% mutate(row1=as.character(seq(1:nrow(.))))
    xy=ais %>% dplyr::select(lon,lat) %>% rename(X=lon,Y=lat)
    id=ais$Bird_ID
    id=ssvid1[ii]
    date=ais$DateTime
    testais=as.ltraj(xy=xy,date=date,id=id)
    
    proxx=Prox(testais, testgull, tc=5*60, dc=55000,local=T,GetSimultaneous=T)%>% mutate(gull=tracks[i],vessel=ssvid1[ii])
    proxx2=proxx %>% filter(prox<.02)
    
     if(nrow(proxx2)>1){
       proxx2=proxx2 %>% mutate(gull=tracks[i],vessel=ssvid1[ii]) %>% 
         left_join(.,gull) %>% left_join(.,ais,by="row1")
    a=ggplot()+
      geom_point(data=proxx2,aes(x=Longitude,y=Latitude),shape=2,color="black",size=4)+
      geom_point(data=gull,aes(x=Longitude,y=Latitude,color=DateTime),shape=2)+
      geom_path(data=gull,aes(x=Longitude,y=Latitude,color=DateTime))+
      geom_path(data=ais,aes(x=lon,y=lat,color=DateTime))+
      geom_point(data=proxx2,aes(x=lon,y=lat),color="black",size=4)+
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
    empty[[length(empty)+1]]=proxx
     }
   }
   master=do.call("rbind",empty)
   return(master)
 }

 write.csv(test,glue("{outdir}/prox_{yr}_5min_.02"))
 
#  ## tring new CD arguments ####
#  a=list.files(outdir,pattern="prox",full.names = T) %>% read.csv()
# master=lapply(a, read.csv)
#   master=do.call("rbind",master)
# 
#  outdir=("/Users/heatherwelch/Dropbox/Gulls/Plots/plots_05_26_21_reduced0.1");dir.create(outdir)
#   master.01=master %>% filter(prox<.01)
# 
#   tracks=unique(master.01$gull)
# 
#   library(foreach)
#   library(doParallel, quietly = TRUE)
#   registerDoParallel(8)
#   
# foreach(i=1:length(tracks),.export = c("master","master.01","outdir","studyarea","myPalette"),.combine=rbind,.packages = c("glue","tidyverse","raster","wildlifeDI","adehabitatLT"),.verbose=T) %dopar%{
#     print(tracks[i])
#    
#     gull_small=master.01 %>% filter(gull==tracks[i]) 
#     gull_tall=master %>% filter(gull==tracks[i]) 
#     
#     ssvid1=gull_small %>% pull(vessel) %>% unique()
#     vessel_tall=master %>% filter(vessel==ssvid1)
#     
#     labels <- pretty(gull$DateTime, 5)
#     
#     empty=list()
#   
#     if(length(ssvid1)>0){
#       # for(ii in 30:30){
#       for(ii in 1:length(ssvid1)){
#         print(ssvid1[ii])
#         
#           a=ggplot()+
#             geom_point(data=gull_small,aes(x=Longitude,y=Latitude),shape=2,color="black",size=4)+
#             geom_point(data=gull,aes(x=Longitude,y=Latitude,color=DateTime),shape=2)+
#             geom_path(data=gull,aes(x=Longitude,y=Latitude,color=DateTime))+
#             geom_path(data=ais,aes(x=lon,y=lat,color=DateTime))+
#             geom_point(data=proxx2,aes(x=lon,y=lat),color="black",size=4)+
#             geom_point(data=ais,aes(x=lon,y=lat,color=DateTime))+
#             scale_color_gradientn(colors=myPalette(100),breaks=labels, labels=labels)+
#             geom_sf(data=studyarea)+
#             ylim(min(gull$Latitude),max(gull$Latitude))+xlim(min(gull$Longitude),max(gull$Longitude))+
#             theme_classic()+
#             ggtitle(glue("Gulltrip {tracks[i]} ssvid {ssvid1[ii]}"))
#           
#           png(glue("{outdir}/vessels_gull_{tracks[i]}_{ssvid1[ii]}.png"),width=20,height=20,units='cm',res=400,type = "cairo")
#           par(ps=10)
#           par(mar=c(4,4,1,1))
#           par(cex=1)
#           # print({grid.arrange(a,b,ncol=2)})
#           print({a})
#           # gg_hm
#           dev.off()
#         }
#         empty[[length(empty)+1]]=proxx
#       }
#     }
#     master=do.call("rbind",empty)
#     return(master)
#   }
#   
  
