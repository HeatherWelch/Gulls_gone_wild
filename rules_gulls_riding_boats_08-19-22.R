library(tidyverse)
library(geosphere)
# dat=read.csv("/Users/heatherwelch/Dropbox/Gulls/Plots/plots_08_19_22_2min_interp_thresh5min_0.1_interactions10_halfmoonclip/prox_2014_10min_.01_int10.csv")

dat <- list.files(path="/Users/heatherwelch/Dropbox/Gulls/Plots/plots_08_19_22_2min_interp_thresh5min_0.1_interactions10_halfmoonclip", full.names = TRUE, pattern=".csv")  %>% 
    lapply(read_csv) %>%
    bind_rows
## finding bearing
test=bearing(dat[,c("Gull_Longitude","Gull_Latitude")])
dat2=dat %>% mutate(bearing_gull=bearing(dat[,c("Gull_Longitude","Gull_Latitude")])) %>% 
  mutate(bearing_vessel=bearing(dat[,c("Vessel_Longitude","Vessel_Latitude")])) 

## finding speed
test=dat2$Vessel_datetime %>% as.POSIXct()

dat3=dat2 %>% mutate(Vessel_datetime=as.POSIXct(Vessel_datetime),
                     Gull_datetime=as.POSIXct(Gull_datetime)) %>% 
  group_by(tripID,ssvid,new_interaction) %>% 
  mutate(vessel_datetime_diff=difftime(Vessel_datetime,lag(Vessel_datetime, 1),units = "secs")) %>% 
  mutate(vessel_datetime_diff=as.numeric(vessel_datetime_diff))%>% 
  mutate(Gull_datetime_diff=difftime(Gull_datetime,lag(Gull_datetime, 1),units = "secs")) %>% 
  mutate(Gull_datetime_diff=as.numeric(Gull_datetime_diff))

## ignore above for finding speed, it's all 120
dat3=dat2 %>% 
  mutate(gull_speed=dist_gull/time_difference) %>% 
  mutate(ais_speed=dist_ais/time_difference)

dat4=dat3 %>% 
  dplyr::select(c(bearing_vessel,bearing_gull,gull_speed,ais_speed,
                  tripID,ssvid,new_interaction)) %>% 
  group_by(tripID,ssvid,new_interaction) %>% 
  mutate(interaction_id= cur_group_id()) %>% ungroup() %>% 
  mutate(diff_speed=gull_speed-ais_speed) %>% 
  mutate(diff_bearing=as.numeric(bearing_gull)-as.numeric(bearing_vessel)) %>% 
  group_by(tripID,ssvid,new_interaction) %>% 
  mutate(mean_bearing_gull=mean(bearing_gull,na.rm=T)) %>% 
  mutate(mean_bearing_vessel=mean(bearing_vessel,na.rm=T)) %>% 
  mutate(mean_gull_speed=mean(gull_speed,na.rm=T)) %>% 
  mutate(mean_ais_speed=mean(ais_speed,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(mean_bearing_diff=mean_bearing_gull-mean_bearing_vessel) %>% 
  mutate(mean_speed_diff=mean_gull_speed-mean_ais_speed) 

### testing filters
dat5=dat4 %>% filter(abs(mean_bearing_diff)<.6) %>% 
  filter(abs(mean_speed_diff)<0.00001)
dat4 %>% filter(tripID=="2018220246") %>% as.data.frame()
dat4 %>% filter(tripID=="2018220244") %>% as.data.frame()

## testing about removing first few points, not gonna do this
dat4=dat3 %>% 
  group_by(tripID,ssvid,new_interaction) %>% 
  mutate(interaction_id= cur_group_id()) %>% ungroup() %>% 
  mutate(diff_speed=gull_speed-ais_speed) %>% 
  mutate(diff_bearing=as.numeric(bearing_gull)-as.numeric(bearing_vessel)) %>% 
  group_by(tripID,ssvid,new_interaction) %>% 
  arrange(Gull_datetime) %>% 
  mutate(time_order=1:n()) %>%
  group_by(tripID,ssvid,new_interaction) %>% 
arrange(rev(Gull_datetime)) %>% 
  mutate(time_order_rev=1:n()) %>% 
  filter(time_order>1,time_order_rev>1) %>% 
  mutate(mean_bearing_gull=mean(bearing_gull,na.rm=T)) %>% 
  mutate(mean_bearing_vessel=mean(bearing_vessel,na.rm=T)) %>% 
  mutate(mean_gull_speed=mean(gull_speed,na.rm=T)) %>% 
  mutate(mean_ais_speed=mean(ais_speed,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(mean_bearing_diff=mean_bearing_gull-mean_bearing_vessel) %>% 
  mutate(mean_speed_diff=mean_gull_speed-mean_ais_speed) %>% 
  dplyr::select(c(bearing_vessel,bearing_gull,diff_bearing,gull_speed,ais_speed,interaction_id,
                  tripID,ssvid,new_interaction,mean_speed_diff,mean_bearing_diff,mean_gull_speed,mean_ais_speed,mean_bearing_gull,mean_bearing_vessel))

dat5=dat4 %>% filter(abs(mean_bearing_diff)<.6) %>% 
  filter(abs(mean_speed_diff)<0.00001)

dat4 %>% filter(tripID=="2018220246") %>% as.data.frame()
dat4 %>% filter(tripID=="2018220244") %>% as.data.frame()
