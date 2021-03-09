## extracting whales and eviro
library(raster)
library(stringr)
envdir="/Users/heatherwelch/Dropbox/benioff_operationalization/operationalization/daily_prediction_layers"
whaledir="/Users/heatherwelch/Dropbox/benioff_operationalization/operationalization/BenioffRuns/shiny-www"
  
e=extent(-123.86,-122,36.95,38.31)
 
## sst #### 
env=list.files(envdir,recursive = T,full.names = T)
env2=grep("sst.grd",env,value=T)
toMatchyear=c("2013","2014","2015","2016","2017","2018","2019")
env3=grep(paste(toMatchyear,collapse = "|"),env2,value=T) %>% grep("-05-",.,value=T)
env_names=gsub("/Users/heatherwelch/Dropbox/benioff_operationalization/operationalization/daily_prediction_layers/","",env3) %>% 
  gsub("/sst.grd","",.)
env4=env3 %>% raster::stack()
env5=crop(env4,e)
names(env5)=env_names
env6=cellStats(env5,mean)  %>% as.data.frame() %>% mutate(date=as.Date(env_names)) %>% rename("sst"=".") %>% 
  mutate(year=year(date)) %>% mutate(day=day(date)) %>% mutate(month=str_pad(month(date),2,"left","0")) %>% 
  mutate(date2=as.Date(glue("2013-{month}-{day}")))

ggplot(env6,aes(x=date2,y=sst,color=as.character(year),group=year))+geom_line()+
  theme_classic()

## whales #### 
env=list.files(whaledir,recursive = F,full.names = T)
env2=grep("grd",env,value=T)
toMatchyear=c("2013","2014","2015","2016","2017","2018","2019")
env3=grep(paste(toMatchyear,collapse = "|"),env2,value=T) %>% grep("-05-",.,value=T)
env_names=gsub("/Users/heatherwelch/Dropbox/benioff_operationalization/operationalization/BenioffRuns/shiny-www/blwh_ensemble_","",env3) %>% 
  gsub(".grd","",.)
env4=env3 %>% raster::stack()
env5=crop(env4,e)
names(env5)=env_names
env6=cellStats(env5,mean)  %>% as.data.frame() %>% mutate(date=as.Date(env_names)) %>% rename("whales"=".") %>% 
  mutate(year=year(date)) %>% mutate(day=day(date)) %>% mutate(month=str_pad(month(date),2,"left","0")) %>% 
  mutate(date2=as.Date(glue("2013-{month}-{day}")))

ggplot(env6,aes(x=date2,y=whales,color=as.character(year),group=year))+geom_line()+
  theme_classic()
