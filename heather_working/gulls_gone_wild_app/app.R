### Benioff to display blue whale predictions from two years

##### Defining global objects####
# source functions
source("load_libraries.R")
library(shinyalert)
library(shiny)
library(ggplot2)
library(magrittr)
library(lubridate)
library(glue)
library(shinydashboard)
library(leaflet)
library(tidyverse)

# studyarea=st_read("data/us_medium_shoreline/us_medium_shoreline.shp")
# bridges=st_read("data/bay_area_bridges/bayarea_bridges.shp")
trash=read.csv("data/lmopdataca_reduced.csv")
gulls=read.csv("data/gulls.csv")
gull_ids=unique(gulls$birdID)

megan_points= data.frame(thing=c("Pier 96 recology","recology sunset","oakland airport","SouthFI"),
                lat=c(37.741734,37.709994,37.713622,37.698321), 
                lon=c(-122.371846,-122.390314,-122.193076,-123.000313), 
                stringsAsFactors=FALSE) 
# #WP=readShapeSpatial("data/shp/WesternApproachSeparationZone.shp")
# projstring <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'
# WP=readOGR("data/shp/WesternPolygon.shp")
# WP <-spTransform(WP, CRS("+proj=longlat +datum=WGS84")) #convert from UTM to LatLon
# #ship_lane=readShapeSpatial("data/shp/WesternApproachShippingLanes.shp")
# #VSR_zone=readShapeSpatial("data/shp/WesternVSRZone.shp")
# TSS=readShapeSpatial("data/shp/TSSpolygon.shp")
# proj4string(TSS)=projstring
# #WP=readShapeSpatial("data/shp/WesternPolygon.shp")
# r2016="data/2016"
# r2009="data/2009"
# r2015="data/2015"

# times=seq(as.Date("2016/01/01"),as.Date("2016/12/31"),by="day") %>% lapply(.,function(x)gsub("2016-","",x)) %>% unlist() %>% 
#   lapply(.,function(x)gsub("-",".",x)) %>% unlist() %>% as.numeric()

#times=seq(as.Date("2016/01/01"),as.Date("2016/12/31"),by="day") %>% lapply(.,function(x)gsub("2016-","",x)) %>% unlist() #%>% 
# 
# months=seq(1,12,by=1)%>% str_pad(.,2,pad="0")
# days=seq(1,31,by=1)%>% str_pad(.,2,pad="0")


ui <- dashboardPage(skin="blue",
                    dashboardHeader(
                      title = "GULL VIZ",
                      titleWidth = 420
                    ),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(id = 'sidebarmenu',
                                  #div(style="text-align:center",tags$div(img(src='blwh_icon3.png',width="40%"))),
                                  menuItem("Select individual", tabName='date',icon=icon("clock-o",lib='font-awesome')),
                                    # column(selectInput("months","Month",months),width = 6),
                                    # column(selectInput("days","Day",days),width = 6)),
                                    selectInput("id", label = 'Bird ID',gull_ids))),
                    
                    dashboardBody(
                      fluidRow(
                        column(h4(htmlOutput("dt2")),width=12),
                        column(h4(div(style="text-align:center",tags$b("2009"))),width=12,leafletOutput("map09"))
                        
                      )
                    ))



server <- shinyServer(function(input, output) {
  
  bight_ext<-list()
  bight_ext$lon<--122.5
  bight_ext$lat<-37.35
  bight_ext$zoom<-7
  
  output$dt2=renderText({
    display=input$id
    paste("<b>Bird ID is ",display,"</b>")
    #HTML(paste(a))
  })
  
  output$map09 <- renderLeaflet({
    
    
    
    bird=as.integer(input$id)
    dat=gulls %>% dplyr::filter(birdID==bird) 
    # dat=gulls %>% dplyr::filter(birdID==gull_ids[1])


    pal <- colorFactor(
      palette = 'Spectral',
      domain = as.character(dat$date)
    )

    lmap <- leaflet()
    lmap <- setView(lmap, bight_ext$lon, bight_ext$lat, zoom=bight_ext$zoom)
    lmap <- addProviderTiles(lmap, "Esri.WorldStreetMap")
    lmap <-addCircleMarkers(lmap,data=megan_points,lat=megan_points$lat,lng=megan_points$lon,radius=3,color = "pink",opacity=1)
    lmap <-addCircleMarkers(lmap,data=dat,lat=dat$Latitude,lng=dat$Longitude,radius=1,color = ~pal(date))
     lmap <-addCircleMarkers(lmap,data=trash,lat=trash$Latitude,lng=trash$Longitude,radius=1,color = "black")
   
 
    # lmap <- addRasterImage(lmap,habsuit,colors=palette2)
     lmap <- addLegend(lmap, "topright", values = unique(as.character(dat$date)),pal=pal,title="Date")
     lmap
    

    
  })
  
  
  })


shinyApp(ui = ui, server = server)