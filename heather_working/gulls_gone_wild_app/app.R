### Benioff to display blue whale predictions from two years

##### Defining global objects####
# source functions
# source("load_libraries.R")
library(shinyalert)
library(shiny)
library(ggplot2)
library(magrittr)
library(lubridate)
library(glue)
library(shinydashboard)
library(leaflet)
library(tidyverse)
# library(viridis)

# studyarea=st_read("data/us_medium_shoreline/us_medium_shoreline.shp")
# bridges=st_read("data/bay_area_bridges/bayarea_bridges.shp")
trash=read.csv("data/lmopdataca_reduced.csv")
gulls=read.csv("data/gulls.csv") %>% mutate(day=yday(date)) %>% mutate(date=as.Date(date))
gull_ids=unique(gulls$birdID)
whales=read.csv("data/OBIS_whales.csv") %>% mutate(date=as.Date(date))


megan_points= data.frame(thing=c("Pier 96 recology","recology sunset","oakland airport","SouthFI"),
                lat=c(37.741734,37.709994,37.713622,37.698321), 
                lon=c(-122.371846,-122.390314,-122.193076,-123.000313), 
                stringsAsFactors=FALSE) 


ui <- dashboardPage(skin="blue",
                    dashboardHeader(
                      title = "GULL VIZ",
                      titleWidth = 420
                    ),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(id = 'sidebarmenu',
                                  #div(style="text-align:center",tags$div(img(src='blwh_icon3.png',width="40%"))),
                                  menuItem("View by individual", tabName='date',icon=icon("clock-o",lib='font-awesome')),
                                  conditionalPanel("input.sidebarmenu ==='date'",
                                    # column(selectInput("months","Month",months),width = 6),
                                    # column(selectInput("days","Day",days),width = 6)),
                                    selectInput("id", label = 'Bird ID',gull_ids)),
                      menuItem("View by date + whales", tabName='date2',icon=icon("clock-o",lib='font-awesome')),
                      conditionalPanel("input.sidebarmenu ==='date2'",
                                    sliderInput("dateSel", "Date",min=min(gulls$date),max=max(gulls$date),value=min(gulls$date),animate = animationOptions(interval = 500, loop = T))
                                  ))),
                    
                    dashboardBody(
                      
                        tabItems(
                          tabItem(tabName = "date",
                                  fluidRow(
                        column(h4(htmlOutput("dt2")),width=12),
                        column(h4(div(style="text-align:center",tags$b("Data"))),width=12,leafletOutput("map09"))
                          )),
                        tabItem(tabName = "date2",
                                fluidRow(
                                  column(h4(htmlOutput("dt3")),width=12),
                                  column(h4(div(style="text-align:center",tags$b("Data"))),width=12,leafletOutput("map10"))
                                ))
                        
                      )
                    ))



server <- shinyServer(function(input, output) {
  
  bight_ext<-list()
  bight_ext$lon<--122.8
  bight_ext$lat<-37.75
  bight_ext$zoom<-10
  
  output$dt2=renderText({
    display=input$id
    paste("<b>Bird ID is ",display,"</b>")
    #HTML(paste(a))
  })
  
  output$dt3=renderText({
    display=input$dateSel
    paste("<b>Date shown is ",display,"</b>")
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
  
  gettingThegulls=reactive({
    req(input$dateSel)
  gulls %>% dplyr::filter(date==input$dateSel)
  })
  
  gettingThewhales=reactive({
    req(input$dateSel)
    whales %>% dplyr::filter(date==input$dateSel)
  })
  
  output$map10 <- renderLeaflet({
    
    
    # 
    # bird=as.integer(input$dateSel)
    # dat=gulls %>% dplyr::filter(date==bird) 

    
    
    # pal <- colorFactor(
    #   palette = 'Spectral',
    #   domain = as.character(dat$date)
    # )
    
    # lmap <- leaflet()
    # lmap <- setView(lmap, bight_ext$lon, bight_ext$lat, zoom=bight_ext$zoom)
    # lmap <- addProviderTiles(lmap, "Esri.WorldStreetMap")
    # lmap <-addCircleMarkers(lmap,data=megan_points,lat=megan_points$lat,lng=megan_points$lon,radius=3,color = "pink",opacity=1)
    # # lmap <-addCircleMarkers(lmap,data=dat,lat=dat$Latitude,lng=dat$Longitude,radius=1,color = ~pal(date))
    # lmap <-addCircleMarkers(lmap,data=trash,lat=trash$Latitude,lng=trash$Longitude,radius=1,color = "black")
    
    leaflet() %>% 
    setView(bight_ext$lon, bight_ext$lat, zoom=bight_ext$zoom) %>% 
    addProviderTiles( "Esri.WorldStreetMap") %>% 
    addCircleMarkers(data=megan_points,lat=megan_points$lat,lng=megan_points$lon,radius=3,color = "pink",opacity=1) %>% 
    # lmap <-addCircleMarkers(lmap,data=dat,lat=dat$Latitude,lng=dat$Longitude,radius=1,color = ~pal(date))
    addCircleMarkers(data=trash,lat=trash$Latitude,lng=trash$Longitude,radius=1,color = "black") 
    
    
   
    # lmap <- addLegend(lmap, "topright", values = unique(as.character(dat$date)),pal=pal,title="Date")
    # lmap
    
    
    
  })
  
  observe({

    pal <- colorFactor(
      palette = 'Spectral',
      domain = as.character(gettingThegulls()$birdID)
    )

    leafletProxy("map10",data=gettingThegulls()) %>%
      clearGroup(group="gulls") %>%
      addCircleMarkers(group="gulls",lat=~Latitude,lng=~Longitude,radius=1,color = ~pal(birdID))
  })
  
  observe({

    ## retained stuff for adding color
    # pal <- colorFactor(
    #   palette = colorRampPalette(rainbow(10))(length(gettingThewhales()$common)), 
    #   domain = gettingThewhales()$common)
    # 
    # leafletProxy("map10",data=gettingThewhales()) %>%
    #   clearGroup(group="whales") %>%
    #   addCircleMarkers(group="whales",lat=~latitude,lng=~longitude,radius=2,color=~pal(common))
    
    leafletProxy("map10",data=gettingThewhales()) %>%
      clearGroup(group="whales") %>%
      addMarkers(group="whales",lat=~latitude,lng=~longitude, popup = ~as.character(common))
                      
  })
  

observe({

  pal <- colorFactor(
    palette = 'Spectral',
    domain = as.character(gettingThegulls()$birdID)
  )

  leafletProxy("map10",data=gettingThegulls()) %>%
    clearControls() %>%
    addLegend("topright", values = ~birdID,pal=pal,title="Bird ID")
})

})


shinyApp(ui = ui, server = server)

# data(quakes)
# 
# pal <- colorFactor(
#   palette = colorRampPalette(rainbow(2)),
#   domain = whales$common)
# 
# # Show first 20 rows from the `quakes` dataset
# leaflet(data = whales[1:20,]) %>% addTiles() %>%
#   addMarkers(~longitude, ~latitude, popup = ~as.character(common), label = ~as.character(common))
