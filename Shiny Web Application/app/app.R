# load packages
library(sf)
library(tmap)
library(leafpop)
library(leaflet)
library(tmaptools)
library(tidyverse)
library(plyr)
library(classInt)

# read in OSM
OSM <- st_read("data/gis_osm_pois_a_free_1.shp")

# read in Londonboroughs
Londonborough <- st_read("data/London_Borough_Excluding_MHW.shp")

# read in Airbnb
Airbnb <- read_csv("data/listings.csv")

# plot xy data
Airbnb <- st_as_sf(Airbnb, coords = c("longitude", "latitude"), 
                   crs = 4326)

# reproject
OSM <- st_transform(OSM, 27700)
Airbnb <- st_transform(Airbnb, 27700)
# we don't need to reproject Londonborough, but it 
# doesn't have a CRS..you could also use set_crs
# it needs to have one for the next step
Londonborough<- st_transform(Londonborough, 27700)

#select hotels only
OSM <- OSM[OSM$fclass == 'hotel',]
Airbnb <- Airbnb[Airbnb$room_type == 'Entire home/apt' &
                     Airbnb$availability_365=='365',]

# make a function for the join
# functions are covered in practical 7
# but see if you can work out what is going on
# hint all you have to do is replace data1 and data2
# with the data you want to use

Joinfun <- function(data1, data2) {
    # join OSM and London boroughs
    joined <- st_join(data1, data2, join = st_within)
    
    # count the number of hotels per borough
    countno <- as.data.frame(count(joined$GSS_CODE))
    
    # join the count back to the borough layer
    counted <-left_join(data2, countno, by=c("GSS_CODE"="x"))
    
    return(counted)
}

# use the function for hotels
Hotels <- Joinfun(OSM, Londonborough)

# then for airbnb
Airbnb <- Joinfun(Airbnb, Londonborough)

# now try to arrange the plots with tmap
breaks = c(0, 5, 12, 26, 57, 286) 

#change the column name from freq for the legend
colnames(Hotels)[colnames(Hotels)=="freq"] <- "Accom count"

#join data
ti<-st_join(Airbnb, Hotels, join=st_equals)
ti<-st_transform(ti,crs = 4326)

# change the names to match those in later selection
names(ti)[names(ti) == "freq"] <- "Airbnb"
names(ti)[names(ti) == "Accom count"] <- "Hotel"

# combine all the data (accomodation count) so we
# can make an appropraite colour range
accomall<-c(ti$`Hotel`,ti$Airbnb)
################################################## final data manipulation
library(shiny)
library(leaflet)
library(RColorBrewer)

# we will use this for our dropdown
choice=c("Hotel", "Airbnb")

# remove any NAs from our data and replace with 0
#as a function later on doesn't play ball with them
ti$Hotel[is.na(ti$Hotel)] <- 0
ti$Airbnb[is.na(ti$Airbnb)] <- 0
################################################## ui

# we'll use bootstrappage - a UI definition that can be passed to Shiny
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    # we're using leaflet and have title the outputID map
    # this will call it from our server function below
    leafletOutput("map", width = "100%", height = "100%"),
    # this sets our input panel placement
    absolutePanel(top = 10, right = 10,
                  # 
                  #our ID will be called later to make it interactive
                  selectInput(inputId = "Accom",
                              # label the drop down
                              label = "Accom type",
                              # what choices will there be
                              # this uses our choices list from
                              # earlier
                              choices = choice,
                              # Here False means you can only select
                              # one option
                              multiple = FALSE
                  ),
                  #gimme some colour options from colourbrewer
                  # here the inoutID is colourbrewerpalette
                  # the lavel is Color Scheme 
                  # rownames provides our choices
                  selectInput("colourbrewerpalette", "Color Scheme",
                              rownames(subset(brewer.pal.info, category %in% c("seq",
                                                                               "div")))
                  ),
                  # add a slider with the ID slide and label
                  # accomodation count
                  sliderInput("slide", "Accomodation count",
                              # min slider value
                              min(accomall, na.rm=TRUE),
                              # max slider value
                              max(accomall, na.rm = TRUE),
                              # range
                              value = range(accomall, na.rm = TRUE),
                              # increments
                              step = 10,
                              sep = ""
                  ),
                  # add a selection for type of data break
                  # ID of cassIntStyle and title
                  # Interval Style
                  selectInput("classIntStyle", "Interval Style",
                              c("Jenks Natural Breaks" = "jenks",
                                "Quantile" = "quantile",
                                "Equal Interval" = "equal",
                                "Pretty" = "pretty"))
    )
)
####################################### server
server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        # note we're using the ID of map calling back to leafletOutput
        # in the user interface
        leaflet(ti) %>% addTiles() %>% setView(-0.0881798, 51.48932, zoom = 10)
    })
    
    # obsever creates a reactive observer to then re-execute any changes
    observe({
        
        # filter the data of ti based on the range provided by the slider
        (ti2<-({ti[ti[[input$Accom]] >= input$slide[1] & ti[[input$Accom]] <= 
                       input$slide[2],]}))
        
        # here we are changing the class breaks using the selection of either
        # airbnb or hotel input$Accom uses what the user selects from
        # the drop down box
        breaks<-classIntervals(ti2[[input$Accom]], n=5, style=input$classIntStyle)
        breaks <- breaks$brks
        # make the color palette using ti (all of the data)
        pal <- colorBin(palette = input$colourbrewerpalette, 
                        domain = ti2[[input$Accom]],
                        bins = breaks
        )
        # map our filtered data from the slider (ti2)
        leafletProxy("map", data=ti2) %>%
            clearShapes() %>% 
            addPolygons(color="white", 
                        weight = 2,
                        opacity = 1,
                        dashArray = "3",
                        # add a popup of borough name and count based on
                        # the drop down of accomodation (hotel or airbnb)
                        # remember the ID we gave to that was Accom
                        popup = paste(ti2$NAME.y,"... ",ti2[[input$Accom]]),
                        fillOpacity = 0.5, 
                        fillColor = ~pal(ti2[[input$Accom]])
            )
    })
    
    observe({
        # call the filter again for this observer
        (ti2<-({ti[ti[[input$Accom]] >= input$slide[1] & ti[[input$Accom]] <=
                       input$slide[2],]}))
        
        # this observer follows the same pattern
        # but adds a legend 
        breaks<-classIntervals(ti2[[input$Accom]], n=5, style=input$classIntStyle)
        breaks <- breaks$brks
        
        pal <- colorBin(palette = input$colourbrewerpalette, 
                        domain = ti2[[input$Accom]],
                        #create bins using the breaks object from earlier
                        bins = breaks
        )
        # here is the Legend
        proxy <- leafletProxy("map", data = ti2)
        proxy %>% clearControls() %>%
            addLegend("bottomright", 
                      pal= pal, 
                      values = ~ti2[[input$Accom]], 
                      title = input$Accom, 
                      labFormat = labelFormat(prefix = ""),
                      opacity = 1
            )
    })
}
shinyApp(ui, server)