###############################################
## Author: Amy Andrews                       ##
## Purpose: wildlife mapping dashboard       ##
## Date: 02/2020                             ##
###############################################

rm(list = ls()) # important to clear stuff

# This is a Shiny web application. 
# You can run the application by clicking
# the 'Run App' button above. 

# make sure you have all these packages installed
# before you run the script, using 'install.packages()'
# make sure to put the package name in quotes!
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(leaflet)
library(lubridate)
library(DT)
library(shinyjs)
library(rgdal)
library(RColorBrewer)

# read in and prepare the data
# this means read the correct sheet
# and clean spelling of the animal names
# and change the date column to a date format
# in a year, month and day column
# then select only the columns we want to show
# you'll have to change the file path
fn <- readline(prompt="Enter filename:" )
sn <- readline(prompt="Enter sheetname:" )
lk_data <- readxl::read_xls(fn, 
                            sheet=sn,
                            skip = 1) %>%
    mutate(Species = ifelse(Species == "Greater kudu", "Greater Kudu", Species)) %>%
    mutate(Species = ifelse(Species == "Water buck" | Species == "water buck" | Species == "Water Buck", "Waterbuck", Species)) %>%
    mutate(Species = ifelse(Species == "Spotted Hyaena", "Spotted Hyena", Species)) %>%
    mutate(Species = ifelse(Species == "Striped hyena" | Species == "striped hyeana", "Striped Hyena", Species)) %>%
    mutate(Species = ifelse(Species == "lesser kudu" | Species == "Lesser kudu", "Lesser Kudu", Species)) %>%
    mutate(Species = ifelse(Species == "klipspringer", "Klipspringer", Species)) %>%
    mutate(Species = ifelse(Species == "Bushback", "Bushbuck", Species)) %>%
    mutate(Species = ifelse(Species == "common zebra", "Common Zebra", Species)) %>%
    mutate(Species = as.factor(Species)) %>%
    mutate(Date = lubridate::ymd(Date)) %>%
    mutate(Year = lubridate::year(Date),
           Month = lubridate::month(Date),
           Day = lubridate::day(Date),
           YearMonth = paste0(Year, "-", Month)) %>%
     dplyr::select(Species, Location, Number = NumIndiv, UTMX, UTMY, Year, Month, Day) %>%
    mutate(Date = dmy(paste0("01-",Month,"-",Year))) # make date for animated map

# get the vector of names for
# species filter - pull these
# unique names out
species <- lk_data %>%
    dplyr::select(Species) %>%
    mutate(Species = as.character(Species)) %>%
    unique() %>%
    pull(Species)


# do some geospatial conversion
# take the utm coordinates and change
# them to long.lat geographic coordinates
# using library rgdal - UTMX to lat, UTMY to long
utms <- SpatialPoints(lk_data[, c("UTMX", "UTMY")], # select just the UTM cols
                      proj4string=CRS("+proj=utm +zone=37")) #create UTM matrix

# get an object for the coordinates
longlats <- spTransform(utms, CRS("+proj=longlat")) #transform

# append new coordinates back to lekurruki data 
lk_data$long <- longlats@coords[,1]
lk_data$lat <- longlats@coords[,2]


# create a colour palette
# for our 27 animals!
n <- 27
set.seed(n)
colours <- colorRampPalette(c("blue", "red", "green", "yellow", "brown", "purple", "orange", "turquoise", "pink"))(n)
# check the colours if you like
# pie(rep(1,n), col=sample(colours, n))

# this is what we'll use in the map
pal <- colorFactor(palette = colours, domain = lk_data$Species)

# Define UI for application
# This is what the application looks like
# when it is run
ui <- dashboardPage(
    
    # can put some help text here
    
    # the title
    dashboardHeader(title = "Lekurruki dashboard"),
    
    # the sidebar for filters
    dashboardSidebar(
        
        # a particular type of drop down
        # which allows us to do 'select all'
        # and 'deselect all' neatly
        pickerInput(inputId = "species_filter", label = "Filter by Species", 
                    choices = species, multiple = TRUE, selected = species,
                    options = list(`actions-box` = TRUE)),
        
        # slider for Year
        sliderInput(inputId = "year_range", label = "Year Range",
                       min = min(lk_data$Year),
                       max = max(lk_data$Year), 
                    value = c(min(lk_data$Year), max(lk_data$Year)),
                    step = 1,
                    sep = ""), # define sep to stop commas in numbers
        
        # slider for Month
        sliderInput(inputId = "month_range", label = "Month Range",
                    min = min(lk_data$Month),
                    max = max(lk_data$Month), 
                    value = c(min(lk_data$Month), max(lk_data$Month)),
                    step = 1,
                    sep = ""), 
        
        # slider for Day
        sliderInput(inputId = "day_range", label = "Day Range",
                    min = min(lk_data$Day),
                    max = max(lk_data$Day), 
                    value = c(min(lk_data$Day), max(lk_data$Day)),
                    step = 1,
                    sep = "")
        
    ),
    
    # the main bit of the dashboard
    dashboardBody(
        
        # have to include this just incase 
        # we want to use javascript add-on elements
        useShinyjs(),
        
        tabsetPanel(
            
            # our first tab has the map in it
            tabPanel("Map", 
                     
                     # this is just so the map is at 100% of the tab 
                     tags$style(type = "text/css", "#animal_map {height: calc(100vh - 80px) !important;}"),
                     
                     # plotting all our things on a map
                     # with loading spinner included 
                     leafletOutput("animal_map") %>% 
                         withSpinner(color="#0dc5c1")
                     
                     ), # end of first tab
                
            # our second tab has the animated map in it
            # and also the animation timeline   
                tabPanel("Animated Map", 
                         
                         # slider for the timeseries as defined by year and month slider
                         uiOutput("timeseries"),
                         
                         # this is just so the map is at 100% of the tab 
                         tags$style(type = "text/css", "#animated_map {height: calc(100vh - 80px) !important;}"),
                         
                         # plotting all our things on a map
                         # with loading spinner included 
                         leafletOutput("animated_map") %>% 
                             withSpinner(color="#0dc5c1")
                         
                ), # end of second tab
            
            # our third tab just has a table of data in it
            tabPanel("Data", 
                     
                     # this is our table
                     # with loading spinner included
                     DT::dataTableOutput("lk_data") %>% 
                         withSpinner(color="#0dc5c1")
                     
                     ) # end of third tab
            
        ) # end of tabsetpanel
    
        ) # end of dashboardbody
    ) # end of dashboardpage 



# Define server for application
# this is all the operations that the 
# app carries out in the background
# when you interact with it
server <- function(input, output, session) {
    
    # define the data as filtered
    # by the user interacting with
    # our filters
    data_reactive <- reactive({
        lk_data %>%
            filter(Species %in% input$species_filter) %>%
            filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>%
            filter(Month >= input$month_range[1] & Month <= input$month_range[2]) %>%
            filter(Day >= input$day_range[1] & Day <= input$day_range[2]) 
    })
    
    

    # output for the datatable 
    output$lk_data <- DT::renderDataTable({
        
        # use our reactive filtered data
        # to make a table
        # so updates with user filtering
        data_reactive() %>%
            select(-Date) # take out to not confuse things
    })
    
    # output for normal non-animated map
    output$animal_map <- renderLeaflet({
        
        # use our reactive filtered data
        # to plot on the map
        # so updates with user filtering
        # for the radius, if there's less
        # than or equal to 5 sightings, have 
        # a small dot. If there are between
        # 6 and 20 or equal to 20 sightings,
        # have a medium dot. If there are more
        # then have the biggest size dot
        data_reactive() %>%
        leaflet() %>%
            setView(lng = 37.30590, lat = 0.400, zoom = 10) %>%
            addTiles() %>%
            addCircleMarkers(~long, ~lat, 
                             popup = ~paste0("Species: ", Species, ", Number: ", Number, ", Date: ", Year, ".", Month, ".", Day), 
                             label = ~paste0("Species: ", Species, ", Number: ", Number, ", Date: ", Year, ".", Month, ".", Day), 
                             radius = ifelse(data_reactive()$Number <= 5, 3, 
                                             ifelse(data_reactive()$Number > 5 & data_reactive()$Number <= 20, 6, 10)),
                             stroke = FALSE, 
                             fillOpacity = 0.5,
                             color = ~pal(Species)) %>%
            addLegend(pal = pal, values = ~species, opacity = 1)
    })
    
    
    # define reactive vector for the timeseries animation
    # depends on the input of year and month as to what the
    # vector of dates comes out as
    ts_vec <- reactive({
        seq.Date(from = min(data_reactive()$Date),
                 to = max(data_reactive()$Date),
                 by = "month"
        )
    })

    # output for the animation slider
    # using ts_vec()
    output$timeseries <- renderUI({

        # have the positions selected at
        # each range extreme already
        sliderTextInput(inputId = "timeseries",
                        label = "Animate over time",
                        choices = ts_vec(),
                        selected = ts_vec()[1],
                        # selected = ts_vec()[c(1,length(ts_vec()))]
                        animate = TRUE,
                        width = "100%")

    })


    # define a second lot of reactive data
    # to be used by the time series vector
    # which is grouped in a summary table by month
    # - so discounting day
    ts_data <- reactive({

        # data for timeseries
        # make day = 1 for all months
        # aggregate by number count
        # this will aggregate our counts by
        # month and year, so that if say elephants
        # were seen in the same place every day of the
        # month, now that number is added all together
        # to produce a total figure for the whole month
        month_totals <- aggregate(data_reactive()$Number, by=list(data_reactive()$Species,
                                                          data_reactive()$Location,
                                                          data_reactive()$long,
                                                          data_reactive()$lat,
                                                          data_reactive()$Date,
                                                          data_reactive()$Year,
                                                          data_reactive()$Month), FUN=sum)

        # assign header names again
        names(month_totals) <- c("Species", "Location", "long", "lat",
                                 "Date", "Year", "Month", "Number")

        # something annoying going on with this bit
        month_totals %>%
        filter(Date == input$timeseries[1])
        # filter(Date >= input$timeseries[1] & Date <= input$timeseries[2])

    })


    # output for animated map
    output$animated_map <- renderLeaflet({

        # for the radius, if there's less
        # than or equal to 5 sightings, have
        # a small dot. If there are between
        # 6 and 20 or equal to 20 sightings,
        # have a medium dot. If there are more
        # then have the biggest size dot
        ts_data() %>%
            leaflet() %>%
            setView(lng = 37.30590, lat = 0.400, zoom = 10) %>%
            addTiles() %>%
            addCircleMarkers(~long, ~lat,
                             popup = ~paste0("Species: ", Species, ", Number: ", Number, ", Date: ", Year, ".", Month),
                             label = ~paste0("Species: ", Species, ", Number: ", Number, ", Date: ", Year, ".", Month),
                             radius = ifelse(ts_data()$Number <= 5, 3,
                                             ifelse(ts_data()$Number > 5 & ts_data()$Number <= 20, 6, 10)),
                             stroke = FALSE,
                             fillOpacity = 0.5,
                             color = ~pal(Species)) %>%
            addLegend(pal = pal, values = ~species, opacity = 1)
    })
    
    
    
} # end of server


# Run the application by putting
# the ui and server together
shinyApp(ui = ui, server = server)
