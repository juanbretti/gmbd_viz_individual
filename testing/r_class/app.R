# GMBD 2020 Intake
# Group E
#   Juan Pedro Bretti Mandarano
#   Nicolas Greull
#   Zhaoxue Li
#   Gauchet van Antwerpen
#   Asad Umar

# The applications takes a few minutes to load

## Libraries ----

# General purpose
library(tidyverse)
library(data.table)
library(lubridate)
# Shiny
library(shiny)
# Visualization
library(ggplot2)
library(ggpubr)
# Mapping
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(htmltools)
# Spatial
library(sp)
library(rgdal)
library(rgeos)

## Load data ----

# data_solar <- readRDS(file = file.path('r_class/data', 'solar_dataset.RData'))
# data_station <- fread(file = file.path('r_class/data', 'station_info.csv'))
data_solar <- readRDS(file = file.path('data', 'solar_dataset.RData'))
data_station <- fread(file = file.path('data', 'station_info.csv'))

## Transform data ----

# Source dataset
data_solar <- data_solar[j = Date2 := as.Date(x = Date, format = "%Y%m%d")]

# Add date conversions
data_solar <- data_solar %>% 
    mutate(Year = year(Date2),
           Month = month(Date2, label = TRUE),
           Year_Month = format(Date2, '%Y-%m'),
           Day = day(Date2),
           Day_Of_Year = yday(Date2),
           Day_Of_Week = wday(Date2, label = TRUE, week_start = 1),
           Weekend = ifelse(Day_Of_Week %in% c('Sat', 'Sun'), 'Weekend', 'Workday'),
           Days_Since_Origin = time_length(interval(origin, Date2), unit = 'day')) %>% 
    as.data.table(.)

# Columns defined from the enunciate
data_solar_col_produ <- colnames(data_solar)[2:99]
data_solar_col_predi <- colnames(data_solar)[100:456]
data_solar_col_dates <- setdiff(colnames(data_solar), c(data_solar_col_produ, data_solar_col_predi))

# Split train and test set
data_solar_train <- data_solar[i = 1:5113]
# data_solar_test <- data_solar[i = 5114:nrow(data_solar), j = .SD, .SDcols = c(data_solar_col_dates, data_solar_col_predi)]

# Positions for map ----
data_position <- data_solar_train %>% 
    select(-all_of(data_solar_col_predi)) %>% 
    pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
    group_by(WeatherStation) %>%
    summarise(ValueMean = tail(Value, 1)) %>% 
    left_join(data_station, by = c('WeatherStation' = 'stid'))

# Labels for the map ----
data_position$Label <- 
    paste('<strong>', data_position$WeatherStation, '</strong>', '<br/>', 
          'Elevation:', round(data_position$elon, 0), 'm', '<br/>',
          'Last production:', round(data_position$ValueMean/1e6, 1), 'million') %>% 
    lapply(HTML)

# Plot for the right div ----
ws_listing <- function(ws_clicked, ws_neig, head_ = 5){
    
    # To have a faster plot, limit the number of curves
    ws_ <- c(ws_clicked, head(ws_neig, head_))
    if(length(ws_neig) <= head_) {
        text_ <- paste(ws_, collapse = ', ')
    } else {
        text_ <- paste0(paste(ws_, collapse = ', '), '...')
    }
    
    return(list(
        ws = ws_, 
        text = text_
    ))
}

plot_left_div <- function(ws_clicked, data = data_solar_train){
    
    data <- data %>%
        dplyr::select(all_of(c(data_solar_col_dates, data_solar_col_produ))) %>%
        pivot_longer(cols = all_of(data_solar_col_produ), names_to = 'WeatherStation', values_to = 'Value') %>% 
        filter(`WeatherStation`==ws_clicked)
    
    p_all <- data %>%
        ggplot() +
        geom_smooth(aes(x = Date2, y = Value/1e6)) +
        labs(x = 'Date', y = 'Production in million')
    
    p_year <- data %>%
        ggplot(aes(x = Year, y = Value/1e6, group = Year)) +
        geom_boxplot() +
        labs(x = 'Year', y = 'Production in million') +
        ggpubr::rotate_x_text()
    
    p_month <- data %>%
        ggplot(aes(x = Month, y = Value/1e6)) +
        geom_boxplot() +
        labs(x = 'Month', y = '') +
        ggpubr::rotate_x_text()
    
    p_day_of_week <- data %>%
        ggplot(aes(x = Day_Of_Week, y = Value/1e6, fill = Weekend)) +
        geom_boxplot() +
        labs(x = 'Day of the week', y = '') +
        theme(legend.position = "none") +
        scale_fill_manual(values=c("gray80", "white")) +
        ggpubr::rotate_x_text()
    
    plot <- ggarrange(
        p_all,
        ggarrange(p_year, p_month, p_day_of_week, ncol = 3),
        nrow = 2
    )
    plot <- annotate_figure(plot, top = text_grob(unique(data$WeatherStation), face = "bold", size = 14))
    
    return(plot)
}

plot_right_div <- function(ws_clicked, ws_neig, data = data_solar_train){

    ws_listing_ <- ws_listing(ws_clicked, ws_neig)
    ws_ <- ws_listing_$ws
    text_ <- ws_listing_$text
    
    p_all <- data %>%
        pivot_longer(cols = all_of(ws_), names_to = 'WeatherStation', values_to = 'Value') %>% 
        ggplot(aes(x = Date2, y = Value/1e6, color = WeatherStation)) +
        geom_smooth() +
        labs(x = 'Date', y = 'Production in million') +
        theme(legend.position = "none")
    
    p_month <- data %>% 
        pivot_longer(cols = all_of(ws_), names_to = 'WeatherStation', values_to = 'Value') %>% 
        ggplot(aes(x = Month, y = Value/1e6, color = WeatherStation)) +
        geom_boxplot() +
        labs(x = 'Month', y = 'Production in million')

    plot <- ggarrange(p_all, p_month, nrow = 2)
    plot <- annotate_figure(plot, top = text_grob(text_, face = "bold", size = 14))

    return(plot)
}

# Creating the spatial dataset ----
#CRS
CRSLatLon<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ") #"+init=epsg:4326"
#http://spatialreference.org/ref/sr-org/7483/, WGS84 Web Mercator (Auxiliary Sphere) (Google, Spotfire)
CRSProj<-CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")

# Create the SpatialPointsDataFrame
WeatherStation_point<-SpatialPointsDataFrame(coords = data_station[, c('elon', 'nlat')], data = data_station, proj4string=CRSLatLon) %>% 
    spTransform(CRSProj)

# Calculate the closest weather stations
ws_distance <- function(spdf, ws, distance) {
    # Add distance to the weather station selected in 'ws'
    spdf@data$Distance <- as.numeric(gDistance(spgeom1 = subset(spdf, stid == ws), spgeom2 = spdf, byid=TRUE))
    # List of neighbors
    ws_neighbors_spdf <- subset(spdf, Distance != 0 & Distance <= distance)
    # Replicate the source of point
    ws_origin <- subset(spdf, stid == ws)
    ws_origin <- as.data.frame(ws_origin@coords)
    ws_origin <- bind_rows(replicate(length(ws_neighbors_spdf), ws_origin, simplify = FALSE))
    # Coords from the neighbors
    ws_destination <- ws_neighbors_spdf@coords
    # https://stackoverflow.com/questions/29287237/connect-xy-points-with-spatial-lines
    # Number of rows
    ws_number <- length(ws_neighbors_spdf)
    # If I have neighbors
    if (ws_number>0) {
        # Creation of the lines
        ws_lines <- vector("list", ws_number)
        for (i in 1:ws_number) {
            ws_lines[[i]] <- Lines(list(Line(rbind(ws_origin[i, ], ws_destination[i,]))), as.character(i))
        }
        ws_lines <- SpatialLines(ws_lines, proj4string = CRSProj) %>% 
            spTransform(CRSLatLon)
        # List of neighbors
        neig_stid <- ws_neighbors_spdf@data$stid
        neig_data <- subset(spdf, Distance <= distance)@data
    } else {
        ws_lines <- neig_stid <- neig_data <- NULL
    }
    
    return(list(
        neig_number = ws_number,
        lines = ws_lines,
        neig_stid = neig_stid,
        neig_data = neig_data
    ))
}

# Table for presentation ----
ws_table <- function(map, ws, range_min, range_max, data_solar = data_solar_train) {

    ws_stid <- c(map$id, ws$neig_stid)
    
    data_production <- data_solar %>%
        select(all_of(c(ws_stid, 'Year_Month', 'Date2'))) %>% 
        filter(between(Date2, as.Date(range_min), as.Date(range_max))) %>%
        pivot_longer(cols = all_of(ws_stid), names_to = 'WeatherStation', values_to = 'Value') %>%
        group_by(Year_Month, WeatherStation) %>% 
        summarise(Value = round(sum(Value)/1e6, 0)) %>% 
        pivot_wider(names_from = Year_Month, values_from = 'Value')

    data_wide <- ws$neig_data %>%
        select(stid, elev, Distance) %>% 
        mutate(Distance = Distance/1e3) %>% 
        mutate_at(vars('elev', 'Distance'), round) %>% 
        left_join(data_production, by = c('stid' = 'WeatherStation'), suffix = c("_Station", "_Production")) %>% 
        # arrange(Distance) %>% 
        rename('Weather Station' = stid, 'Elevation [m]' = elev, 'Distance [km]' = Distance)
    
    data_long <- data_solar %>%
        select(all_of(c('Date2', ws_stid))) %>% 
        rename(Date = Date2) %>% 
        filter(between(Date, as.Date(range_min), as.Date(range_max)))

    return(list(
        data_wide = data_wide,
        data_long = data_long
    ))
}


## App ----

## UI ----
ui <- fluidPage(
    navbarPage("Weather stations", id = "nav",
    tabPanel("Map",
            tags$head(includeCSS("styles.css")),
            div(class = "map_uppper", 
                leafletOutput(outputId = "map", width = "100%", height = "100%"),
                absolutePanel(id = "controls", class = "panel panel-default", 
                              top = 60, right = "26%", width = 330, fixed = TRUE, draggable = TRUE, bottom = "auto", height = "auto", left = "auto",
                              sliderInput(inputId = "distance", label = "Distance [km]", min = 10, max = 500, value = 100, step = 10, ticks = FALSE))
            ),
            div(class = "selection_plot", 
                plotOutput("selection_plot", height = '100%'),
            ),
            div(class = "neighbors_table_wide", 
                dateRangeInput(inputId = 'date_range', label = 'Filter by date', min = min(data_solar$Date2), max = max(data_solar$Date2), start = '2007-01-01', end = '2007-12-31'),
                DT::dataTableOutput(outputId = "neighbors_table_wide"),
            ),
            div(class = "neighbors_plot", 
                plotOutput("neighbors_plot", height = '100%'),
            ),
            tags$div(id="cite", 'IE, GMBD, Intake 2020, Group E')
    ),
    tabPanel("Data",
             htmlOutput('selection_text'),
             br(),
             DT::dataTableOutput(outputId = "neighbors_table_long")
    )
))

## Server ----
server <- function( input, output, session ){
    
    # First draw
    output$map <- renderLeaflet({
        leaflet(data = data_position) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addCircleMarkers(
                lng=~elon, lat=~nlat,
                radius = ~ValueMean/1e6, 
                label = ~Label,
                layerId = ~WeatherStation,
                group = 'data_solar') %>% 
            addHeatmap(
                lng = ~elon, lat = ~nlat,
                intensity = ~ValueMean,
                layerId = 'Heat',
                blur = 90, max = 1, radius = 60, minOpacity = 0.5)
    }) 

    # Check events over the map
    observeEvent(c(input$map_marker_click, input$distance), ignoreNULL = FALSE, ignoreInit = TRUE, {
        map_ <- input$map_marker_click
        dist_ <- input$distance
        # If there is any input
        if (!is.null(map_) & !is.null(dist_)) {
            # Calculate neighbors
            ws_ <- ws_distance(WeatherStation_point, map_$id, dist_*1e3)
            # Plot the lines, only if there are neighbors
            if (ws_$neig_number>0) {
                leaflet::leafletProxy(mapId = "map") %>%
                    clearGroup('lines') %>% 
                    clearGroup('DT_selected') %>% 
                    addPolygons(
                        data = ws_$lines,
                        opacity = 0.5,
                        group = 'lines') 
                # Table with the list of neighbors
                output$neighbors_table_wide <- DT::renderDataTable(ws_table(map_, ws_, input$date_range[1], input$date_range[2])$data_wide, rownames = FALSE, width = 0.9)
                output$neighbors_table_long <- DT::renderDataTable(ws_table(map_, ws_, input$date_range[1], input$date_range[2])$data_long, rownames = FALSE, width = 0.9, selection = 'none')
                # Plot
                output$selection_plot <- renderPlot(plot_left_div(map_$id))
                output$neighbors_plot <- renderPlot(plot_right_div(map_$id, ws_$neig_stid))
            }
            output$selection_text <- renderUI({
                str1 <- paste('Listing', '<strong>', ws_listing(map_$id, ws_$neig_stid)$text, '</strong>', 'weather stations')
                str2 <- paste('From', '<strong>', input$date_range[1], '</strong>', 'to', '<strong>', input$date_range[2], '</strong>')
                HTML(paste(str1, str2, sep = '<br/>'))
            })
        }
    })
    
    # Check events over the Data Table
    observeEvent(input$neighbors_table_wide_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
        map_ <- input$map_marker_click
        dist_ <- input$distance
        row_ <- input$neighbors_table_wide_rows_selected
        if (!is.null(row_)) {
            ws_ <- ws_distance(WeatherStation_point, map_$id, dist_*1e3)
            leaflet::leafletProxy(mapId = "map") %>%
                clearGroup('DT_selected') %>% 
                addCircleMarkers(
                    lng=ws_$neig_data$elon[row_], lat=ws_$neig_data$nlat[row_],
                    label = ws_$neig_data$stid[row_],
                    color = 'red',
                    group = 'DT_selected')
        } else {
            leaflet::leafletProxy(mapId = "map") %>%
                clearGroup('DT_selected')
        }
    })
} 

# Run the application 
shinyApp(ui = ui, server = server)
