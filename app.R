# GMBD 2020 Intake
#   Addison Pelayo
#   Juan Pedro Bretti Mandarano

## Libraries ----

# General purpose
library(tidyverse)
library(data.table)
library(lubridate)
library(readxl)
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

# Load data ----
df_data <- read_csv(unz("source/WDI_csv.zip", "WDIData.csv"))
df_country <- read_csv(unz("source/WDI_csv.zip", "WDICountry.csv"))
df_country_lat_lon <- read_tsv('source/country latitude longitude name.tsv')

indicators_ <- c('Poverty headcount ratio at national poverty lines (% of population)',
    'People using safely managed drinking water services (% of population)',
    'Community health workers (per 1,000 people)',
    'People using at least basic sanitation services (% of population)',
    'Literacy rate, adult total (% of people ages 15 and above)',
    'Net migration',
    'Public private partnerships investment in transport (current US$)')

columns_ <- c('Country Name', 'Indicator Name', 'Year', 'Value', 'Region', 'Income Group', 'latitude', 'longitude')

# Prepare data ----
df_data2 <- df_data %>% 
    filter(`Indicator Name` %in% indicators_) %>% 
    pivot_longer(starts_with(c('1', '2')), names_to='Year', values_to='Value') %>% 
    filter(!is.na(`Value`)) %>% 
    mutate(`Year` = as.numeric(`Year`)) %>% 
    inner_join(df_country, by='Country Code') %>% 
    inner_join(df_country_lat_lon, by=c('2-alpha code'='country')) %>% 
    select(!!columns_)

# Positions for map ----
data_position <- df_data2 %>% 
    filter(`Indicator Name` == 'Net migration') %>% 
    group_by(`Country Name`) %>%
    top_n(n=1, wt=`Year`)

# Labels for the map ----
data_position$Label <- 
    paste('<strong>', data_position[['Country Name']], '</strong>', '<br/>', 
          'Data year:', data_position[['Year']], '</strong>', '<br/>', 
          'Last Indicator:', round(data_position$Value, 0), 'units') %>% 
    lapply(HTML)

# Plots ----
plot_top_right_div <- function(country_clicked, indicator, data = df_data2){
    
    data <- data %>%
        filter(`Country Name`==country_clicked,
               `Indicator Name`==indicator)
    
    p_all <- data %>%
        ggplot() +
        geom_smooth(aes(x = Year, y = Value)) +
        labs(x = 'Date', y = 'Value')
    
    plot <- ggarrange(p_all, nrow = 1)
    plot <- annotate_figure(plot, top = text_grob(country_clicked, face = "bold", size = 14))
    
    return(plot)
}

country_listing <- function(country_clicked, country_neig, head_ = 5){
    
    # To have a faster plot, limit the number of curves
    country_ <- c(country_clicked, head(country_neig, head_))
    if(length(country_neig) <= head_) {
        text_ <- paste(country_, collapse = ', ')
    } else {
        text_ <- paste0(paste(country_, collapse = ', '), '...')
    }
    
    return(list(
        country = country_, 
        text = text_
    ))
}

plot_bottom_right_div <- function(country_clicked, country_neig, data = df_data2){

    country_listing_ <- country_listing(country_clicked, country_neig)
    country_ <- country_listing_$country
    text_ <- country_listing_$text
    
    p_all <- data %>%
        ggplot(aes(x = `Year`, y = `Value`, color = `Country Name`)) +
        geom_smooth() +
        labs(x = 'Year', y = 'Value') +
        theme(legend.position = "none")
    
    plot <- ggarrange(p_all, nrow = 1)
    plot <- annotate_figure(plot, top = text_grob(text_, face = "bold", size = 14))

    return(plot)
}

# Creating the spatial dataset ----
#CRS
CRSLatLon<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ") #"+init=epsg:4326"
#http://spatialreference.org/ref/sr-org/7483/, WGS84 Web Mercator (Auxiliary Sphere) (Google, Spotfire)
CRSProj<-CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")

# Create the SpatialPointsDataFrame
country_point<-SpatialPointsDataFrame(coords = data_position[, c('longitude', 'latitude')], data = data_position, proj4string=CRSLatLon) %>% 
    spTransform(CRSProj)

# Calculate the closest weather stations
country_distance <- function(spdf, country, distance) {
    # Add distance to the weather station selected in 'country'
    spdf@data$Distance <- as.numeric(gDistance(spgeom1 = subset(spdf, `Country Name` == country), spgeom2 = spdf, byid=TRUE))
    # List of neighbors
    country_neighbors_spdf <- subset(spdf, Distance != 0 & Distance <= distance)
    # Replicate the source of point
    country_origin <- subset(spdf, `Country Name` == country)
    country_origin <- as.data.frame(country_origin@coords)
    country_origin <- bind_rows(replicate(length(country_neighbors_spdf), country_origin, simplify = FALSE))
    # Coords from the neighbors
    country_destination <- country_neighbors_spdf@coords
    # https://stackoverflow.com/questions/29287237/connect-xy-points-with-spatial-lines
    # Number of rows
    country_number <- length(country_neighbors_spdf)
    # If I have neighbors
    if (country_number>0) {
        # Creation of the lines
        country_lines <- vector("list", country_number)
        for (i in 1:country_number) {
            country_lines[[i]] <- Lines(list(Line(rbind(country_origin[i, ], country_destination[i,]))), as.character(i))
        }
        country_lines <- SpatialLines(country_lines, proj4string = CRSProj) %>% 
            spTransform(CRSLatLon)
        # List of neighbors
        neig_stid <- country_neighbors_spdf@data$`Country Name`
        neig_data <- subset(spdf, Distance <= distance)@data
    } else {
        country_lines <- neig_stid <- neig_data <- NULL
    }
    
    return(list(
        neig_number = country_number,
        lines = country_lines,
        neig_stid = neig_stid,
        neig_data = neig_data
    ))
}

# Table for presentation ----
country_table <- function(country_click, country_neig, range_min, range_max, data = df_data2) {

    country_list <- c(country_click, country_neig$neig_stid)
    
    data_production <- data %>%
        filter(`Country Name` %in% country_list,
               between(`Year`, range_min, range_max)) %>%
        group_by(`Country Name`, `Indicator Name`) %>% 
        summarise(`Value`=mean(`Value`)) %>% 
        pivot_wider(names_from = `Indicator Name`, values_from = `Value`)
    
    return(list(
        data_wide = data_production,
        data_long = data_production
    ))
}

## App ----

## UI ----
ui <- fluidPage(
    navbarPage("World Development Indicators", id = "nav",
    tabPanel("Map",
            tags$head(includeCSS("styles.css")),
            div(class = "map_uppper", 
                leafletOutput(outputId = "map", width = "100%", height = "100%"),
                absolutePanel(id = "controls", class = "panel panel-default", 
                              top = 60, right = "26%", width = 330, fixed = TRUE, draggable = TRUE, bottom = "auto", height = "auto", left = "auto",
                              selectInput(inputId = 'indicator', label = 'Indicator', choices = unique(df_data2$`Indicator Name`), selected=unique(df_data2$`Indicator Name`)[1]),
                              sliderInput(inputId = "distance", label = "Distance [km]", min = 1000, max = 20000, value = 5000, step = 100, ticks = FALSE))
            ),
            div(class = "selection_plot", 
                plotOutput("selection_plot", height = '100%'),
            ),
            div(class = "neighbors_table_wide", 
                sliderInput(inputId = 'year_range', label = 'Filter by year', min = min(df_data2$Year), max = max(df_data2$Year), step=1, ticks=FALSE, value=c(1990, 2010)),
                DT::dataTableOutput(outputId = "neighbors_table_wide"),
            ),
            div(class = "neighbors_plot", 
                plotOutput("neighbors_plot", height = '100%'),
            ),
            tags$div(id="cite", 'IE, GMBD, Intake 2020, Pelayo & Bretti')
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
                lng=~longitude, lat=~latitude,
                radius = ~Value/1e6, 
                label = ~Label,
                layerId = ~`Country Name`,
                group = 'data_solar') %>% 
            addHeatmap(
                lng = ~longitude, lat = ~latitude,
                intensity = ~Value,
                layerId = 'Heat',
                blur = 90, max = 1, radius = 60, minOpacity = 0.2)
    }) 

    # Check events over the map
    observeEvent(c(input$map_marker_click, input$distance, input$indicator, input$year_range), ignoreNULL = FALSE, ignoreInit = TRUE, {
        map_ <- input$map_marker_click
        dist_ <- input$distance
        # If there is any input
        if (!is.null(map_) & !is.null(dist_)) {
            # Calculate neighbors
            country_neig <- country_distance(country_point, map_$id, dist_*1e3)
            # Plot the lines, only if there are neighbors
            if (country_neig$neig_number>0) {
                leaflet::leafletProxy(mapId = "map") %>%
                    clearGroup('lines') %>% 
                    clearGroup('DT_selected') %>% 
                    addPolygons(
                        data = country_neig$lines,
                        opacity = 0.2,
                        group = 'lines') 
                # Table with the list of neighbors
                output$neighbors_table_wide <- DT::renderDataTable(country_table(map_$id, country_neig, input$year_range[1], input$year_range[2])$data_wide, rownames = FALSE, width = 0.9)
                output$neighbors_table_long <- DT::renderDataTable(country_table(map_$id, country_neig, input$year_range[1], input$year_range[2])$data_long, rownames = FALSE, width = 0.9, selection = 'none')
                # Plot
                output$selection_plot <- renderPlot(plot_top_right_div(map_$id, input$indicator))
                output$neighbors_plot <- renderPlot(plot_bottom_right_div(map_$id, country_neig$neig_stid))
            }
            output$selection_text <- renderUI({
                str1 <- paste('Listing', '<strong>', country_listing(map_$id, country_neig$neig_stid)$text, '</strong>', 'weather stations')
                str2 <- paste('From', '<strong>', input$year_range[1], '</strong>', 'to', '<strong>', input$year_range[2], '</strong>')
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
            country_neig <- country_distance(country_point, map_$id, dist_*1e3)
            leaflet::leafletProxy(mapId = "map") %>%
                clearGroup('DT_selected') %>% 
                addCircleMarkers(
                    lng = country_neig$neig_data$longitude[row_], lat=country_neig$neig_data$latitude[row_],
                    label = country_neig$neig_data$`Country Name`[row_],
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
