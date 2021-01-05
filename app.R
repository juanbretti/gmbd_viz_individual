# GMBD 2020 Intake
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
library(gganimate)
library(gifski)
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

df_age_range <- data.frame(
    Name = c('0 to 14 years', '15 to 64 years', '65 and up years'),
    Indicator = c('SP.POP.0014.TO.ZS', 'SP.POP.1564.TO.ZS', 'SP.POP.65UP.TO.ZS'),
    Color = c("#0D9637", "#FFC800", "#E86B0C")
)

columns_ <- c('Country Name', 'Indicator Name', 'Indicator Code', 'Year', 'Value', 'latitude', 'longitude')

age_range_pre <- 'SP.POP'
age_range <- c("0004", "0509", "1014", "1519", "2024", "2529", "3034", "3539", "4044", "4549", "5054", "5559", "6064", "6569", "7074", "7579", "80UP")
age_range_gender <- c('FE', 'MA')
age_range_post <- '5Y'
indicators_ <- apply(expand.grid(age_range_pre, age_range, age_range_gender, age_range_post), 1, function(x) paste(x, collapse="."))

indicators_ <- c(indicators_, df_age_range$Indicator, 'SP.DYN.CBRT.IN', 'SP.DYN.CDRT.IN', 'SP.POP.TOTL', 'SP.POP.TOTL.FE.IN', 'SP.POP.TOTL.MA.IN', 'SP.POP.GROW')

# Prepare data ----

df_data2 <- df_data %>% 
    filter(`Indicator Code` %in% indicators_) %>% 
    pivot_longer(starts_with(c('1', '2')), names_to='Year', values_to='Value') %>% 
    filter(!is.na(`Value`)) %>% 
    mutate(`Year` = as.numeric(`Year`)) %>% 
    inner_join(df_country, by='Country Code') %>% 
    inner_join(df_country_lat_lon, by=c('2-alpha code'='country')) %>% 
    select(!!columns_)

# Positions for map ----
data_position <- df_data2 %>% 
    filter(`Indicator Code` == 'SP.POP.TOTL') %>% 
    group_by(`Country Name`) %>%
    top_n(n=1, wt=`Year`)
# Labels for the map ----
data_position$Label <- 
    paste('<strong>', data_position[['Country Name']], '</strong>', '<br/>', 
          round(data_position$Value/1e6, 0), 'million persons in', data_position[['Year']]) %>% 
    lapply(HTML)

# Memory cleanup
rm(list=c('df_data', 'df_country', 'df_country_lat_lon'))

# Plots ----
plot_top_right_div <- function(country_clicked, age_rage, range_min, range_max, data = df_data2, data_age_range=df_age_range){
    
    # Case selector
    columns_ <- c('Indicator Code', 'Year', 'Value')
    range_base <- filter(data_age_range, `Name` == age_rage)$Indicator
    country_base <- country_clicked
    ranges_ <- data_age_range$Indicator
    colors_ <- data_age_range$Color
    
    # Prepare data
    df_data3 <- data %>% 
        filter(`Country Name` == country_base,
               `Indicator Code` %in% ranges_,
               between(`Year`, range_min, range_max)) %>% 
        dplyr::select(!!columns_) %>% 
        arrange(`Year`)
    
    # For the axis labels
    data_first <- df_data3 %>% 
        group_by(`Indicator Code`) %>% 
        top_n(-1, `Year`)
    
    data_last <- df_data3 %>% 
        group_by(`Indicator Code`) %>% 
        top_n(1, `Year`)
    
    # Prepare the geom_text
    data_first_ <- data_first[data_first['Indicator Code'] == range_base, 'Value']
    data_last_ <- data_last[data_first['Indicator Code'] == range_base, 'Value']
    
    data_change <- if(data_last_>=data_first_) {
        paste0(', an increase of ', round(data_last_-data_first_, 1), '%')
    } else {
        paste0(', a decrease of ', abs(round(data_last_-data_first_, 1)), '%')
    }
    
    if (range_base == 'SP.POP.0014.TO.ZS') {
        data_change_0014 = data_change
        data_change_1564 = ""
        data_change_65UP = ""
    } else if (range_base == 'SP.POP.1564.TO.ZS') {
        data_change_0014 = ""
        data_change_1564 = data_change
        data_change_65UP = ""
    } else if (range_base == 'SP.POP.65UP.TO.ZS') {
        data_change_0014 = ""
        data_change_1564 = ""
        data_change_65UP = data_change
    }
    
    df_data3 <- df_data3 %>%
        mutate(`Indicator Code`=recode_factor(`Indicator Code`, 
                                              `SP.POP.65UP.TO.ZS`=paste0('65 and up', data_change_65UP), 
                                              `SP.POP.1564.TO.ZS`=paste0('15 to 64 years', data_change_1564), 
                                              `SP.POP.0014.TO.ZS`=paste0('0 to 14 years', data_change_0014)))
    
    data_last_df <- data.frame(x=max(df_data3$Year)-1, y=data_last[, 'Value'], label=rev(levels(df_data3$`Indicator Code`)))
    
    # The plot
    plot <- ggplot(df_data3) +
        theme_minimal() +
        geom_area(aes(y=`Value`, x=`Year`, fill=`Indicator Code`), position='stack', alpha=0.8 , size=0) +
        scale_y_continuous(breaks = cumsum(data_first[, 'Value'][[1]]), 
                           labels = scales::percent_format(accuracy = 1L, scale = 1),
                           sec.axis = sec_axis(~ ., 
                                               breaks = cumsum(data_last[, 'Value'][[1]]),
                                               labels = scales::percent_format(accuracy = 1L, scale = 1))) +
        theme(axis.title.y=element_blank(), 
              legend.position = "none",
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.line = element_line(),
              axis.ticks = element_line()) +
        geom_text(data = data_last_df, aes(x=x, y=cumsum(Value), label=label), hjust = 'right', vjust = 1) +
        labs(title = paste0("Population in ", country_base),
             subtitle = "Age ranges over time") +
        scale_fill_manual(values = rev(colors_))

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

plot_bottom_right_div <- function(country_clicked, country_neig, age_rage, range_min, range_max, data = df_data2, data_age_range = df_age_range){

    # Case selector
    columns_ <- c('Country Name', 'Year', 'Value')
    range_base <- filter(data_age_range, `Name` == age_rage)$Indicator
    country_neigh <- c(country_neig, country_clicked)
    country_base <- country_clicked
    colors_ <- data_age_range$Color
    
    # Title
    df <- filter(data_age_range, `Indicator` == range_base)
    plot_title <- df$Name
    color_ <- df$Color
    
    # Prepare data
    df_data3 <- data %>% 
        filter(`Country Name` %in% country_neigh,
               `Indicator Code` == range_base,
               between(`Year`, range_min, range_max)) %>% 
        dplyr::select(!!columns_) %>% 
        mutate(`Country Name`=factor(`Country Name`, levels=country_neigh))
    
    # Selected country
    df_data31 <- df_data3 %>% 
        filter(`Country Name` == country_base)
    
    # For the axis labels
    data_first <- df_data3 %>% 
        group_by(`Country Name`) %>% 
        top_n(-1, `Year`)
    
    data_last <- df_data3 %>% 
        group_by(`Country Name`) %>% 
        top_n(1, `Year`) %>% 
        mutate(`Color`=`Country Name` == country_base,
               `Label`=paste0(round(`Value`, 0), '% ', `Country Name`),
               `Color Axis`=ifelse(`Country Name` == country_base, color_, 'grey'))
    
    # The plot
    plot <- ggplot(df_data3) +
        theme_minimal() +
        geom_line(aes(y=`Value`, x=`Year`, group=`Country Name`), size=1, colour='black', alpha=0.3) +
        geom_line(data=df_data31, aes(y=`Value`, x=`Year`, group=`Country Name`), size=1.5, colour=color_, alpha=0.8) +
        scale_y_continuous(breaks = data_first[, 'Value'][[1]], 
                           labels = scales::percent_format(accuracy = 1L, scale = 1),
                           sec.axis = sec_axis(~ .,
                                               breaks = data_last[, 'Value'][[1]],
                                               labels = data_last$`Label`)) +
        labs(title = paste0("Age range for ", plot_title),
             subtitle = paste0(country_base, " and neighbors countries over time")) +
        theme(axis.title.y=element_blank(), 
              axis.text.y = element_text(colour = data_last$`Color Axis`),
              legend.position = "none",
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.line = element_line(),
              axis.ticks = element_line()) +
        scale_color_manual(values = c("grey", color_))

    return(plot)
}

plot_bottom_left_div <- function(country_click, country_neig, range_min, range_max, data = df_data2){
    columns_ <- c('Country Name', 'Indicator Code', 'Year', 'Value')
    indicator_ = c('SP.POP.0014.TO.ZS', 'SP.POP.65UP.TO.ZS', 'SP.POP.TOTL')
    
    country_list <- c(country_click, country_neig)
    
    # Prepare data
    df_data3 <- data %>% 
        filter(`Country Name` %in% country_list,
               `Indicator Code` %in% indicator_,
               between(`Year`, range_min, range_max)) %>% 
        dplyr::select(!!columns_) %>% 
        mutate(`Country Name`=factor(`Country Name`, levels=country_list),
               `Color`=`Country Name`==country_click) %>% 
        pivot_wider(names_from=`Indicator Code`, values_from=`Value`)
    
    # The plot
    plot_ <- ggplot(df_data3, 
                    aes(x=`SP.POP.0014.TO.ZS`, 
                        y=`SP.POP.65UP.TO.ZS`, 
                        size=`SP.POP.TOTL`/1e7, 
                        group=`Country Name`, 
                        colour=`Color`, 
                        label=`Country Name`)) +
        theme_minimal() +
        geom_point(show.legend = FALSE, alpha = 0.7) +
        transition_time(`Year`) +
        geom_text(nudge_y=-1) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L, scale = 1)) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1L, scale = 1)) +
        labs(title = "Evolution of ages over time ",
             subtitle = paste0("Year: {round(frame_time, 0)}, ", country_click, " and neighbors countries over time"),
             caption = "Point size proportional to total population",
             x = "0 to 14 years", 
             y = "65 and up years") +
        theme(legend.position = "none",
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.line = element_line(),
              axis.ticks = element_line()) +
        scale_color_manual(values = c("grey", 'blue'))
    
    # Save the animation
    anim_save("outfile.gif", plot_, nframes = 50, duration = 5, height=500, width=500)
}

# Table ----
country_table <- function(country_click, country_neig, range_min, range_max, data = df_data2, data_age_range = df_age_range) {
    
    columns_ <- c('Country Name', 'Indicator Name', 'Value')
    
    country_list <- c(country_click, country_neig)
    
    table_ <- data %>%
        filter(`Country Name` %in% country_list,
               `Indicator Code` %in% data_age_range$Indicator,
               between(`Year`, range_min, range_max)) %>%
        dplyr::select(!!columns_) %>% 
        group_by(`Country Name`, `Indicator Name`) %>% 
        summarise(`Value`=mean(`Value`)) %>% 
        pivot_wider(names_from = `Indicator Name`, values_from = `Value`) %>% 
        mutate_at(vars(2:4), function(x) round(x, 2))
    
    return(table_)
}

# Pyramid plots ----

plot_pyramid <- function(country_click, country_neig, range_min, range_max, top_, data = df_data2) {
    # https://en.wikipedia.org/wiki/Demographic_transition#/media/File:Demographic-TransitionOWID.png
    
    # Parameters
    country_list_ <- c(country_click, country_neig)
    
    data00 <- data %>% 
        filter(`Country Name` %in% country_list_,
               between(`Year`, range_min, range_max))
    
    # Top
    df_top <- data00 %>% 
        filter(`Indicator Code` == 'SP.POP.TOTL') %>% 
        group_by(`Country Name`) %>% 
        summarise(Total=sum(`Value`)) %>% 
        top_n(top_, Total) %>%
        arrange(desc(Total)) %>% 
        pull(`Country Name`)
    
    df_top <- unique(c(country_click, df_top))
    
    # Prepare data
    data0 <- data00 %>% 
        mutate(top_country=ifelse(`Country Name` %in% df_top, `Country Name`, 'Rest of neighboards')) %>% 
        mutate(top_country=factor(top_country, levels=c(df_top, 'Rest of neighboards')))
    
    # Plot 1
    data1 <- data0 %>% 
        filter(`Indicator Code` %in% c('SP.DYN.CBRT.IN', 'SP.DYN.CDRT.IN')) %>% 
        group_by(`Year`, `Indicator Code`, `top_country`) %>% 
        summarise(`Value`=mean(`Value`))
    
    data11 <- data1 %>% 
        pivot_wider(names_from = `Indicator Code`, values_from=`Value`)
    
    data2 <- data0 %>% 
        filter(`Indicator Code` == 'SP.POP.TOTL') %>%
        group_by(`Year`, `Indicator Code`, `top_country`) %>% 
        summarise(`Value`=mean(`Value`))
    
    coeff <- max(data2$Value)/max(data1$Value)/1e6
    
    plot_1 <- ggplot() +
        geom_area(data=data2, aes(x=`Year`, y=`Value`/coeff/1e6, fill=`Indicator Code`), size=2, alpha = 0.3) +
        geom_line(data=data1, aes(x=`Year`, y=`Value`, color=`Indicator Code`), size=1, alpha = 0.6) +
        geom_ribbon(data=data11, aes(x=`Year`, y=`SP.DYN.CBRT.IN`, ymin=`SP.DYN.CDRT.IN`, ymax=`SP.DYN.CBRT.IN`), fill="blue", alpha=0.2) +
        scale_y_continuous(
            name = "Ratio [%]",
            labels = scales::percent_format(accuracy = 1L, scale = 1),
            sec.axis = sec_axis(~.*coeff, name="Population [Million]")
        ) +
        facet_wrap(vars(`top_country`)) +
        theme_minimal() +
        labs(title = "Birth and death rates",
             subtitle = paste0(country_click, " and neighbors countries over time")) +
        theme(axis.title.y=element_blank(), 
              legend.position = "bottom",
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.line = element_line(),
              axis.ticks = element_line()) +
        scale_color_manual(values=c('darkgreen', 'darkblue'), name =element_blank(), labels = c("Birth rate (per 1,000 people)", "Death rate (per 1,000 people)")) +
        scale_fill_manual(values=c('orange'), name=element_blank(), labels="Total population [Million, right axis]")
    
    # Plot 2
    # Indicators
    age_range_pre <- 'SP.POP'
    age_range <- data.frame(Number = c("0004", "0509", "1014", "1519", "2024", "2529", "3034", "3539", "4044", "4549", "5054", "5559", "6064", "6569", "7074", "7579", "80UP"),
                            Axis_Label = c("", "", "", "15 years", "", "", "", "", "", "", "", "55 years", "", "", "", "", "80 and up"))
    age_range_gender <- c('FE', 'MA')
    age_range_post <- '5Y'
    indicators_ <- apply(expand.grid(age_range_pre, age_range$Number, age_range_gender, age_range_post), 1, function(x) paste(x, collapse="."))
    
    indicators_FE <- data.frame(Label=apply(expand.grid(age_range_pre, age_range$Number, 'FE', age_range_post), 1, function(x) paste(x, collapse=".")),
                                Label_Index=factor(age_range$Number, levels=age_range$Number),
                                Label_Text=age_range$Axis_Label,
                                Gender='Female')
    indicators_MA <- data.frame(Label=apply(expand.grid(age_range_pre, age_range$Number, 'MA', age_range_post), 1, function(x) paste(x, collapse=".")),
                                Label_Index=factor(age_range$Number, levels=age_range$Number),
                                Label_Text=age_range$Axis_Label,
                                Gender='Male')
    
    indicators_FEMA <- bind_rows(indicators_FE, indicators_MA) %>% 
        mutate(`Gender`=factor(`Gender`, levels = c('Male', 'Female')))
    
    # Dataset
    data3 <- data0 %>% 
        filter(`Indicator Code` %in% indicators_) %>% 
        inner_join(indicators_FEMA, by = c("Indicator Code" = "Label")) %>% 
        group_by(`Gender`, `top_country`, `Label_Index`, `Label_Text`) %>% 
        summarize(`Value`=mean(`Value`)) %>% 
        mutate(`Value`=`Value`*ifelse(`Gender`=='Male', -1, 1))
    
    # Plot
    plot_2 <- ggplot(data3) +
        theme_minimal() +
        geom_bar(aes(y=`Value`, x=`Label_Index`, fill=`Gender`), stat = "identity", alpha=0.3) +
        facet_wrap(vars(`top_country`)) +
        scale_x_discrete(breaks = data3$Label_Index, labels = data3$Label_Text) +
        scale_y_discrete(breaks = NULL) +
        coord_flip() +
        labs(title = "Population pyramid",
             subtitle = paste0(country_click, " and neighbors countries over time"),
             caption = "Bars width proportional to the gender total population") +
        theme(axis.title.y=element_blank(), 
              axis.title.x=element_blank(), 
              legend.position = "bottom",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.line = element_line(),
              axis.ticks = element_line()) +
        scale_fill_manual(values=c('blue', 'red'))
    
    return(list(plot_1, plot_2))
}


# Spatial ----
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

## UI ----
ui <- fluidPage(
    navbarPage("World Development Indicators", id = "nav",
    tabPanel("Description",
             tags$head(includeCSS("styles.css")),
             br(),
             h1('Evolution of age ranges and transitions'),
             br(),
             span('The following study allows the user to analysis the'),
             tags$b('evolution of age ranges'),
             span('over time, across the world.'),
             br(),
             span('Compares a base country with the closest ones.'),
             br(),
             span('This study shows how the number of births has decreased over time. In contrast, the longevity has been increased. It could be an issue for future retirement programs.'),
             br(),
             br(),
             span('Additionally, is included a'),
             tags$b('demographic transition'),
             span(': a phenomenon and theory which refers to the historical shift from high birth rates and high infant death rates in societies with minimal technology, education and economic development, to low birth rates and low death rates in societies with advanced technology, education and economic development, as well as the stages between these two scenarios.'),
             br(),
             br(),
             br(),
             h3('Usage'),
             span('Use the named tags at the very top of the page.'),
             br(),
             span('Best view with a vertical resolution greater than 1080 pixels.'),
             br(),
             br(),
             span("Start from the"),
             tags$b("Map"),
             span("tab, and select a country of reference."),
             br(),
             span('Use the filters on the right to limit the search and output data.'),
             br(),
             br(),
             span('The first generation of the plots, takes some computing power and time.'),
             span('Please be patience', style = 'color:red'),
             span('for the'),
             tags$b("map"),
             span(","),
             tags$b("area plot"),
             span(","),
             tags$b("scatter plot"),
             span(", and"),
             tags$b("line plot"),
             span("to complete."),
             br(),
             br(),
             span("Use the"),
             tags$b("Demographic transition"),
             span("tab to list the numeric data inside the plots."),
             br(),
             span('After the selection of the reference country, the application shows the demographic transition for the closest ones.'),
             br(),
             span("Select the numer of countries to compare with using the slider, the reminder of the neighbors are aggregated on the 'Rest of neighboards'."),
             br(),
             br(),
             span("Use the"),
             tags$b("Data"),
             span("tab to list the numeric data inside the plots."),
             br(),
             span('Also, the countries listed are selectable. Selecting those, highlights at the map for easy localization.'),
             br(),
             br(),
             span('The application used these indicators:'),
             br(),
             tags$ul(
                tags$li("Population ages 0-14 (% of total population)"),
                tags$li("Population ages 15-64 (% of total population)"),
                tags$li("Population ages 65 and above (% of total population)"),
                tags$li("Population, total")
             ),
             br(),
             br(),
             h3('Interpretation example'),
             span('Go to the'),
             tags$b("Map"),
             span('tab on the top of the page'),
             span('Regarding the "Filters" selections:'),
             tags$ul(
                 tags$li('Leave as it is the "Age range" filter at "0 to 14 years"'),
                 tags$li('Leave as it is the "Time range" filter from "1990 to 2010"'),
                 tags$li('Change the "Distance" filter to "3000 km"'),
                 tags$li('Select Argentina in South America')
             ),
             span('The size of the blue circles, is proportional to the total country population. As can be noticed, Argentina is smaller compared to Brazil.'),
             br(),
             span('The blue lines, are representing the closest countries, that are at 3000 km at most. In this case, Argentina is neighboard to Uruguay, Paraguay, Bolivia and Chile.'),
             br(),
             span('The plot at the top right is showing the evolution of the total population in age ranges. The range from 0 to 14, has decreased ~5% over the selected period of time. In contrast, the age range higher than 65 years old, has increased ~1%.'),
             br(),
             span('The plot at the bottom left, shows the relation between the "0 to 14 years" and "65 and up years". As it can be noticed, the population is reducing the low ages and increasing proportionally the higher ones. Also, it can be noticed the faster changer is Paraguay.'),
             br(),
             span('The plot at the bottom right, shows the the the evolution of children between 0 to 14 years old, the exact value at the year 1960, the final value at 2010, colored in green is the selected country (Argentina).'),
             br(),
             span('The slope at this lines, shows the speed of change. As mentioned, Paraguay is the faster changer.'),
             br(),
             br(),
             span('Go to the'),
             tags$b("Demographic transition"),
             span('tab on the top of the page'),
             br(),
             span('The solid orange area, shows at the total population over time. The plots are sorted by total population, that means Argentina is the highest of the group and Uruguay the smallest.'),
             br(),
             span('The top gren line, represents the birth rate, and the bottom blue, the death rate. Having these lines not parallel, could reflect an increase in the total alive population.'),
             br(),
             span('The reference plot at the right from Wikipedia, presents different stages in a country evolution.'),
             br(),
             span('There is a noticeable change in Chile between 1995 and 2000, regarding the slopes. This reflects in the shape of the population pyramid.'),
             br(),
             span('The bottom pyramids, shows the speed of the change in total population. Also the color is proportional to the number of men and woman.'),
             br(),
             span('In particular, comparing the shape from Bolivia and Chile, we see the first is in a "Stable or slow increase", while the second one is more in a "Falling and then stable" stage.'),
             br(),
             span('This simple plot force the government to take actions accordingly.'),
             br(),
             br(),
             span('Go to the'),
             tags$b("Data"),
             span('tab on the top of the page'),
             br(),
             span('Shows an average summary of the values at the previous tabs.'),
             br(),
             span('These values can be filtered and sorted.'),
             br(),
             br(),
             h3('Source of information'),
             span("World Development Indicators (WDI): Data Catalog. More at"),
             tags$a(href="https://datacatalog.worldbank.org/dataset/world-development-indicators", "World Development Indicators"),
             span('.'),
             br(),
             br(),
             span("Demographic transition. More at"),
             tags$a(href="https://en.wikipedia.org/wiki/Demographic_transition", "Wikipedia"),
             span('.'),
             br(),
             br(),
             span("Dataset Publishing Language. More at"),
             tags$a(href="https://developers.google.com/public-data/docs/canonical/countries_csv", "Google Developers"),
             span('.'),
             br(),
             br(),
             tags$div(id="cite_top", 'IE, GMBD, Intake 2020, Juan Pedro Bretti Mandarano')
    ),
    tabPanel("Map",
            tags$head(includeCSS("styles.css")),
            div(class = "map_uppper", 
                leafletOutput(outputId = "map", width = "100%", height = "100%"),
                absolutePanel(id = "controls", class = "panel panel-default", 
                              top = 60, right = "26%", width = 330, fixed = TRUE, draggable = TRUE, bottom = "auto", height = "auto", left = "auto",
                              h3('Filters'),
                              selectInput(inputId = 'age_rage', label = 'Age range', choices = df_age_range$Name, selected=df_age_range$Name[1]),
                              sliderInput(inputId = 'year_range', label = 'Time range [years]', min = min(df_data2$Year), max = max(df_data2$Year), step=1, ticks=FALSE, value=c(1990, 2010)),
                              sliderInput(inputId = "distance", label = "Distance [km]", min = 1000, max = 20000, value = 5000, step = 500, ticks = TRUE))
            ),
            div(class = "plot_top_right_div", 
                plotOutput("plot_top_right_div", height = '100%')
            ),
            div(class = "plot_bottom_left_div", 
                imageOutput("plot_bottom_left_div", width = 400, height = 300)
            ),
            div(class = "plot_bottom_right_div", 
                plotOutput("plot_bottom_right_div", height = '100%')
            ),
            tags$div(id="cite", 'IE, GMBD, Intake 2020, Juan Pedro Bretti Mandarano'),
            tags$div(id="source", 'Source: World Development Indicators, and others.')
    ),
    tabPanel("Demographic transition",
            tags$head(includeCSS("styles.css")),
            div(class = "pyramid_uppper_left", 
                 absolutePanel(id = "controls", class = "panel panel-default", 
                               top = "60px", left = "10px", width = 330, fixed = TRUE, draggable = FALSE, bottom = "auto", height = "auto", right = "auto",
                               h3('Control'),
                               sliderInput(inputId = "top", label = "Top countries", min = 1, max = 10, value = 5, step = 1, ticks = TRUE))
            ),
            div(class = "pyramid_uppper_right", 
                span('Reference plot from'),
                tags$a(href="https://en.wikipedia.org/wiki/Demographic_transition#/media/File:Demographic-TransitionOWID.png", "Wikipedia"),
                span("."),
                img(src="1920px-Demographic-TransitionOWID.png",height=1450/8,width=1920/8), 
                style="text-align: right; vertical-align: text-top;"
            ),
            div(class = "plot_pyramid_top_div", 
                plotOutput("plot_pyramid_top", height = '100%'),
            ),
            div(class = "plot_pyramid_bottom_div", 
                plotOutput("plot_pyramid_bottom", height = '100%'),
            ),
            tags$div(id="cite", 'IE, GMBD, Intake 2020, Juan Pedro Bretti Mandarano'),
            tags$div(id="source", 'Source: World Development Indicators, and others.')
    ),
    tabPanel("Data",
             htmlOutput('selection_text'),
             br(),
             DT::dataTableOutput(outputId = "country_table"),
             tags$div(id="cite", 'IE, GMBD, Intake 2020, Juan Pedro Bretti Mandarano'),
             tags$div(id="source", 'Source: World Development Indicators, and others.')
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
                radius = ~lapply(Value, function(x) min(40, x/1e7)),
                label = ~Label,
                layerId = ~`Country Name`,
                group = 'data_base',
                color = '#213C93')
    }) 
    
    # Check events over the map
    observeEvent(c(input$map_marker_click, input$distance, input$year_range, input$age_rage, input$top), ignoreNULL = FALSE, ignoreInit = TRUE, {
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
                output$country_table <- DT::renderDataTable(country_table(map_$id, country_neig$neig_stid, input$year_range[1], input$year_range[2]), rownames = FALSE, width = 0.9)
                # Plot
                output$plot_top_right_div <- renderPlot(plot_top_right_div(map_$id, input$age_rage, input$year_range[1], input$year_range[2]))
                output$plot_bottom_right_div <- renderPlot(plot_bottom_right_div(map_$id, country_neig$neig_stid, input$age_rage, input$year_range[1], input$year_range[2]))
                # Pyramid
                plot_ <- plot_pyramid(map_$id, country_neig$neig_stid, input$year_range[1], input$year_range[2], input$top)
                output$plot_pyramid_top <- renderPlot(plot_[[1]])
                output$plot_pyramid_bottom <- renderPlot(plot_[[2]])
            }
            output$selection_text <- renderUI({
                str1 <- paste('Listing', '<strong>', country_listing(map_$id, country_neig$neig_stid)$text, '</strong>', 'countries')
                str2 <- paste('From', '<strong>', input$year_range[1], '</strong>', 'to', '<strong>', input$year_range[2], '</strong>')
                HTML(paste(str1, str2, sep = '<br/>'))
            })
            output$plot_bottom_left_div <- renderImage({
                plot_bottom_left_div(map_$id, country_neig$neig_stid, input$year_range[1], input$year_range[2])
                list(src = "outfile.gif", contentType = "image/gif")}, 
                deleteFile = TRUE)
        }
    })
    
    # Check events over the Data Table
    observeEvent(input$country_table_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
        map_ <- input$map_marker_click
        dist_ <- input$distance
        row_ <- input$country_table_rows_selected
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
