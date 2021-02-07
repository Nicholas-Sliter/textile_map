#Project initial
library(shiny)
library(shinythemes)
library(readxl)
library(rgdal)
library(tidyverse)
library(stringr)
library(debkeepr)
library(leaflet)

#source to function file
source('functions.R')

#Read in the data
VOC.data <- read_csv("VOC_clean.csv")
WIC.data <- read_csv("WIC_clean.csv")
joined.data.original <- read_csv("joined.csv")
map.data.original <- readOGR("countries.geojson")

joined.data.original <- joined.data.original %>%
  mutate(dest_country = ifelse(str_detect(dest_loc_region, "Indonesia"),
                               "Indonesia",
                               ifelse(str_detect(dest_loc_region, "India"),
                                      "India",
                                      ifelse(str_detect(dest_loc_region, "Malaysia"),
                                             "Malaysia",
                                             dest_loc_region))))
joined.data <- joined.data.original

totalValues <- joined.data %>%
  group_by(dest_country) %>%
  select(dest_country, textile_quantity, deb_dec) %>%
  na.omit() %>%
  summarise(total_Quant = sum(textile_quantity),
            total_Dec = sum(deb_dec))

map.data <- map.data.original

map.data@data <- left_join(map.data.original@data,
                           totalValues,
                           by = c("ADMIN" = "dest_country"))

bins <- c(0, 30000, 100000, 250000, 1000000, 2500000, 5500000)

country.colors <- colorBin(palette = "YlOrRd",
                           domain = totalValues$total_Quant,
                           bins = bins)

ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel("Textiles"),
                sidebarPanel(
                  radioButtons(inputId = "dataSet",
                               label = "Choose data of interest",
                               choices = c("WIC", "VOC", "Both"),
                               selected = "Both"),
                  radioButtons(inputId = "dataType",
                               label = "Choose data type of interest",
                               choices = c("Quantity", "Value"),
                               selected = "Quantity"),
                  selectizeInput(inputId = "textileName",
                                 label = "Choose textile(s) of interest",
                                 choices = levels(factor(joined.data$textile_name)),
                                 multiple = TRUE),
                  selectizeInput(inputId = "colors",
                                 label = "Choose color(s) of interest",
                                 choices = levels(factor(joined.data$textile_color_arch)),
                                 multiple = TRUE),
                  actionButton(inputId = "updateBtn",
                               label = "Click to update map!"),
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel(title = "Map Explorer",
                             leafletOutput(outputId = "countriesMap")
                    )
                  )
                )
)

server <- function(input, output, session) {
  
  output$countriesMap <- renderLeaflet({
    input$updateBtn
    
    dataSet <- isolate(input$dataSet)
    dataType <- isolate(input$dataType)
    textileName <- isolate(input$textileName)
    colors <- isolate(input$colors)
    
    joined.data <- joined.data.original
    
    
    if(length(textileName) != 0){
      joined.data <- joined.data %>% 
        filter(textile_name %in% textileName)
    }
    if(length(colors) != 0){
      joined.data <- joined.data %>% 
        filter(textile_color_arch %in% colors)
    }
    
    if(dataSet != "Both"){
      totalValues <- joined.data %>%
        filter(company == dataSet) %>%
        group_by(dest_country) %>%
        select(dest_country, textile_quantity, deb_dec) %>%
        na.omit() %>%
        summarise(total_Quant = sum(textile_quantity),
                  total_Dec = sum(deb_dec))
    }
    else{
      totalValues <- joined.data %>%
        group_by(dest_country) %>%
        select(dest_country, textile_quantity, deb_dec) %>%
        na.omit() %>%
        summarise(total_Quant = sum(textile_quantity),
                  total_Dec = sum(deb_dec))
    }
    
    map.data@data <- left_join(map.data.original@data,
                                        totalValues,
                                        by = c("ADMIN" = "dest_country"))
    
    viewLat <- 35
    viewLong <- 53
    viewZoom <- 2
    
    if(dataType == "Quantity"){
      bins <- c(0, 30000, 100000, 250000, 1000000, 2500000, 5500000)
      
      country.colors <- colorBin(palette = "YlOrRd",
                                 domain = totalValues$total_Quant,
                                 bins = bins)
      
      map.data %>%
        leaflet() %>%
        addPolygons(fillColor = ~country.colors(total_Quant),
                    fillOpacity = .7,
                    color = "black",
                    opacity = 1,
                    label = ~ADMIN,
                    popup = ~as.character(total_Quant)) %>%
        setView(lat = viewLat, lng = viewLong, zoom = viewZoom) %>%
        addLegend(pal = country.colors,
                  values = map.data@data$ADMIN,
                  title = "Quantities of Textiles Shipped")
    }
    else if(dataType == "Value"){
      bins <- c(0, 30000, 100000, 250000, 1000000, 2500000, 5500000, 21000000)
      
      country.colors <- colorBin(palette = "YlOrRd",
                                 domain = totalValues$total_Quant,
                                 bins = bins)
      
      map.data %>%
        leaflet() %>%
        addPolygons(fillColor = ~country.colors(total_Dec),
                    fillOpacity = .7,
                    color = "black",
                    opacity = 1,
                    label = ~ADMIN,
                    popup = ~as.character(total_Dec)) %>%
        setView(lat = viewLat, lng = viewLong, zoom = viewZoom) %>%
        addLegend(pal = country.colors,
                  values = map.data@data$ADMIN,
                  title = "Value of Textiles Shipped")
    }
  })
}

shinyApp(ui, server)