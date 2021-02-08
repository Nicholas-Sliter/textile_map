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

latLongZoom.original <- data.frame("Area" = c("World", "Europe", "Africa", 
                                              "Middle East", "Pacfic Islands", "Asia"),
                                   "Lat" = c(30, 49.8, -6, 27, 0, 32),
                                   "Long" = c(53, 15.47, 30, 72.5, 116, 115),
                                   "Magnify" = c(2, 4.25, 2.5, 4, 4, 3.25))

latLongZoom <- latLongZoom.original

#Read in the data
joined.data.original <- read_csv("joined.csv")
map.data.original <- readOGR("filteredCountries.GeoJSON") 







# joined.data.original <- joined.data.original %>%
#   mutate(dest_country = ifelse(str_detect(dest_loc_region, "Indonesia"),
#                                "Indonesia",
#                                ifelse(str_detect(dest_loc_region, "India"),
#                                       "India",
#                                       ifelse(str_detect(dest_loc_region, "Malaysia"),
#                                              "Malaysia",
#                                              dest_loc_region))))

joined.data.original <- joined.data.original %>%
  mutate(colorGroup = ifelse(is.na(textile_color_arch),
                             "No color indicated",
                             ifelse(str_detect(textile_color_arch, "gold"),
                                    "gold",
                                    ifelse(str_detect(textile_color_arch, "red") | str_detect(textile_color_arch, "scarlet") | str_detect(textile_color_arch, "purple"),
                                           "red",
                                           ifelse(str_detect(textile_color_arch, "blue") | str_detect(textile_color_arch, "green"),
                                                  "blue-green",
                                                  ifelse(str_detect(textile_color_arch, "white"),
                                                         "white",
                                                         ifelse(str_detect(textile_color_arch, "black"),
                                                                "black",
                                                                ifelse(str_detect(textile_color_arch, "grey"),
                                                                       "grey",
                                                                       ifelse(str_detect(textile_color_arch, "yellow"),
                                                                              "yellow",
                                                                              ifelse(str_detect(textile_color_arch, "silver"),
                                                                                     "silver",
                                                                                     no = "Other"))))))))))

joined.data <- joined.data.original

map.data <- map.data.original


ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel("Textiles"),
                sidebarPanel(
                  radioButtons(inputId = "dataSet",
                               label = "Choose company of interest",
                               choices = c("WIC", "VOC", "Both"),
                               selected = "Both"),
                  radioButtons(inputId = "dataType",
                               label = "Choose data type of interest",
                               choices = c("Quantity", "Value"),
                               selected = "Quantity"),
                  selectizeInput(inputId = "zoomTo",
                                 label = "Zoom to:",
                                 choices = levels(factor(latLongZoom$Area)),
                                 selected = "World"),
                  selectizeInput(inputId = "textileName",
                                 label = "Choose textile(s) of interest",
                                 choices = levels(factor(joined.data$textile_name)),
                                 multiple = TRUE),
                  selectizeInput(inputId = "colors",
                                 label = "Choose color(s) of interest",
                                 choices = levels(factor(joined.data$colorGroup)),
                                 multiple = TRUE),
                  selectizeInput(inputId = "patterns",
                                 label = "Choose pattern(s) of interest",
                                 choices = levels(factor(joined.data$textile_pattern_arch)),
                                 multiple = TRUE),
                  selectizeInput(inputId = "process",
                                 label = "Choose process(es) of interest",
                                 choices = levels(factor(joined.data$textile_process_arch)),
                                 multiple = TRUE),
                  selectizeInput(inputId = "fibers",
                                 label = "Choose fiber(s) of interest",
                                 choices = levels(factor(joined.data$textile_fiber_arch)),
                                 multiple = TRUE),
                  selectizeInput(inputId = "geography",
                                 label = "Choose geography of interest",
                                 choices = levels(factor(joined.data$textile_geography_arch)),
                                 multiple = TRUE),
                  selectizeInput(inputId = "qualities",
                                 label = "Choose quality(s) of interest",
                                 choices = levels(factor(joined.data$textile_quality_arch)),
                                 multiple = TRUE),
                  selectizeInput(inputId = "inferredQualities",
                                 label = "Choose inferred quality(s) of interest",
                                 choices = levels(factor(joined.data$textile_quality_inferred)),
                                 multiple = TRUE),
                  actionButton(inputId = "updateBtn",
                               label = "Click to update map!"),
                  radioButtons(inputId = "pieChart",
                               label = "Choose a modifier for the pie chart:",
                               choices = c(colnames(joined.data[,c(19:26)])),
                               selected = "textile_name"),
                  radioButtons(inputId = "barChart",
                               label = "Choose a modifier for the bar chart:",
                               choices = c(colnames(joined.data[,c(19:26)])),
                               selected = "textile_name"),
                  checkboxInput(inputId = "omitNAs",
                                label = "Omit NAs in pie chart"),
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel(title = "Map Explorer",
                             leafletOutput(outputId = "countriesMap"),
                             plotOutput(outputId = "pieChart"),
                             plotOutput(outputId = "barChart")
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
    patterns <- isolate(input$patterns)
    process <- isolate(input$process)
    fibers <- isolate(input$fibers)
    geography <- isolate(input$geography)
    qualities <- isolate(input$qualities)
    inferredQualities <- isolate(input$inferredQualities)
    area <- isolate(input$zoomTo)
    
    joined.data <- joined.data.original
    
    
    if(length(textileName) != 0){
      joined.data <- joined.data %>% 
        filter(textile_name %in% textileName)
    }
    if(length(colors) != 0){
      joined.data <- joined.data %>% 
        filter(colorGroup %in% colors)
    }
    if(length(patterns) != 0){
      joined.data <- joined.data %>% 
        filter(textile_pattern_arch %in% patterns)
    }
    if(length(process) != 0){
      joined.data <- joined.data %>% 
        filter(textile_process_arch %in% process)
    }
    if(length(fibers) != 0){
      joined.data <- joined.data %>% 
        filter(textile_fiber_arch %in% fibers)
    }
    if(length(geography) != 0){
      joined.data <- joined.data %>% 
        filter(textile_geography_arch %in% geography)
    }
    if(length(qualities) != 0){
      joined.data <- joined.data %>% 
        filter(textile_quality_arch %in% qualities)
    }
    if(length(inferredQualities) != 0){
      joined.data <- joined.data %>% 
        filter(textile_quality_inferred %in% inferredQualities)
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
    
    latLongZoom <- latLongZoom.original %>%
      filter(Area == area)
    
    viewLat <- latLongZoom[,"Lat"]
    viewLong <- latLongZoom[,"Long"]
    viewZoom <- latLongZoom[,"Magnify"]
    
    if(dataType == "Quantity"){
      bins <- totalValues$total_Quant %>%
        auto_bin() 
      
      country.colors <- colorBin(palette = "YlOrRd",
                                 domain = totalValues$total_Quant,
                                 bins = bins)
      
      map.data %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(fillColor = ~country.colors(total_Quant),
                    fillOpacity = .7,
                    color = "black",
                    opacity = 1,
                    weight = 1,
                    label = ~ADMIN,
                    popup = ~paste("Total Quantity:", format(ifelse(is.na(total_Quant), 0, total_Quant), big.mark = ",", scientific = FALSE), sep = " "),
                    layerId = ~ADMIN) %>%
        setView(lat = viewLat, lng = viewLong, zoom = viewZoom) %>%
        addLegend(pal = country.colors,
                  values = map.data@data$ADMIN,
                  title = "Quantities of Textiles Shipped")
    }
    else if(dataType == "Value"){
      bins <- totalValues$total_Dec %>%
        auto_bin()
      
      country.colors <- colorBin(palette = "YlOrRd",
                                 domain = totalValues$total_Quant,
                                 bins = bins)
      
      map.data %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(fillColor = ~country.colors(total_Dec),
                    fillOpacity = .7,
                    color = "black",
                    opacity = 1,
                    weight = 1,
                    label = ~ADMIN,
                    layerId = ~ADMIN,
                    popup = ~paste("Total Value:", format(ifelse(is.na(total_Dec), 0, total_Dec), big.mark = ",", scientific = FALSE), "guilders", sep = " ")) %>%
        setView(lat = viewLat, lng = viewLong, zoom = viewZoom) %>%
        addLegend(pal = country.colors,
                  values = map.data@data$ADMIN,
                  title = "Value of Textiles Shipped")
    }
  })
  
  output$pieChart <- renderPlot({
    input$updateBtn
    name <- input$countriesMap_shape_click$id
    
    #name <- "Netherlands"
    
    if(length(name) != 0){
      modifier <- isolate(input$pieChart)
      modifierObj <- paste("`", modifier, "`", sep = "")
      dataSet <- isolate(input$dataSet)
      textileName <- isolate(input$textileName)
      colors <- isolate(input$colors)
      patterns <- isolate(input$patterns)
      process <- isolate(input$process)
      fibers <- isolate(input$fibers)
      geography <- isolate(input$geography)
      qualities <- isolate(input$qualities)
      inferredQualities <- isolate(input$inferredQualities)
      
      joined.data <- joined.data.original
      
      if(length(textileName) != 0){
        joined.data <- joined.data %>% 
          filter(textile_name %in% textileName)
      }
      if(length(colors) != 0){
        joined.data <- joined.data %>% 
          filter(colorGroup %in% colors)
      }
      if(length(patterns) != 0){
        joined.data <- joined.data %>% 
          filter(textile_pattern_arch %in% patterns)
      }
      if(length(process) != 0){
        joined.data <- joined.data %>% 
          filter(textile_process_arch %in% process)
      }
      if(length(fibers) != 0){
        joined.data <- joined.data %>% 
          filter(textile_fiber_arch %in% fibers)
      }
      if(length(geography) != 0){
        joined.data <- joined.data %>% 
          filter(textile_geography_arch %in% geography)
      }
      if(length(qualities) != 0){
        joined.data <- joined.data %>% 
          filter(textile_quality_arch %in% qualities)
      }
      if(length(inferredQualities) != 0){
        joined.data <- joined.data %>% 
          filter(textile_quality_inferred %in% inferredQualities)
      }
      
      pie.data <- joined.data %>%
        filter(dest_country == name) %>%
        select(textile_quantity,
               all_of(modifier))
      
      if(input$omitNAs){
        pie.data <- pie.data %>%
          na.omit()
      }
      else{
        pie.data[2][is.na(pie.data[2])] <- "None indicated"
      }
      
      if(dataSet != "Both"){
        pie.data <- pie.data %>%
          filter(company == dataSet)
      }
      
      if(nrow(pie.data) != 0){
        pie.data %>%
          ggplot(aes(x="",
                     y = textile_quantity)) + 
          geom_bar(stat="identity", 
                   width=1,
                   aes_string(fill=modifier))+ 
          coord_polar("y", start=0) +
          labs(x = NULL,
               y = NULL,
               fill = NULL) +

          theme_bw() +
          ggtitle(label = paste(modifier, "distribution for", name, "with these filters."))
      }
      else{
        ggplot() +
          ggtitle(label = paste(name, " has no data for these filters and ", modifier, ".", sep = ""))
      }
    }
    else{
      ggplot() +
        ggtitle(label = "Select a country with data for these textiles in order to display a pie chart here.")
    }
    
  })
  
  output$barChart <- renderPlot({
    input$updateBtn
    name <- input$countriesMap_shape_click$id
    
    if(length(name) != 0){
      modifier <- isolate(input$barChart)
      modifierObj <- paste("`", modifier, "`", sep = "")
      dataSet <- isolate(input$dataSet)
      textileName <- isolate(input$textileName)
      colors <- isolate(input$colors)
      patterns <- isolate(input$patterns)
      process <- isolate(input$process)
      fibers <- isolate(input$fibers)
      geography <- isolate(input$geography)
      qualities <- isolate(input$qualities)
      inferredQualities <- isolate(input$inferredQualities)
      orig_yr <- isolate(input$orig_yr)
      
      joined.data <- joined.data.original
      
      if(length(textileName) != 0){
        joined.data <- joined.data %>% 
          filter(textile_name %in% textileName)
      }
      if(length(colors) != 0){
        joined.data <- joined.data %>% 
          filter(colorGroup %in% colors)
      }
      if(length(patterns) != 0){
        joined.data <- joined.data %>% 
          filter(textile_pattern_arch %in% patterns)
      }
      if(length(process) != 0){
        joined.data <- joined.data %>% 
          filter(textile_process_arch %in% process)
      }
      if(length(fibers) != 0){
        joined.data <- joined.data %>% 
          filter(textile_fiber_arch %in% fibers)
      }
      if(length(geography) != 0){
        joined.data <- joined.data %>% 
          filter(textile_geography_arch %in% geography)
      }
      if(length(qualities) != 0){
        joined.data <- joined.data %>% 
          filter(textile_quality_arch %in% qualities)
      }
      if(length(inferredQualities) != 0){
        joined.data <- joined.data %>% 
          filter(textile_quality_inferred %in% inferredQualities)
      }
      
      bar.data <- joined.data %>%
        filter(dest_country == name) %>%
        select(textile_quantity, 
               orig_yr,
               all_of(modifier))
      
      if(input$omitNAs){
        bar.data <- bar.data %>%
          na.omit()
      }
      else{
        bar.data[2][is.na(bar.data[2])] <- "None indicated"
      }
      
      if(dataSet != "Both"){
        bar.data <- bar.data %>%
          filter(company == dataSet)
      }
      
      if(nrow(bar.data) != 0){
        bar.data %>%
          ggplot(aes(x = orig_yr, y = textile_quantity)) + 
          geom_bar(stat="identity", 
                   aes_string(fill=modifier)) + 
          labs(x = "Original Year",
               y = "Textile Quantity",
               fill = NULL) +
          theme_bw() +
          ggtitle(label = paste(modifier, "distribution for", name, "with these filters."))
      }
      else{
        ggplot() +
          ggtitle(label = paste(name, " has no data for these filters and ", modifier, ".", sep = ""))
      }
    }
    else{
      ggplot() +
        ggtitle(label = "Select a country with data for these textiles in order to display a bar chart here.")
    }
    
  })
}

shinyApp(ui, server)