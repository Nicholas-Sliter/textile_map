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

#make copies of original data
joined.data <- joined.data.original
map.data <- map.data.original

#Creating the UI
ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel("Interactive Textile Explorer"),
                sidebarPanel(#All inputs will go in this sidebarPanel
                  radioButtons(inputId = "dataSet",
                               label = "Choose company of interest",
                               choices = c("WIC", "VOC", "Both"),
                               selected = "Both"),
                  radioButtons(inputId = "dataType",
                               label = "Choose data type of interest",
                               choices = c("Quantity", "Value"),
                               selected = "Quantity"),
                  radioButtons(inputId = "regionChoice",
                               label = "Select one",
                               choices = c("Origin", "Destination"),
                               selected = "Origin"),
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
                  actionButton(inputId = 'table_updateBtn',
                               label = 'Click to update table!'),
                  br(), br(), #The inputs for the pie chart and bar chart
                  selectInput(inputId = "pieChart",
                              label = "Choose a modifier for the pie chart:",
                              choices = c(colnames(joined.data[,c(19:26)])),
                              selected = "textile_name"),
                  checkboxInput(inputId = "omitNAs",
                                label = "Omit NAs in charts"),
                  selectInput(inputId = "barChart",
                              label = "Choose a modifier for the bar chart:",
                              choices = c(colnames(joined.data[,c(19:26)])),
                              selected = "textile_name"),
                  checkboxInput(inputId = "facet",
                                label = "Facet by textile names")
                ),
                mainPanel(
                  tabsetPanel(#All of the outputs go here (introduction, map/graphs, data tables)
                    tabPanel(title= "Introduction",
                             h2("Dutch Textile Trade from 1710 to 1715"),
                             h5("Interact with Dutch West India Company (WIC) and East India Company (VOC) textile shipments from 1710 to 1715, with data compiled by Kehoe and Anderson. The Map Explorer allows the user to choose a company and data type of interest, while filtering by textile modifiers, and displays an interactive world map with a complementary pie chart and bar chart when a specific country is selected. The Table Explorer displays the compiled and cleaned dataset.")),
                    tabPanel(title = "Map Explorer",
                             leafletOutput(outputId = "countriesMap"),
                             plotOutput(outputId = "pieChart"),
                             plotOutput(outputId = "barChart")
                    ),
                    tabPanel(title = "Table Explorer",
                             dataTableOutput('update_inputs'))
                  )
                )
)

server <- function(input, output, session) {
  
  #Render the data table based on the given search
  output$update_inputs <- renderDataTable(searchDelay = 1000,{
    input$table_updateBtn
    isolate(filter_by_inputs(joined.data.original,isolate(input)))}) #filters the data for what has been searched

  #The map of countries to be rendered
  output$countriesMap <- renderLeaflet({
    #We only want it to update when the updateBtn is pushed
    input$updateBtn
    
    #reading in all of the inputs, isolating them
    dataSet <- isolate(input$dataSet)
    dataType <- isolate(input$dataType)
    regionChoice <- isolate(input$regionChoice)
    textileName <- isolate(input$textileName)
    colors <- isolate(input$colors)
    patterns <- isolate(input$patterns)
    process <- isolate(input$process)
    fibers <- isolate(input$fibers)
    geography <- isolate(input$geography)
    qualities <- isolate(input$qualities)
    inferredQualities <- isolate(input$inferredQualities)
    area <- isolate(input$zoomTo)
    table_update <- isolate(input$table_updateBtn)
    
    #Every time, we want to start with all of the data to filter through
    joined.data <- joined.data.original
    
    #Use the function to filter the inputs
    joined.data <- filter_by_inputs(joined.data,input)
    
    if(regionChoice == "Destination"){ #Decide which to filter by
      joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))
      if(dataSet != "Both"){ #
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
    }
    else{
      if(dataSet != "Both"){
        totalValues <- joined.data %>%
          filter(company == dataSet) %>%
          group_by(orig_country) %>%
          select(orig_country, textile_quantity, deb_dec) %>%
          na.omit() %>%
          summarise(total_Quant = sum(textile_quantity),
                    total_Dec = sum(deb_dec))
      }
      else{
        totalValues <- joined.data %>%
          group_by(orig_country) %>%
          select(orig_country, textile_quantity, deb_dec) %>%
          na.omit() %>%
          summarise(total_Quant = sum(textile_quantity),
                    total_Dec = sum(deb_dec))
      }
      
      map.data@data <- left_join(map.data.original@data,
                                 totalValues,
                                 by = c("ADMIN" = "orig_country"))
    }
    
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
      regionChoice <- isolate(input$regionChoice)
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
      
      if(regionChoice == "Destination"){
        pie.data <- joined.data %>%
          filter(dest_country == name) %>%
          select(textile_quantity,
                 deb_dec,
                 all_of(modifier))
      }
      else {
        pie.data <- joined.data %>%
          filter(orig_country == name) %>%
          select(textile_quantity,
                 deb_dec,
                 all_of(modifier))
      }
      
      if(input$omitNAs){
        pie.data <- pie.data %>%
          na.omit()
      }
      else{
        pie.data[3][is.na(pie.data[3])] <- "None indicated"
      }
      
      if(dataSet != "Both"){
        pie.data <- pie.data %>%
          filter(company == dataSet)
      }
      
      if(isolate(input$dataType) == "Quantity"){
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
            scale_fill_discrete(name = paste(modifier)) +
            theme_void() +
            ggtitle(label = paste(modifier, "distribution for", name, "with these filters."))
        }
        else{
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", modifier, ".", sep = ""))
        }
      }
      else{
        if(nrow(pie.data) != 0){
          pie.data %>%
            ggplot(aes(x="",
                       y = deb_dec)) +
            geom_bar(stat="identity",
                     width=1,
                     aes_string(fill=modifier))+
            coord_polar("y", start=0) +
            labs(x = NULL,
                 y = NULL,
                 fill = NULL) +
            scale_fill_discrete(name = paste(modifier)) +
            theme_void() +
            ggtitle(label = paste(modifier, "monetary distribution for", name, "with these filters."))
        }
        else{
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", modifier, ".", sep = ""))
        }
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
      regionChoice <- isolate(input$regionChoice)
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
      if(regionChoice == "Destination"){
        
        bar.data <- joined.data %>%
          
          filter(dest_country == name) %>%
          select(textile_quantity,
                 deb_dec,
                 orig_yr,
                 all_of(modifier))
      }
      else{
        bar.data <- joined.data %>%
          filter(orig_country == name) %>%
          select(textile_quantity,
                 deb_dec,
                 orig_yr,
                 all_of(modifier))
      }
      
      if(input$omitNAs){
        bar.data <- bar.data %>%
          na.omit()
      }
      else{
        bar.data[4][is.na(bar.data[4])] <- "None indicated"
      }
      
      if(dataSet != "Both"){
        bar.data <- bar.data %>%
          filter(company == dataSet)
      }
      
      if(isolate(input$dataType) == "Quantity"){
        if(nrow(bar.data) != 0){
          if(input$facet){
            bar.data %>%
              ggplot(aes(x = orig_yr, y = textile_quantity)) +
              geom_bar(stat="identity",
                       aes_string(fill=modifier)) +
              labs(x = "Original Year",
                   y = "Textile Quantity",
                   fill = NULL) +
              scale_fill_discrete(name = paste(modifier)) +
              theme_bw() +
              ggtitle(label = paste(modifier, "distribution for", name, "with these filters.")) +
              facet_wrap(~textile_name)
          }
          else{
            bar.data %>%
              ggplot(aes(x = factor(orig_yr), y = textile_quantity)) +
              geom_bar(stat="identity",
                       aes_string(fill=modifier)) +
              labs(x = "Original Year",
                   y = "Textile Quantity",
                   fill = NULL) +
              scale_fill_discrete(name = paste(modifier)) +
              theme_bw() +
              ggtitle(label = paste(modifier, "distribution for", name, "with these filters."))
          }}
        else{
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", modifier, ".", sep = ""))
        }}
      else{
        if(nrow(bar.data) != 0){
          if(input$facet){
            bar.data %>%
              ggplot(aes(x = factor(orig_yr), y = textile_quantity)) +
              geom_bar(stat="identity",
                       aes_string(fill=modifier)) +
              labs(x = "Original Year",
                   y = "Textile Quantity",
                   fill = NULL) +
              scale_fill_discrete(name = paste(modifier)) +
              theme_bw() +
              ggtitle(label = paste(modifier, "distribution for", name, "with these filters.")) +
              facet_wrap(~textile_name)
          }
          else{
            bar.data %>%
              ggplot(aes(x = orig_yr, y = deb_dec)) +
              geom_bar(stat="identity",
                       aes_string(fill=modifier)) +
              labs(x = "Original Year",
                   y = "Textile Value (guilders)",
                   fill = NULL) +
              scale_fill_discrete(name = paste(modifier)) +
              theme_bw() +
              ggtitle(label = paste(modifier, "monetary distribution for", name, "with these filters."))
          }}
        else{
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", modifier, ".", sep = ""))
        }}
      }
    else{
      ggplot() +
        ggtitle(label = "Select a country with data for these textiles in order to display a bar chart here.")
    }
    
  })
}

shinyApp(ui, server)
