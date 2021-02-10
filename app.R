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

#Creating a modifier choice vector
modVec <- c("Textile Name" = "textile_name",
            "Color" = "colorGroup",
            "Pattern" = "textile_pattern_arch",
            "Process" = "textile_process_arch",
            "Fiber Type" = "textile_fiber_arch",
            "Value Range" = "textile_quality_inferred",
            "Geography" = "textile_geography_arch",
            "Quality" = "textile_quality_arch")

#Creating the UI
ui <- fluidPage(theme = shinytheme("sandstone"),
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
                  # uiOutput(outputId = 'filtered_inputs'),
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
                  selectizeInput(inputId = "inferredQualities",
                                 label = "Choose value range(s) of interest",
                                 choices = levels(factor(joined.data$textile_quality_inferred)),
                                 multiple = TRUE),
                  selectizeInput(inputId = "geography",
                                 label = "Choose geography of interest",
                                 choices = levels(factor(joined.data$textile_geography_arch)),
                                 multiple = TRUE),
                  selectizeInput(inputId = "qualities",
                                 label = "Choose quality(s) of interest",
                                 choices = levels(factor(joined.data$textile_quality_arch)),
                                 multiple = TRUE),
                  selectizeInput(inputId = "year",
                                 label = "Year:",
                                 choices = levels(factor(c(joined.data$orig_yr,joined.data$dest_yr))),
                                 multiple = TRUE),
                  actionButton(inputId = "updateBtn",
                               label = "Click to update map!"),
                  br(), br(),
                  actionButton(inputId = 'table_updateBtn',
                               label = 'Click to update table!'),
                  br(), br(), #The inputs for the pie chart and bar chart
                  selectInput(inputId = "pieChart",
                              label = "Choose a modifier for the pie chart:",
                              choices = modVec,
                              selected = "textile_name"),
                  checkboxInput(inputId = "omitNAs",
                                label = "Omit NAs in charts"),
                  selectInput(inputId = "barChart",
                              label = "Choose a modifier for the bar chart:",
                              choices = modVec,
                              selected = "textile_name"),
                  checkboxInput(inputId = "facet",
                                label = "Facet by modifier"),
                  actionButton(inputId = 'graph_updateBtn',
                               label = 'Click to update graphs!')
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
                             dataTableOutput('update_inputs'),
                             downloadButton("downloadData", "Download") #download button
                    )
                  )
                )
)

server <- function(input, output, session) {
  
  #can we filter by origin or dest yr?
  #and maybe we can put bubbles on the graph at lat/long for orig and dest, highlight the orig or dest 
  #button when the other is clicked
  
  #Render the data table based on the given search
  #let's modify this to allow hiding of inputs as well
  
  
  #creates table
  output$update_inputs <- renderDataTable(searchDelay = 1000,{
    input$table_updateBtn
    isolate(filter_by_inputs(joined.data.original,isolate(input)))}) #filters the data for what has been searched
  
  # Downloadable .xls of table dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$table_updateBtn, ".xls", sep = "")
    },
    content = function(file) {
      write_excel_csv(isolate(filter_by_inputs(joined.data.original,isolate(input))), file)
    }
  )

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
    graph_update <- isolate(input$graph_updateBtn)
    
    #Every time, we want to start with all of the data to filter through
    joined.data <- joined.data.original
    
    #Use the function to filter the inputs
    joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))
    
    choice <- get_regionChoice(regionChoice)
    totalValues <- filter_totalValue(joined.data,regionChoice,dataSet)
    
    map.data@data <- left_join(map.data.original@data, #Join with the map data, using the original map data each time
                               totalValues,
                               by = c("ADMIN" = choice))
    
    #This will be used to zoom to a specific region on the map
    latLongZoom <- latLongZoom.original %>%
      filter(Area == area)
    
    viewLat <- latLongZoom[,"Lat"]
    viewLong <- latLongZoom[,"Long"]
    viewZoom <- latLongZoom[,"Magnify"]

    #create the actual map
    create_leaflet_map(map.data,totalValues,dataType,c(viewLat,viewLong,viewZoom))
    
  })
  
  #Used to render the plot for pie chart
  output$pieChart <- renderPlot({
    input$updateBtn
    input$graph_updateBtn
    name <- input$countriesMap_shape_click$id
    
    #only want to do this if they clicked on a country
    if(length(name) != 0){
      #Read in all of the inputs, but isolated
      modifier <- isolate(input$pieChart)
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
      
      #Again, reusing the original data
      joined.data <- joined.data.original
      
      #Filter all the inputs
      joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))
      choice <- get_regionChoice(regionChoice) #get dest or orig
      
      #We care specifically about the destination here
      pie.data <- joined.data %>%
        filter(joined.data[choice] == name) %>%
        select(textile_quantity,
               deb_dec,
               all_of(modifier),
               company)
      
      
      #   if(regionChoice == "Destination"){ #Only dest_country
      #   pie.data <- joined.data %>%
      #     filter(dest_country == name) %>%
      #     select(textile_quantity,
      #            deb_dec,
      #            all_of(modifier),
      #            company)
      # }
      # else { #Only orig_country
      #   pie.data <- joined.data %>%
      #     filter(orig_country == name) %>%
      #     select(textile_quantity,
      #            deb_dec,
      #            all_of(modifier),
      #            company)
      # }
      
      #Omit na of the selected columns to avoid errors
      if(input$omitNAs){
        pie.data <- pie.data %>%
          na.omit()
      }
      else{ #Fix a problem for if NA is the only data point
        pie.data[3][is.na(pie.data[3])] <- "None indicated"
      }
      
      if(dataSet != "Both"){ #Controlling for company selection
        pie.data <- pie.data %>%
          filter(company == dataSet)
      }
      
      if(isolate(input$dataType) == "Quantity"){ #If they're interested in quantity
        if(nrow(pie.data) != 0){ #check to see if there are values left to publish
          pie.data %>% 
            ggplot(aes(x="",
                       y = textile_quantity)) +
            geom_bar(stat="identity",
                     width=1,
                     aes_string(fill=modifier))+
            coord_polar("y", start=0) + #This line in particular changes the bar chart to a pie chart
            labs(x = NULL,
                 y = NULL,
                 fill = NULL) +
            scale_fill_discrete(name = paste(names(modVec)[modVec == modifier])) +
            theme_void() +
            ggtitle(label = paste(names(modVec)[modVec == modifier], "distribution for", name, "with these filters."))
        }
        else{ #No rows were found
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }
      }
      else{ #This will do total value the same way, except graphing deb_dec
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
            scale_fill_discrete(name = paste(names(modVec)[modVec == modifier])) +
            theme_void() +
            ggtitle(label = paste(names(modVec)[modVec == modifier], "monetary distribution for", name, "with these filters."))
        }
        else{
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }
      }
    }
    else{ #This comes up if they have not clicked any countries
      ggplot() +
        ggtitle(label = "Select a country with data for these textiles in order to display a pie chart here.")
    }
  })
  
  #Rendering the bar chart - this works nearly the exact same way as the pie chart
  #except when it is graphing the outputs, it is doing so with a bar chart instead of a pie chart
  output$barChart <- renderPlot({
    input$updateBtn
    input$graph_updateBtn
    name <- input$countriesMap_shape_click$id
    
    if(length(name) != 0){
      modifier <- isolate(input$barChart)
      modifierObj <- paste("`", names(modVec)[modVec == modifier], "`", sep = "")
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
      
      joined.data <- isolate(filter_by_inputs(joined.data,isolate(input)))
      
      if(regionChoice == "Destination"){
        
        bar.data <- joined.data %>%
          
          filter(dest_country == name) %>%
          select(textile_quantity,
                 deb_dec,
                 orig_yr,
                 all_of(modifier),
                 company)
      }
      else{
        bar.data <- joined.data %>%
          filter(orig_country == name) %>%
          select(textile_quantity,
                 deb_dec,
                 orig_yr,
                 all_of(modifier),
                 company)
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
          if(isolate(input$facet)){
            bar.data %>%
              ggplot(aes(x = factor(orig_yr), y = textile_quantity)) +
              geom_bar(stat="identity",
                       aes_string(fill=modifier)) +
              labs(x = "Original Year",
                   y = "Textile Quantity",
                   fill = NULL) +
              scale_fill_discrete(name = paste(names(modVec)[modVec == modifier])) +
              theme_bw() +
              ggtitle(label = paste(names(modVec)[modVec == modifier], "distribution for", name, "with these filters.")) +
              facet_wrap(~get(modifier))
          }
          else{
            bar.data %>%
              ggplot(aes(x = factor(orig_yr), y = textile_quantity)) +
              geom_bar(stat="identity",
                       aes_string(fill=modifier)) +
              labs(x = "Original Year",
                   y = "Textile Quantity",
                   fill = NULL) +
              scale_fill_discrete(name = paste(names(modVec)[modVec == modifier])) +
              theme_bw() +
              ggtitle(label = paste(names(modVec)[modVec == modifier], "distribution for", name, "with these filters."))
          }}
        else{
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }}
      else{
        if(nrow(bar.data) != 0){
          if(isolate(input$facet)){
            bar.data %>%
              ggplot(aes(x = factor(orig_yr), y = textile_quantity)) +
              geom_bar(stat="identity",
                       aes_string(fill=modifier)) +
              labs(x = "Original Year",
                   y = "Textile Quantity",
                   fill = NULL) +
              scale_fill_discrete(name = paste(names(modVec)[modVec == modifier])) +
              theme_bw() +
              ggtitle(label = paste(names(modVec)[modVec == modifier], "distribution for", name, "with these filters.")) +
              facet_wrap(~get(modifier))
          }
          else{
            bar.data %>%
              ggplot(aes(x = factor(orig_yr), y = deb_dec)) +
              geom_bar(stat="identity",
                       aes_string(fill=modifier)) +
              labs(x = "Original Year",
                   y = "Textile Value (guilders)",
                   fill = NULL) +
              scale_fill_discrete(name = paste(names(modVec)[modVec == modifier])) +
              theme_bw() +
              ggtitle(label = paste(names(modVec)[modVec == modifier], "monetary distribution for", name, "with these filters."))
          }}
        else{
          ggplot() +
            ggtitle(label = paste(name, " has no data for these filters and ", names(modVec)[modVec == modifier], ".", sep = ""))
        }}
    }
    else{
      ggplot() +
        ggtitle(label = "Select a country with data for these textiles in order to display a bar chart here.")
    }
    
  })
}

shinyApp(ui, server)
