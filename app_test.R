#rewrite test

# @Middlebury College - 2021
# Interactive textile explorer
# Originally authored by Ev Berger-Wolf, Camryn Kluetmeier, Jason Rickenbacker, and Nicholas Sliter
# Under the instruction of Prof. Carrie Anderson at Middlebury College
# Code maintained and rewritten by Nicholas Sliter


#Libraries used:
library(shiny)
library(shinythemes)
library(readxl)
library(rgdal)
library(tidyverse)
library(stringr)
library(debkeepr)
library(leaflet)
library(viridis)
library(htmltools)


#link our external function files
source('functions.R')



#Read in datasets if files exist, else call clean.R 
data <- read_csv("joined.csv")
map <- readOGR("filteredCountries.GeoJSON")



joined.data <- data
joined <- data

#Creating a modifier choice vector for use in UI and server functions
modVec <- c("Textile Name" = "textile_name",
            "Color" = "colorGroup",
            "Pattern" = "textile_pattern_arch",
            "Process" = "textile_process_arch",
            "Fiber Type" = "textile_fiber_arch",
            "Value Range" = "textile_quality_inferred",
            "Geography" = "textile_geography_arch",
            "Quality" = "textile_quality_arch")


#Define zoom locations for map
latLongZoom.original <- data.frame("Area" = c("World", "Europe", "Africa",
                                              "Middle East", "Pacfic Islands", "Asia"),
                                   "Lat" = c(30, 49.8, -6, 27, 0, 32),
                                   "Long" = c(53, 15.47, 30, 72.5, 116, 115),
                                   "Magnify" = c(2, 4.25, 2.5, 4, 4, 3.25))

latLongZoom <- latLongZoom.original




#use rmapshaper::ms_simplify to simplify map


ui <- fluidPage(theme = shinytheme("sandstone"),
                
                titlePanel("Interactive Textile Explorer"),
                
                
                sidebarPanel(
                  
                  {
                  r <- div()
                  #no-gutters
                  r$attribs$class <-  "row gy-5"
                  
                  
                  
                  a <- div(class = "col-sm-3")
                  b <- div(class = "col-sm-3")
                  c <- div(class = "col-sm-3")
              
                  
                  a <- tagAppendChild(a, radioButtons(inputId = "dataSet",
                                                      label = "Company",
                                                      choices = c("WIC", "VOC", "Both"),
                                                      selected = "Both"))
                  b <- tagAppendChild(b,radioButtons(inputId = "dataType",
                                                     label = "Data Type",
                                                     choices = c("Quantity", "Value"),
                                                     selected = "Quantity"))
                  c <- tagAppendChild(c,radioButtons(inputId = "regionChoice",
                                                     label = "Location",
                                                     choices = c("Origin", "Destination")))
                  
                  r <- tagAppendChild(r,a)
                  r <- tagAppendChild(r,b)
                  r <- tagAppendChild(r,c)
                  
                  
                  r
                  
                  },
                  
                  # radioButtons(inputId = "dataSet",
                  #              label = "Choose company of interest",
                  #              choices = c("WIC", "VOC", "Both"),
                  #              selected = "Both"),
                  # radioButtons(inputId = "dataType",
                  #              label = "Choose data type of interest",
                  #              choices = c("Quantity", "Value"),
                  #              selected = "Quantity"),
                  # radioButtons(inputId = "regionChoice",
                  #              label = "Select one",
                  #              choices = c("Origin", "Destination"),
                  #              selected = "Origin"),
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
                
                
                
                
                
                
                
                # titlePanel("Interactive Textile Explorer"),
                # sidebarPanel(#All inputs will go in this sidebarPanel
                #   h4("Explore different facets of the data by selecting inputs below:"),
                # radioButtons(inputId = "dataSet",
                #              label = "Choose company of interest",
                #              choices = c("WIC", "VOC", "Both"),
                #              selected = "Both"),
                # radioButtons(inputId = "dataType",
                #              label = "Choose data type of interest",
                #              choices = c("Quantity", "Value"),
                #              selected = "Quantity"),
                # radioButtons(inputId = "regionChoice",
                #              label = "Select one",
                #              choices = c("Origin", "Destination"),
                #              selected = "Origin"),
                # selectizeInput(inputId = "zoomTo",
                #                label = "Zoom to:",
                #                choices = levels(factor(latLongZoom$Area)),
                #                selected = "World"),
                # selectizeInput(inputId = "textileName",
                #                label = "Choose textile(s) of interest",
                #                choices = levels(factor(joined.data$textile_name)),
                #                multiple = TRUE),
                # # uiOutput(outputId = 'filtered_inputs'),
                # selectizeInput(inputId = "colors",
                #                label = "Choose color(s) of interest",
                #                choices = levels(factor(joined.data$colorGroup)),
                #                multiple = TRUE),
                # selectizeInput(inputId = "patterns",
                #                label = "Choose pattern(s) of interest",
                #                choices = levels(factor(joined.data$textile_pattern_arch)),
                #                multiple = TRUE),
                # selectizeInput(inputId = "process",
                #                label = "Choose process(es) of interest",
                #                choices = levels(factor(joined.data$textile_process_arch)),
                #                multiple = TRUE),
                # selectizeInput(inputId = "fibers",
                #                label = "Choose fiber(s) of interest",
                #                choices = levels(factor(joined.data$textile_fiber_arch)),
                #                multiple = TRUE),
                # selectizeInput(inputId = "inferredQualities",
                #                label = "Choose value range(s) of interest",
                #                choices = levels(factor(joined.data$textile_quality_inferred)),
                #                multiple = TRUE),
                # selectizeInput(inputId = "geography",
                #                label = "Choose geography of interest",
                #                choices = levels(factor(joined.data$textile_geography_arch)),
                #                multiple = TRUE),
                # selectizeInput(inputId = "qualities",
                #                label = "Choose quality(s) of interest",
                #                choices = levels(factor(joined.data$textile_quality_arch)),
                #                multiple = TRUE),
                # selectizeInput(inputId = "year",
                #                label = "Year:",
                #                choices = levels(factor(c(joined.data$orig_yr,joined.data$dest_yr))),
                #                multiple = TRUE),
                # actionButton(inputId = "updateBtn",
                #              label = "Click to update map!"),
                # br(), br(),
                # actionButton(inputId = 'table_updateBtn',
                #              label = 'Click to update table!'),
                # br(), br(), #The inputs for the pie chart and bar chart
                # selectInput(inputId = "pieChart",
                #             label = "Choose a modifier for the pie chart:",
                #             choices = modVec,
                #             selected = "textile_name"),
                # checkboxInput(inputId = "omitNAs",
                #               label = "Omit NAs in charts"),
                # selectInput(inputId = "barChart",
                #             label = "Choose a modifier for the bar chart:",
                #             choices = modVec,
                #             selected = "textile_name"),
                # checkboxInput(inputId = "facet",
                #               label = "Facet by modifier"),
                # actionButton(inputId = 'graph_updateBtn',
                #              label = 'Click to update graphs!')
mainPanel(
  tabsetPanel(#All of the outputs go here (introduction, map/graphs, data tables, sources)
    # tabPanel(title= "Introduction",
    #          h2("Dutch Textile Trade from 1710 to 1715", align = "center"),
    #          img(src = "HARC_textiles.png", height = 350, width = 550, style="display: block; margin-left: auto; margin-right: auto;"),
    #          br(),br(),
    #          p("Through the seventeenth and early eighteenth centuries, the Dutch Republic – what we would today call the Netherlands – dominated global trade. The Dutch East India Company (VOC for short), chartered in 1602, commanded the Indian Ocean, while the Dutch West India Company (WIC for short), chartered in 1621, sought to gain control over trade in Western Africa and the Americas. The companies traded goods ranging from gold, ivory, and enslaved peoples to sugar, spices, and especially textiles. In fact, of the many types of commodities included on Dutch East and West India Company cargo lists, textiles – of every color and variety – were by far the most numerous. This app is the first in a three part series – which bring together archival, visual, and material data collected by Professors Marsely Kehoe and Carrie Anderson – and will enable scholars in a range of disciplines to make meaningful connections between these data types and thus contribute more broadly to our understanding of historic textiles. Some of the questions our apps aim to answer are:"),
    #          em("What kinds of and how many textiles were exported/imported by the Dutch East and West Indies Company and where? How did patterns in textile circulation change over time? Which textile types were most popular and in which geographical regions? Which colors? Which patterns? What did these textiles look like? How were they represented in images and what social values did they carry?"),
    #          br(), br(),
    #          p("The app that we designed is an interactive map focused on the trade of textiles from 1710 to 1715. The Map Explorer allows the user to choose a company and data type of interest, while filtering by textile modifiers, and displays an interactive world map with a complementary pie chart and bar chart when a specific country is selected. The Table Explorer displays the compiled and cleaned dataset."),
    #          p("The information presented within this app is messy historical data transcribed from invoices and ledgers that is currently part of a larger ongoing research project investigating interconnected patterns of textile trade in the VOC and WIC. Many of the textile names and types are now obsolete and at present have been cleaned and grouped to the best of our ability using secondary source materials. Historically, the Dutch used the tripartite format of Holland guilders as their currency. Using the debkeepr package developed by Dr. Jesse Sadler, a historian of early modern Europe from Virginia Tech, the currency values are converted in a decimal format for ease of visualization. Uncertainty still remains between differences between Dutch and Indian guilders and unit discrepancies. For the WIC dataset, one piece is equal to one ell (~ 27 inches), but for the VOC dataset this relationship varies."),
    # ),
    tabPanel(title = "Map Explorer",
             leafletOutput(outputId = "countriesMap"),
             plotOutput(outputId = "pieChart"),
             plotOutput(outputId = "barChart")
    ),
    tabPanel(title = "Table Explorer",
             dataTableOutput('update_inputs'),
             downloadButton("downloadData", "Download Table") #download button
    )
  )
)
)

server <- function(input, output, session) {
  
  
  
  #------- Initialize the Memory ----------
  # selected_vals = reactiveValues(
  #   
  #   dataSet = input$dataSet,
  #   dataType = input$dataType,
  #   regionChoice = input$regionChoice,
  #   zoomTo = input$zoomTo,
  #   textileName = input$textileName,
  #   colors = input$colors,
  #   patterns = input$patterns,
  #   process = input$process,
  #   fibers = input$fibers,
  #   inferredQualities = input$inferredQualities,
  #   geography = input$geography,
  #   qualities = input$qualities,
  #   year = input$year,
  #   
  #                                
  #                                
  #                                
  #   )
  selected_vals = reactiveValues(

    dataSet = "Both",
    dataType = "Quantity",
    regionChoice = "Destination",
    zoomTo = "World",
    textileName = c(),
    colors = c(),
    patterns = c(),
    process = c(),
    fibers = c(),
    inferredQualities = c(),
    geography = c(),
    qualities = c(),
    year = c(),




    )
  
  #https://stackoverflow.com/questions/54128702/how-to-keep-values-after-changing-chained-selectinputs-in-shiny-in-r
  observe({
    #store the currently selected values for replacement when inputs update
    req(input$dataSet,input$dataType,input$regionChoice,input$zoomTo,input$textileName,
        input$colors,input$patterns,input$process,input$fibers,input$inferredQualities,
        input$geography,input$qualities,input$year)
    
    selected_vals$dataSet <- input$dataSet
    selected_vals$dataType <- input$dataType
    selected_vals$regionChoice <- input$regionChoice
    selected_vals$zoomTo <- input$zoomTo
    selected_vals$textileName <- input$textileName
    selected_vals$colors <- input$colors
    selected_vals$patterns <- input$patterns
    selected_vals$process <- input$process
    selected_vals$fibers <- input$fibers
    selected_vals$inferredQualities <- input$inferredQualities
    selected_vals$geography <- input$geography
    selected_vals$qualities <- input$qualities
    selected_vals$year <- input$year
    
    
  })
  
  
  
  
  convert_colname_to_df <- function(colname){
    #Convert a input colname in shiny to the equivalent column name in the data frame
    
    
    #setClass("employee", slots=list(name="character", id="numeric", contact="character"))
    lookup_obj <- new("lookup_obj",
            "textileName" = "textile_name",
            "color" = "colorGroup",
            "patterns" = "textile_pattern_arch",
            "process" = "textile_process_arch",
            "fibers" = "textile_fiber_arch",
            "inferredQualities" = "textile_quality_inferred",
            "geography" = "textile_geography_arch",
            "qualities" = "textile_quality_arch",
            "year" = return_yrColname(input$regionChoice),
      
      )
    
    if (exists(lookup_obj[colname])){
      return (lookup_obj[colname])
    }
    else{
      return (-1);
       
    }
    
  }
  
  
  
  observe({
    #update inputs
    
    #data <- joined
    
    for (colname in colnames(selected_vals)){
      
      col <- selected_vals$colname
      
      df_colname <- convert_colname_to_df(colname)
      
      
      
      #if (df_colname != -1){
        
        #then filter it
        
        
        
        
        
        
      #}
      #else{
        #filter by 
        
        
        
        
      #}
      
      
      #filter choices
      
      if (colname == 'dataSet'){
        #this has different logic as it simply switches the sheet
        
        #maybe ignore it
      }
      
      #filter selected
      
      
      
      
      #check input type and run diff update if necessary
      #update input with choices and selected
      updateSelectizeInput(session = getDefaultReactiveDomain(),
                           inputId = colname,
                           choices = levels(factor(joined[[df_colname]])),
                           selected = col)
      
    }
    
    
  })
  
  #update inputs
  
  #output$selected_vals
  
  output$filtered_data <- reactive({
      
      filter_by_inputs()###,isolate(input))
    
      
    })
  
  
  
}



shinyApp(ui, server)

