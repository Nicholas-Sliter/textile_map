#functions.r
library(tidyverse)
library(debkeepr)
library(leaflet)
library(jsonlite)


#https://stackoverflow.com/a/52511866
getExtension <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
} 







# validate_read_datasets <- function(path){
#   x
#   type
#   
#   if (!file.exists(path)){
#     #Create files
#     clean.R
#   }
#   
#   #Get type and file
#   type <- getExtension(path);
#   
#   if (type == 'csv'){
#     
#     
#   }
#   
#   
#   return(x)
#   
# }


#Create a new col deb_lsd in data containing the total value in guilders, stuivers, and penningen
new_deb_lsd_col <- function(data,
                            l="value_guldens",
                            s="value_stuivers",
                            d="value_penningen"){

  # return(data %>% mutate(deb_lsd = deb_lsd(ifelse(is.na(getElement(data,l)),0,as.numeric(getElement(data,l))),
  #                                          ifelse(is.na(getElement(data,s)),0,as.numeric(getElement(data,s))),
  #                                          ifelse(is.na(getElement(data,d)),0,as.numeric(getElement(data,d))),
  #                                          bases = c(20, 16))))

  return(data %>% mutate(deb_lsd = deb_lsd(clean_deb_values(data,l),
                                           clean_deb_values(data,s),
                                           clean_deb_values(data,d),
                                           bases = c(20, 16))))



}


#clean deb values before creating deb col
clean_deb_values <- function(data,col){
  str_replace_all(getElement(data,col),'x','0')
  str_replace_all(getElement(data,col),' ',"")
  ifelse(is.na(getElement(data,col)) | is.null(getElement(data,col)) | is.na(as.numeric(getElement(data,col))),
         0,
         as.numeric(getElement(data,col)))



}


#No longer needed
# valueToNumeric_or_na_toZero <- function(data,value){
#   #replace x with 0
#   str_replace_all(getElement(data,value),'x','0')
#   str_replace_all(getElement(data,value),' ',"")
#
#   ifelse(is_na_or_null(value)),return(0),
#
#   else {
#     ifelse((!is.na(as.numeric(getElement(data,value))))
#     return(as.numeric(getElement(data,value)))
#   }
#   else{
#     return(0)
#   }
# }
#
#
#
# is_na_or_null <- function(element){
#   if (is.na(element) | is.null(element)){
#   return(TRUE)}
#   else{
#     return(FALSE)
#   }
# }

#create deb dec col using debkeepr
new_deb_dec_col<- function(data,
                           l="value_guldens",
                           s="value_stuivers",
                           d="value_penningen"){
  return(data %>% mutate(deb_dec = as.numeric(deb_as_decimal(new_deb_lsd_col(data,l,s,d)))))

}

#create deb dec from lsd
deb_dec_from_lsd<- function(data,colname = 'deb_lsd'){
  return(data %>% mutate(deb_dec = as.numeric(deb_as_decimal(getElement(data,colname)))))

}

#return TRUE if numeric, FALSE if not numeric or factored
detect_varType <- function(data,colname){
  vector <- getElement(data,colname)
  if(is.factor(vector)){
    return(FALSE)
  }
  else if(!is.numeric(vector)){
    return(FALSE)
  }
  else if (is.numeric(vector)){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


#get type of graph to use from var types
get_graphType <- function(data,x,y,args=c(main=c(),aes=c())){
  x_type <- detect_varType(data,x)
  y_type <- detect_varType(data,y)


  #X and Y are both cont.
  if (x_type & y_type){
    #scatterplot
    return(
      geom_point()
    )

  }
  #X XOR Y are cont.
  else if(xor(x_type,y_type)){
    #barchart
    return(
      geom_col()
    )
  }
  #X and Y are discrete
  else{
    #use a bubble plot using geom_count
    return(
      geom_count()
      #args[1],aes(args[2])
    )
  }


  #to do args, get length of vector


}

#do binning to create color choices based on data
auto_bin <- function(data) {
  if(NROW(data) == 0){
    maxValue <- 0
  }
  else{
    maxValue <- max(data)
  }
  if(maxValue < 50){
    maxValue <- 50
  }

  return(c(0, ceiling(maxValue / 50), ceiling(maxValue / 25),
           ceiling(maxValue / 5), ceiling(maxValue * 2 / 5), ceiling(maxValue)))
}


#set to numeric, NA -> 0 ####NOT FUCTIONING####
to_numeric_naToZero <- function(colname){


  return(colname <- colname %>%
           apply(margin=2,
                 FUN=(if(is.na(as.numeric(colname))){return(0)}
                      else{return(as.numeric(colname))}))
  )

}



convert_RegionToCountryName <- function(colvector,type='dest',returnValue=1){
  #takes a list of regions containing country names and returns clean country names
  #sep by ", " then take second portion of string to get country name from region
  #then need to deal with " x, southwest coast of India" while respecting things like
  # "Sri Lanka"


  #two types: dest and orig
  nameList <- colvector
  #data <- data %>% mutate (countries = "")
  vector <- nameList
  cardinals <- c('Southwest ', 'Northwest ', 'Southeast ', 'Northeast ')
  for(i in 1:length(nameList)){


    #only split if necessary
    temp <- nameList[i]
    if(str_detect(temp,", ")){
      temp <- str_split(nameList[i], ', ')[[1]][2]}
    if(str_detect(temp, 'and ')){
      temp <- str_split(temp, 'and ')[[1]][2]}
    if(str_detect(temp, 'of ')){
      temp <- str_split(temp, 'of ')[[1]][2]}
    l <- length(str_split(temp, " "))
    if(l>1){
      if(str_count(temp,"[[:upper:]]" < l)){
        temp <- str_split(temp, " ")
        lword <- temp[-1]
        pword <- temp[-2]
        if(str_detect(pword,"[[:upper:]]")){
          pword <- paste(pword, " ", sep="")
        }
        else{
          pword <- ""
        }
        temp <- paste(pword,lword,sep="")
      }
    }
    if(sum(str_detect(temp, cardinals))>0){
      temp <- min(str_replace_all(temp, cardinals, ""))
    }

    vector[i] <- temp
    #getElement(data,get(new_colname))[i] <- temp

  }
  return(vector)
}



#get values per peice and classify as inexpessive, mid-range, or expenssive
value_per_cols <- function(data){
  # data <- data %>% mutate(quantity = as.numeric(ifelse(!is.na(data$quant_ells),
  #                                            data$quant_ells,
  #                                            ifelse(is.na(data$textile_quantity) | data$textile_quantity == 0,
  #                                                   1,
  #                                                   data$texile_quantity))))
  return(data %>%
           mutate(value_per_piece = ceiling(deb_dec/textile_quantity)) %>%
           mutate(textile_quality_inferred = ifelse(value_per_piece < 4,
                                                    "Inexpensive",
                                                    ifelse(value_per_piece >= 4 & value_per_piece <= 10,
                                                           "Mid-range",
                                                           ifelse(value_per_piece > 10,
                                                                  "Expensive",
                                                                  NA)))))
}

#No longer needed
# get_quantity <- function(col1,col2){
#   # return(select(mutate(data,
#   #                      quantity=as.numeric(ifelse(is.na(quant_ells) & is.na(textile_quantity),
#   #                                                 1,
#   #                                                 ifelse(is.na(quant_ells),
#   #                                                        textile_quantity,
#   #                                                        quant_ells)))),'quantity'))
#
#   return(as.numeric(ifelse(is.na(col2) & is.na(col1),
#                                                   1,
#                                                   ifelse(is.na(col2),
#                                                          col1,
#                                                          col2))))
#
# }
#


#if NA set to 1 unit
# get_quantity <- function(data){
#   #gets quantity from dataframe and returns a vector
#   col1 <- getElement(data,"quant_ells")
#   col2 <- getElement(data, "textile_quantity")
#   vector <- select(mutate(data,quantity=ifelse(is.na(col1) & is.na(col2),
#                                                1,
#                                                ifelse(is.na(col1),
#                                                       col2,
#                                                       col1))),'quantity')
#
#   return(vector)
# }


# add_quantity <- function(data){
#   #adds quantity to dataframe
#   col1 <- getElement(data,"quant_ells")
#   col2 <- getElement(data, "textile_quantity")
#   data <- mutate(data,quantity=ifelse(is.na(col1) & is.na(col2),
#                                       0,
#                                       ifelse(is.na(col1),
#                                              col2,
#                                              col1)))
#
#   return(data)
# }


#cleans textile name
clean_textile_name <- function(data){
  cleaned <- data %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "adaties",
                                          "adathaies")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "aliabalijs",
                                          "alliballes")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "alibanees",
                                          "allibannes")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "allegias",
                                          "allejaes")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "amirtje|amirtjes",
                                          "amirtje")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "armozijn",
                                          "armosynen")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "atchiabanijs",
                                          "aichuabannys")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "broules|sjadderboraal",
                                          "Chiadder Boraal")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "cabayen|cambayen",
                                          "cabajen")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "cambolin",
                                          "camboolees")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "Deken",
                                          "Dekens")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "mannenhoed",
                                          "hoed")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "muris",
                                          "morees")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "negros kleden",
                                          "negro kleden")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "niquanias",
                                          "nickanees")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "patholen",
                                          "patola")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "photas",
                                          "photaes")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "rok",
                                          "rocken")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "salempuris|salempuris|salempouris",
                                          "salempores")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "serassen",
                                          "sarassa")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "sjaalkoord|sjaalstof",
                                          "sjaal")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "tansjeebs",
                                          "tanjeebs")) %>%
    mutate(textile_name = str_replace_all(textile_name,
                                          "tessergaren|tesser",
                                          "tussur"))
  return(cleaned)
}


#get color groups
getColorGroups <- function(data){
  data <- data %>% mutate(colorGroup = ifelse(is.na(textile_color_arch),
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

  return(data)
}



getColorLists <- function(x, colors = c("white","yellow","red","blue","purple","green","black", "brown", "grey", "silver", "gold")){
  ##define color list
  
  ##a private function that takes in a string and a list of colors and returns a vector of the colors in the list
  identifyColors <- function(string, colors){
    
    l = c()
    if(is.na(string) || length(string) == 0){
      l <- append(l,"No color indicated")
      return(l)
    }
    
    for (c in colors){
      if (str_detect(string, c)){
        l <- append(l,c)
      }
    }
    if (length(l) == 0){
      l <- append(l,"Other")
    }
    
    return(toString(l))
  }
  
  identifyColors_vec <- Vectorize(identifyColors,vectorize.args = "string")
  
  ##Create a empty vector for each element in colorList
  data <- x %>% mutate(colorList = identifyColors_vec(x$textile_color_arch,colors))
  
  return (data)
}



flatten <- function(list){
  #string <- ""
  
  
  
  string <- toString(list)
  #for (i in 1:length(list)){
    
    #string <- paste0(string,", ",list[i])
    
  #}
  
  return (string)
}


unflatten <- function(string){
  list <- c()
  
  split <- str_split(string, ", ")[[1]]
  
  for (i in 1:length(split)){
    
    list <- append(list, split[i])
    
  }
  
  
  return (list)
  #return (as.list(split[[1]]))
  
  
  
}

vec_unflatten <- Vectorize(unflatten, vectorize.args = 'string', SIMPLIFY = TRUE)



#unfinished - idea for future

sort_inputs <- function(){


}


# private_filter_by <- function(data, col, data_col){
#   if(length(col) != 0){
#     data <- data %>%
#       filter(data_col %in% col)
#   }
#   return(data)
#
# }

#return a function if a condition is TRUE, else return onFalse function
return_function_onCondition <- function(functionToReturn, condition=TRUE, onFalse = geom_blank()){

  if(condition){
    return(functionToReturn)
  }
  return(onFalse)

}

#Makes sure that you're using a string value
return_stringByDataType <- function(dataType){
  if(dataType == 'Value'){
    return('Value')
  }
  else if (dataType == 'Quantity'){

    return('Quantity')
  }
  else{
    return(NULL)
  }
}


#Pull out a specific column
get_col <- function(data,colname){
  if(typeof(colname) == typeof('')){
    x <- data[[colname]]
  }
  else{
    x <- data[[deparse(substitute(colname))]]
  }
  return(x)

}

#Pull a specific column based on data type
return_colByDataType <- function(data,dataType){
  # return(switch(dataType,
  #               'Value'= get_col(data,'textile_quantity'),
  #               'Quantity'= get_col(data,'deb_dec')))
  
  
  return(switch(dataType,
               'Value'= get_col(data,'total_Dec'),
               'Quantity'= get_col(data,'total_Quant')))

}


return_colnameByDataType <- function(dataType){
 
  return(switch(dataType,
                'Value' = 'textile_quantity',
                'Quantity' = 'deb_dec'))
  
  
}







createBarChart <- function(data,input_vec){#dataType,year,modVec,regionChoice,modifier,facet){
  
  countryName <- input_vec['name']
  modifier <- input_vec['modifier']
  modifierObj <- input_vec['modifierObj']
  dataSet <- input_vec['dataSet']
  dataType <- input_vec['dataType']
  regionChoice <- input_vec['regionChoice']
  textileName <- input_vec['textileName']
  colors <- input_vec['colors']
  patterns <- input_vec['patterns']
  process <- input_vec['process']
  fibers <- input_vec['fibers']
  geography <- input_vec['geography']
  qualities <- input_vec['qualities']
  inferredQualities <-input_vec['inferredQualities']
  #orig_yr <- input_vec['orig_yr']
  #dest_yr <- input_vec['dest_yr']
  year <- input_vec['year']
  facet <- input_vec['facet']
  
  if(length(countryName) == 0 || (is.null(countryName) || is.na(countryName))){
    return( 
      
      ggplot() +
                ggtitle(label = "Select a country with data for these textiles in order to display a bar chart here.")
      
      
      )
      
  }
    
  
  x_col <- c()
  if (regionChoice == "Origin"){
  
    x_col <- data$orig_yr
  }
  else{
    x_col <- data$dest_yr
  
  }
  
  y_col <- c()
  if (dataType == "Quantity"){
    
    y_col <- data$textile_quantity
    
  }
  else{
    
    y_col <- data$deb_dec
    
  }
  
  
  
  
  if(nrow(data) != 0){
    
    bar_plot <- data %>%
      
     ggplot(
        #aes_string
       #return_yrColname(regionChoice)
        aes(x = factor(x_col),#factor(data[year]), #want to change this from orig year to be determined based on dest or origin
                   y = y_col #data[modifierObj])
        )
        
        
      ) +
      
      geom_bar(stat = 'identity',
               aes_string(fill = modifier)
               
      ) +
      
      labs(x = paste(regionChoice, "Year"),
            y = paste0("Textile ", return_stringByDataType(dataType)),
            fill = NULL
       ) +
       
      scale_fill_viridis(discrete = TRUE,
                          name = modifierObj,
                          option = "magma"
      ) +
       
      theme_bw() +
       
      ggtitle(label = paste(modifierObj, "distribution for", countryName, "with these filters."))
      
      if(facet){
        bar_plot <- bar_plot + facet_wrap(~get(modifier))
      }

      return(bar_plot)
    
    
  
  }
  
  else{
           ggplot() +
             ggtitle(label = paste(name, " has no data for these filters and ", modifierObj, ".", sep = ""))
         }


  
}



createPieChart <- function(data){
  
  
  
}



#Used to title the graphs
return_titleByDataType <- function(dataType){
  if(dataType == 'Value'){
    title = "Value of Textiles Shipped"
  }
  else if (dataType == 'Quantity'){
    title = "Quantities of Textiles Shipped"
  }
  return(title)
}

#Determine what will pop-up when the map is clicked
return_popupByDataType <- function(data,dataType){
  col <- return_colByDataType(data,dataType)
  if(dataType == 'Value'){
    popup = paste("Total Value:", format(ifelse(is.na(col), 0, col), big.mark = ",", scientific = FALSE), "guilders", sep = " ")

  }
  else if (dataType == 'Quantity'){
    popup = paste("Total Quantity:", format(ifelse(is.na(col), 0, col), big.mark = ",", scientific = FALSE), sep = " ")

  }

  return(popup)
}


#grab the correct colors based on the bin
get_binByDataType <- function(data,dataType){
  col <- return_colByDataType(data,dataType)
  bins <- col %>%
    auto_bin()

  return(country.colors <- colorBin(palette = "YlOrRd",
                                    domain = col,
                                    bins = bins))
}

#grab the column name based on the input of regionChoice
return_yrColname <- function(region){
  if(region == "Destination"){
    return("dest_yr")
  }
  else if(region == "Origin"){
    return("orig_yr")
  }
  else{
    return(NULL)
  }
}

#make sure you're using a string
return_yrString <- function(region){
  if(region == "Destination"){
    return("dest_yr")
  }
  else if(region == "Origin"){
    return("orig_yr")
  }
  else{
    return(NULL)
  }
}




get_unique_colors <- function(data){
  list <- c()
  
  pre_unique <- unique(as.vector(
    data$colorList))
  
  numRows = length(pre_unique)
  
  for (i in 1:numRows){
    
    str <- pre_unique[i]
    strlist <- strsplit(str, ", ")
    
    for (j in 1:length(strlist)){
      list <- append(list,strlist[[1]][j])
    }
    
  }
  
  return (colors <-
    unique(as.vector(
      list
    )))
  
  
  
}



filter_colors <- function(data,color_choices){
  
  if (!is.null(color_choices)){
  
  #data <- data %>% filter(colorList %in% color_choices)
  
  data <- data %>% filter(str_detect(colorList,color_choices))
  
  
  # colors <- get_unique_colors(data)
  # 
  # data <- data %>% filter(colorList %in% colors)
  
  }
  
  return (data)
  
  
  
}




#filter inputs based on all selections
filter_by_inputs <- function(data,input){
  private_filter_by <- function(data, col, data_col){
    if(length(col) != 0){
      data <- data %>%
        filter(data_col %in% col)
    }
    return(data)

  }

  if(isolate(input$dataSet) != "Both"){
    data <- private_filter_by(data,isolate(input$dataSet),data$company)
  }
  data <- private_filter_by(data,isolate(input$textileName),data$textile_name)
  #data <- private_filter_by(data,isolate(input$colors),data$colorList)#data$colorGroup)
  
  
  
  data <- filter_colors(data,input$colors)
  #filter by color
  
  
  
  data <- private_filter_by(data,isolate(input$patterns),data$textile_pattern_arch)
  data <- private_filter_by(data,isolate(input$process),data$textile_process_arch)
  data <- private_filter_by(data,isolate(input$fibers),data$textile_fiber_arch)
  data <- private_filter_by(data,isolate(input$geography),data$textile_geography_arch)
  data <- private_filter_by(data,isolate(input$qualities),data$textile_quality_arch)
  data <- private_filter_by(data,isolate(input$inferredQualities),data$textile_quality_inferred)
  data <- private_filter_by(data,isolate(input$year),data[[return_yrColname(isolate(input$regionChoice))]])

  return(data)
}

#Creates the map based on some data set and the certain zoom
create_leaflet_map <- function(mapdata,valuedata,dataType,lat_long=c(lat,long,zoom)){
  country.colors <- get_binByDataType(valuedata,dataType)
  #Mapping the data
  mapdata %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(fillColor = ~country.colors(return_colByDataType(mapdata@data,dataType)), #We only use polygons for countries we may have actually used
                fillOpacity = .7,
                color = "black",
                opacity = 1,
                weight = 1,
                label = ~ADMIN,
                popup = ~return_popupByDataType(mapdata@data,dataType),
                layerId = ~ADMIN) %>%
    setView(lat = lat_long[1], lng = lat_long[2], zoom = lat_long[3]) %>%
    addLegend(pal = country.colors,
              values = mapdata@data$ADMIN,
              title = return_titleByDataType(dataType))

}

#problem here with group by dest_country

filter_totalValue <- function(data,region,dataSet){

  choice <- get_regionChoice(region)
  # if(dataSet != "Both"){
  #   data <- data %>% filter(company == dataSet)
  # }

  data <- data %>% #Total values to graph things later on and color the map
    group_by_at(choice)  %>%
    select(choice, 'textile_quantity', 'deb_dec') %>%
    na.omit() %>%
    summarise(total_Quant = sum(textile_quantity),
              total_Dec = sum(deb_dec))

  return(data)
}

#Grabs the column of interest
get_regionCol <- function(data,region){
  # if(region == "Destination"){
  #   return(get_col(data,'dest_country'))
  # }
  # else if(region == "Origin"){
  #   return(get_col(data,'orig_country'))
  # }
  # else{
  #   return(NULL)
  # }


  return(get_col(data,get_regionChoice(region)))


}

#grab the desired region
get_regionChoice <- function(region){
  if(region == "Destination"){
    return("dest_country")
  }
  else if(region == "Origin"){
    return("orig_country")
  }
  else{
    return(NULL)
  }

}



##___A lot of functions that may be useful, but we did not end up using___##


#function to get/return orig or dest

# get_orig_or_dest <- function(input,output="function"){
#   #uses button input to determine outputting dest or orig results, and output to determine
#   #whether to output a string or a function (for ggplot)
#
#
#
#   switch(output){
#
#
#
#
#
#   }
#
#
#
#   if (output=='function'){
#     if (input=="orig"){
#       out <- geom_
#
#     }
#     else{
#
#
#     }
#
#   }
#   else if (output=='string'){
#     if (input=="orig"){
#
#
#     }
#     else{
#
#
#     }
#   }
#   else{
#     #defualt edge case (return NULL)
#    out <-  NULL
#   }
#
#   return(out)
#
#   }
#


# get_quantity_base <- function(element1,element2){
#   #vectorized
#   ifelse(is.na(element1) & is.na(element2),
#          return(0),
#          ifelse(is.na(element1),
#                 return(element2),
#                 return(element1)))
#
# }

# get_quantity_base <- function(element1,element2){
#   if(is.na(element1) & is.na(element2)){
#     return(0)
#   }
#   else if(is.na(element1)){
#     return(element2)
#   }
#   else if(is.na(element2)){
#     return(element1)
#   }
#   else{
#     return(0)
#   }
# }





# countries <- typeof(nameList)
# for (i in 1:length(nameList)){
#   temp <- str_split(nameList[i], ', ')[[1]][2]
#   #if length is now > 1
#   #compare length to number of capitals
#   if (str_count("[[:upper:]]" < length(temp))) {
#     #we need to remove words, keep last word plus preceding if upper
#
#   }
#   else{
#     countries[i] <- temp
#   }
# }
#
# countries <- str_split(nameList, ', ')[[1]][2]
# #split by space, keep last word then check if prior words are capitalized, if so, keep them
# countries <- str_split(nameList, ' ')
# countries <- if(str_detect(countries,"[[:upper:]]")){
#
# }
# return(countries)


# get_quantity <- function(data){
#   col1 <- getElement(data,"quant_ells")
#   col2 <- getElement(data, "textile_quantity")
#   Vectorize(FUN = get_quantity_base(col1,col2))
#
# }
#
#
# test <-  Vectorize(get_quantity_base,
#                    vectorize.args = c('element1','element2'))
# getElement(data,new_colname) <-
#
# data <- data %>%
#   mutate(new_colname = str_split(nameList, ', ')[[1]][2]) %>%
#   mutate(new_colname = ifelse(
#     str_count(new_colname,"[[:upper:]]" < length(nameList)),
#     new_colname,
#     paste(ifelse(str_detect(new_colname[length(new_colname)-1],"[[:upper:]]")),
#           as.character(new_colname[length(new_colname)-1]),
#           NULL)
#
#   #new_colname <- paste(type,'_countries', sep="")
#
#   )
#   )
#
