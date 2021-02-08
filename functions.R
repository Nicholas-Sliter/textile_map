#functions.r
library(tidyverse)
library(debkeepr)


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


clean_deb_values <- function(data,col){
  str_replace_all(getElement(data,col),'x','0')
  str_replace_all(getElement(data,col),' ',"")
    ifelse(is.na(getElement(data,col)) | is.null(getElement(data,col)) | is.na(as.numeric(getElement(data,col))),
         0,
         as.numeric(getElement(data,col)))
  
  
  
}



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


new_deb_dec_col<- function(data,
                           l="value_guldens",
                           s="value_stuivers",
                           d="value_penningen"){
  return(data %>% mutate(deb_dec = as.numeric(deb_as_decimal(new_deb_lsd_col(data,l,s,d)))))
  
}


deb_dec_from_lsd<- function(data,colname = 'deb_lsd'){
  return(data %>% mutate(deb_dec = as.numeric(deb_as_decimal(getElement(data,colname)))))
  
}

#return the *type* (if numeric or not) of a vector
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


get_uniqueCountryList <- function(nameList1,namelist2){
  return(unique())
  
}



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


add_quantity <- function(data){
  #adds quantity to dataframe
  col1 <- getElement(data,"quant_ells")
  col2 <- getElement(data, "textile_quantity")
  data <- mutate(data,quantity=ifelse(is.na(col1) & is.na(col2),
                                      0,
                                      ifelse(is.na(col1),
                                             col2,
                                             col1)))
  
  return(data)
}



clean_textile_name <- function(data){
  cleaned <- data %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "adaties", 
                                          "adathaies")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "aliabalijs", 
                                          "alliballes")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "alibanees", 
                                          "allibannes")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "allegias", 
                                          "allejaes")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "amirtje|amirtjes", 
                                          "amirtje")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "armozijn", 
                                          "armosynen")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "atchiabanijs", 
                                          "aichuabannys")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "broules|sjadderboraal", 
                                          "Chiadder Boraal")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "cabayen|cambayen", 
                                          "cabajen")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "cambolin", 
                                          "camboolees")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "mannenhoed", 
                                          "hoed")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "muris", 
                                          "morees")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "negros kleden", 
                                          "negro kleden")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "niquanias", 
                                          "nickanees")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "patholen", 
                                          "patola")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "photas", 
                                          "photaes")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "rok", 
                                          "rocken")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "salempuris", 
                                          "salempores")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "serassen", 
                                          "sarassa")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "sjaalkoord|sjaalstof", 
                                          "sjaal")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "tansjeebs", 
                                          "tanjeebs")) %>%
    mutate(textile_name = str_replace_all(data$textile_name,
                                          "tesser|tessergaren", 
                                          "tussur"))
  return(cleaned)
}





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



filter_by_inputs <- function(data,input){
  private_filter_by <- function(data, col, data_col){
    if(length(col) != 0){
      data <- data %>% 
        filter(data_col %in% col)
    }
    return(data)
    
  }
  
  data <- private_filter_by(data,input$textileName,data$textile_name)
  data <- private_filter_by(data,input$colors,data$colorGroup)
  data <- private_filter_by(data,input$patterns,data$textile_pattern_arch)
  data <- private_filter_by(data,input$process,data$textile_process_arch)
  data <- private_filter_by(data,input$fibers,data$textile_fiber_arch)
  data <- private_filter_by(data,input$geography,data$textile_geography_arch)
  data <- private_filter_by(data,input$qualities,data$textile_quality_arch)
  data <- private_filter_by(data,input$inferredQualities,data$textile_quality_inferred)
  
  return(data)
  
  # 
  # if(length(input$textileName) != 0){
  #   joined.data <- joined.data %>% 
  #     filter(textile_name %in% input$textileName)
  # }
  # if(length(input$colors) != 0){
  #   joined.data <- joined.data %>% 
  #     filter(colorGroup %in% input$colors)
  # }
  # if(length(input$patterns) != 0){
  #   joined.data <- joined.data %>% 
  #     filter(textile_pattern_arch %in% input$patterns)
  # }
  # if(length(input$process) != 0){
  #   joined.data <- joined.data %>% 
  #     filter(textile_process_arch %in% input$process)
  # }
  # if(length(input$fibers) != 0){
  #   joined.data <- joined.data %>% 
  #     filter(textile_fiber_arch %in% input$fibers)
  # }
  # if(length(input$geography) != 0){
  #   joined.data <- joined.data %>% 
  #     filter(textile_geography_arch %in% input$geography)
  # }
  # if(length(input$qualities) != 0){
  #   joined.data <- joined.data %>% 
  #     filter(textile_quality_arch %in% input$qualities)
  # }
  # if(length(input$inferredQualities) != 0){
  #   joined.data <- joined.data %>% 
  #     filter(textile_quality_inferred %in% input$inferredQualities)
  # }
  
  
  
}







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
