#functions.r
library(tidyverse)
library(debkeepr)


#Create a new col deb_lsd in data containing the total value in guilders, stuivers, and penningen
new_deb_lsd_col <- function(data,
                            l="value_guldens",
                            s="value_stuivers",
                            d="value_penningen"){
  
  return(data %>% mutate(deb_lsd = deb_lsd(ifelse(is.na(getElement(data,l)),0,as.numeric(getElement(data,l))),
                                           ifelse(is.na(getElement(data,s)),0,as.numeric(getElement(data,s))),
                                           ifelse(is.na(getElement(data,d)),0,as.numeric(getElement(data,d))),
                                           bases = c(20, 16))))
  
}

new_deb_dec_col<- function(data,
                           l="value_guldens",
                           s="value_stuivers",
                           d="value_penningen"){
  return(data %>% mutate(deb_dec = as.numeric(deb_as_decimal(new_deb_lsd_col(data,l,s,d)))))
  
}


deb_deb_from_lsd<- function(data,colname = 'deb_lsd'){
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
  maxValue <- max(data)
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



convert_RegionToCountryName <- function(nameList,geoMatchList){
  #takes a list of regions containing country names and returns clean country names
  #sep by ", " then take second portion of string to get country name from region
  #then need to deal with " x, southwest coast of India" while respecting things like
  # "Sri Lanka"
  
  countries <- typeof(nameList)
  for (i in 1:length(nameList)){
    temp <- str_split(nameList[i], ', ')[[1]][2]
    #if length is now > 1
    #compare length to number of capitals
    if (str_count("[[:upper:]]" < length(temp))) {
      #we need to remove words, keep last word plus preceding if upper
      
    }
    else{
      countries[i] <- temp
    }
  }
  
  countries <- str_split(nameList, ', ')[[1]][2]
  #split by space, keep last word then check if prior words are capitalized, if so, keep them
  countries <- str_split(nameList, ' ')
  countries <- if(str_detect(countries,"[[:upper:]]")){
    
  }
  return(countries)
  
}


get_uniqueCountryList <- function(nameList1,namelist2){
  return(unique())
  
}



value_per_cols <- function(data){
  return(data %>%
           mutate(value_per_piece = deb_dec/as.numeric(textile_quantity)) %>%
           mutate(textile_quality_inferred = ifelse(value_per_piece < 4, 
                                                    "Inexpensive",
                                                    ifelse(value_per_piece >= 4 & value_per_piece <= 10,
                                                           "Mid-range",
                                                           ifelse(value_per_piece > 10,
                                                                  "Expensive",
                                                                  NA)))))
}



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

get_quantity <- function(data){
  col1 <- getElement(data,"quant_ells")
  col2 <- getElement(data, "textile_quantity")
  vector <- select(mutate(data,quantity=ifelse(is.na(col1) & is.na(col2),
                                               0,
                                               ifelse(is.na(col1),
                                                      col2,
                                                      col1))),'quantity')
  
  return(vector)
}




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