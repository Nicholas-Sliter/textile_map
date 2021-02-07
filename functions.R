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



