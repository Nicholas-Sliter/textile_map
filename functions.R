#functions.r
library(tidyverse)
library(debkeepr)


#Create a new col deb_lsd in data containing the total value in guilders, stuivers, and penningen
new_deb_lsd_col <- function(data,
                            l="value_guldens",
                            s="value_stuivers",
                            d="value_penningen"){
  return(data %>% mutate(deb_lsd = deb_lsd(getElement(data,l),
                                           getElement(data,s),
                                           getElement(data,d),
                                           bases = c(20, 16))))
  
}

new_deb_dec_col<- function(data,
                           l="value_guldens",
                           s="value_stuivers",
                           d="value_penningen"){
  return(data %>% mutate(deb_dec = as.numeric(deb_as_decimal(deb_lsd(getElement(data,l),
                                           getElement(data,s),
                                           getElement(data,d),
                                           bases = c(20, 16))))))
  
}


deb_deb_from_lsd<- function(data,colname = 'deb_lsd'){
  return(data %>% mutate(deb_dec = as.numeric(deb_as_decimal(getElement(data,colname)))))
  
}





