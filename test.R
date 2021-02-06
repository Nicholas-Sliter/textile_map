#test

library(shiny)
library(tidyverse)
library(readxl)
library(stringr)
library(leaflet)
library(debkeepr)



VOC <- read_xlsx(file.choose())
VOC_main <-  VOC
WIC <- read_xlsx(file.choose())
WIC_main <- WIC

#clean here  
VOC_main$total_value <- str_remove_all(VOC_main$total_value,'\\.','')
VOC_valueVector <- str_split(VOC_main$total_value,',')


VOC_deb <- VOC_main %>% mutate(value_guldens = as.numeric(value_guldens),
                              value_stuivers = as.numeric(value_stuivers),
                              value_penningen = as.numeric(value_penningen)) 
VOC_deb <- VOC_deb %>% mutate(deb_lsd = deb_lsd(l = VOC_deb$value_guldens,
                                                s = VOC_deb$value_stuivers,
                                                d = VOC_deb$value_penningen,
                                                bases = c(20, 16)
  
  ))


#if NA then set to 0

new_deb_lsd_col <- function(data,
                            l=value_guldens,
                            s=value_stuivers,
                            d=value_penningen){
  return(data %>% mutate(deb_lsd = deb_lsd(getElement(data,l),
                                           getElement(data,s),
                                           getElement(data,d),
                                           bases = c(20, 16))))
  
}


VOC_deb <- VOC_deb %>% new_deb_lsd_col()

%>%
  mutate(textile_quantity = as.numeric(str_replace_all(
                                        str_split(textile_quantity, " ")[1],
                                        
    textile_quantity)

    
  for (i in 1:length(VOC_deb$textile_quantity)){
    #remove bad characters, round, change to numeric, etc, set to NA if bad
    
    VOC_deb$textile_quantity[i] <- str_split(VOC_deb$textile_quantity[i], " ")[[1]][1]
    VOC_deb$textile_quantity[i] <- str_split(VOC_deb$textile_quantity[i], "\\.")[[1]][1]
    VOC_deb$textile_quantity[i] <- ifelse(str_detect(VOC_deb$textile_quantity[i],"[:alpha:][:punct:]&!\\."),NA,VOC_deb$textile_quantity[i])
    VOC_deb$textile_quantity[i] <- as.numeric(VOC_deb$textile_quantity[i])
    
  }
  
  
joined <- left_join(WIC_
  

    
# DOnt remove . , but round to nearest whole
#replace x with 0, replace strings to NA, replace entire string with NA if / is present?
#split weird fractions before fraction


#create the correct currency base for the Dutch with debkeepr by mutating a new column
lsd_Acredam <- WIC_Acredam %>% 
  mutate(lsd = deb_lsd(l = value_guldens, s = value_stuivers, d = value_penningen, 
                       bases = c(20,16)))

#create a graphable decimal column with Dutch currency values
deci_Acredam <- lsd_Acredam %>%
  mutate(deci_lsd = deb_as_decimal(lsd))



VOC.data <- VOC.data.original %>%
  mutate(quantity.clean = ifelse(str_detect(textile_quantity,
                                            "[:punct:]&!\\."),
                                 NA,
                                 as.numeric(textile_quantity)),
         guldens.clean = ifelse(is.na(value_guldens),
                                0,
                                as.numeric(value_guldens)),
         stuivers.clean = ifelse(is.na(value_stuivers),
                                 0,
                                 as.numeric(value_stuivers)),
         pennigen.clean = ifelse(is.na(value_penningen),
                                 0,
                                 as.numeric(pennigen.clean)),
         lsd = deb_lsd(l = guldens.clean, s = stuivers.clean, d = pennigen.clean, base = c(20, 16)),
         dec = as.numeric(deb_as_decimal(lsd)))



VOC.data <- VOC.original %>%
  mutate(quantity.clean = ifelse(str_detect(textile_quantity,
                                            "([:alpha:]|[:punct:])&!\\."),
                                 NA,
                                 round(as.numeric(textile_quantity)))
         




Join <- 
  


  
  
#Get data, read it in, clean it, join it
  
#Shiny UI with inputs and outputs and layout
  
#Leaflet graphs + others? that use either VOC or WIC or both + the input selectors (for fabric type and modifiers?)
  
#We want: show a map that has all countries where textiles are shipped to, displays quantity and price
#bubble graph (with quantity/and or price) click on bubble to get info section

#summary/graph on bubble click?
#
  
  
  
  
  
  
  
  








#import both WIC and VOC Or *let user choose between WIC and VOC*

VOC = read_xlsx()
VOC_main <- 
  WIC = read_xlsx()
WIC_main <-
  Join =
  
  
  #let user choose VOC or WIC
  choice <- input$data_set #WIC, VOC, or both
data <- assign(input$data_set)

#cleaning, dependent on data set chosen


#mutate cols for lat and long based on user selection
#use variables set to inputs to accomplish this




leaflet() %>% addMarkers() %>% addTiles()