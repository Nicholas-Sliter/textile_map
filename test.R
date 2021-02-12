#test

library(shiny)
library(tidyverse)
library(readxl)
library(stringr)
library(leaflet)
library(debkeepr)
library(rgdal)
#source to function file
source('functions.R')


VOC <- read_xlsx('VOC_2021_TextilesDataset.xlsx')
VOC_main <-  VOC
WIC <- read_xlsx('WIC_2021_Total_InELLS.xlsx')
WIC_main <- WIC

#clean here  
VOC_main$total_value <- str_remove_all(VOC_main$total_value,'\\.','')
VOC_valueVector <- str_split(VOC_main$total_value,',')


#add if NA then set to 0
VOC_deb <- VOC_main %>% mutate(value_guldens = ifelse(is.na(value_guldens),
                                                      0,
                                                      as.numeric(value_guldens)),
                               value_stuivers = ifelse(is.na(value_stuivers),
                                                       0,
                                                       as.numeric(value_stuivers)),
                               value_penningen = ifelse(is.na(value_penningen),
                                                        0,
                                                        as.numeric(value_penningen))) 




VOC_deb <- VOC_deb %>% mutate(deb_lsd = deb_lsd(l = VOC_deb$value_guldens,
                                                s = VOC_deb$value_stuivers,
                                                d = VOC_deb$value_penningen,
                                                bases = c(20, 16)
                                                
))








#if NA then set to 0

new_deb_lsd_col <- function(data,
                            l="value_guldens",
                            s="value_stuivers",
                            d="value_penningen"){
  return(data %>% mutate(deb_lsd = deb_lsd(getElement(data,l),
                                           getElement(data,s),
                                           getElement(data,d),
                                           bases = c(20, 16))))
  
}


WIC_deb <- WIC_main %>% new_deb_lsd_col()
WIC_deb <- WIC_deb %>% mutate(deb_dec = as.numeric(deb_as_decimal(WIC_deb$deb_lsd)))

VOC_deb <- VOC_deb %>% new_deb_lsd_col()
VOC_deb <- VOC_deb %>% mutate(deb_dec = as.numeric(deb_as_decimal(VOC_deb$deb_lsd)))

for (i in 1:length(VOC_deb$textile_quantity)){
  #remove bad characters, round, change to numeric, etc, set to NA if bad
  
  VOC_deb$textile_quantity[i] <- str_split(VOC_deb$textile_quantity[i], " ")[[1]][1]
  VOC_deb$textile_quantity[i] <- str_split(VOC_deb$textile_quantity[i], "\\.")[[1]][1]
  VOC_deb$textile_quantity[i] <- ifelse(str_detect(VOC_deb$textile_quantity[i],"[:alpha:][:punct:]&!\\."),NA,VOC_deb$textile_quantity[i])
  VOC_deb$textile_quantity[i] <- as.numeric(VOC_deb$textile_quantity[i])
  
}


#make price per unit and fill out quality_inferred 

#get rid of empty 39th col then remove the uneccesary cols
VOC_toJoin <- VOC_deb[-39]
VOC_toJoin <- VOC_toJoin %>% select(-'source',
                                    -'exchange_nr',
                                    -'orig_date',
                                    -'dest_date',
                                    -'textile_subcategory',
                                    -'textile_other_unknown_arch',
                                    -'value_currency',
                                    -'guilders_per',
                                    -'stuivers_per',
                                    -'pennigen_per',
                                    -'meas_per',
                                    -'value_guldens',
                                    -"value_stuivers",
                                    -"value_penningen",
                                    -'deb_lsd')
#Change types and add quant_ells and unit ells cols
VOC_toJoin <- VOC_toJoin %>% 
  mutate(quant_ells = as.numeric(NA), units_ells = as.numeric(NA)) %>%
  mutate(dest_yr = as.numeric(dest_yr)) %>%
  mutate(textile_quantity = as.numeric(textile_quantity))

#change col names
colnames(VOC_toJoin)[which(names(VOC_toJoin) == "dest_loc_port_modern")] <- "dest_loc_port"
colnames(VOC_toJoin)[which(names(VOC_toJoin) == "dest_loc_region_modern")] <- "dest_loc_region"           

WIC_toJoin <- WIC_deb %>% select(-'source',
                                 -'exchange_nr',
                                 -'orig_date',
                                 -'dest_date',
                                 -'textile_subcategory',
                                 -'textile_other_unknown_arch',
                                 -'value_currency',
                                 -'guilders_per',
                                 -'stuivers_per',
                                 -'pennigen_per',
                                 -'meas_per',
                                 -'value_guldens',
                                 -"value_stuivers",
                                 -"value_penningen",
                                 -'deb_lsd')
#set dest yr to final dest yr if dest_yr is NA, then remove final_dest_yr, and change types
WIC_toJoin <- WIC_toJoin %>% mutate(dest_yr = ifelse(is.na(dest_yr), final_dest_yr, dest_yr)) %>%
  select(-'final_dest_yr') %>% mutate(units_ells = as.numeric(units_ells))


joined <- full_join(WIC_toJoin,VOC_toJoin,by=colnames(WIC_toJoin))
#use quant_ells if available if not, use textile_quantity


#VOC_toJoin <- VOC_toJoin %>%
#  mutate(ID = paste(company, row_number(), sep = "_"))
#WIC_toJoin <- WIC_toJoin %>%
#  mutate(ID = paste(company, row_number(), sep = "_"))



#joined <- full_join(VOC_toJoin, WIC_toJoin,
#                    by = "ID")

#write all to files
write.csv(joined,'joined.csv')
write.csv(WIC_toJoin,'WIC_clean.csv')
write.csv(VOC_toJoin,'VOC_clean.csv')
# DOnt remove . , but round to nearest whole
#replace x with 0, replace strings to NA, replace entire string with NA if / is present?
#split weird fractions before fraction



###____This code below was used in order to filter the geojson file with every country to only have what we had data for___###
map.data <- readOGR("countries.geojson")         

map.data %>%
  leaflet() %>%
  addPolygons()
         
countries <- joined.data %>%
  group_by(dest_loc_region) %>%
  summarise() #List of regions

#put everything into a country
joined.data <- joined.data %>%
  mutate(dest_country = ifelse(str_detect(dest_loc_region, "Indonesia"),
                               "Indonesia",
                               ifelse(str_detect(dest_loc_region, "India"),
                                      "India",
                                      ifelse(str_detect(dest_loc_region, "Malaysia"),
                                             "Malaysia",
                                             dest_loc_region))))
#Same thing for origin country
joined.data <- joined.data %>%
  mutate(orig_country = ifelse(str_detect(orig_loc_region_modern, "Indonesia"),
                               "Indonesia",
                               ifelse(str_detect(orig_loc_region_modern, "India"),
                                      "India",
                                      ifelse(str_detect(orig_loc_region_modern, "Malaysia"),
                                             "Malaysia",
                                             dest_loc_region))))

#Testing totalValues
totalValues <- joined.data %>%
  group_by(dest_country) %>%
  select(dest_country, textile_quantity, deb_dec) %>%
  na.omit() %>%
  summarise(total_Quant = sum(textile_quantity),
            total_Dec = sum(deb_dec))

totalValues <- joined.data %>%
  group_by(orig_country) %>%
  select(orig_country, textile_quantity, deb_dec) %>%
  na.omit() %>%
  summarise(total_Quant = sum(textile_quantity),
            total_Dec = sum(deb_dec))

#Give vectors of the country names
dest_country <- as.vector(totalValues[['dest_country']])
orig_country <- as.vector(totalValues[['orig_country']])

print(dest_country)
print(orig_country)

#Subset the data to only have information for countries of interest
map.data <- map.data %>%
  subset(ADMIN %in% dest_country | ADMIN %in% orig_country)

#Write that data to a file
map.data %>%
  writeOGR(dsn = "countriesFiltered.geoJSON", layer = "countriesFiltered.geoJSON", driver = "geoJSON")

map.data %>% #Make sure the map works as intended
  leaflet() %>%
  addPolygons(fillColor = ~country.colors(textile_quantity),
              fillOpacity = .7,
              color = "black",
              opacity = 1,
              layerId = ~ADMIN) %>%
  addLegend(pal = country.colors(),
            values = ~textile_quantity,
            title = "Textile Imports by Region")
