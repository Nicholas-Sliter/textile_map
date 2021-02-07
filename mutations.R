##Add a value per quantity column and a quality inferred column
library(tidyverse)
#Read in files
wicdata <- read_csv("WIC_clean.csv")
vocdata <- read_csv("VOC_clean.csv")
joineddata <- read_csv("joined.csv")

#Make function to add value_per column and quality inferred column
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

#Run function for the three data sets
wicdata_clean2 <- value_per_cols(wicdata)
vocdata_clean2 <- value_per_cols(vocdata)
joined2 <- value_per_cols(joineddata)

wicdata_clean2 %>%
  write.csv(file = "WIC_cleanNEW.csv")
vocdata_clean2 %>%
  write.csv(file = "VOC_cleanNEW.csv")
joined2 %>%
  write.csv(file = "joinedNEW.csv")
