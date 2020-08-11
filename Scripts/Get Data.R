### Get Google's Global Mobility Report Data and filter for Scottish Local Authorities

# Libraries
library(tidyverse)
library(stringr)

# url for data
gmr_link <- paste0("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?",
                   "cachebust=0882c1be467632f3")

# lookup for Scottish local authorities
la_lookup <- read.csv("Data/Scottish Local Authorities.csv")

# get data and filter
data <- read.csv(gmr_link) %>% filter(sub_region_1 %in% la_lookup$la)

# aggregate
data<- rbind(cbind((as_tibble(matrix(nrow = 172, ncol = 7)) %>% setNames(names(data)[1:7])), 
      (data %>% group_by(date, .drop = FALSE) %>%
         summarise_at(.vars = vars(retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline), 
                      .funs = funs(mean(.,na.rm = TRUE))))), data) %>%
  mutate(sub_region_1 = replace_na(sub_region_1, "Scotland"))

# save
write.csv(data, "Data/Global Mobility Report - Scotland.csv", row.names = FALSE)

# get a lookup for indicator names
indicators_lkp <- names(gmr)[9:14]
indicators <- gsub("_percent_change_from_baseline", "", indicators_lkp)
indicators <- gsub("_", " ", indicators) %>% str_to_title()
indicators <- gsub("And", "and", indicators)

lookup_data <- tibble(variable_name = indicators_lkp,
                      index = 9:14,
                      formatted = indicators)

write.csv(lookup_data, "Data/Indicator Lookup.csv", row.names = FALSE)
