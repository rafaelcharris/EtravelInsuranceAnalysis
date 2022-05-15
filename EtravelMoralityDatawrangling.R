####==========================================================================####
# 1. Initialization
####==========================================================================###

#set working directory
setwd("D:/Rafael/Dropbox/4 diversificación Open Evidence/10_ETravel Insurance")

library(tidyverse)
library(janitor)
library(stringr)
library(writexl)
####==========================================================================####
# 2. Database
####==========================================================================###
mortality_df <- read_excel("./2_Data/MortalityRate.xlsx", sheet = "Sheet2") %>%
  clean_names() %>%
  rename("region" = "germany",
         "above_median" = "dummy") %>%
  filter(!is.na(above_median)) %>%
  select(region, above_median) %>%
  #parse strings
  mutate(region = str_to_lower(region),
         region = str_replace(region, "\\s\\(nuts 2016\\)", ""),
     region = chartr("áîûóü","aiuou", region))

df <- read_excel("./2_Data/etravel_clean.xlsx")

italy <- read.csv("./2_Data/countries/IT_data_mortality.csv", sep = ";")
germany <- read.csv("./2_Data/countries/DE_data_mortality.csv", sep = ";")
finland <- read.csv("./2_Data/countries/FI_data_mortality.csv", sep = ";")
hungary <- read.csv("./2_Data/countries/HU_data_mortality.csv", sep = ";")
romania <- read.csv("./2_Data/countries/RO_data_mortality.csv", sep = ";")
# Merge all countries into one data set
country_info <- bind_rows(germany,italy,finland,hungary,romania) %>%
  rename("country" = "country_code",
         "zipcode" = "postal_code") %>%
  select(state, country, zipcode, mortality_rate)

#Merge country_info with morality_df
# I should get a dummy for each region with their zipcode


####==========================================================================####
# Francesco Method
####==========================================================================###
####==========================================================================####
# 1. Merge big country df con el experimental 
####==========================================================================###
df$zipcode <- as.numeric(df$zipcode)
df <- df %>%
  mutate(country = ifelse(country == "FL", "FI", country))

df_merged <- inner_join(df, country_info, by = c("country", "zipcode")) %>%
  distinct(rowid, .keep_all = TRUE) # no entiendo por qué hay duplicados

#There should be no greece in this df

#algunas personas no han sido mergeadas y no son griegas
not_merged = anti_join(df, df_merged, by = c("zipcode")) %>%
  filter(country != "GR")
#checkear qu estos zipcodes no se encuentrasn
not_merged$zipcode %in% country_info$zipcode

####==========================================================================####
# 2. calcular la media per country 
####==========================================================================###

df_merged <- df_merged %>% 
  group_by(country) %>%
  mutate(median_mortality = median(mortality_rate, na.rm = TRUE),
        percentil_60 = quantile(mortality_rate, probs =c(0.6)),
        percentil_75 = quantile(mortality_rate, probs =c(0.75)),                                 
        above_mortality = ifelse(mortality_rate>median_mortality, 1,0),
        above_p60  = ifelse(mortality_rate>percentil_60, 1,0),
        above_p75  = ifelse(mortality_rate>percentil_75, 1,0)
         ) 

df_merged %>%
  writexl::write_xlsx("./2_Data/mortality_data_merged.xlsx")

