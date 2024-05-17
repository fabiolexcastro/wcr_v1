
## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## May 7th - 2024

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, gridExtra, rgeos, glue, gtools, xlsx, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
tasm <- read_csv('./tbl/values/AgERA5/prec-tmax-tmin_daily.csv')
prec <- read_csv('./tbl/values/chirps/chirps_all.csv')

# Tidy temperature --------------------------------------------------------
tasm <- tasm %>%
  dplyr::select(-prec, ID, id, Longitude, Latitude, date, tmin, tmax) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(ID, id, Longitude, Latitude, year, month) %>% 
  dplyr::summarise(tmin = mean(tmin), 
                   tmax = mean(tmax)) %>% 
  ungroup() %>% 
  mutate(month = ifelse(month < 10, paste0('0', month), as.character(month)), 
         date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d')) %>% 
  dplyr::select(ID, id, Longitude, Latitude, date, year, month, tmin, tmax)

# Tidy precipitation ------------------------------------------------------
prec <- prec %>% 
  mutate(month = ifelse(month < 10, paste0('0', month), as.character(month)), 
         date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d')) %>% 
  dplyr::select(ID, date, chirps)

# Join chirps - AgERA5 ----------------------------------------------------
tble <- inner_join(tasm, prec)
write.csv(tble, './tbl/values/prec-tasm_chirps-agera5_monthly.csv', row.names = FALSE)

# Make the climatologie ---------------------------------------------------
clma <- tble %>% 
  dplyr::select(-year) %>% 
  group_by(ID, id, Longitude, Latitude, month) %>% 
  dplyr::summarise_if(is.numeric, mean) %>% 
  ungroup()

write.csv(clma, './tbl/values/prec-tasm_chirps-agera5_climatologie.csv', row.names = FALSE)







