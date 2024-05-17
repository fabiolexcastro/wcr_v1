

## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## May 7th - 2024

## Make just one table for all the variables

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, gridExtra, rgeos, glue, gtools, xlsx, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Tabular data 
pnts <- read_excel('./tbl/points/IMLVT Site info.xlsx')
colnames(pnts) <- gsub(' ', '_', colnames(pnts))
colnames(pnts)[1] <- 'X1'

## Climate data (tabular data)
tmin <- read_csv('./tbl/values/AgERA5/tmin_monthly.csv')
tmax <- read_csv('./tbl/values/AgERA5/tmax_monthly.csv')
srad <- read_csv('./tbl/values/AgERA5/srad_monthly.csv')
vpor <- read_csv('./tbl/values/AgERA5/vpor_monthly.csv')
rhum <- read_csv('./tbl/values/AgERA5/rhum_monthly.csv')
prec <- read_csv('./tbl/values/chirps/chirps_all.csv')
etps <- read_csv('./tbl/values/etp-penman/etp-monthly.csv') %>% mutate(id = gsub('\t\t', '', id))

## Soils data (tabular data) 
soil <- openxlsx::read.xlsx('./tbl/values/soils/soils_3sl.xlsx')
soil <- as_tibble(soil)

# Base table --------------------------------------------------------------
base <- tmin %>% distinct(id, ID, lon = Longitude, lat = Latitude)

# To tidy the tables ------------------------------------------------------

## Minimum temperature
tmin <- tmin %>%
  transmute(
    id = id, 
    lon = Longitude, 
    lat = Latitude, 
    ID, 
    variable = 'tmin', 
    date = str_sub(var, start = nchar(var) - 9, end = nchar(var)), 
    date = as.Date(date, format = '%Y-%m-%d'),
    value = value - 273.15
  )

tmax <- tmax %>% 
  transmute(
    id = id, 
    lon = Longitude, 
    lat = Latitude, 
    ID, 
    variable = 'tmax', 
    date = str_sub(var, start = nchar(var) - 9, end = nchar(var)), 
    date = as.Date(date, format = '%Y-%m-%d'),
    value = value - 273.15
  )

srad <- srad %>% 
  transmute(
    id = id, 
    lon = Longitude, 
    lat = Latitude, 
    ID, 
    variable = 'srad', 
    date = str_sub(var, start = nchar(var) - 9, end = nchar(var)), 
    date = as.Date(date, format = '%Y-%m-%d'),
    value = value /1000000 # solar radiation from j to kj
  )

vpor <- vpor %>% 
  transmute(
    id = id, 
    lon = Longitude, 
    lat = Latitude, 
    ID, 
    variable = 'vpor', 
    date = str_sub(var, start = nchar(var) - 9, end = nchar(var)), 
    date = as.Date(date, format = '%Y-%m-%d'),
    value = value
  )

prec <- prec %>% 
  transmute(
    ID, 
    month = ifelse(month < 10, paste0('0', month), month),
    date = paste0(year, '-', month, '-01'), 
    date = as.Date(date, format = '%Y-%m-%d'), 
    variable = 'prec', 
    value = chirps
  ) %>% 
  inner_join(
    ., base, by = c('ID' = 'ID')
  ) %>% 
  dplyr::select(
    -month
  )

etps <- etps %>% 
  inner_join(., base, by = c('id', 'ID')) %>% 
  transmute(
    ID, 
    id, 
    date = date, 
    value, 
    variable = 'etp'
  )

rhum <- rhum %>% 
  transmute(
    id = id, 
    lon = Longitude, 
    lat = Latitude, 
    ID, 
    value,
    variable = 'rhum', 
    date = str_sub(var, start = nchar(var) - 9, end = nchar(var)), 
    date = as.Date(date, format = '%Y-%m-%d')
  )

# Spread the table --------------------------------------------------------
tmin <- tmin %>% spread(variable, value)
tmax <- tmax %>% spread(variable, value)
prec <- prec %>% spread(variable, value)
srad <- srad %>% spread(variable, value)
vpor <- vpor %>% spread(variable, value)
etps <- etps %>% spread(variable, value) 
rhum <- rhum %>% spread(variable, value)

nrws <- list(tmin, tmax, prec, rhum, srad, vpor, etps) %>% map_dbl(., nrow) %>% tibble(nrow = ., vars = c('tmin', 'tmax', 'prec', 'rhum', 'srad', 'vpor', 'etps'))

# Daily to monthly --------------------------------------------------------
tmin <- tmin %>% 
  mutate(year = year(date), month = month(date), month = ifelse(month < 10, paste0('0', month), as.character(month))) %>% 
  group_by(ID, id, year, month, lon, lat) %>% 
  dplyr::summarise(tmin = mean(tmin)) %>% 
  ungroup() %>% 
  mutate(date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d')) %>% 
  dplyr::select(ID, id, lon, lat, date, tmin)

tmax <- tmax %>% 
  mutate(year = year(date), month = month(date), month = ifelse(month < 10, paste0('0', month), as.character(month))) %>% 
  group_by(ID, id, year, month, lon, lat) %>% 
  dplyr::summarise(tmax = mean(tmax)) %>% 
  ungroup() %>% 
  mutate(date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d')) %>% 
  dplyr::select(ID, id, lon, lat, date, tmax)

rhum <- rhum %>% 
  mutate(year = year(date), month = month(date), month = ifelse(month < 10, paste0('0', month), as.character(month))) %>% 
  group_by(ID, id, year, month, lon, lat) %>% 
  dplyr::summarise(rhum = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d')) %>% 
  dplyr::select(ID, id, lon, lat, date, rhum)

srad <- srad %>% 
  mutate(year = year(date), month = month(date), month = ifelse(month < 10, paste0('0', month), as.character(month))) %>% 
  group_by(ID, id, year, month, lon, lat) %>% 
  dplyr::summarise(srad = mean(srad, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d')) %>% 
  dplyr::select(ID, id, lon, lat, date, srad)

vpor <- vpor %>% 
  mutate(year = year(date), month = month(date), month = ifelse(month < 10, paste0('0', month), as.character(month))) %>% 
  group_by(ID, id, year, month, lon, lat) %>% 
  dplyr::summarise(vpor = mean(vpor, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d')) %>% 
  dplyr::select(ID, id, lon, lat, date, vpor)

## Check the nrows
length(unique(prec$ID))
length(unique(tmin$ID))
length(unique(tmax$ID))
length(unique(rhum$ID))
length(unique(etps$ID))
length(unique(srad$ID))
length(unique(rhum$ID))


tbls <- list(
  prec, 
  tmin, 
  tmax, 
  etps, 
  rhum, 
  vpor, 
  srad
) %>% 
  reduce(., inner_join)

write.csv(tbls, './tbl/values/climate-vars_monthly.csv', row.names = FALSE)

# Check the table ---------------------------------------------------------
tbls %>% 
  group_by(ID, id) %>% 
  dplyr::summarise(count = n()) %>% 
  ungroup() %>% 
  View()







