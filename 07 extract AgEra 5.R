


## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## April 30th / 2024

## Get the values for the trials
## AgERA5

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, glue, gtools, readxl, climateR, rnaturalearthdata, rnaturalearth)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Vector data
pnts <- read_excel('./tbl/points/IMLVT Site info.xlsx')[,-1]
colnames(pnts) <- gsub(' ', '_', colnames(pnts))

## Raster data
root <- '//catalogue/WFP_ClimateRiskPr1/1.Data/AgERA5'
dirs <- dir_ls(root, type = 'directory')
vars <- basename(dirs)

## Dates 
year <- paste0('_', 1994:2023)

# Function ----------------------------------------------------------------
extract_values <- function(var){
  
  cat('To process: ', var, '\n')
  fls <- grep(var, dirs, value = T) %>% 
    dir_ls() %>% 
    as.character() %>% 
    grep(paste0(year, collapse = '|'), ., value = T)
  
  tbls <- map(.x = year, .f = function(y){
    
    fle <- grep(y, fls, value = T)
    rst <- terra::rast(fle)
    dts <- time(rst)
    names(rst) <- paste0(var, '_', dts)
    vls <- cbind(pnts[,c('id', 'Longitude', 'Latitude')], terra::extract(rst, pnts[,c('Longitude', 'Latitude')]))
    cat('Done!\n')
    rm(rst); gc(reset = TRUE)
    return(vls)
  
  })
  
  dfrm <- tbls %>% reduce(., inner_join)
  dfrm <- dfrm %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -c(id, Longitude, Latitude, ID)) %>% as_tibble()
  dfrm <- mutate(dfrm, id = gsub('\t\t', '', id))
  return(dfrm)
  
}

# To extract the values ---------------------------------------------------
tmax <- extract_values(var = '2m_temperature-24_hour_maximum')
tmin <- extract_values(var = '2m_temperature-24_hour_minimum')
prec <- extract_values(var = 'precipitation_flux')
srad <- extract_values(var = 'solar_radiation_flux')
rhum <- extract_values(var = '2m_relative_humidity')
vpor <- extract_values(var = 'vapour_pressure')

prec <- rename(prec, date = variable)
prec <- mutate(prec, variable = 'prec')

write.csv(srad, './tbl/values/AgERA5/srad_monthly.csv', row.names = FALSE)
write.csv(vpor, './tbl/values/AgERA5/vpor_monthly.csv', row.names = FALSE)
write.csv(tmax, './tbl/values/AgERA5/tmax_monthly.csv', row.names = FALSE)
write.csv(tmin, './tbl/values/AgERA5/tmin_monthly.csv', row.names = FALSE)
write.csv(prec, './tbl/values/AgERA5/prec_monthly.csv', row.names = FALSE)
write.csv(rhum, './tbl/values/AgERA5/rhum_monthly.csv', row.names = FALSE)

# Read the tables ---------------------------------------------------------
prec <- read_csv('./tbl/values/AgERA5/prec_monthly.csv')
tmin <- read_csv('./tbl/values/AgERA5/tmin_monthly.csv')
tmax <- read_csv('./tbl/values/AgERA5/tmax_monthly.csv')
srad <- read_csv('./tbl/values/AgERA5/srad_monthly.csv')
vpor <- read_csv('./tbl/values/AgERA5/vpor_monthly.csv')

# Join the tables into only one -------------------------------------------
tmax

## Tmax
tmax <- rename(tmax, tmax = value)
tmax <- mutate(tmax, date = str_sub(var, nchar(var) - 9, nchar(var)))
tmax <- mutate(tmax, date = as.Date(date, format = '%Y-%m-%d'))
tmax <- dplyr::select(tmax, -var)
tmax <- tmax %>% mutate(tmax = tmax - 273.15)

## Tmin
tmin <- rename(tmin, tmin = value)
tmin <- mutate(tmin, date = str_sub(var, nchar(var) - 9, nchar(var)))
tmin <- mutate(tmin, date = as.Date(date, format = '%Y-%m-%d'))
tmin <- dplyr::select(tmin, -var)
tmin <- tmin %>% mutate(tmin = tmin - 273.15)

## Prec
prec <- rename(prec, prec = value)
prec <- mutate(prec, date = str_sub(var, nchar(var) - 9, nchar(var)))
prec <- mutate(prec, date = as.Date(date, format = '%Y-%m-%d'))
prec <- dplyr::select(prec, -var)

list(prec, tmin, tmax) %>% map(nrow)

# Join the tables ---------------------------------------------------------
tbls <- list(prec, tmin, tmax)
tbls <- reduce(tbls, inner_join)
write.csv(tbls, './tbl/values/AgERA5/prec-tmax-tmin_daily.csv', row.names = FALSE)

# Daily to monthly --------------------------------------------------------
smmr <- tbls %>% 
  mutate(month = month(date), 
         year = year(date)) %>% 
  group_by(id, ID, year, month) %>% 
  dplyr::summarise(prec = sum(prec, na.rm = T), 
                   tmin = mean(tmin, na.rm = T), 
                   tmax = mean(tmax, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(month = ifelse(month < 10, paste0('0', month), as.character(month))) %>% 
  mutate(date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d')) %>% 
  dplyr::select(-month, -year)

# To write the table ------------------------------------------------------
write.csv(smmr, './tbl/values/AgERA5/prec-tmax-tmin_monthly.csv', row.names = FALSE)
