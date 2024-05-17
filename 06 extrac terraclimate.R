

## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## April 30th / 2024

## Get the values for the trials
## Terraclimate

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, glue, gtools, readxl, climateR, rnaturalearthdata, rnaturalearth)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Vector data
wrld <- ne_countries(returnclass = 'sf', scale = 50)

## Points data
pnts <- read.xlsx('./tbl/points/IMLVT Site info.xlsx')
pnts <- as_tibble(pnts)
isos <- unique(pnts$Country.ISO.code)

## List the datasets raster 
fles <- as.character(dir_ls('./data/tif/terraclimate_world'))

# To extract the valuzs ---------------------------------------------------
vles <- map(.x = 1:length(fles), .f = function(i){
  
  ## To start the analysis
  cat('To process: ', basename(fles[i]), '\n')
  
  ## To read the raster
  file <- fles[i]
  rstr <- rast(file)
  
  ## Get the name of the variable
  varb <- basename(fles[i]) %>% str_split(., pattern = '_') %>% map_chr(2) %>% unique()
  names(rstr) <- glue('{varb}_{time(rstr)}')
  
  ## To extract the values 
  vles <- as_tibble(cbind(pnts[,c('id', 'Longitude', 'Latitude')], terra::extract(rstr, pnts[,c('Longitude', 'Latitude')])))
  return(vles)
  
})
vles <- vles %>% reduce(., inner_join, by = c('id', 'ID', 'Longitude', 'Latitude'))

# Tidy the table ----------------------------------------------------------
vles <- vles %>% 
  gather(var, value, -id, -Longitude, -Latitude, -ID) %>% 
  separate(
    data = ., col = 'var', into = c('variable', 'date'), sep = '_'
  ) %>% 
  mutate(
    date = as.Date(date, format = '%Y-%m-%d')
  ) %>% 
  spread(
    variable, value
  )

write.csv(vles, './tbl/values/terraclimate/terraclimate_2015-2023.csv', row.names = FALSE)









