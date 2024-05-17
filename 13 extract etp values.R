

## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## May 7th - 2024

## Make just one table for all the variables

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, crayon, gridExtra, rgeos, glue, gtools, xlsx, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Tabular data 
pnts <- read_excel('./tbl/points/IMLVT Site info.xlsx')
colnames(pnts) <- gsub(' ', '_', colnames(pnts))
colnames(pnts)[1] <- 'X1'

## Climate data (raster data)
fles <- dir_ls('./workspace/etp/FAO-Penman-Montieth/ET_0_monthly', regexp = '.tif$')
fles <- as.character(fles)

# To extract the values  --------------------------------------------------

## Function
extr.vles <- function(fle){

  cat(green('To process: ', basename(fle), '\t'))
  rst <- rast(fle)
  vle <- terra::extract(rst, pnts[,c('Longitude', 'Latitude')])
  dte <- basename(fle) %>% gsub('.tif$', '', .)
  colnames(vle) <- c('ID', dte)
  return(vle)
  
}

## To apply the function 
vles <- map(fles, extr.vles)
vles <- vles %>% reduce(., inner_join, by = 'ID')
vles <- as_tibble(vles)
vles <- gather(vles, var, value, -ID) %>% mutate(date = str_sub(var, 4, nchar(var)), date = paste0(date, '-01'), date = as.Date(date, format = '%Y-%m-%d')) %>% dplyr::transmute(ID, date, variable = 'etp', value)

base <- dplyr::select(pnts, ID = X1, id)
vles <- inner_join(vles, base, by = c('ID'))

write.csv(vles, './tbl/values/etp-penman/etp-monthly.csv', row.names = FALSE)
