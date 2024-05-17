
## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## April 23th / 2024

## Get the values for the trials
## ERA-5 V1.1

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, glue, tidyverse, rgeos, gtools, openxlsx, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Tabular data
pnts <- read_excel('./tbl/points/IMLVT Site info.xlsx')[,-1]
colnames(pnts) <- gsub(' ', '_', colnames(pnts))

## Raster data (list)
root <- '//ALLIANCEDFS.ALLIANCE.CGIAR.ORG/data_cluster17/observed/gridded_products/era5/sis-agromet/nc'
dirs <- as.character(dir_ls(root, type = 'directory'))

## Years
year <- 2015:2023
year <- paste0('_', year)

# Extract values ----------------------------------------------------------

## Function to use
extr.vles <- function(var){
  
  var <- 'precipitation_flux'
  
  cat('To process: ', var, '\n')
  fls <- dir_ls(grep(var, dirs, value = T))
  fls <- as.character(fls)
  fls <- grep(paste0(year, collapse = '|'), fls, value = T)
  
  rsl <- map(.x = 2015:2022, .f = function(y){
    
    cat('To process: ', y , '\n')
    fl <- grep(paste0('_', y), fls, value = T)
    nm <- basename(fl)
    
    vl <- map(.x = 1:length(fl), .f = function(x){
      
      cat('To process raster: ', x, '\n')
      f <- fl[x]
      r <- rast(f)
      n <- names(r)
      v <- terra::extract(r, pnts[,c('Longitude', 'Latitude')])
      colnames(v) <- c('ID', paste0(n, '_', as.character(time(r))))
      v <- as_tibble(v)
      return(v)
      
    })
    
    vv <- map(2:length(vl), function(i){v <- vl[[i]][,-1]})
    vv <- bind_cols(vv)
    df <- cbind(vl[[1]], vv)
    
    ## To write the final table 
    dr <- glue('./tbl/values/era5_1')
    write.csv(df, glue('{dr}/{var}_raw-{y}.csv'), row.names = FALSE)
    cat('Done!\n')
    
  })
  
  
}

## To apply the function
extr.vles(var = '2m_temperature')
extr.vles(var = 'precipitation_flux')
extr.vles(var = '2m_relative_humidity')
extr.vles(var = 'solar_radiation_flux')


rslt <- read.xlsx('./tbl/values/era5_1/joined/tasm_era5_1.csv')

