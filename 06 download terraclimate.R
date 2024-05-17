
## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## May 2th / 2024

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, pals, glue, tidyverse, rgeos, gtools, openxlsx, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
root <- 'https://climate.northwestknowledge.net/TERRACLIMATE-DATA'
urle <- 'https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_tmax_1993.nc'
year <- 2015:2023

# Function to download ----------------------------------------------------
down.rstr <- function(varb){
  
  cat('To process: ', varb, '\n')
  urls <- paste0(root, '/', 'Terraclimate_', varb, '_', year, '.nc')
  dout <- './data/tif/terraclimate/world'
  
  map(.x = 1:length(urls), .f = function(i){
    
    cat('To download', basename(urls[i]), '\n')
    url <- urls[i]
    fle <- paste0(dout, '/', basename(url))
    download.file(url = url, destfile = fle, mode = 'wb')
    rst <- rast(fle)
    
    
  })
  
  
}

