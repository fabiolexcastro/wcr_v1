


## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## April 19th / 2024

## Get the values for the trials
## CHIRTS - ERA

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, glue, gtools, xlsx, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Vector data
pnts <- read_excel('./tbl/points/IMLVT Site info.xlsx')[,-1]
colnames(pnts) <- gsub(' ', '_', colnames(pnts))

## Raster data
root <- '//catalogue/workspace-cluster9/2024/DOWNLOAD/CHIRTS-ERA5/data/tif'
dirs <- as.character(dir_ls(root))
vars <- basename(dirs)

# Function to use ---------------------------------------------------------
extr.vles <- function(var){
  
  # var <- 'tmax'
  
  ## To list the files
  cat('To process:', var, '\n')
  fls <- grep(var, dirs, value = T) %>% 
    dir_ls(type = 'directory') %>% 
    as.character() %>% 
    grep(paste0(2015:2023, collapse = '|'), ., value = T)
  
  ## To read the data and extract the values
  tbl <- map(.x = fls, .f = function(x){
    
    ## To list the files (.tif)
    cat('To process the year: ', basename(x), '\n')
    fl <- dir_ls(x, regexp = '.tif$') %>% as.character()
    yr <- basename(x)
    
    ## To extract the values
    tb <- map(.x = 1:12, .f = function(m){
      
      ## Filtering the month
      cat('To process month: ', month.abb[m], '\n')
      m <- ifelse(m < 10, paste0('0', m), as.character(m))
      
      ## Read the raster
      r <- rast(grep(paste0('.', yr, '.', m, '.'), fl, value = T))
      
      ## Extract the values
      names(r) <- unlist(map(str_split(names(r), '_'), 2))
      v <- terra::extract(r, pnts[,c('Longitude', 'Latitude')])
      # v <- cbind(pnts, v)
      # v <- as_tibble(v)
      v <- as_tibble(v)
      return(v)
      
    })
    
    tb <- tb %>% reduce(., inner_join, by = 'ID')
    cat('Year done!\n')
    return(tb)

  })
  
  tbl <- as_tibble(cbind(tbl[[1]], bind_cols(map(.x = 2:length(tbl), .f = function(x){tbl[[x]][,-1]}))))
  nms <- colnames(tbl) %>% str_sub(., 1, 4) %>% unique()
  write.csv(tbl, glue('./tbl/values/chirts-era5/{var}_tsr-daily.csv'), row.names = FALSE)

}

# To apply the function ---------------------------------------------------
extr.vles(var = 'tmin')
extr.vles(var = 'tmax')






