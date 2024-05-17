
## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## May 14th / 2024

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, pals, glue, tidyverse, rgeos, gtools, openxlsx, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Tabular data
pnts <- read_excel('./tbl/points/IMLVT Site info.xlsx')[,-1]
colnames(pnts) <- gsub(' ', '_', colnames(pnts))

## Raster data (ISRIC)
root <- '//catalogue/WFP_ClimateRiskPr1/1.Data/SoilGrids250m/Physical soil properties'
dirs <- dir_ls(root, type = 'directory') %>% as.character()

# Function to extract  ----------------------------------------------------
extr.vles <- function(dir){
  
  cat('To process: ', basename(dir), '\t')
  fls <- as.character(dir_ls(dir, regexp = '.tif$'))
  spd <- c('sl1', 'sl2', 'sl3')
  fls <- grep(paste0(spd, collapse = '|'), fls, value = T)
  rst <- terra::rast(fls)
  vls <- terra::extract(rst, pnts[,c('Longitude', 'Latitude')])
  vls <- cbind(pnts[,c('id')], vls)
  vls <- as_tibble(vls)
  vls <- mutate(vls, variable = basename(dir))
  colnames(vls)[3:5] <- spd
  rm(rst); gc(reset = T)
  cat(green('Done!\n'))
  return(vls) 
}

# To apply the function ---------------------------------------------------
vles <- map(dirs, extr.vles)
vles <- bind_rows(vles)
vles <- full_join(vles, pnts, by = 'id')
write.xlsx(vles, './tbl/values/soils/soils_3sl.xlsx')

# End ---------------------------------------------------------------------
unique(vles$variable)










