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
pnts <- read_excel('./tbl/points/IMLVT Site info.xlsx')
colnames(pnts) <- gsub(' ', '_', colnames(pnts))
colnames(pnts)[1] <- 'X1'

## Raster data
root <- '//CATALOGUE/WFP_ClimateRiskPr1/1.Data/Chirps'
fles <- as.character(dir_ls(root))

# Get the values ----------------------------------------------------------
map(.x = 1994:2014, .f = function(y){
  
  cat('To process ', y, '\n')
  year <- paste0('0.', y, '.')
  file <- grep(year, fles, value = T)
  
  tbls <- map(.x = file, .f = function(f){
    
    cat('To process: ', basename(f), '\n')
    rstr <- rast(f)
    names(rstr) <- str_sub(names(rstr), 13, nchar(names(rstr)))
    names(rstr) <- glue('prec_{names(rstr)}')
    vles <- terra::extract(rstr, pnts[,c('Longitude', 'Latitude')])
    return(vles)
    
  })
  
  tbls <- tbls %>% reduce(., inner_join, by = 'ID')
  tbls <- as_tibble(tbls)
  dout <- glue('./tbl/values/chirps')
  write.csv(tbls, glue('{dout}/prec_{y}.csv'), row.names = FALSE)
  
})

# Tidy the results tables -------------------------------------------------
fles <- dir_ls('./tbl/values/chirps', regexp = '.csv')
fles <- as.character(fles)
fles <- grep('prec_', fles, value = T)

tbls <- map(fles, read_csv, show_col_types = FALSE)
tbls <- tbls %>% reduce(., inner_join, by = 'ID')

# Gather the table --------------------------------------------------------
tbls <- tbls %>% 
  gather(var, value, -ID) %>% 
  separate(data = ., col = 'var', into = c('variable', 'date'), sep = '_') %>% 
  mutate(date = as.Date(date, format = '%Y.%m.%d')) 

## Add the year and month column 
smmr <- tbls %>%
  mutate(year = year(date), 
         month = month(date)) %>% 
  group_by(ID, year, month) %>% 
  dplyr::summarise(value = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(month = ifelse(month < 10, paste0('0', month), as.character(month))) %>% 
  mutate(date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d')) %>% 
  dplyr::select(ID, date, year, month, value)

# Join with points table --------------------------------------------------
smmr <- inner_join(smmr, pnts, by = c('ID' = 'X1'))
smmr <- mutate(smmr, id = gsub('\t\t', '', id))

# To make the graph  ------------------------------------------------------
gids <- unique(smmr$ID)
map(.x = gids, .f = function(i){
  
  tbl <- filter(smmr, ID == i)
  nme <- unique(tbl$id)
  
  g.prec <- ggplot(data = tbl, aes(x = date, y = value)) + 
    geom_col() + 
    scale_x_date(date_labels = '%Y', breaks = 'year') +
    labs(col = '', x = '', y = 'Precipitation (mm)', caption = 'AgERA5 1.1') + 
    ggtitle(label = nme) +
    theme_light() + 
    theme(
      plot.title = element_text(hjust = 0.5, face = 'bold')
    )
  
  g.prec
  
})

# List the prec files presults ---------------------------------------------
fles <- dir_ls('./tbl/values/chirps', regexp = '.csv$')
fles <- grep('prec', fles, value = T)
tble <- map(fles, read_csv)
tble <- tble %>% purrr::reduce(., inner_join, by = 'ID')

## Gather the table 
prec <- tble %>% 
  gather(var, value, -ID) %>% 
  separate(col = 'var', into = c('vr', 'date'), sep = '_') %>% 
  mutate(
    date = as.Date(date, format = '%Y.%m.%d'), 
    month = month(date), 
    year = year(date)
  )

prec.smmr <- prec %>% 
  group_by(ID, vr, year, month) %>% 
  dplyr::summarise(value = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::rename(chirps = value) %>% 
  dplyr::select(-vr)

write.csv(prec.smmr, './tbl/values/chirps/chirps_all.csv', row.names = FALSE)



