
## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## April 23th / 2024

## Get the values for the trials
## ERA-5 V1.1

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, pals, glue, tidyverse, rgeos, gtools, openxlsx, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## List the files
fles <- dir_ls('./tbl/values/era5_1')
vars <- c('2m_relativ_humidity', '2m_temperature', '2m_precipitation_flux')

## Points
pnts <- read.xlsx('./tbl/points/IMLVT Site info.xlsx')
pnts <- as_tibble(pnts)

# Tidy temperatre ---------------------------------------------------------
tasm <- as.character(grep('2m_temperature', fles, value = T))
tasm <- map(.x = 1:length(tasm), .f = function(i){

  cat('To process: ', tasm[i], '\n')
  tas <- tasm[i]
  tas <- read_csv(tas, show_col_types = FALSE)
  tas <- gather(tas, var, value, -ID)
  tas <- mutate(tas, variable = str_sub(var, 20, 22))
  tas <- mutate(tas, date = str_sub(var, 28, nchar(var)))
  tas <- mutate(tas, date = as.Date(date, formals = '%Y-%m-%d'))
  tas <- transmute(tas, ID, date, variable = paste0('T', variable), value)
  tas <- filter(tas, variable != 'Tmea')
  tas <- drop_na(tas, date)
  dts <- unique(tas$date)
  tas <- spread(tas, variable, value)
  return(tas)
  
})
tasm <- bind_rows(tasm)
write.csv(tasm, './tbl/values/era5_1/joined/tasm_era5_1.csv', row.names = FALSE)
tasm <- read_csv('./tbl/values/era5_1/joined/tasm_era5_1.csv')

# Convert to celcius ------------------------------------------------------
tasm <- mutate(tasm, TMax = TMax - 273.15)
tasm <- mutate(tasm, TMin = TMin - 273.15)

# Join with the points table ----------------------------------------------
tasm.2 <- inner_join(tasm, pnts, by = c('ID' = 'X1'))
as.data.frame(table(pnts$Country)) %>% arrange(desc(Freq))
tasm.2 <- tasm.2 %>% mutate(id = factor(id))
gids <- unique(tasm.2$ID)

# To make the graph  ------------------------------------------------------
make.graph <- function(x){
  
  cat('To process: ', x, '\n')
  tas <- filter(tasm.2, ID == x)
  tas <- mutate(tas, year = year(date), month = month(date))
  tas <- tas %>% group_by(id, Site, Country, year, month) %>% summarise(TMax = mean(TMax, na.rm = T), TMin = mean(TMin, na.rm = T)) %>% ungroup()
  tas <- tas %>% mutate(month = ifelse(month < 10, paste0('0', month), month))
  tas <- tas %>% mutate(date = as.Date(paste0(year, '-', month, '-01')))
  tas <- tas %>% gather(var, value, -id, -Site, -Country, -year, -month, -date)
  tas <- tas %>% mutate(var = ifelse(var == 'TMax', 'Maximum temperature', 'Minimum temperature'))
  tas <- tas %>% mutate(var = factor(var, levels = c('Minimum temperature', 'Maximum temperature')))
  nme <- tas$id
  nme <- unique(nme)
  nme <- as.character(nme)
  nme <- gsub('\t\t', '', nme)
  tas <- mutate(tas, id = nme)
  
  print(nme)
  
  g.tas <- ggplot(data = tas, aes(x = date, y = value, group = var, col = var)) + 
    geom_line() + 
    scale_color_manual(values = c('#04B431', '#086A87')) +
    scale_x_date(date_abels = '%Y', breaks = 'year') +
    labs(col = '', x = 'Year', y = 'Temperature (°C)', caption = 'AgERA5 1.1') + 
    ggtitle(label = nme) +
    theme_light() + 
    theme(legend.position = 'bottom', 
          plot.title = element_text(face = 'bold', hjust = 0.5),
          legend.key.width = unit(1.5, 'line')) + 
    guides(color = guide_legend(override.aes = list(size = 8)))

  dout <- './png/graphs/AgEra5'
  ggsave(plot = g.tas, filename = glue('{dout}/{nme}.png'), units = 'in', width = 9, height = 7, dpi = 300)
  cat('Done!\n')
  return(tas)
  
}

# To apply the function ---------------------------------------------------
tasm.smmr <- map(gids, make.graph)
tasm.smmr

tasm.2
tasm.smmr

write.csv(tasm.2, './tbl/values/era5_1/joined/tasm_era5_1_v2.csv', row.names = FALSE)

# Write the excel files temperature ---------------------------------------
nmes <- as.character(unlist(map(tasm.smmr, function(x){unique(x$id)})))
xlsx::write.xlsx(tasm.smmr[[1]], './tbl/values/era5_1/monthly/monthly_tasm.xlsx', sheetName = nmes[1], append = FALSE)
for(i in 2:length(nmes)){
  cat(i, '\t')
  xlsx::write.xlsx(tasm.smmr[[i]], './tbl/values/era5_1/monthly/monthly_tasm.xlsx', sheetName = nmes[i], append = TRUE)  
}

lngt <- length(excel_sheets('./tbl/values/era5_1/monthly/monthly_tasm.xlsx'))

xlsx::write.xlsx(tasm.smmr[[lngt+1]], './tbl/values/era5_1/monthly/monthly_tasm_p2.xlsx', sheetName = nmes[lngt+1], append = FALSE)

for(i in 30:length(nmes)){
  cat(i, '\t')
  xlsx::write.xlsx(tasm.smmr[[i]], './tbl/values/era5_1/monthly/monthly_tasm_p2.xlsx', sheetName = nmes[i], append = TRUE)  
}

xlsx::write.xlsx(tasm.smmr[[33]], './tbl/values/era5_1/monthly/monthly_tasm_p3.xlsx', sheetName = nmes[33], append = FALSE)  
xlsx::write.xlsx(tasm.smmr[[34]], './tbl/values/era5_1/monthly/monthly_tasm_p3.xlsx', sheetName = nmes[34], append = FALSE)  


# Precipitation -----------------------------------------------------------
prec <- grep('precipitation', fles, value = T)
prec <- as.character(prec)
prec <- map(prec, read_csv, show_col_types = FALSE)

## Tidy the precipitation table -------------------------------------------
prec <- reduce(prec, inner_join, by = 'ID')
colnames(prec) <- gsub('Precipitation_Flux_', 'Prec_', colnames(prec))
prec <- gather(prec, var, value, -ID)
prec <- prec %>% 
  mutate(date = str_sub(var, 6, nchar(var)), 
         date = as.Date(date, format = '%Y-%m-%d'), 
         year = year(date), 
         month = month(date), 
         month = ifelse(month < 10, paste0('0', month), as.character(month)))

## Aggregate to monthly dataset -------------------------------------------
prec.smmr <- prec %>% 
  group_by(ID, year, month) %>% 
  dplyr::summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d'))

prec.smmr <- inner_join(prec.smmr, pnts, by = c('ID' = 'X1'))


write.csv(prec.smmr, './tbl/values/era5_1/joined/prec_era5_1.csv', row.names = FALSE)

prec.smmr <- read_csv('./tbl/values/era5_1/joined/prec_era5_1.csv')

# Function to make the figure ---------------------------------------------
plot.prec <- function(x){
  
  cat('To process: ', x, '\n')
  dfrm <- filter(prec.smmr, ID == x)
  nme <- unique(dfrm$id)
  
  gppt <- ggplot(data = dfrm, aes(x = date, y = value)) + 
    geom_col() +
    scale_x_date(date_labels = '%Y', breaks = 'year') +
    labs(col = '', x = 'Year', y = 'Precipitación (mm)', caption = 'AgERA5 1.1') + 
    ggtitle(label = nme) +
    theme_light() + 
    theme(legend.position = 'bottom', 
          plot.title = element_text(face = 'bold', hjust = 0.5),
          legend.key.width = unit(1.5, 'line')) 
  
  dout <- './png/graphs/AgEra5'
  ggsave(plot = gppt, filename = glue('{dout}/prec_{nme}.png'), units = 'in', width = 9, height = 7, dpi = 300)
  cat('Done!\n')
  
}

map(unique(pnts$X1), plot.prec)








