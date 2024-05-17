

## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## May 7th - 2024

## Get the values for the trials
## CHIRTS - ERA

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, gridExtra, rgeos, glue, gtools, xlsx, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
prec <- read_csv('./tbl/values/prec_monthly_alls.csv')
prec <- filter(prec, source == 'CHIRPS')

ides <- prec %>% distinct(ID, id)
write.csv(ides, 'ides.csv', row.names = FALSE)

tasm <- read_csv('./tbl/values/era5_1/joined/tasm_era5_1_v2.csv')

# Daily to monthly tasm ---------------------------------------------------
tasm <- tasm %>% mutate(year = year(date), month = month(date)) %>% group_by(ID, year, month) %>% dplyr::summarise(TMax = mean(TMax, na.rm=T), TMin = mean(TMin, na.rm = T)) %>% ungroup() %>% mutate(month = ifelse(month < 10, paste0('0', month), as.character(month)))
tasm <- tasm %>% mutate(date = paste0(year, '-', month, '-01'), date = as.Date(date, format = '%Y-%m-%d'))
tasm <- dplyr::select(tasm, ID, date, tmax = TMax, tmin = TMin)

nrow(prec)
nrow(tasm)

# Join both ---------------------------------------------------------------
tble <- inner_join(prec, tasm, by = c('ID', 'date'))
write.csv(tble, './tbl/values/era5-chirps_monthly.csv', row.names = FALSE)

# To make the graph -------------------------------------------------------

## Function
make.graph <- function(gid){
  
  # gid <- 1
  
  cat('GID: ', gid, '\n')
  tbl <- filter(tble, ID == gid)
  tbl <- mutate(tbl, month = month(date))
  ttl <- unique(tbl$id)
  smm <- tbl %>% group_by(ID, month) %>% summarise(prec = mean(ppt, na.rm = T), tmax = mean(tmax, na.rm = T), tmin = mean(tmin, na.rm = T)) %>% ungroup()
  
  gprec <- ggplot(
  ) + 
    geom_col(
      data = smm, 
      aes(
        x = month,
        y = prec
      )
    ) + 
    labs(
      x = '', 
      y = 'Precipitation (mm)'
    ) + 
    scale_fill_manual(
      values = 'lightblue', 
      name = 'Prec'
    ) +
    coord_cartesian(
      ylim = c(0, 500)
    ) +
    ggtitle(
      label = unique(tbl$id)
    ) +
    theme_light() + 
    theme(
      axis.text.y = element_text(
        angle = 90, 
        hjust = 0.5
      ), 
      plot.title = element_text(
        hjust = 0.5, 
        face = 'bold'
      )
    ) 
  
  rlc <- mean(smm$prec) / mean(smm$tmax); rlc <- round(rlc, 0)
  rlc <- 15
  
  smm.tas <- smm %>% dplyr::select(month, tmin, tmax) %>% gather(var, value, -month) %>% mutate(var = factor(var, levels = c('tmin', 'tmax')))
  
  gall <- gprec + 
    geom_line(
      data = smm.tas,
      aes(
        x = month,
        y = value * rlc, 
        group = var,
        col = var
      )
    ) + 
    scale_color_manual(
      values = c('#886A08', '#DF0101'), 
      name = 'Temp.'
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(~./rlc, name = 'Temperature ºC')
    ) + 
    theme(
      legend.position = 'bottom'
    )
  
  
  ggsave(
    plot = gall, 
    filename = glue('./png/graphs/climatologies/prec-tasm_clima_{ttl}.png'), 
    units = 'in', 
    width = 7, 
    height = 6, 
    dpi = 300
  )
  
  smm <- mutate(smm, name = ttl)
  return(list(gall, smm))
  
}

## Apply the function
rslt <- map(1:34, make.graph)
ggpl <- map(rslt, 1)
smmr <- map(rslt, 2)
smmr <- bind_rows(smmr)

# Make in a pdf
pdf('./pdf/plots_prec-tasm_chirps-agera5.pdf', onefile = TRUE)
for(i in 1:34){
  print(ggpl[[i]])
}
dev.off()






