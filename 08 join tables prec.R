

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
chrp <- read_csv('./tbl/values/chirps/chirps_all.csv')
tclm <- read_csv('./tbl/values/terraclimate/terraclimate_2015-2023.csv')
ager <- read_csv('./tbl/values/AgERA5/AgERA5_2015-2023_monthly.csv')
ag11 <- read_csv('./tbl/values/era5_1/joined/prec_era5_1.csv')

# Join all the tables into only one  --------------------------------------

## Terraclimate
tclm <- dplyr::select(tclm, ID, date, ppt) %>% mutate(source = 'Terraclimate')

## CHIRPS
chrp <- mutate(chrp, month = ifelse(month < 10, paste0('0', month), as.character(month)), date = paste0(year, '-', month, '-01'), date = as.Date(date, format = '%Y-%m-%d'))
chrp <- dplyr::select(chrp, ID, date, ppt = chirps) %>% mutate(source = 'CHIRPS')

## AgERA
ager <- ager %>% dplyr::select(ID, id, Longitude, Latitude, date, ppt = prec) %>% mutate(source = 'AgERA-5')
base <- ager %>% dplyr::select(ID, id, Longitude, Latitude) %>% distinct()
ager <- dplyr::select(ager, ID, date, ppt, source)
ager <- mutate(ager, year = year(date), month = month(date)) %>% group_by(ID, year, month,source) %>% dplyr::summarise(ppt = sum(ppt, na.rm = T)) %>% ungroup()
ager <- mutate(ager, month = ifelse(month < 10, paste0('0', month), as.character(month)), date = paste0(year, '-', month, '-01'), date = as.Date(date, format = '%Y-%m-%d'))
ager <- dplyr::select(ager, ID, date, ppt, source)

## AgERA 1.1
ag11 <- ag11 %>% dplyr::select(date, ID, ppt = value)
ag11 <- mutate(ag11, source = 'AgERA-5 1.1')
ag11 <- ag11 %>% mutate(year = year(date), month = month(date)) %>% group_by(year, month, source, ID) %>% dplyr::summarise(ppt = sum(ppt, na.rm = T)) %>% ungroup()
ag11 <- ag11 %>% mutate(month = ifelse(month < 10, paste0('0', month), as.character(month)))
ag11 <- ag11 %>% mutate(date = paste0(year, '-', month, '-01'), date = as.Date(date, format = '%Y-%m-%d'))
ag11 <- ag11 %>% dplyr::select(ID, date, ppt, source)

## Join alls into a list 
tble <- list(tclm, chrp, ager, ag11) %>% bind_rows()
tble <- inner_join(tble, base, by = 'ID')

write.csv(tble, './tbl/values/prec_monthly_alls.csv', row.names = FALSE)

# To make the graph  ------------------------------------------------------

## Function to use
make.graph <- function(gid){
  
  ## To filter
  cat('To process: ', gid, '\n')
  tbl <- filter(tble, ID == gid)
  ttl <- unique(tbl$id)
  
  ## To make the graph (bar)
  ggb <- ggplot(
    data = tbl, 
    aes(
      x = date, 
      y = ppt
    )
  ) + 
    geom_col() + 
    facet_wrap(
      .~source
    ) +
    scale_x_date(
      date_labels = '%Y', 
      breaks = 'year'
    ) +
    ggtitle(
      label = unique(tbl$id)
    ) +
    labs(x = '', y = 'Precipitation (mm)') +
    theme_light() + 
    theme(
      plot.title = element_text(face = 'bold', hjust = 0.5),
      strip.text = element_text(face = 'bold'), 
      axis.text.y = element_text(angle = 90, hjust = 0.5)
    ) 
  
  ## To make the graph (line)
  ggl <- ggplot(
    data = tbl, 
    aes(
      x = date, 
      y = ppt, 
      col = source
    )
  ) + 
    geom_line(
      size = 1.1
    ) + 
    scale_color_manual(
      values = brewer.pal(n = 4, name = 'Set1')
    ) +
    scale_x_date(
      date_labels = '%Y', 
      breaks = 'year'
    ) +
    scale_y_continuous(
      limits = c(0, 1500), 
      breaks = seq(0, 1500, 250), 
      labels = seq(0, 1500, 250)
    ) +
    ggtitle(
      label = unique(tbl$id)
    ) +
    labs(x = '', y = 'Precipitation (mm)', col = 'Source') +
    theme_light() + 
    theme(
      legend.position = 'bottom', 
      plot.title = element_text(face = 'bold', hjust = 0.5),
      strip.text = element_text(face = 'bold'), 
      axis.text.y = element_text(angle = 90, hjust = 0.5)
    ) 
  
  dir <- './png/graphs/comparison'
  ggsave(plot = ggb, filename = glue('{dir}/prec_bars_{ttl}.jpg'), units = 'in', width = 9, height = 7, dpi = 300)
  ggsave(plot = ggl, filename = glue('{dir}/prec_line_{ttl}.jpg'), units = 'in', width = 9, height = 7, dpi = 300)
  cat('Done!\n')
  return(ggl)
  
}

## To apply the function
ggls <- map(1:34, make.graph)

# Join all the graphs into only one pdf -----------------------------------
pdf('./pdf/plots_prec-lines.pdf', onefile = TRUE)
for(i in 1:34){
  print(ggls[[i]])
}
dev.off()
