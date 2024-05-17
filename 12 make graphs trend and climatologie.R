

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
clma <- read_csv('./tbl/values/prec-tasm_chirps-agera5_climatologie.csv')
tble <- read_csv('./tbl/values/prec-tasm_chirps-agera5_monthly.csv')
clma
tble


# To make the graphs ------------------------------------------------------

## Function ------- 
make.graph <- function(gid, min, max){
  
  # gid <- 1
  
  cat('>>> ', gid, '\n')  
  tbl <- filter(tble, ID == gid)
  clm <- filter(clma, ID == gid)
  
  ## Trend graphics
  gprec <- ggplot(
  ) + 
    geom_col(
      data = tbl %>% mutate(var = 'prec') %>% filter(year >= 2015), 
      aes(
        x = date,
        y = chirps, 
        fill = var
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
      ylim = c(min, max)
    ) +
    scale_x_date(
      date_labels = '%Y', 
      breaks = 'year'
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
  
  # rlc <- mean(tbl$prec) / mean(tbl$tmax); rlc <- round(rlc, 0)
  rlc <- 15
  
  tbl.tas <- tbl %>% dplyr::select(date, tmin, tmax) %>% gather(var, value, -date) %>% mutate(var = factor(var, levels = c('tmin', 'tmax')))
  
  gall <- gprec + 
    geom_line(
      data = tbl.tas %>% 
        mutate(year = year(date)) %>% 
        filter(year >= 2015),
      aes(
        x = date,
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
    filename = paste0('./png/graphs/AgEra5-chirps/prec-tasm_time-series_', unique(tbl$id), '.jpg'), 
    units = 'in', 
    width = 9,
    height = 7, 
    dpi = 300
  )
  
  gtsr <- gall
  
  ## Climatologie graphics
  gprec <- ggplot(
  ) + 
    geom_col(
      data = clm, 
      aes(
        x = month,
        y = chirps
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
  
  # rlc <- mean(smm$prec) / mean(smm$tmax); rlc <- round(rlc, 0)
  rlc <- 15
  
  smm.tas <- clm %>% dplyr::select(month, tmin, tmax) %>% gather(var, value, -month) %>% mutate(var = factor(var, levels = c('tmin', 'tmax')))
  
  gall <- gprec + 
    geom_line(
      data = smm.tas,
      aes(
        x = month,
        y = value * rlc, 
        group = var,
        col = var
      ), 
      fill = 'lightblue'
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
    filename = paste0('./png/graphs/AgEra5-chirps/prec-tasm_climatologie_', unique(tbl$id), '.jpg'), 
    units = 'in', 
    width = 9,
    height = 7, 
    dpi = 300
  )
  
  gclm <- gall
  
  gall <- ggpubr::ggarrange(gtsr, gclm, ncol = 2, nrow = 1)
  ggsave(plot = gall, filename = glue('./png/graphs/AgEra5-chirps/prec-tasm_tsr-clm_{unique(tbl$id)}.jpg'), units = 'in', width = 11, height = 6.5)
  return(gall)
    
}

## To apply the function -------
gids <- unique(clma$ID)
ggpl <- map(.x = 1:length(gids), .f = function(k){
  make.graph(gid = k, min = 0, max = 500)
})
ggpl <- .Last.value

# Save the graphs into a PDF ----------------------------------------------

pdf('./pdf/plots_tsr-clma_ppt-tas_chirps-agera5.pdf', onefile = TRUE, width = 13, height = 7)
for(i in 1:34){
  print(ggpl[[i]])
}
dev.off()


