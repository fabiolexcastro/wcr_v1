
## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## April 19th / 2024

## Get the values for the trials
## CHIRTS - ERA

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, glue, gtools, xlsx, readxl, lubridate)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
prec <- read_csv('./tbl/values/era5_1/joined/prec_era5_1.csv')
tasm <- read_csv('./tbl/values/era5_1/joined/tasm_era5_1_v2.csv')

# Summarise to monthly tasm ------------------------------------------------
tasm <- tasm %>% 
  mutate(month = month(date),
         day = day(date), 
         year = year(date)) %>% 
  group_by(ID, year, month, id) %>% 
  dplyr::summarise(tmax = mean(TMax), tmin = mean(TMin)) %>% 
  ungroup() %>% 
  mutate(month = ifelse(month < 10, paste0('0', month), as.character(month)), 
         date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d')) 

# Summarise to monthly prec -----------------------------------------------
prec <- prec %>% 
  group_by(ID, year, month, id) %>% 
  dplyr::summarise(prec = sum(value)) %>% 
  ungroup() %>% 
  mutate(date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d'))

# Join temperature + precipitation ----------------------------------------
tble <- inner_join(tasm, dplyr::select(prec, ID, date, prec), by = c('ID', 'date'))
unique(tble$ID)
write.csv(tble, './tbl/values/era5_1/joined/tasm-prec_monthly.csv', row.names = FALSE)

# To make the graph  ------------------------------------------------------
tble <- read_csv('./tbl/values/era5_1/joined/tasm-prec_monthly.csv')
ides <- unique(tble$ID)
tble <- mutate(tble, id = gsub('\t\t', '', id))

make.graph <- function(gid){
  
  cat('To process: ', gid, '\n')
  tbl <- filter(tble, ID == gid)
  
  gprec <- ggplot(
  ) + 
    geom_col(
      data = tbl %>% mutate(var = 'prec'), 
      aes(
        x = date,
        y = prec, 
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
      ylim = c(0, 500)
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
  
  rlc <- mean(tbl$prec) / mean(tbl$tmax); rlc <- round(rlc, 0)
  rlc <- 15
  
  tbl.tas <- tbl %>% dplyr::select(date, tmin, tmax) %>% gather(var, value, -date) %>% mutate(var = factor(var, levels = c('tmin', 'tmax')))
  
  gall <- gprec + 
    geom_line(
      data = tbl.tas,
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
  
  gall
  
  ggsave(
    plot = gall, 
    filename = paste0('./png/graphs/AgEra5/prec_tasm/prec-tasm_monthly_', unique(tbl$id), '.jpg'), 
    units = 'in', 
    width = 9,
    height = 7, 
    dpi = 300
  )
  
  cat('Finish!\n')
  
}

# To apply the function ---------------------------------------------------
map(ides, make.graph)

# Climatologies  ----------------------------------------------------------

clma <- tble %>% 
  group_by(ID, id, month) %>% 
  dplyr::summarise(tmax = mean(tmax, na.rm = T), 
                   tmin = mean(tmin, na.rm = T), 
                   prec = mean(prec, na.rm = T)) %>% 
  ungroup() %>% 
  inner_join(
    ., 
    tibble(
      month = c(paste0('0', 1:9), 10:12), 
      month_abb = month.abb
    ), 
    by = 'month'
  ) %>% 
  mutate(
    month_abb = factor(month_abb, levels = month.abb)
  )


smmr <- clma %>% 
  group_by(ID, id) %>% 
  dplyr::top_n(x = ., n = 1, wt = prec) %>% 
  ungroup() %>% 
  dplyr::select(ID, id, month, prec, month_abb)

smmr.prec <- smmr %>% 
  dplyr::select(id, prec) %>% 
  spread(id, prec)
write.csv(smmr.prec, './tbl/values/era5_1/joined/max_prec-clma.csv')


write.csv(clma, './tbl/values/era5_1/joined/prec_tasm_climatologies.csv', row.names = FALSE)

# Make climatologies ------------------------------------------------------
make.clma <- function(gid){
  
  ## Filtering
  cat('To process: ', gid, '\n')
  clm <- filter(clma, ID == gid)
  
  ## To make the graph
  gpt <- ggplot(
  ) + 
    geom_col(
      data = clm, 
      aes(
        x = month_abb, 
        y = prec, 
      ), 
      fill = 'lightblue'
    ) +
    scale_fill_manual(
      values = 'lightblue', 
      name = 'Prec'
    ) +
    labs(
      x = '', 
      y = 'Precipitation (mm)'
    ) + 
    coord_cartesian(
      ylim = c(0, 500)
    ) +
    ggtitle(
      label = unique(clm$id)
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
  
  ## Temperature graph
  tas <- clm %>% 
    dplyr::select(-prec) %>% 
    gather(var, value, -c(ID, id, month, month_abb)) %>% 
    mutate(
      var = factor(
        var, levels = c('tmin', 'tmax')
      )
    )

  gal <- gpt + 
    geom_line(
      data = tas,
      aes(
        x = month_abb,
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
      sec.axis = sec_axis(~./rlc, name = 'Temperature (ºC)')
    ) + 
    theme(
      legend.position = 'bottom'
    )
  
  ggsave(
    plot = gal, 
    filename = paste0('./png/graphs/AgEra5/prec_tasm_clma/prec-tasm_monthly_', unique(clm$id), '.jpg'), 
    units = 'in', 
    width = 9,
    height = 7, 
    dpi = 300
  )
  
}

map(1:34, make.clma)


