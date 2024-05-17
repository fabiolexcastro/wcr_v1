
## Author: Fabio Alexander Castro - Llanos 
## Alliance Bioversity - CIAT 
## May 9th - 2024

## Get the DTW clustering

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, gridExtra, rgeos, glue, gtools, readxl, dtw, dtwclust)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Functions ---------------------------------------------------------------
select_cluster <- function(number){
  hclus <- stats::cutree(data_hdtw[[number]], k = number) %>% # hclus <- cluster::pam(dist_ts, k = 2)$clustering has a similar result
    as.data.frame(.) %>%
    dplyr::rename(.,cluster_group = .) %>%
    tibble::rownames_to_column("type_col") %>% 
    setNames(c('ISO3', 'cluster_group'))
}

# Load data ---------------------------------------------------------------
tble <- read_csv('./tbl/values/prec_monthly_alls.csv')

# Tidy the table ----------------------------------------------------------
tble <- tble %>% spread(source, ppt)
colnames(tble) <- gsub(' ', '_', colnames(tble))
colnames(tble) <- gsub('-', '', colnames(tble))
tble <- dplyr::select(tble, ID, date, AgERA5:Terraclimate)
tble <- drop_na(tble)

# Z - normalization  ------------------------------------------------------
znor <- tble %>% 
  dplyr::select(-ID, -date) %>% 
  zscore() %>% 
  as_tibble() %>% 
  mutate(id = tble$ID, date = tble$date, .before = 'AgERA5') %>% 
  mutate(id = as.character(id)) %>% 
  gather(source, value, -id, -date)

# DTW Clustering ----------------------------------------------------------
data_list <- as.list(unstack(znor, value~source))
nclust <- 2L:3L
data_hdtw <- tsclust(data_list, 
                     type = "hierarchical", 
                     k = nclust,  
                     distance = "dtw_basic",
                     centroid = shape_extraction, 
                     control = hierarchical_control(method = "complete"), 
                     args = tsclust_args(dist = list(window.size = 1L)))

data_eval <- data_hdtw %>%
  sapply(cvi, type = "internal") %>%
  as.data.frame() %>%
  rownames_to_column("cvi") %>%
  gather(key = "cluster", value = "value", -cvi) %>%
  filter(cvi == "Sil" | cvi == "D" | cvi == "COP") %>%
  mutate(cvi2 = ifelse(cvi == "Sil", "SIL* - to be maximized", 
                       ifelse(cvi == "D", "D* - to be maximized", 
                              ifelse(cvi == "COP", "COP* - to be minimized", cvi)))) %>% 
  as_tibble()

data_eval <- inner_join(data_eval, tibble(cluster = glue('V{1:2}'), cluster_num = 1:2), by = 'cluster')
data_eval <- mutate(data_eval, cluster_num = factor(cluster_num, levels = 1:2))

gg_eval <- ggplot() +
  geom_line(data = data_eval, aes(x = cluster_num, y = value, group = cvi)) +
  scale_x_discrete(breaks = 2:10) +
  facet_wrap(~cvi2, scales = "free", ncol = 1) +
  labs(x = '# Clusters', y = 'Value') +
  theme_ipsum_es() +
  theme(strip.text = element_text(family = 'Roboto', face = 'bold'))

# Select 3 clusters -------------------------------------------------------

hclust_2 <- select_cluster(2)
hclust_3 <- select_cluster(2)

