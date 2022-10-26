# Households EICV
# Author: Gabriel Gonzalez Sutil

# Preliminaries ----------------------------------------------------------------
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())
pacman::p_load('data.table','dplyr','plyr','haven')
setwd('G:/My Drive/research/rwanda_drive/engel_curves/data/nisr_eicv/data/')

# Function ---------------------------------------------------------------

erase_label <- function(data, var){
  for(i in var) attr(data[,c(i)], "label") <- NULL # Cluster erase label
  return(data)
} 

# Import Section ---------------------------------------------------------------
eicv5 <- read_sav('eicv5/cs_S0_S5_Household.sav')
section0 <- eicv5 %>% select('hhid','clust','province','district', 'ur',
                             'region', 'weight', 'pop_wt', 'poverty', 'quintile',
                             starts_with("s0"))
section0 <- as.data.frame(section0); rm(eicv5)

# Adjust names
setnames(section0, c('clust','ur', 'weight','s0qb','s0q18y', 's0q18m'),
         c('cluster','rural','hh_wt','UBUDEHE','year','month'))

# Adjust values
section0$poor[section0$poverty == 3] <- 0
section0$poor[section0$poverty != 3] <- 1
section0$aux <- section0$poverty
section0$poverty <- NA
section0$poverty[section0$aux == 1] <- 'extreme'
section0$poverty[section0$aux == 2] <- 'moderetely'
section0$poverty[section0$poor == 0] <- 'non poor'
section0$aux <- NULL
section0$rural[section0$rural == 2] <- 1
section0$rural[section0$rural == 1] <- 0
section0$UBUDEHE[section0$UBUDEHE == 5] <- NA

# Erase Labels
variables <- c('hhid', 'cluster','province','rural','district','region','hh_wt',
                 'pop_wt','poverty','quintile','UBUDEHE','year','month')
section0 <- erase_label(section0, variables)  

# Order and save
section0 <- section0 %>% select('hhid', 'month', 'year', 'hh_wt', 'pop_wt', 
                                 'cluster','province','district', 'region', 
                                 'rural', 'poor', 'poverty', 'quintile','UBUDEHE')
households <- section0; rm(section0)
save(households, file = "G:/My Drive/research/rwanda_drive/engel_curves/data/processed/households.Rdata")
