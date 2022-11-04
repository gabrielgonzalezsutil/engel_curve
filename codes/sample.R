# Sample construction
# Author: Gabriel Gonzalez Sutil

# Preliminaries ----------------------------------------------------------------
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())
pacman::p_load('data.table','dplyr','plyr','haven','labelled','ggplot2',
               'ggExtra','mgcv')
setwd('G:/My Drive/research/rwanda_drive/engel_curve/data/processed')

# Function ---------------------------------------------------------------------

desc_stats <- function(descriptive, data, variables, names){
  means <- data.frame()
  median <- data.frame()
  standard <- data.frame()
  min <- data.frame()
  max <- data.frame()
  i <- 1
  for(v in variables){
    means[i,1] <- round(mean(eicv[,c(v)]),2)
    median[i,1] <- round(median(eicv[,c(v)]),2)
    standard[i,1] <- round(sd(eicv[,c(v)]),2)
    min[i,1] <- round(min(eicv[,c(v)]),2)
    max[i,1] <- round(max(eicv[,c(v)]),2)
    i <- i + 1
  }
  rm(i, v)
  colnames(means) <- 'mean';   colnames(standard) <- 'st.dev.';
  colnames(median) <- 'median';   colnames(min) <- 'min';
  colnames(max) <- 'max'
  aux <- data.frame('variables' = names, 'mean' = means, 'median' = median, 
                    'st.dev.' = standard, 'min' = min, 'max' = max)
  rm(means, median, min, max, standard)
  descriptive <- rbind(descriptive, aux)
  return(descriptive)
  rm(aux)
}

# Expenditure ------------------------------------------------------------------
load("rent.Rdata")
load("water.Rdata")
load("electricity.Rdata")
load("cooking.Rdata")
load("expenditure.Rdata")

# Electricity
exp <- electricity[,c('hhid','cluster','district','province','pay_elec',
                      'electricity')]
rm(electricity)

# Water
water$water <- water$wasac + water$water_other 
exp <- merge(exp, water[,c('hhid','cluster','district','province','water')],
             by = c('hhid','cluster','district','province'), all.x = T)
rm(water)

# Rent 
setnames(rent, 'rent_total','rent')
exp <- merge(exp, rent[,c('hhid','cluster','district','province','rent')],
             by = c('hhid','cluster','district','province'), all.x = T)
rm(rent)

# Rent 
exp <- merge(exp, expenditure, by = c('hhid','cluster','district','province'), all.x = T)
rm(expenditure)

# Cooking
exp <- merge(exp, cooking[,c('hhid','cluster','district','province','fuel_fossil',
                             'fuel_coal','fuel_biomass','fuel_biogas','fuel_other')], 
             by = c('hhid','cluster','district','province'), all.x = T)
rm(cooking)

# Total Expenditure
exp$other_energy <- exp$f_petrol + exp$f_diesel + exp$f_gas + exp$f_kerosene +
  exp$f_charcoal + exp$f_wood + exp$fuel_biogas + exp$fuel_biomass + exp$fuel_other
exp$electricity <- exp$electricity + exp$f_fuel_generator
exp$energy <- exp$electricity + exp$other_energy
exp$transport <- exp$a_transport + exp$m_transport + exp$f_transport
exp$belongings <- exp$a_clothing + exp$a_belongings
exp$health <- exp$a_health + exp$m_health
exp$leisure <- exp$a_leisure + exp$m_leisure + exp$f_leisure
exp$communication <- exp$m_communication + exp$f_communication
exp$housing <- exp$a_housing + exp$a_furnishings + exp$m_housing + 
  exp$f_energy_materials
exp$other <- exp$a_other + exp$m_care + exp$m_other + exp$f_care +
  exp$f_repairs + exp$f_other

exp <- exp %>% select(hhid, cluster, district, province, pay_elec, energy, 
                      electricity, other_energy, food, water, 
                      belongings, health, leisure, communication, housing, other) %>%
  mutate(exp_total =  energy + food + water + belongings + 
           health + leisure + communication + housing + other)

exp <- exp[exp$pay_elec == 1 & exp$exp_total > 0,]

max(exp$exp_total)

exp$elec_share <- exp$electricity / exp$exp_total

exp <- merge(housing, expenditure, by = c('hhid', 'cluster','district','province'), all.x = T)

rm(households, demographics)


load("households.Rdata")
load("demographics.Rdata")



eicv <- merge(households, demographics, 
              by = c('hhid', 'cluster','district','province','month','year'), all.x = T)

rm(households, demographics)

# Tables  ----------------------------------------------------------------------

##### Energy use

# Levels

# Descriptive of energy use

# Quintile


##### Descriptive statistics 
table3 <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(table3) <- c('variables','mean','median','st.dev.','min','max')

# Expenditure
table3[nrow(table3)+1,] <- c('Expenditure Share','','','','','')
table3[nrow(table3)+1,] <- c('','','','','','')
table3[nrow(table3)+1,] <- c('','','','','','')

# Demographics
table3[nrow(table3)+1,] <- c('Demographics','','','','','')
variables <- c('people','effective','hh_male','0_16','17_30','31_60','61_100','hh_rwandese')
names <- c('  Family Size', '  Average Members/Month','  Male HH', '  Under 16 yrs', '  17 to 30 yrs',
           '  31 to 60 yrs', '  More than 60 yrs', '  Rwandese')
table3 <- desc_stats(table3, eicv, variables, names)




