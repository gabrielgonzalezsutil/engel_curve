# Annual Expenditure EICV
# Author: Gabriel Gonzalez Sutil

# Preliminaries ----------------------------------------------------------------
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())
pacman::p_load('data.table','dplyr','plyr','haven')
setwd('G:/My Drive/research/rwanda_drive/engel_curve/data/nisr_eicv/data/')

# Function ---------------------------------------------------------------

erase_label <- function(data, var){
  for(i in var) attr(data[,c(i)], "label") <- NULL # Cluster erase label
  return(data)
} 

annual_expenditure <- function(expenditure, data, new_var, init, fin){
  aux <- as.data.frame(data[data$s8a1q0 >= init &  data$s8a1q0 <= fin,])
  aux <- aux %>%
    group_by(hhid, clust, province, district) %>% 
    dplyr::summarise(new_var = sum(s8a1q3)) 
  setnames(aux, 'new_var', new_var)
  expenditure <- merge(expenditure, aux, 
                       by = c('hhid','clust','province','district'), all.x = T)
  return(expenditure)
} 

month_expenditure <- function(expenditure, data, new_var, init, fin){
  aux <- as.data.frame(data[data$s8a2q0 >= init &  data$s8a2q0 <= fin,])
  aux <- aux %>%
    group_by(hhid, clust, province, district) %>% 
    dplyr::summarise(new_var = sum(s8a2q3) * 12) 
  setnames(aux, 'new_var', new_var)
  expenditure <- merge(expenditure, aux, 
                       by = c('hhid','clust','province','district'), all.x = T)
  return(expenditure)
} 

freq_expenditure <- function(expenditure, data, new_var, init, fin){
  aux <- as.data.frame(data[data$s8a3q0 >= init &  data$s8a3q0 <= fin,])
  aux <- aux %>%
    group_by(hhid, clust, province, district) %>% 
    dplyr::summarise(new_var = sum(annual_exp)) 
  setnames(aux, 'new_var', new_var)
  expenditure <- merge(expenditure, aux, 
                       by = c('hhid','clust','province','district'), all.x = T)
  return(expenditure)
} 

# Annual expenditure -------------------------------------------------
exp <- read_sav('eicv5/cs_S8A1_expenditure.sav')
annual_exp <- unique(as.data.frame(exp[,c('hhid','clust', 'province','district')])) 
exp$s8a1q3[is.na(exp$s8a1q3)] <- 0

# Expenditure
annual_exp <- annual_expenditure(annual_exp, exp, 'a_clothing',1,17)
annual_exp <- annual_expenditure(annual_exp, exp, 'a_belongings',18,28)
annual_exp <- annual_expenditure(annual_exp, exp, 'a_housing',29,30)
annual_exp <- annual_expenditure(annual_exp, exp, 'a_furnishings',31,53)
annual_exp <- annual_expenditure(annual_exp, exp, 'a_transport',54,57)
annual_exp <- annual_expenditure(annual_exp, exp, 'a_leisure',58,61)
annual_exp <- annual_expenditure(annual_exp, exp, 'a_health',62,67)
annual_exp <- annual_expenditure(annual_exp, exp, 'a_other',68,69)

setnames(annual_exp, c('clust'), c('cluster'))
variables <- c('hhid','cluster','province','district')
annual_exp <- erase_label(annual_exp, variables)

# Month expenditure -------------------------------------------------
exp <- read_sav('eicv5/cs_S8A2_expenditure.sav')
month_exp <- unique(as.data.frame(exp[,c('hhid','clust', 'province','district')])) 
exp$s8a2q3[is.na(exp$s8a2q3)] <- 0

# Expenditure
month_exp <- month_expenditure(month_exp, exp, 'm_housing',1,9)
month_exp <- month_expenditure(month_exp, exp, 'm_transport',10,13)
month_exp <- month_expenditure(month_exp, exp, 'm_leisure',14,24)
month_exp <- month_expenditure(month_exp, exp, 'm_care',25,35)
month_exp <- month_expenditure(month_exp, exp, 'm_communication',36,39)
month_exp <- month_expenditure(month_exp, exp, 'm_health',42,55)
month_exp <- month_expenditure(month_exp, exp, 'm_other',40,41)

setnames(month_exp, c('clust'), c('cluster'))
month_exp <- erase_label(month_exp, variables)

# Frequent expenditure -------------------------------------------------
exp <- read_sav('eicv5/cs_S8A3_expenditure.sav')
freq_exp <- unique(as.data.frame(exp[,c('hhid','clust', 'province','district')])) 
exp$s8a3q3[is.na(exp$s8a3q3)] <- 0
exp$month_exp <- exp$s8a3q3 + exp$s8a3q4 + exp$s8a3q5 + exp$s8a3q6 + exp$s8a3q7 + 
  exp$s8a3q8 + exp$s8a3q9 + exp$s8a3q10 + exp$s8a3q11+ exp$s8a3q12 + 
  exp$s8a3q13
exp$annual_exp <- exp$month_exp
exp$annual_exp[exp$s8a3q2 > 0] <- exp$month_exp[exp$s8a3q2 > 0] * exp$s8a3q2[exp$s8a3q2 > 0]

# Expenditure
freq_exp <- freq_expenditure(freq_exp, exp, 'f_leisure',1,5)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_care',6,10)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_petrol',11,11)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_diesel',12,12)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_transport',13,18)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_gas',19,19)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_kerosene',20,20)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_charcoal',21,21)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_wood',22,22)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_energy_materials',23,26)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_fuel_generator',27,27)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_repairs',28,30)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_communication',31,34)
freq_exp <- freq_expenditure(freq_exp, exp, 'f_other',35,37)

setnames(freq_exp, c('clust'), c('cluster'))
freq_exp <- erase_label(freq_exp, variables)

# Food expenditure -------------------------------------------------
exp <-read_sav('eicv5/cs_S8B_expenditure.sav')
exp$month_exp <- exp$s8bq3 + exp$s8bq4 + exp$s8bq5 + exp$s8bq6 + exp$s8bq7 + 
  exp$s8bq8 + exp$s8bq9 + exp$s8bq10 + exp$s8bq11+ exp$s8bq12 + exp$s8bq13
exp$annual_exp <- exp$month_exp
exp$annual_exp[exp$s8bq2 > 0] <- exp$month_exp[exp$s8bq2 > 0] * exp$s8bq2[exp$s8bq2 > 0]

# Expenditure
food_exp <- exp %>% group_by(hhid, clust, province, district) %>% 
    dplyr::summarise(food = sum(annual_exp)) 
setnames(food_exp, c('clust'), c('cluster'))
food_exp <- erase_label(food_exp, variables)
rm(exp)

# Expenditure aggregate ---------------------------------------------
expenditure <- annual_exp
expenditure <- merge(expenditure, month_exp, 
                     by = c('hhid','cluster','province','district'), all.x = T)
expenditure <- merge(expenditure, freq_exp, 
                     by = c('hhid','cluster','province','district'), all.x = T)
expenditure <- merge(expenditure, food_exp, 
                     by = c('hhid','cluster','province','district'), all.x = T)
rm(annual_exp, food_exp, freq_exp, month_exp)

save(expenditure, file = "G:/My Drive/research/rwanda_drive/engel_curve/data/processed/expenditure.Rdata")
