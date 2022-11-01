# Demographics EICV
# Author: Gabriel Gonzalez Sutil

# Preliminaries ----------------------------------------------------------------
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())
pacman::p_load('data.table','dplyr','plyr','haven')
setwd('G:/My Drive/research/rwanda_drive/engel_curve/data/nisr_eicv/data/')

# Functions --------------------------------------------------------------------

erase_label <- function(data, var){
  for(i in var) attr(data[,c(i)], "label") <- NULL # Cluster erase label
  return(data)
} 

variable <- function(raw, file, condition = NULL, name){
  file[,c(name)] <- NULL
  if(is.null(condition)){
    aux <- raw[,c('hhid','cluster','province','district')] %>% 
      group_by(hhid, cluster, province, district) %>% tally()    
  }else{
    aux <- raw[condition,c('hhid','cluster','province','district')] %>% 
      group_by(hhid, cluster, province, district) %>% tally()    
  }
  setnames(aux, 'n', name)
  file <- merge(file, aux, by = c('hhid','cluster','province','district'), all.x = T)
  if(!is.null(condition)){
    file[is.na(file[,c(name)]),c(name)] <- 0   
  }
  rm(aux)
  return(file)
}

# Import Section ---------------------------------------------------------------
eicv5 <- read_sav('eicv5/cs_S1_S2_S3_S4_S6A_S6E_Person.sav')
aux <- eicv5 %>% select('hhid','clust','province','district',starts_with("s1"),'s0q18y','s0q18m')
rm(eicv5)
section1 <- unique(aux[,c('hhid','clust','province','district','s0q18y','s0q18m')])

# Year and Month
setnames(section1, c('s0q18y','s0q18m','clust'), c('year','month','cluster'))
setnames(aux, c('clust'), c('cluster'))

# Erase Labels
section1 <- as.data.frame(section1)
variables <- c('hhid', 'cluster','province','district','year','month')
section1 <- erase_label(section1, variables)  

# Create Variables -------------------------------------------------------------

# Number of members in the household
section1 <- variable(aux, section1, condition = NULL, 'people')
section1 <- variable(aux, section1, condition = (aux$s1q16 == 1), 'members')
section1$no_members <- section1$people - section1$members

# Number of members in the household by gender
section1 <- variable(aux, section1, condition = (aux$s1q1 == 1), 'male')
section1 <- variable(aux, section1, condition = (aux$s1q1 == 2), 'female')
section1 <- variable(aux, section1, condition = (aux$s1q2 == 1 & aux$s1q1 == 1), 'hh_male')
section1 <- variable(aux, section1, condition = (aux$s1q2 == 1 & aux$s1q1 == 2), 'hh_female')

# Number of members in the household by age range
section1 <- variable(aux, section1, condition = (aux$s1q3y <= 16), '0_16')
section1 <- variable(aux, section1, condition = (aux$s1q3y > 16 & aux$s1q3y <= 30), '17_30')
section1 <- variable(aux, section1, condition = (aux$s1q3y > 30 & aux$s1q3y <= 60), '31_60')
section1 <- variable(aux, section1, condition = (aux$s1q3y > 60), '61_100')

# Head of Household Culture
section1$hh_born <- aux$s1q6[aux$s1q2 == 1]
section1$hh_rwandese <- 1 * (section1$hh_born == 60)
section1$hh_burundian <- 1 * (section1$hh_born == 61)
section1$hh_congolese <- 1 * (section1$hh_born == 62)
section1$hh_ugandan <- 1 * (section1$hh_born == 63)
section1$hh_tanzanian <- 1 * (section1$hh_born == 64)
section1$hh_kenyan <- 1 * (section1$hh_born == 65)
section1$hh_african <- 1 * (section1$hh_born == 66)
section1$hh_foreign <- 1 * (section1$hh_born == 67)
section1$hh_born <- NULL

section1$hh_district <- as.numeric(aux$s1q5[aux$s1q2 == 1])
section1$hh_district[section1$hh_rwandese == 0] <- 0

# Effective people in the house
aux$s1q8[aux$s1q7 == 2] <- 0
aux$s1q7[aux$s1q7 == 2] <- 0
aux$absent <- aux$s1q8 * aux$s1q7
section1 <- variable(aux, section1, condition = (aux$absent > 0), 'absent')
section1$effective <- round(((section1$members * 12) - section1$absent)/12,2)
section1$absent <- NULL

# Order and save
section1 <- section1 %>% select('hhid', 'month', 'year', 'cluster','province',
                                'district', everything())
demographics <- section1
rm(section1, aux)
save(demographics, file = "G:/My Drive/research/rwanda_drive/engel_curve/data/processed/demographics.Rdata")
