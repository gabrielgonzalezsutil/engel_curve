# Expenditure EICV
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

# Rent expenditure -------------------------------------------------
section5 <- read_sav('eicv5/cs_S0_S5_Household.sav')
rent <- as.data.frame(section5[,c('hhid','clust','province','district',
                                 's5aq11','s5bq7','s5bq4a','s5bq4b',
                                 's5bq5','s5bq6a','s5bq6b','s5bq9a','s5bq9b')]) 
setnames(rent, c('clust','s5aq11','s5bq7','s5bq4a','s5bq4b', 's5bq5',
                 's5bq6a','s5bq6b','s5bq9a','s5bq9b'),
         c('cluster','occupancy','outside','rent_amount','rent_unit',
           'rent_inkind','rent_kind_amount','rent_kind_unit','rent_other_amount',
           'rent_other_unit'))
variables <- c('hhid','cluster','province','district')
rent <- erase_label(rent, variables)

# Payments cash
rent$rent_amount[is.na(rent$rent_amount)] <- 0
rent$rent_cash <- 0
rent$rent_cash[rent$rent_unit == 1 & !is.na(rent$rent_unit)] <- rent$rent_amount[rent$rent_unit == 1 & !is.na(rent$rent_unit)] * 12
rent$rent_cash[rent$rent_unit == 2 & !is.na(rent$rent_unit)] <- rent$rent_amount[rent$rent_unit == 2 & !is.na(rent$rent_unit)] * 3
rent$rent_cash[rent$rent_unit == 3 & !is.na(rent$rent_unit)] <- rent$rent_amount[rent$rent_unit == 3 & !is.na(rent$rent_unit)] * 1

# In kind
rent$rent_kind_amount[is.na(rent$rent_kind_amount)] <- 0
rent$rent_kind <- 0
rent$rent_kind[rent$rent_kind_unit == 1 & !is.na(rent$rent_kind_unit)] <- rent$rent_kind_amount[rent$rent_kind_unit == 1 & !is.na(rent$rent_kind_unit)] * 12
rent$rent_kind[rent$rent_kind_unit == 2 & !is.na(rent$rent_kind_unit)] <- rent$rent_kind_amount[rent$rent_kind_unit == 2 & !is.na(rent$rent_kind_unit)] * 3
rent$rent_kind[rent$rent_kind_unit == 3 & !is.na(rent$rent_kind_unit)] <- rent$rent_kind_amount[rent$rent_kind_unit == 3 & !is.na(rent$rent_kind_unit)] * 1

# Total Rent
rent$rent_total <- rent$rent_cash + rent$rent_kind 

# Shared Rent
rent$rent_other_amount[is.na(rent$rent_other_amount)] <- 0
rent$rent_other <- 0
rent$rent_other[rent$rent_other_unit == 1 & !is.na(rent$rent_other_unit)] <- rent$rent_other_amount[rent$rent_other_unit == 1 & !is.na(rent$rent_other_unit)] * 12
rent$rent_other[rent$rent_other_unit == 2 & !is.na(rent$rent_other_unit)] <- rent$rent_other_amount[rent$rent_other_unit == 2 & !is.na(rent$rent_other_unit)] * 3
rent$rent_other[rent$rent_other_unit == 3 & !is.na(rent$rent_other_unit)] <- rent$rent_other_amount[rent$rent_other_unit == 3 & !is.na(rent$rent_other_unit)] * 1

# Ownership
rent$owner <- 0
rent$owner[rent$occupancy == 1] <- 1

# Shared Rent
rent$rent_alone <- 0
rent$rent_alone[rent$occupancy == 2 & rent$outside == 2] <- 1
rent$rent_out <- 0
rent$rent_out[rent$outside == 1 & (rent$rent_cash == 0 & rent$rent_kind == 0)] <- 1
rent$rent_shared <- 0
rent$rent_shared[rent$outside == 1 & (rent$rent_cash != 0 | rent$rent_kind != 0)] <- 1
rent$rent_no <- 0
rent$rent_no[rent$owner == 0 & rent$rent_alone == 0 & rent$rent_out == 0 & rent$rent_shared == 0] <- 1
rent$occupancy <- as.numeric(rent$occupancy)

rent$rent_subsidy <- 0
rent$rent_subsidy[rent$rent_no == 1 | rent$out == 1] <- 100
rent$rent_subsidy[rent$rent_shared == 1] <- round(rent$rent_other[rent$rent_shared == 1] / (rent$rent_total[rent$rent_shared == 1] + rent$rent_other[rent$rent_shared == 1]),2)

# Order variables
rent <- rent %>% select(hhid, cluster, district, province, rent_total, rent_cash, 
                        rent_kind, rent_other, owner, rent_subsidy, occupancy, 
                        rent_alone, rent_out, rent_shared, rent_no)

# Water -------------------------------------------------------------------
water <- as.data.frame(section5[,c('hhid','clust','province','district',
                                         's5cq1','s5cq2','s5cq9a','s5cq9b',
                                         's5cq10','s5cq11','s5cq12','s5cq13',
                                         's5cq14')]) 
setnames(water, c('clust','s5cq1','s5cq2','s5cq9a','s5cq9b', 's5cq10',
                        's5cq11','s5cq12','s5cq13','s5cq14'),
         c('cluster','source','distance','water_unit','water_amount',
           'other_buy','water_priv_amount','water_sold','sold_amount','contribute'))
variables <- c('hhid','cluster','province','district')
water <- erase_label(water, variables)

# Source
water$piped <- 0
water$piped[water$source == 1 | water$source == 2] <- 1
water$private <- 0
water$private[water$other_buy == 1] <- 1
water$distance[is.na(water$distance)] <- 0

# WASAC
water$wasac <- 0
water$wasac[!is.na(water$water_unit) & water$water_unit > 0] <- 12 * (water$water_amount[!is.na(water$water_unit) & water$water_unit > 0] / water$water_unit[!is.na(water$water_unit) & water$water_unit > 0])
  
# Private
water$water_other <- 0
water$water_other[water$private == 1] <- water$water_priv_amount[water$private == 1] * 365

# Private
water$water_source <- 0
water$water_source <- water$contribute * 12
water <- erase_label(water, 'water_source')

# Water sold
water$water_income <- 0
water$water_income[water$water_sold == 1] <- water$sold_amount[water$water_sold == 1] * 365

# Order variables
water <- water %>% select(hhid, cluster, district, province, piped, wasac, 
                        private, other, water_source, water_income)


# Electricity -------------------------------------------------------------
electricity <- as.data.frame(section5[,c('hhid','clust','province','district',
                                         's5cq16','s5cq16a1','s5cq16a4','s5cq16a5',
                                         's5cq16a6','s5cq16a7','s5cq16a8',
                                         '')]) 
setnames(electricity, c('clust','s5aq11','s5bq7','s5bq4a','s5bq4b', 's5bq5','s5bq6a','s5bq6b'),
         c('cluster','occupancy','outside','rent_amount','rent_unit',
           'rent_inkind','rent_kind_amount','rent_kind_unit'))
variables <- c('hhid','cluster','province','district')
rent <- erase_label(rent, variables) 

unique(section5$s5cq16a7)





# Import Section ---------------------------------------------------------------
section4 <- read_sav('eicv5/cs_S1_S2_S3_S4_S6A_S6E_Person.sav')

section1 <- read_sav('eicv5/cs_S1_S2_S3_S4_S6A_S6E_Person.sav')
services <- read_sav('eicv5/cs_S5F_Access_to_services.sav')
employment <- read_sav('eicv5/cs_S6B_Employement_6C_Salaried_S6D_Business.sav')
livestock1 <- read_sav('eicv5/cs_S7A1_livestock.sav')
livestock2 <- read_sav('eicv5/cs_S7A2_livestock.sav')
livestock3 <- read_sav('eicv5/cs_S7A3_livestock.sav')
agr1 <- read_sav('eicv5/cs_S7B1_land_Agriculture.sav')
agr2 <- read_sav('eicv5/cs_S7B2_land_Agriculture.sav')
parcel <- read_sav('eicv5/cs_S7C_parcels.sav')
large_crop <- read_sav('eicv5/cs_S7D_large_crop.sav')
small_crop <- read_sav('eicv5/cs_S7E_small_crop.sav')
inc_agr <- read_sav('eicv5/cs_S7F_income_agriculture.sav')
exp_agr <- read_sav('eicv5/cs_S7G_expenditure_agriculture.sav')
trans_agr <- read_sav('eicv5/cs_S7H_transformation_agriculture.sav')
exp1 <- read_sav('eicv5/cs_S8A1_expenditure.sav')
exp2 <- read_sav('eicv5/cs_S8A2_expenditure.sav')
exp3 <- read_sav('eicv5/cs_S8A3_expenditure.sav')
exp4 <- read_sav('eicv5/cs_S8B_expenditure.sav')
farming <- read_sav('eicv5/cs_S8C_farming.sav')
transfer_in <- read_sav('eicv5/cs_S9B_transfers_in.sav')
vup_rssp_1 <- read_sav('eicv5/cs_S9C_Vup_ubudehe_and_Rssp_schemes.sav')
vup_rssp_2 <- read_sav('eicv5/cs_S9C3_Vup_ubudehe_and_Rssp_schemes.sav')
vup_rssp_3 <- read_sav('eicv5/cs_S9C4_Vup_ubudehe_and_Rssp_schemes.sav')
other_inc <- read_sav('eicv5/cs_S9D_other_income.sav')
other_exp <- read_sav('eicv5/cs_S9E_Other_expenditure.sav')
credits1 <- read_sav('eicv5/cs_S10A1_Credits.sav')
credits2 <- read_sav('eicv5/cs_S10A2_Listing_of_credits.sav')
durable1 <- read_sav('eicv5/cs_S10B1_Durable_household_goods.sav')
durable2 <- read_sav('eicv5/cs_S10B2_Durable_household_goods.sav')
savings <- read_sav('eicv5/cs_S10C1_Savings.sav')
tontine <- read_sav('eicv5/cs_S10C2_Tontine.sav')
income <- read_sav('eicv5/EICV5_Poverty_file.sav')

max(income$cons1)

# Expenditure ------------------------------------------------------------------
aux <- income[,c('hhid','clust','province','district','exp1','exp4',
                 'exp5','exp6','exp7','exp8','exp9','exp10','exp11',
                 'exp12','exp13','exp14_2','exp15_2','exp16_2','exp16a_2',
                 'exp17','exp18','cons1','food','quintile','decile')]
setnames(aux, c('exp1','exp4', 'exp5','exp6','exp7','exp8','exp9','exp10',
                'exp11','exp12','exp13','exp14_2','exp15_2','exp16_2',
                'exp16a_2','exp17','exp18','cons1','food'),
         c('exp_edu','exp_rent_imp','exp_rent_act','exp_maint','exp_water',
           'exp_elec','exp_inkind','exp_employer_subsidy','exp_other_inkind',
           'exp_annual','exp_month','exp_freq','exp_food','own_food','own_nonfood',
           'exp_durable','inkind_value','exp_total','exp_food'))

# To check total consumption look file saved below. 
# setwd('G:/My Drive/rwanda_drive/engel_curves/outputs')
# write.csv(aux,'expenditure.csv')

# Adjust some missing values - I checked and they are zeros!
aux$exp_durable[is.na(aux$exp_durable)] <- 0
aux$inkind_value[is.na(aux$inkind_value)] <- 0

# There is a mistake in EICV5 for electricity consumption (monthly is multiplied
# by 13 and not 12)
aux$exp_total <- aux$exp_total - aux$exp_elec 
aux$exp_elec <- (aux$exp_elec/13) * 12 
aux$exp_total <- aux$exp_total + aux$exp_elec 

aux$exp_rent <- aux$exp_rent_imp + aux$exp_rent_act + aux$exp_maint
aux$exp_other <- aux$exp_total - aux$exp_food - aux$exp_edu - aux$exp_rent - 
  aux$exp_water - aux$exp_elec - aux$exp_durable

##################################### TODO #####################################
# Check all the expenditure values

# Other energy expenditures







aux <- aux[,c('hhid','clust','province','district','exp_food','exp_edu',
                 'exp_rent', 'exp_water', 'exp_elec', 'exp_durable', 'exp_other',
                 'exp_total')]

# Monthly
aux$exp_food <- aux$exp_food/12
aux$exp_edu <- aux$exp_edu/12
aux$exp_water <- aux$exp_water/12
aux$exp_elec <- aux$exp_elec/12
aux$exp_durable <- aux$exp_durable/12
aux$exp_other <- aux$exp_other/12
aux$exp_total <- aux$exp_total/12
  
# Share
aux$food_share <- aux$exp_food/aux$exp_total * 100
aux$edu_share <- aux$exp_edu/aux$exp_total * 100
aux$water_share <- aux$exp_water/aux$exp_total * 100
aux$elec_share <- aux$exp_elec/aux$exp_total * 100
aux$durable_share <- aux$exp_durable/aux$exp_total * 100
aux$other_share <- aux$exp_other/aux$exp_total * 100

eicv <- merge(eicv, aux, by = c('hhid','clust','province','district'), all.x = T)
rm(aux)


setwd('G:/My Drive/research/rwanda_drive/engel_curve/data/processed')
load('households.Rdata')
expenditure <- households[,c('hhid','cluster','province','district')]
rm(households)






