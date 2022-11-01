# Housing Expenditure EICV
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
water$water_other[water$private == 1] <- water$water_priv_amount[water$private == 1] * 52.14286

# Private
water$water_source <- 0
water$water_source <- water$contribute * 12
water <- erase_label(water, 'water_source')

# Water sold
water$water_income <- 0
water$water_income[water$water_sold == 1] <- water$sold_amount[water$water_sold == 1] * 365

# Order variables
water <- water %>% select(hhid, cluster, district, province, piped, wasac, private, 
                        water_other, water_source, distance, water_income)


# Electricity -------------------------------------------------------------
electricity <- as.data.frame(section5[,c('hhid','clust','province','district',
                                         's5cq16','s5cq16a1','s5cq16a4','s5cq16a5',
                                         's5cq16a6','s5cq16a8','s5cq16a9','s5cq17')]) 
setnames(electricity, c('clust','s5cq16','s5cq16a1','s5cq16a4','s5cq16a5',
                        's5cq16a6','s5cq16a8','s5cq16a9','s5cq17'),
         c('cluster','source','main_source','pay','availability',
           'availability_evening','blackouts','duration_blackouts','electricity'))
variables <- c('hhid','cluster','province','district')
electricity <- erase_label(electricity, variables) 

# Source electricity
electricity$pay[electricity$main_source == 7] <- 3
electricity$pay_elec <- 0
electricity$pay_elec[electricity$pay != 3] <- 1

electricity$grid <- 0 
electricity$grid[electricity$main_source == 1 | electricity$main_source == 2] <- 1
electricity$generator <- 0 
electricity$generator[electricity$main_source == 3] <- 1
electricity$solar <- 0 
electricity$solar[electricity$main_source == 4] <- 1
electricity$lantern <- 0 
electricity$lantern[electricity$main_source == 5] <- 1
electricity$battery <- 0 
electricity$battery[electricity$main_source == 6] <- 1
electricity$other <- 0 
electricity$other[electricity$main_source == 8] <- 1
electricity$no_electricity <- 0 
electricity$no_electricity[electricity$main_source == 7] <- 1

electricity$light_grid <- 0 
electricity$light_grid[electricity$source == 1 | electricity$source == 2] <- 1
electricity$light_generator <- 0 
electricity$light_generator[electricity$source == 4] <- 1
electricity$light_solar <- 0 
electricity$light_solar[electricity$source == 9] <- 1
electricity$light_battery <- 0 
electricity$light_battery[electricity$source == 10 | electricity$source == 12] <- 1
electricity$light_latern <- 0 
electricity$light_latern[electricity$source == 8] <- 1
electricity$light_other <- 0 
electricity$light_other[electricity$source == 3 | electricity$source == 5 |
                          electricity$source == 6 | electricity$source == 7 |
                          electricity$source == 11 | electricity$source == 13] <- 1


# Payment
electricity$electricity[is.na(electricity$electricity)] <- 0 
electricity$electricity <- electricity$electricity * 12 
electricity <- erase_label(electricity, 'electricity')

# Not good quality 
electricity$duration_blackouts[is.na(electricity$duration_blackouts)] <- 0
electricity$duration_blackouts[electricity$duration_blackouts == 888] <- NA
electricity$availability[is.na(electricity$availability)] <- 0
electricity$availability[electricity$availability == 888] <- NA
electricity$availability_evening[is.na(electricity$availability_evening)] <- 0
electricity$availability_evening[electricity$availability_evening == 888] <- NA

# Order variables
electricity <- electricity %>% select(hhid, cluster, district, province, 
                                      electricity, no_electricity, pay_elec, 
                                      grid, generator, solar, lantern, battery, 
                                      other, light_grid, light_generator, 
                                      light_solar, light_battery, light_latern,
                                      light_other)

# Fuels -------------------------------------------------------------
cooking <- as.data.frame(section5[,c('hhid','clust','province','district',
                                     's5cq18','s5cq18b71','s5cq18b72',
                                     's5cq18b8a','s5cq18b8b')]) 
setnames(cooking, c('clust','s5cq18','s5cq18b71','s5cq18b72','s5cq18b8a',
                        's5cq18b8b'), c('cluster','source','fuel1','fuel2',
                                        'fuel1_cost','fuel2_cost'))
variables <- c('hhid','cluster','province','district')
electricity <- erase_label(cooking, variables) 

# Fuels

# Order variables
electricity <- electricity %>% select(hhid, cluster, district, province, 
                                      electricity, no_electricity, on_grid, 
                                      grid, generator, solar, lantern, battery, 
                                      other, light_grid, light_generator, 
                                      light_solar, light_battery, light_latern,
                                      light_other)

# Save data --------------------------------------------------------------------
save(rent, file = "G:/My Drive/research/rwanda_drive/engel_curve/data/processed/rent.Rdata")
save(water, file = "G:/My Drive/research/rwanda_drive/engel_curve/data/processed/water.Rdata")
save(electricity, file = "G:/My Drive/research/rwanda_drive/engel_curve/data/processed/electricity.Rdata")
save(cooking, file = "G:/My Drive/research/rwanda_drive/engel_curve/data/processed/cooking.Rdata")

