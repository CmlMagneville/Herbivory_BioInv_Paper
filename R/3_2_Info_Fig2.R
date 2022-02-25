###############################################################################
##
## 3_2_Info_Fig2.R
##
## Purpose: Compute some figures related to the fig2 given in the paper
##
## Camille Magneville
##
## cleaned version: 25/02/2022
##
###############################################################################


# 1 - Proportion of daily activity realised by each species ####


# load the data:
bites_sp_prop_df <- readRDS(here::here("transformed_data/bites_sp_prop_df.rds"))

# new column that will contain prop of daily act for each species:

bites_sp_prop_df$prop_graz <- rep(NA, nrow(bites_sp_prop_df))

for (i in unique(bites_sp_prop_df$day_cam)) {
  
  data <- bites_sp_prop_df[which(bites_sp_prop_df$day_cam == i), ]
  
  # fill  daily prop of bites nb realised by SS:
  bites_sp_prop_df[which(bites_sp_prop_df$species == "Sarpa_salpa" & bites_sp_prop_df$day_cam == i), 5] <- (data$bites_nb_sp[which(data$species == "Sarpa_salpa")] / data$bites_nb_tot[which(data$species == "Sarpa_salpa")]) * 100
  
  # fill  daily prop of bites nb realised by SR:
  bites_sp_prop_df[which(bites_sp_prop_df$species == "Siganus_rivulatus" & bites_sp_prop_df$day_cam == i), 5] <- (data$bites_nb_sp[which(data$species == "Siganus_rivulatus")] / data$bites_nb_tot[which(data$species == "Siganus_rivulatus")]) * 100
  
  # fill  daily prop of bites nb realised by SL:
  bites_sp_prop_df[which(bites_sp_prop_df$species == "Siganus_luridus" & bites_sp_prop_df$day_cam == i), 5] <- (data$bites_nb_sp[which(data$species == "Siganus_luridus")] / data$bites_nb_tot[which(data$species == "Siganus_luridus")]) * 100
  
  # fill  daily prop of bites nb realised by SC:
  bites_sp_prop_df[which(bites_sp_prop_df$species == "Sparisoma_cretense" & bites_sp_prop_df$day_cam == i), 5] <- (data$bites_nb_sp[which(data$species == "Sparisoma_cretense")] / data$bites_nb_tot[which(data$species == "Sparisoma_cretense")]) * 100
  
}

# 2 - compute mean of prop of graz act by species:

#SS:
mean(bites_sp_prop_df$prop_graz[which(bites_sp_prop_df$species == "Sarpa_salpa")])
#SR
mean(bites_sp_prop_df$prop_graz[which(bites_sp_prop_df$species == "Siganus_rivulatus")])
#SC
mean(bites_sp_prop_df$prop_graz[which(bites_sp_prop_df$species == "Sparisoma_cretense")])
#SL
mean(bites_sp_prop_df$prop_graz[which(bites_sp_prop_df$species == "Siganus_luridus")])
