###############################################################################
##
## 11_2_Info_SI4.R
##
## Purpose: Compute some figures related to the Supp Fig4 given in the paper
##
## Camille Magneville
##
## cleaned version: 28/02/2022
##
###############################################################################

# load data:
bites_nb_cam_day_df <- readRDS(here::here("transformed_data", "bites_nb_cam_day_df.rds"))


# compute the mean proportion of tme allocated to grazing for each species.
mean(bites_nb_cam_day_df[which(bites_nb_cam_day_df$species == "Sarpa_salpa"), "prop_bites_fr_nb"])
mean(bites_nb_cam_day_df[which(bites_nb_cam_day_df$species == "Siganus_rivulatus"), "prop_bites_fr_nb"])
mean(bites_nb_cam_day_df[which(bites_nb_cam_day_df$species == "Sparisoma_cretense"), "prop_bites_fr_nb"])
mean(bites_nb_cam_day_df[which(bites_nb_cam_day_df$species == "Siganus_luridus"), "prop_bites_fr_nb"])
