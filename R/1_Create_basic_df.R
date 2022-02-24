###############################################################################
##
## 1_Create_basic_df.R
##
## Purpose: Creates basic dataframes that will be used for further analysis...
## ...  these df (dataframes) are saved in the transformed_data folder
##
## Camille Magneville
##
## cleaned version: 24/02/2022
##
###############################################################################


# 1 - Load the raw data ####


# Load presence/absence dataframe:
final_pres_abs_tot_0_1 <- readRDS(here::here("raw_data/final_0_1_all_cam_all_days.rds"))

# Load abundance dataframe:
final_pres_abs_tot <- readRDS(here::here("raw_data/final_df_abund_all_cam_all_days.rds"))

# Load grazing dataframe:
final_pres_abs_graz_tot <- readRDS(here::here("raw_data/final_fd_behav_all_cam_all_days.rds"))


