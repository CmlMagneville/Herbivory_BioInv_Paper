###############################################################################
##
## 8_2_Info_Fig6.R
##
## Purpose: Compute some figures related to the Fig6 given in the paper
##
## Camille Magneville
##
## cleaned version: 28/02/2022
##
###############################################################################


# 1 - Compute the proportion of herbivory done each day for ss and sr only ####


# Load data:
bites_nb_all_cam_top_df <- readRDS(here::here("transformed_data", "bites_nb_all_cam_top_df.rds"))

# Retrieve day nms:
days <- unique(bites_nb_all_cam_top_df$day)

# 02/10:
prop_02_sssr <- (sum(bites_nb_all_cam_top_df$bites_nb[which(bites_nb_all_cam_top_df$day == days[1] 
                                                            & bites_nb_all_cam_top_df$species %in% 
                                                              c("Sarpa_salpa", "Siganus_rivulatus"))]) / 
                   sum(bites_nb_all_cam_top_df$bites_nb[which(bites_nb_all_cam_top_df$species %in% 
                                                                c("Sarpa_salpa", "Siganus_rivulatus"))]))*100

# 03/10:
prop_03_sssr <- (sum(bites_nb_all_cam_top_df$bites_nb[which(bites_nb_all_cam_top_df$day == days[2] 
                                                            & bites_nb_all_cam_top_df$species %in% 
                                                              c("Sarpa_salpa", "Siganus_rivulatus"))]) / 
                   sum(bites_nb_all_cam_top_df$bites_nb[which(bites_nb_all_cam_top_df$species %in% 
                                                                c("Sarpa_salpa", "Siganus_rivulatus"))]))*100

# 04/10:
prop_04_sssr <- (sum(bites_nb_all_cam_top_df$bites_nb[which(bites_nb_all_cam_top_df$day == days[3] 
                                                            & bites_nb_all_cam_top_df$species %in% 
                                                              c("Sarpa_salpa", "Siganus_rivulatus"))]) / 
                   sum(bites_nb_all_cam_top_df$bites_nb[which(bites_nb_all_cam_top_df$species %in% 
                                                                c("Sarpa_salpa", "Siganus_rivulatus"))]))*100



# 2 - Compute the proportion of herbivory done by ss and sr (separated) in the afternoon ####

timespan_morning <- hms::as_hms(c(hms::as_hms("08:00:00"):hms::as_hms("13:09:59")))
timespan_afternoon <- hms::as_hms(c(hms::as_hms("13:10:00"):hms::as_hms("18:10:00")))


# SS:
ss_afternoon <- (sum(bites_nb_all_cam_top_df$bites_nb[which(
  bites_nb_all_cam_top_df$species == "Sarpa_salpa" &
    hms::as_hms(bites_nb_all_cam_top_df$time) %in% timespan_afternoon)]) / 
    sum(bites_nb_all_cam_top_df$bites_nb[which(
      bites_nb_all_cam_top_df$species == "Sarpa_salpa")]))*100

# SR:
sr_afternoon <- (sum(bites_nb_all_cam_top_df$bites_nb[which(
  bites_nb_all_cam_top_df$species == "Siganus_rivulatus" &
    hms::as_hms(bites_nb_all_cam_top_df$time) %in% timespan_afternoon)]) / 
    sum(bites_nb_all_cam_top_df$bites_nb[which(
      bites_nb_all_cam_top_df$species == "Siganus_rivulatus")]))*100

# SS and SR:
sssr_afternoon <- (sum(bites_nb_all_cam_top_df$bites_nb[which(
  bites_nb_all_cam_top_df$species %in% c("Siganus_rivulatus", "Sarpa_salpa")  &
    hms::as_hms(bites_nb_all_cam_top_df$time) %in% timespan_afternoon)]) / 
    sum(bites_nb_all_cam_top_df$bites_nb[which(
      bites_nb_all_cam_top_df$species %in% c("Siganus_rivulatus", "Sarpa_salpa"))]))*100

sssr_morning <- (sum(bites_nb_all_cam_top_df$bites_nb[which(
  bites_nb_all_cam_top_df$species %in% c("Siganus_rivulatus", "Sarpa_salpa")  &
    hms::as_hms(bites_nb_all_cam_top_df$time) %in% timespan_morning)]) / 
    sum(bites_nb_all_cam_top_df$bites_nb[which(
      bites_nb_all_cam_top_df$species %in% c("Siganus_rivulatus", "Sarpa_salpa"))]))*100


# 3 - Compare presence of species and grazing ####

# SR:
# sequences with no individual grazing:
seq_sr_nograze <- nrow(bites_nb_all_cam_top_df[which(bites_nb_all_cam_top_df$species == "Siganus_rivulatus" & bites_nb_all_cam_top_df$bites_nb == 0), ])

# prop of sequences with no grazing that have a mean maxN != 0:
prop_sr <- (nrow(bites_nb_all_cam_top_df[which(
  bites_nb_all_cam_top_df$species == "Siganus_rivulatus" & 
    bites_nb_all_cam_top_df$bites_nb == 0 &
    bites_nb_all_cam_top_df$mean_maxN != 0), ]) / 
    nrow(bites_nb_all_cam_top_df[which(
      bites_nb_all_cam_top_df$species == "Siganus_rivulatus" & 
        bites_nb_all_cam_top_df$bites_nb == 0), ])) * 100


# SS:

# sequences with no individual grazing:
seq_ss_nograze <- nrow(bites_nb_all_cam_top_df[which(bites_nb_all_cam_top_df$species == "Sarpa_salpa" & bites_nb_all_cam_top_df$bites_nb == 0), ])

# prop of sequences with no grazing that have a mean maxN != 0:
prop_ss <- (nrow(bites_nb_all_cam_top_df[which(
  bites_nb_all_cam_top_df$species == "Sarpa_salpa" & 
    bites_nb_all_cam_top_df$bites_nb == 0 &
    bites_nb_all_cam_top_df$mean_maxN != 0), ]) / 
    nrow(bites_nb_all_cam_top_df[which(
      bites_nb_all_cam_top_df$species == "Sarpa_salpa" & 
        bites_nb_all_cam_top_df$bites_nb == 0), ])) * 100



