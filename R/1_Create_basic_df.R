###############################################################################
##
## 1_Create_basic_df.R
##
## Purpose: Creates basic dataframes that will be used for further analysis...
## ...  these df (dataframes) are saved in the transformed_data folder ...
## ... /!\ it's a long process that can take some time
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

# Load annotation data:
annot_data <- readRDS(here::here("raw_data/annot_data.rds"))


####


# 2 - Create the new df  sums up for each sequence, the maxN of sp ... ####
# ... the nb of bites, the mean number of grazing individuals and the max nb ...
# ...of grazing individuals


# new df:
all_info_df <- final_pres_abs_graz_tot

# add a column to add sequence number:
all_info_df$seq <- rep(NA, nrow(all_info_df))
all_info_df$daycam <- rep(NA, nrow(all_info_df))

# complete sequence number:
for (i in (1:nrow(all_info_df))) {
  time <- all_info_df$time[i]
  
  for (j in (1:nrow(annot_data))) {
    annot_start_hour <- hms::as_hms(annot_data$real_time_start[j])
    annot_stop_hour <- hms::as_hms(annot_data$real_time_stop[j])
    
    if (annot_start_hour <= time && time <= annot_stop_hour) {
      seq <- paste0("seq", sep = "_", rownames(annot_data)[j])
      break
    }
    
  }
  
  all_info_df$seq[i] <- seq
  print(paste0("on en est a la seq", sep = " ", seq))
  all_info_df$daycam[i] <- paste0(all_info_df$recording_day[i], sep = "_", all_info_df$cam_nm[i])
  print(paste0("on en est au daycam", sep = " ", all_info_df$daycam[i]))
  
  
}

saveRDS(all_info_df, file = here::here("transformed_data/all_info_df.rds"))


# the df is not the one we want, we want, one row = une seq, so create a new df and fill it:

final_all_info_df <- as.data.frame(matrix(ncol = 13, nrow = 62*12*4))
colnames(final_all_info_df) <- c("seq_id", "seq_nb", "day", "cam", "day_cam", "start_hour",
                                 "species", "maxN", "tot_bites_nb", "max_nb_ind_graz", 
                                 "mean_nb_ind_graz", "nb_frames_grazing", "nb_frames_presence")

# fill it: 
n <- 1
for (dc in unique(all_info_df$daycam)) {
  for (s in unique(all_info_df$seq)) {
    
    data <- all_info_df[which(all_info_df$daycam == dc & all_info_df$seq == s), ]
    # retrieve unique seq id
    seq_id <- paste0(s, sep = "_", dc)
    
    # retrieve bites number per species:
    ss_bites_nb <- sum(data$Sarpa_salpa_grazing)
    sc_bites_nb <- sum(data$Sparisoma_cretense_grazing)
    sl_bites_nb <- sum(data$Siganus_luridus_grazing)
    sr_bites_nb <- sum(data$Siganus_rivulatus_grazing)
    
    # retrieve maxN per species:
    # ss:
    values_list <- list()
    for(i in 1:nrow(data)) {
      value <- data$Sarpa_salpa_grazing[i] + data$Sarpa_salpa_swimming[i]
      values_list <- append(values_list, value)
    }
    ss_maxN <- max(unlist(values_list))
    # sc: 
    values_list <- list()
    for(i in 1:nrow(data)) {
      value <- data$Sparisoma_cretense_grazing[i] + data$Sparisoma_cretense_swimming[i]
      values_list <- append(values_list, value)
    }
    sc_maxN <- max(unlist(values_list))
    # sl: 
    values_list <- list()
    for(i in 1:nrow(data)) {
      value <- data$Siganus_luridus_grazing[i] + data$Siganus_luridus_swimming[i]
      values_list <- append(values_list, value)
    }
    sl_maxN <- max(unlist(values_list))
    # sr:
    values_list <- list()
    for(i in 1:nrow(data)) {
      value <- data$Siganus_rivulatus_grazing[i] + data$Siganus_rivulatus_swimming[i]
      values_list <- append(values_list, value)
    }
    sr_maxN <- max(unlist(values_list))
    
    # retrieve maximum number of individual grazing:
    ss_max_ind_graz <- max(data$Sarpa_salpa_grazing)
    sc_max_ind_graz <- max(data$Sparisoma_cretense_grazing)
    sl_max_ind_graz <- max(data$Siganus_luridus_grazing)
    sr_max_ind_graz <- max(data$Siganus_rivulatus_grazing)
    
    # retrieve mean number of individual grazing:
    ss_mean_ind_graz <- mean(data$Sarpa_salpa_grazing)
    sc_mean_ind_graz <- mean(data$Sparisoma_cretense_grazing)
    sl_mean_ind_graz <- mean(data$Siganus_luridus_grazing)
    sr_mean_ind_graz <- mean(data$Siganus_rivulatus_grazing)
    
    # retrieve the number of frames where grazing:
    ss_fr_graz_nb <- nrow(data[which(data$Sarpa_salpa_grazing != 0), ])
    sc_fr_graz_nb <- nrow(data[which(data$Sparisoma_cretense_grazing != 0), ])
    sl_fr_graz_nb <- nrow(data[which(data$Siganus_luridus_grazing != 0), ])
    sr_fr_graz_nb <- nrow(data[which(data$Siganus_rivulatus_grazing != 0), ])
    
    # retrieve the number of frames where present:
    ss_fr_presence_nb <- nrow(data[which(data$Sarpa_salpa_grazing != 0), ]) + 
      nrow(data[which(data$Sarpa_salpa_swimming != 0), ]) - 
      nrow(data[which(data$Sarpa_salpa_grazing != 0 & data$Sarpa_salpa_swimming != 0), ])
    sc_fr_presence_nb <- nrow(data[which(data$Sparisoma_cretense_grazing != 0), ]) + 
      nrow(data[which(data$Sparisoma_cretense_swimming != 0), ]) - 
      nrow(data[which(data$Sparisoma_cretense_grazing != 0 & data$Sparisoma_cretense_swimming != 0), ])
    sl_fr_presence_nb <- nrow(data[which(data$Siganus_luridus_grazing != 0), ]) + 
      nrow(data[which(data$Siganus_luridus_swimming != 0), ]) - 
      nrow(data[which(data$Siganus_luridus_grazing != 0 & data$Siganus_luridus_swimming != 0), ])
    sr_fr_presence_nb <- nrow(data[which(data$Siganus_rivulatus_grazing != 0), ]) + 
      nrow(data[which(data$Siganus_rivulatus_swimming != 0), ]) - 
      nrow(data[which(data$Siganus_rivulatus_grazing != 0 & data$Siganus_rivulatus_swimming != 0), ])
    
    # now fill the new df!
    final_all_info_df$seq_id[c(n, n+1, n+2, n+3)] <- rep(seq_id, 4)
    final_all_info_df$seq_nb[c(n, n+1, n+2, n+3)] <- rep(s, 4)
    final_all_info_df$day[c(n, n+1, n+2, n+3)] <- rep(unique(all_info_df$recording_day[which(all_info_df$daycam == dc)]), 4)
    final_all_info_df$cam[c(n, n+1, n+2, n+3)] <- rep(unique(all_info_df$cam_nm[which(all_info_df$daycam == dc)]), 4)
    final_all_info_df$day_cam[c(n, n+1, n+2, n+3)] <- rep(dc, 4)
    final_all_info_df$start_hour[c(n, n+1, n+2, n+3)] <- rep(data$time[1], 4)
    final_all_info_df$species[c(n, n+1, n+2, n+3)] <- c("Sarpa_salpa", "Sparisoma_cretense", "Siganus_luridus", "Siganus_rivulatus")
    final_all_info_df$maxN[c(n, n+1, n+2, n+3)] <- c(ss_maxN, sc_maxN, sl_maxN, sr_maxN)
    final_all_info_df$tot_bites_nb[c(n, n+1, n+2, n+3)] <- c(ss_bites_nb, sc_bites_nb, sl_bites_nb, sr_bites_nb)
    final_all_info_df$max_nb_ind_graz[c(n, n+1, n+2, n+3)] <- c(ss_max_ind_graz, sc_max_ind_graz,
                                                                sl_max_ind_graz, sr_max_ind_graz)
    final_all_info_df$mean_nb_ind_graz[c(n, n+1, n+2, n+3)] <- c(ss_mean_ind_graz, sc_mean_ind_graz,
                                                                 sl_mean_ind_graz, sr_mean_ind_graz)
    final_all_info_df$nb_frames_grazing[c(n, n+1, n+2, n+3)] <- c(ss_fr_graz_nb, sc_fr_graz_nb,
                                                                  sl_fr_graz_nb, sr_fr_graz_nb)
    final_all_info_df$nb_frames_presence[c(n, n+1, n+2, n+3)] <- c(ss_fr_presence_nb,                                                                                  sc_fr_presence_nb,                                                                                  sl_fr_presence_nb,                                                                                  sr_fr_presence_nb)
    
    print(paste0(final_all_info_df$seq_id[n], sep = " ", "done"))
    
    n <- n + 4
    
  }
  
}

final_all_info_df$start_hour <- hms::as_hms(final_all_info_df$start_hour)

saveRDS(final_all_info_df, file = here::here("transformed_data/final_all_info_df.rds"))
