###############################################################################
##
## 7_1_Temp_var_Fig5.R
##
## Purpose: Prepare data and create the Fig5 of the paper ... 
## ... illutrating Variation of the grazing activity based on the proportion ...
## ... of total grazing realised per sequence (1min40s) throughout the three ...
## ... sampling days with the four 1m²- spots pooled ...
## ...  Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 25/02/2022
##
###############################################################################


# 1 - Prepare data ####


# Load data:
final_all_info_df <- readRDS(here::here("transformed_data", "final_all_info_df.rds"))

## Create the df:

# load data of summed bites number for all cam per second:
bites_nb_all_cam_df <- final_all_info_df
bites_nb_all_cam_df$seq_day <- paste0(bites_nb_all_cam_df$seq_nb, 
                                      sep = "_", 
                                      bites_nb_all_cam_df$day)

# create new df:
bites_nb_all_cam_top_df <- as.data.frame(matrix(ncol = 7, nrow = 62*4*3))
colnames(bites_nb_all_cam_top_df) <- c("day", "time", "seq", "seq_id", "species", "bites_nb",
                                       "mean_maxN")



# fill the new df:
n <- 1
for (sd in unique(bites_nb_all_cam_df$seq_day)) {
  
  data <- bites_nb_all_cam_df[which(bites_nb_all_cam_df$seq_day == sd), ]
  
  # retrieve bites number for each species:
  bites_ss <- sum(data[which(data$species == "Sarpa_salpa"), "tot_bites_nb"])
  bites_sr <- sum(data[which(data$species == "Siganus_rivulatus"), "tot_bites_nb"])
  bites_sc <- sum(data[which(data$species == "Sparisoma_cretense"), "tot_bites_nb"])
  bites_sl <- sum(data[which(data$species == "Siganus_luridus"), "tot_bites_nb"])
  
  # retrieve mean maxN for each species:
  maxN_ss <- mean(data[which(data$species == "Sarpa_salpa"), "maxN"])
  maxN_sr <- mean(data[which(data$species == "Siganus_rivulatus"), "maxN"])
  maxN_sc <- mean(data[which(data$species == "Sparisoma_cretense"), "maxN"])
  maxN_sl <- mean(data[which(data$species == "Siganus_luridus"), "maxN"])
  
  # fill new df:
  bites_nb_all_cam_top_df$day[c(n, n+1, n+2, n+3)] <- rep(unique(data$day), 4)
  bites_nb_all_cam_top_df$time[c(n, n+1, n+2, n+3)] <- rep(data$start_hour[1], 4)
  bites_nb_all_cam_top_df$seq[c(n, n+1, n+2, n+3)] <- rep(unique(data$seq_nb), 4)
  bites_nb_all_cam_top_df$seq_id[c(n, n+1, n+2, n+3)] <- rep(unique(data$seq_day), 4)
  bites_nb_all_cam_top_df$species[c(n, n+1, n+2, n+3)] <- c("Sarpa_salpa", "Siganus_rivulatus", 
                                                            "Sparisoma_cretense", "Siganus_luridus")
  bites_nb_all_cam_top_df$bites_nb[c(n, n+1, n+2, n+3)] <- c(bites_ss, bites_sr,
                                                             bites_sc, bites_sl)
  bites_nb_all_cam_top_df$mean_maxN[c(n, n+1, n+2, n+3)] <- c(maxN_ss, maxN_sr,
                                                              maxN_sc, maxN_sl)
  
  n <- n + 4
  
}

bites_nb_all_cam_top_df$time <- hms::as_hms(bites_nb_all_cam_top_df$time)


# gather data at day/cam scale by gathering species:
bites_nb_all_sp_top_df <- as.data.frame(matrix(ncol = 5, nrow = 62*3))
colnames(bites_nb_all_sp_top_df) <- c("day", "time", "seq", "seq_id", 
                                      "bites_nb")

# fill it:
n <- 1
for (seq_id in unique(bites_nb_all_cam_top_df$seq_id)) {
  
  data <- bites_nb_all_cam_top_df[which(bites_nb_all_cam_top_df$seq_id == seq_id), ]
  tot_bites <- sum(data$bites_nb)
  
  bites_nb_all_sp_top_df$day[n] <- unique(data$day)
  bites_nb_all_sp_top_df$time[n] <- unique(data$time)
  bites_nb_all_sp_top_df$seq[n] <- unique(data$seq)
  bites_nb_all_sp_top_df$seq_id[n] <- unique(data$seq_id)
  bites_nb_all_sp_top_df$bites_nb[n] <- tot_bites
  
  n <- n + 1
}

bites_nb_all_sp_top_df <- dplyr::distinct(bites_nb_all_sp_top_df)
bites_nb_all_sp_top_df$time <- hms::as_hms(bites_nb_all_sp_top_df$time)

saveRDS(bites_nb_all_sp_top_df, file = here::here("transformed_data", "bites_nb_all_sp_top_df.rds"))



# 2 - Plot ####


prop_herb_variation_graph <- ggplot2::ggplot(data = bites_nb_all_sp_top_df, 
                                             ggplot2::aes(x = seq, y = (bites_nb / sum(bites_nb))*100)) +
  ggplot2::geom_bar(stat = "identity", color = "grey40") +
  
  ggplot2::ylab("Proportion of total herbivory") +
  ggplot2::xlab("") + 
  
  ggplot2::facet_wrap(~day) +
  
  ggplot2::scale_x_discrete(labels = rev(c("", "18:00:00", 
                                           "", "", "", "", "", "17:00:00",
                                           "", "", "", "", "", "16:00:00",
                                           "", "", "", "", "", "15:00:00",
                                           "", "", "", "", "", "14:00:00",
                                           "", "", "", "", "", "13:00:00",
                                           "", "", "", "", "", "12:00:00",
                                           "", "", "", "", "", "11:00:00",
                                           "", "", "", "", "", "10:00:00",
                                           "", "", "", "", "", "09:00:00",
                                           "", "", "", "", "", "08:00:00"))) +
  
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10,colour = "gray40", angle = 90)) +
  
  ggplot2::geom_hline(yintercept = (13.36/1978)*100, linetype = "dashed", color = "coral", 
                      size = 1)


# save the plot: 
ggplot2::ggsave(filename = here::here("outputs/Fig5.pdf"),
                plot = prop_herb_variation_graph,
                device = "pdf",
                scale = 1,
                height = 4000,
                width = 6000,
                units = "px",
                dpi = 600)
