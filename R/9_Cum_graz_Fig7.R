###############################################################################
##
## 9_Cum_graz_Fig7.R
##
## Purpose: Prepare data and create the Fig7 of the paper ... 
## ... illutrating the heatmaps of grazing activity (top maps for A and B) ...
## ... and abundance intensity ...
## ... Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 25/02/2022
##
###############################################################################


# 1 - Get fish mouth data ####


# Load data:
fish_mouth_df <- read.csv(here::here("raw_data", "fish_mouth_size.csv"), sep = ",", dec = ".")
metadata <- read.csv(here::here("raw_data", "metadata_fish_mouth_size.csv"), sep = ",", dec = ".")


# reduce data for the four species of interest:
fish_mouth_df2 <- fish_mouth_df[which(fish_mouth_df$taxonomy %in% 
                                        c("Siganus_rivulatus", "Siganus_luridus",
                                          "Sarpa_salpa", "Sparisoma_cretense")), ]

# reduce data to only Crete data:
fish_mouth_df3 <- fish_mouth_df2[which(fish_mouth_df2$country_Season == "Greece_Autumn"), ]

# give the right format to width and height:
fish_mouth_df3$mouth_depth <- as.numeric(fish_mouth_df3$mouth_depth)
fish_mouth_df3$mouth_width <- as.numeric(fish_mouth_df3$mouth_width)


### Sarpa salpa:
fish_mouth_ss <- fish_mouth_df3[which(fish_mouth_df3$taxonomy == "Sarpa_salpa" & ! is.na(fish_mouth_df3$mouth_depth) 
                                      & ! is.na(fish_mouth_df3$mouth_width)
                                      & fish_mouth_df3$standard_length <= 120
                                      & fish_mouth_df3$standard_length >= 100), ]
fish_mouth_ss <- fish_mouth_ss %>%
  mutate(fish_mouth_ss, ellipse = (fish_mouth_ss$mouth_depth*0.1)*(fish_mouth_ss$mouth_width*0.1)*pi)

mean_mouth_cm2_ss <- mean(fish_mouth_ss$ellipse)

### Siganus rivulatus:
fish_mouth_sr <- fish_mouth_df3[which(fish_mouth_df3$taxonomy == "Siganus_rivulatus" & 
                                        ! is.na(fish_mouth_df3$mouth_depth) 
                                      & ! is.na(fish_mouth_df3$mouth_width)
                                      & fish_mouth_df3$standard_length >= 120
                                      & fish_mouth_df3$standard_length <= 200), ]
fish_mouth_sr <- fish_mouth_sr %>%
  mutate(fish_mouth_sr, ellipse = (fish_mouth_sr$mouth_depth*0.1)*(fish_mouth_sr$mouth_width*0.1)*pi)

mean_mouth_cm2_sr <- mean(fish_mouth_sr$ellipse)

### Sparisoma cretense:
fish_mouth_sc <- fish_mouth_df3[which(fish_mouth_df3$taxonomy == "Sparisoma_cretense" & 
                                        ! is.na(fish_mouth_df3$mouth_depth) 
                                      & ! is.na(fish_mouth_df3$mouth_width)), ]
fish_mouth_sc <- fish_mouth_sc %>%
  mutate(fish_mouth_sc, ellipse = (fish_mouth_sc$mouth_depth*0.1)*(fish_mouth_sc$mouth_width*0.1)*pi)

mean_mouth_cm2_sc <- mean(fish_mouth_sc$ellipse)

### Siganus luridus:
fish_mouth_sl <- fish_mouth_df3[which(fish_mouth_df3$taxonomy == "Siganus_luridus" & 
                                        ! is.na(fish_mouth_df3$mouth_depth) 
                                      & ! is.na(fish_mouth_df3$mouth_width) 
                                      & fish_mouth_df3$standard_length >= 120
                                      & fish_mouth_df3$standard_length <= 200), ]
fish_mouth_sl <- fish_mouth_sl %>%
  mutate(fish_mouth_sl, ellipse = (fish_mouth_sl$mouth_depth*0.1)*(fish_mouth_sl$mouth_width*0.1)*pi)

mean_mouth_cm2_sl <- mean(fish_mouth_sl$ellipse)

# how many individuals per species:
# we consider only sarpa salpa juveniles and adults of the 3 other species:
ss_ind_nb <- nrow(fish_mouth_df3[which(fish_mouth_df3$taxonomy == "Sarpa_salpa" & ! is.na(fish_mouth_df3$mouth_depth) 
                                       & ! is.na(fish_mouth_df3$mouth_width)
                                       & fish_mouth_df3$standard_length <= 120
                                       & fish_mouth_df3$standard_length >= 100), ])

sr_ind_nb <- nrow(fish_mouth_df3[which(fish_mouth_df3$taxonomy == "Siganus_rivulatus" &
                                         ! is.na(fish_mouth_df3$mouth_depth) & ! is.na(fish_mouth_df3$mouth_width)  & fish_mouth_df3$standard_length >= 120
                                       & fish_mouth_df3$standard_length <= 200), ])

sc_ind_nb <- nrow(fish_mouth_df3[which(fish_mouth_df3$taxonomy == "Sparisoma_cretense" &
                                         ! is.na(fish_mouth_df3$mouth_depth) 
                                       & ! is.na(fish_mouth_df3$mouth_width)), ])
sl_ind_nb <- nrow(fish_mouth_df2[which(fish_mouth_df3$taxonomy == "Siganus_luridus" 
                                       & ! is.na(fish_mouth_df3$mouth_depth) 
                                       & ! is.na(fish_mouth_df3$mouth_width)
                                       & fish_mouth_df3$standard_length >= 120
                                       & fish_mouth_df3$standard_length <= 200), ])



# 2 - Prepare data ####


# Load data:
final_all_info_df <- readRDS(here::here("transformed_data", "final_all_info_df.rds"))

# create the df for the plot:
cumul_graz_df <- reshape::melt(final_all_info_df[, c("seq_nb", "day", "cam", "species", "tot_bites_nb")])

# convert bites (value column) into potentiel eaten biomass:
cumul_graz_df$value[which(cumul_graz_df$species == "Sarpa_salpa")] <- cumul_graz_df$value[which(cumul_graz_df$species == "Sarpa_salpa")]*mean_mouth_cm2_ss

cumul_graz_df$value[which(cumul_graz_df$species == "Siganus_rivulatus")] <-
  cumul_graz_df$value[which(cumul_graz_df$species ==
                              "Siganus_rivulatus")]*mean_mouth_cm2_sr

cumul_graz_df$value[which(cumul_graz_df$species == "Sparisoma_cretense")] <-
  cumul_graz_df$value[which(cumul_graz_df$species == "Sparisoma_cretense")]*mean_mouth_cm2_sc

cumul_graz_df$value[which(cumul_graz_df$species == "Siganus_luridus")] <-
  cumul_graz_df$value[which(cumul_graz_df$species == "Siganus_luridus")]*mean_mouth_cm2_sl

# add a new column that will resume day and seq
cumul_graz_df$seq_day <- paste0(cumul_graz_df$seq_nb, sep = "_", cumul_graz_df$day)

# sum values for all cameras in a new df (nrow = nb seq + nb days + nb sp):
cumul_graz_df2 <- as.data.frame(matrix(ncol = 5, nrow = 62*4*3))
colnames(cumul_graz_df2) <- c("seq_nb", "day", "seq_day", "species", "cumul_bites")

# fill the new df:
n <- 1
for (d in unique(cumul_graz_df$day)) {
  for (s in unique(cumul_graz_df$seq_nb)) {
    
    data <- cumul_graz_df[which(cumul_graz_df$day == d & cumul_graz_df$seq_nb == s), ]
    
    sum_cam_ss <- sum(data$value[which(data$species == "Sarpa_salpa")])
    sum_cam_sr <- sum(data$value[which(data$species == "Siganus_rivulatus")])
    sum_cam_sc <- sum(data$value[which(data$species == "Sparisoma_cretense")])
    sum_cam_sl <- sum(data$value[which(data$species == "Siganus_luridus")])
    
    # fill the new df:
    cumul_graz_df2$seq_nb[c(n, n+1, n+2, n+3)] <- rep(s, 4)
    cumul_graz_df2$day[c(n, n+1, n+2, n+3)] <- rep(d, 4)
    cumul_graz_df2$seq_day[c(n, n+1, n+2, n+3)] <- rep(unique(data$seq_day), 4)
    cumul_graz_df2$species[c(n, n+1, n+2, n+3)] <- c("Sarpa_salpa", 
                                                     "Siganus_rivulatus", 
                                                     "Sparisoma_cretense", 
                                                     "Siganus_luridus")
    cumul_graz_df2$cumul_bites[c(n, n+1, n+2, n+3)] <- c(sum_cam_ss,
                                                         sum_cam_sr,
                                                         sum_cam_sc, 
                                                         sum_cam_sl)
    
    n <- n + 4
    
    
  }
}


# order species:
cumul_graz_df2$species <- factor(cumul_graz_df2$species,       
                                 levels = c("Sparisoma_cretense", "Sarpa_salpa",  "Siganus_luridus", "Siganus_rivulatus"))

# order sequences:
cumul_graz_df2$seq_nb <- factor(cumul_graz_df2$seq_nb,       
                                levels = c("seq_1","seq_2","seq_3","seq_4","seq_5","seq_6","seq_7","seq_8","seq_9", "seq_10","seq_11","seq_12","seq_13","seq_14","seq_15","seq_16","seq_17","seq_18","seq_19","seq_20", "seq_21","seq_22","seq_23","seq_24","seq_25","seq_26","seq_27","seq_28","seq_29","seq_30","seq_31", "seq_32","seq_33","seq_34", "seq_35", "seq_36", "seq_37", "seq_38", "seq_39", "seq_40", "seq_41", "seq_42", "seq_43", "seq_44",
                                           "seq_45", "seq_46", "seq_47", "seq_48", "seq_49", "seq_50", "seq_51", "seq_52", "seq_53", "seq_54", "seq_55","seq_56", "seq_57", "seq_58", "seq_59", "seq_60", "seq_61", "seq_62"))

# order sequences day: 
cumul_graz_df2$seq_day <- factor(cumul_graz_df2$seq_day,       
                                 levels = c("seq_1_02-10-2019","seq_2_02-10-2019","seq_3_02-10-2019","seq_4_02-10-2019","seq_5_02-10-2019","seq_6_02-10-2019","seq_7_02-10-2019","seq_8_02-10-2019","seq_9_02-10-2019", "seq_10_02-10-2019","seq_11_02-10-2019","seq_12_02-10-2019","seq_13_02-10-2019","seq_14_02-10-2019","seq_15_02-10-2019","seq_16_02-10-2019","seq_17_02-10-2019","seq_18_02-10-2019","seq_19_02-10-2019","seq_20_02-10-2019", "seq_21_02-10-2019","seq_22_02-10-2019","seq_23_02-10-2019","seq_24_02-10-2019","seq_25_02-10-2019","seq_26_02-10-2019","seq_27_02-10-2019","seq_28_02-10-2019","seq_29_02-10-2019","seq_30_02-10-2019","seq_31_02-10-2019", "seq_32_02-10-2019","seq_33_02-10-2019","seq_34_02-10-2019", "seq_35_02-10-2019", "seq_36_02-10-2019", "seq_37_02-10-2019", "seq_38_02-10-2019", "seq_39_02-10-2019", "seq_40_02-10-2019", "seq_41_02-10-2019", "seq_42_02-10-2019", "seq_43_02-10-2019", "seq_44_02-10-2019",
                                            "seq_45_02-10-2019", "seq_46_02-10-2019", "seq_47_02-10-2019", "seq_48_02-10-2019", "seq_49_02-10-2019", "seq_50_02-10-2019", "seq_51_02-10-2019", "seq_52_02-10-2019", "seq_53_02-10-2019", "seq_54_02-10-2019", "seq_55_02-10-2019","seq_56_02-10-2019", "seq_57_02-10-2019", "seq_58_02-10-2019", "seq_59_02-10-2019", "seq_60_02-10-2019", "seq_61_02-10-2019", "seq_62_02-10-2019",
                                            
                                            "seq_1_03-10-2019","seq_2_03-10-2019","seq_3_03-10-2019","seq_4_03-10-2019","seq_5_03-10-2019","seq_6_03-10-2019","seq_7_03-10-2019","seq_8_03-10-2019","seq_9_03-10-2019", "seq_10_03-10-2019","seq_11_03-10-2019","seq_12_03-10-2019","seq_13_03-10-2019","seq_14_03-10-2019","seq_15_03-10-2019","seq_16_03-10-2019","seq_17_03-10-2019","seq_18_03-10-2019","seq_19_03-10-2019","seq_20_03-10-2019", "seq_21_03-10-2019","seq_22_03-10-2019","seq_23_03-10-2019","seq_24_03-10-2019","seq_25_03-10-2019","seq_26_03-10-2019","seq_27_03-10-2019","seq_28_03-10-2019","seq_29_03-10-2019","seq_30_03-10-2019","seq_31_03-10-2019", "seq_32_03-10-2019","seq_33_03-10-2019","seq_34_03-10-2019", "seq_35_03-10-2019", "seq_36_03-10-2019", "seq_37_03-10-2019", "seq_38_03-10-2019", "seq_39_03-10-2019", "seq_40_03-10-2019", "seq_41_03-10-2019", "seq_42_03-10-2019", "seq_43_03-10-2019", "seq_44_03-10-2019",
                                            "seq_45_03-10-2019", "seq_46_03-10-2019", "seq_47_03-10-2019", "seq_48_03-10-2019", "seq_49_03-10-2019", "seq_50_03-10-2019", "seq_51_03-10-2019", "seq_52_03-10-2019", "seq_53_03-10-2019", "seq_54_03-10-2019", "seq_55_03-10-2019","seq_56_03-10-2019", "seq_57_03-10-2019", "seq_58_03-10-2019", "seq_59_03-10-2019", "seq_60_03-10-2019", "seq_61_03-10-2019", "seq_62_03-10-2019",
                                            
                                            "seq_1_04-10-2019","seq_2_04-10-2019","seq_3_04-10-2019","seq_4_04-10-2019","seq_5_04-10-2019","seq_6_04-10-2019","seq_7_04-10-2019","seq_8_04-10-2019","seq_9_04-10-2019", "seq_10_04-10-2019","seq_11_04-10-2019","seq_12_04-10-2019","seq_13_04-10-2019","seq_14_04-10-2019","seq_15_04-10-2019","seq_16_04-10-2019","seq_17_04-10-2019","seq_18_04-10-2019","seq_19_04-10-2019","seq_20_04-10-2019", "seq_21_04-10-2019","seq_22_04-10-2019","seq_23_04-10-2019","seq_24_04-10-2019","seq_25_04-10-2019","seq_26_04-10-2019","seq_27_04-10-2019","seq_28_04-10-2019","seq_29_04-10-2019","seq_30_04-10-2019","seq_31_04-10-2019", "seq_32_04-10-2019","seq_33_04-10-2019","seq_34_04-10-2019", "seq_35_04-10-2019", "seq_36_04-10-2019", "seq_37_04-10-2019", "seq_38_04-10-2019", "seq_39_04-10-2019", "seq_40_04-10-2019", "seq_41_04-10-2019", "seq_42_04-10-2019", "seq_43_04-10-2019", "seq_44_04-10-2019",
                                            "seq_45_04-10-2019", "seq_46_04-10-2019", "seq_47_04-10-2019", "seq_48_04-10-2019", "seq_49_04-10-2019", "seq_50_04-10-2019", "seq_51_04-10-2019", "seq_52_04-10-2019", "seq_53_04-10-2019", "seq_54_04-10-2019", "seq_55_04-10-2019","seq_56_04-10-2019", "seq_57_04-10-2019", "seq_58_04-10-2019", "seq_59_04-10-2019", "seq_60_04-10-2019", "seq_61_04-10-2019", "seq_62_04-10-2019"))


# compute cumulative sum for SARPA SALPA:
cumul_graz_df2$cumul_bites[which(cumul_graz_df2$species == "Sarpa_salpa")] <- cumsum(cumul_graz_df2$cumul_bites[which(cumul_graz_df2$species == "Sarpa_salpa")])

# compute cumulative sum for SIGANUS RIVULATUS:
cumul_graz_df2$cumul_bites[which(cumul_graz_df2$species == "Siganus_rivulatus")] <- cumsum(cumul_graz_df2$cumul_bites[which(cumul_graz_df2$species == "Siganus_rivulatus")])

# compute cumulative sum for SPARISOMA CRETENSE:
cumul_graz_df2$cumul_bites[which(cumul_graz_df2$species == "Sparisoma_cretense")] <- cumsum(cumul_graz_df2$cumul_bites[which(cumul_graz_df2$species == "Sparisoma_cretense")])

# compute cumulative sum for SIGANUS LURIDUS:
cumul_graz_df2$cumul_bites[which(cumul_graz_df2$species == "Siganus_luridus")] <- cumsum(cumul_graz_df2$cumul_bites[which(cumul_graz_df2$species == "Siganus_luridus")])



# 2 - Plot ####



# give the colours palette:
colours_pal = c("#481567FF", "#DCE319FF", "#55C667FF", "#238A8DFF") 

# plot:

cumul_graze_graph <- ggplot2::ggplot(data = cumul_graz_df2, 
                                     ggplot2::aes(x = seq_nb, y = cumul_bites*0.0001, group = species, color = species)) +
  ggplot2::geom_line(size = 1) + 
  
  ggplot2::scale_color_manual(values = colours_pal,
                              labels = c("Sparisoma cretense", "Sarpa salpa",  "Siganus luridus", "Siganus rivulatus")) +
  
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7,colour = "gray40", angle = 90, hjust = 0.5, vjust = 0) , legend.position = "bottom") +
  
  ggplot2::ylab("Potential grazed area (m2)") +
  ggplot2::xlab("Time (sequence)") + 
  
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
                                           "", "", "", "", "", "08:00:00")))


# save the plot: 
ggplot2::ggsave(filename = here::here("outputs/Fig7.pdf"),
                plot = cumul_graze_graph,
                device = "pdf",
                scale = 1,
                height = 4000,
                width = 6000,
                units = "px",
                dpi = 600)
