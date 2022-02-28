###############################################################################
##
## 8_1_Bites_MaxN_Var_Fig6.R
##
## Purpose: Prepare data and create the Fig6 of the paper ... 
## ... illutrating Heatmaps of grazing activity and abundance intensity ...
## ... for S. salpa and S. rivulatus ...
## ...  Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 28/02/2022
##
###############################################################################


# 1 - Prepare data for grazing heatmaps ####


## SARPA SALPA

# load data:
bites_nb_all_cam_top_df <- readRDS(here::here("transformed_data", "bites_nb_all_cam_top_df.rds"))

# create the df for the plot: 
cam_heatmap_df_ss <- reshape::melt(bites_nb_all_cam_top_df[which(bites_nb_all_cam_top_df$species == "Sarpa_salpa"), 
                                                           c("seq", "day", "bites_nb")])

# plot heatmap:
cam_heatmap_df_ss$seq <- factor(cam_heatmap_df_ss$seq,       
                                levels = c("seq_1","seq_2","seq_3","seq_4","seq_5","seq_6","seq_7","seq_8","seq_9", "seq_10","seq_11","seq_12","seq_13","seq_14","seq_15","seq_16","seq_17","seq_18","seq_19","seq_20", "seq_21","seq_22","seq_23","seq_24","seq_25","seq_26","seq_27","seq_28","seq_29","seq_30","seq_31", "seq_32","seq_33","seq_34", "seq_35", "seq_36", "seq_37", "seq_38", "seq_39", "seq_40", "seq_41", "seq_42", "seq_43", "seq_44",
                                           "seq_45", "seq_46", "seq_47", "seq_48", "seq_49", "seq_50", "seq_51", "seq_52", "seq_53", "seq_54", "seq_55","seq_56", "seq_57", "seq_58", "seq_59", "seq_60", "seq_61", "seq_62"))
cam_heatmap_df_ss <- cam_heatmap_df_ss[, -3]
colnames(cam_heatmap_df_ss) <- c("x", "y", "value")

# create categories of bites number:
cam_heatmap_df_ss2 <- cam_heatmap_df_ss %>%
  # convert state to factor and reverse order of levels
  mutate(y = factor(y,levels = rev(sort(unique(y))))) %>%
  # create a new variable from count
  mutate(valuesfactor = cut(value, breaks=c(-1,0,10,50,100,max(value, na.rm = T)),
                            labels=c("0","1-10","10-50","50-100",">100"))) %>%
  # change level order
  mutate(valuesfactor = factor(as.character(valuesfactor),levels = levels(valuesfactor)))



## SIGANUS RIVULATUS

# create the df for the plot: 
cam_heatmap_df_sr <- reshape::melt(bites_nb_all_cam_top_df[which(bites_nb_all_cam_top_df$species == "Siganus_rivulatus"), c("seq", "day", "bites_nb")])

# plot la heatmap:
cam_heatmap_df_sr$seq <- factor(cam_heatmap_df_sr$seq,       
                                levels = c("seq_1","seq_2","seq_3","seq_4","seq_5","seq_6","seq_7","seq_8","seq_9", "seq_10","seq_11","seq_12","seq_13","seq_14","seq_15","seq_16","seq_17","seq_18","seq_19","seq_20", "seq_21","seq_22","seq_23","seq_24","seq_25","seq_26","seq_27","seq_28","seq_29","seq_30","seq_31", "seq_32","seq_33","seq_34", "seq_35", "seq_36", "seq_37", "seq_38", "seq_39", "seq_40", "seq_41", "seq_42", "seq_43", "seq_44",
                                           "seq_45", "seq_46", "seq_47", "seq_48", "seq_49", "seq_50", "seq_51", "seq_52", "seq_53", "seq_54", "seq_55","seq_56", "seq_57", "seq_58", "seq_59", "seq_60", "seq_61", "seq_62"))
cam_heatmap_df_sr <- cam_heatmap_df_sr[, -3]
colnames(cam_heatmap_df_sr) <- c("x", "y", "value")

# create categories of bites number:
cam_heatmap_df_sr2 <- cam_heatmap_df_sr %>%
  # convert state to factor and reverse order of levels
  mutate(y = factor(y,levels = rev(sort(unique(y))))) %>%
  # create a new variable from count
  mutate(valuesfactor = cut(value, breaks=c(-1,0,10,50,100,max(value, na.rm = T)),
                            labels=c("0","1-10","10-50","50-100",">100"))) %>%
  # change level order
  mutate(valuesfactor = factor(as.character(valuesfactor),levels = levels(valuesfactor)))



# 2 - Plot grazing heatmaps ####


## SARPA SALPA

heatmap_bites_ss_cam <- ggplot2::ggplot(cam_heatmap_df_ss2, ggplot2::aes(x= x, y = y, fill = valuesfactor)) + 
  ggplot2::geom_tile(color = "gray80") +
  # remove x and y axis labels
  ggplot2::labs(x = "", y = "") +
  # remove extra space
  ggplot2::scale_x_discrete(labels = rev(c("18:10:00", "18:00:00", 
                                           "17:50:00", "17:40:00", "17:30:00", "17:20:00", "17:10:00", "17:00:00",
                                           "16:50:00", "16:40:00", "16:30:00", "16:20:00", "16:10:00", "16:00:00",
                                           "15:50:00", "15:40:00", "15:30:00", "15:20:00", "15:10:00", "15:00:00",
                                           "14:50:00", "14:40:00", "14:30:00", "14:20:00", "14:10:00", "14:00:00",
                                           "13:50:00", "13:40:00", "13:30:00", "13:20:00", "13:10:00", "13:00:00",
                                           "12:50:00", "12:40:00", "12:30:00", "12:20:00", "12:10:00", "12:00:00",
                                           "11:50:00", "11:40:00", "11:30:00", "11:20:00", "11:10:00", "11:00:00",
                                           "10:50:00", "10:40:00", "10:30:00", "10:20:00", "10:10:00", "10:00:00",
                                           "09:50:00", "09:40:00", "09:30:00", "09:20:00", "09:10:00", "09:00:00",
                                           "08:50:00", "08:40:00", "08:30:00", "08:20:00", "08:10:00",                                          "08:00:00"))) +
  
  ggplot2::scale_fill_manual(values = c("grey95", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494")) + 
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Total bites number / 4m² / seq")) +
  ggplot2::theme(legend.position = "top", legend.direction = "horizontal",
                 legend.title = ggplot2::element_text(colour = "gray40"),
                 legend.margin = ggplot2::margin(grid::unit(0, "cm")),
                 legend.text = ggplot2::element_text(colour = "gray40",size = 7,face = "bold"),
                 legend.key.height = grid::unit(0.8, "cm"),
                 legend.key.width = grid::unit(0.2, "cm"),
                 axis.text.x = ggplot2::element_text(size = 10,colour = "gray40", angle = 90),
                 axis.text.y = ggplot2::element_text(vjust = 0.2,colour = "gray40"),
                 axis.ticks = ggplot2::element_line(size = 0.4),
                 plot.background = ggplot2::element_blank(),
                 panel.border= ggplot2::element_blank(),
                 plot.margin = ggplot2::margin(0.7, 0.4, 0.1, 0.2, "cm"),
                 plot.title = ggplot2::element_text(colour = "gray40", hjust = 0, size = 14, face = "bold"))


## SIGANUS RIVULATUS

heatmap_bites_sr_cam <- ggplot2::ggplot(cam_heatmap_df_sr2, ggplot2::aes(x= x, y = y, fill = valuesfactor)) + 
  ggplot2::geom_tile(color = "gray80") +
  # remove x and y axis labels
  ggplot2::labs(x = "", y = "") +
  # remove extra space
  ggplot2::scale_x_discrete(labels = rev(c("18:10:00", "18:00:00", 
                                           "17:50:00", "17:40:00", "17:30:00", "17:20:00", "17:10:00", "17:00:00",
                                           "16:50:00", "16:40:00", "16:30:00", "16:20:00", "16:10:00", "16:00:00",
                                           "15:50:00", "15:40:00", "15:30:00", "15:20:00", "15:10:00", "15:00:00",
                                           "14:50:00", "14:40:00", "14:30:00", "14:20:00", "14:10:00", "14:00:00",
                                           "13:50:00", "13:40:00", "13:30:00", "13:20:00", "13:10:00", "13:00:00",
                                           "12:50:00", "12:40:00", "12:30:00", "12:20:00", "12:10:00", "12:00:00",
                                           "11:50:00", "11:40:00", "11:30:00", "11:20:00", "11:10:00", "11:00:00",
                                           "10:50:00", "10:40:00", "10:30:00", "10:20:00", "10:10:00", "10:00:00",
                                           "09:50:00", "09:40:00", "09:30:00", "09:20:00", "09:10:00", "09:00:00",
                                           "08:50:00", "08:40:00", "08:30:00", "08:20:00", "08:10:00",                                          "08:00:00"))) +
  
  ggplot2::scale_fill_manual(values = c("grey95", "#a1dab4", "#41b6c4", "#2c7fb8")) + 
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Total bites number / 4m² / seq")) +
  ggplot2::theme(legend.position = "top", legend.direction = "horizontal",
                 legend.title = ggplot2::element_text(colour = "gray40"),
                 legend.margin = ggplot2::margin(grid::unit(0, "cm")),
                 legend.text = ggplot2::element_text(colour = "gray40",size = 7,face = "bold"),
                 legend.key.height = grid::unit(0.8, "cm"),
                 legend.key.width = grid::unit(0.2, "cm"),
                 axis.text.x = ggplot2::element_text(size = 10,colour = "gray40", angle = 90),
                 axis.text.y = ggplot2::element_text(vjust = 0.2,colour = "gray40"),
                 axis.ticks = ggplot2::element_line(size = 0.4),
                 plot.background = ggplot2::element_blank(),
                 panel.border= ggplot2::element_blank(),
                 plot.margin = ggplot2::margin(0.7, 0.4, 0.1, 0.2, "cm"),
                 plot.title = ggplot2::element_text(colour = "gray40", hjust = 0, size = 14, face = "bold"))


# save the plot for SS: 
ggplot2::ggsave(filename = here::here("outputs/Fig6A_grazing.pdf"),
                plot = heatmap_bites_ss_cam,
                device = "pdf",
                scale = 1,
                height = 2000,
                width = 6000,
                units = "px",
                dpi = 600)


# save the plot for SR: 
ggplot2::ggsave(filename = here::here("outputs/Fig6B_grazing.pdf"),
                plot = heatmap_bites_sr_cam,
                device = "pdf",
                scale = 1,
                height = 2000,
                width = 6000,
                units = "px",
                dpi = 600)


# 3 - Prepare data for maxN ####


## SARPA SALPA

# create the df for the plot: 
cam_heatmap_df_ss <- reshape::melt(bites_nb_all_cam_top_df[which(bites_nb_all_cam_top_df$species == "Sarpa_salpa"), c("seq", "day", "mean_maxN")])

# plot la heatmap:
cam_heatmap_df_ss$seq <- factor(cam_heatmap_df_ss$seq,       
                                levels = c("seq_1","seq_2","seq_3","seq_4","seq_5","seq_6","seq_7","seq_8","seq_9", "seq_10","seq_11","seq_12","seq_13","seq_14","seq_15","seq_16","seq_17","seq_18","seq_19","seq_20", "seq_21","seq_22","seq_23","seq_24","seq_25","seq_26","seq_27","seq_28","seq_29","seq_30","seq_31", "seq_32","seq_33","seq_34", "seq_35", "seq_36", "seq_37", "seq_38", "seq_39", "seq_40", "seq_41", "seq_42", "seq_43", "seq_44",
                                           "seq_45", "seq_46", "seq_47", "seq_48", "seq_49", "seq_50", "seq_51", "seq_52", "seq_53", "seq_54", "seq_55","seq_56", "seq_57", "seq_58", "seq_59", "seq_60", "seq_61", "seq_62"))
cam_heatmap_df_ss <- cam_heatmap_df_ss[, -3]
colnames(cam_heatmap_df_ss) <- c("x", "y", "value")

# create categories of bites number:
cam_heatmap_df_ss2 <- cam_heatmap_df_ss %>%
  # convert state to factor and reverse order of levels
  mutate(y = factor(y,levels = rev(sort(unique(y))))) %>%
  # create a new variable from count
  mutate(valuesfactor = cut(value, breaks=c(-1,0,1,3,5,max(value, na.rm = T)),
                            labels=c("0","0-1","1-3","3-5",">5"))) %>%
  # change level order
  mutate(valuesfactor = factor(as.character(valuesfactor),levels = levels(valuesfactor)))


## SIGANUS RIVULATUS

# create the df for the plot: 
cam_heatmap_df_sr <- reshape::melt(bites_nb_all_cam_top_df[which(bites_nb_all_cam_top_df$species == "Siganus_rivulatus"),
                                                           c("seq", "day", "mean_maxN")])

# plot la heatmap:
cam_heatmap_df_sr$seq <- factor(cam_heatmap_df_sr$seq,       
                                levels = c("seq_1","seq_2","seq_3","seq_4","seq_5","seq_6","seq_7","seq_8","seq_9", "seq_10","seq_11","seq_12","seq_13","seq_14","seq_15","seq_16","seq_17","seq_18","seq_19","seq_20", "seq_21","seq_22","seq_23","seq_24","seq_25","seq_26","seq_27","seq_28","seq_29","seq_30","seq_31", "seq_32","seq_33","seq_34", "seq_35", "seq_36", "seq_37", "seq_38", "seq_39", "seq_40", "seq_41", "seq_42", "seq_43", "seq_44",
                                           "seq_45", "seq_46", "seq_47", "seq_48", "seq_49", "seq_50", "seq_51", "seq_52", "seq_53", "seq_54", "seq_55","seq_56", "seq_57", "seq_58", "seq_59", "seq_60", "seq_61", "seq_62"))
cam_heatmap_df_sr <- cam_heatmap_df_sr[, -3]
colnames(cam_heatmap_df_sr) <- c("x", "y", "value")

# create categories of bites number:
cam_heatmap_df_sr2 <- cam_heatmap_df_sr %>%
  # convert state to factor and reverse order of levels
  mutate(y = factor(y,levels = rev(sort(unique(y))))) %>%
  # create a new variable from count
  mutate(valuesfactor = cut(value, breaks=c(-1,0,1,3,5,max(value, na.rm = T)),
                            labels=c("0","0-1","1-3","3-5",">5"))) %>%
  # change level order
  mutate(valuesfactor = factor(as.character(valuesfactor), levels = levels(valuesfactor)))


# 4  - Plot for maxN ####


## SARPA SALPA

heatmap_maxN_ss_cam <- ggplot2::ggplot(cam_heatmap_df_ss2, ggplot2::aes(x= x, y = y, fill = valuesfactor)) + 
  ggplot2::geom_tile(color = "gray80") +
  # remove x and y axis labels
  ggplot2::labs(x = "", y = "") +
  # remove extra space
  ggplot2::scale_x_discrete(labels = rev(c("18:10:00", "18:00:00", 
                                           "17:50:00", "17:40:00", "17:30:00", "17:20:00", "17:10:00", "17:00:00",
                                           "16:50:00", "16:40:00", "16:30:00", "16:20:00", "16:10:00", "16:00:00",
                                           "15:50:00", "15:40:00", "15:30:00", "15:20:00", "15:10:00", "15:00:00",
                                           "14:50:00", "14:40:00", "14:30:00", "14:20:00", "14:10:00", "14:00:00",
                                           "13:50:00", "13:40:00", "13:30:00", "13:20:00", "13:10:00", "13:00:00",
                                           "12:50:00", "12:40:00", "12:30:00", "12:20:00", "12:10:00", "12:00:00",
                                           "11:50:00", "11:40:00", "11:30:00", "11:20:00", "11:10:00", "11:00:00",
                                           "10:50:00", "10:40:00", "10:30:00", "10:20:00", "10:10:00", "10:00:00",
                                           "09:50:00", "09:40:00", "09:30:00", "09:20:00", "09:10:00", "09:00:00",
                                           "08:50:00", "08:40:00", "08:30:00", "08:20:00", "08:10:00",                                          "08:00:00"))) +
  
  ggplot2::scale_fill_manual(values = c("white", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000")) + 
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Mean maxN / 4m² / seq")) +
  ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal",
                 legend.title = ggplot2::element_text(colour = "gray40"),
                 legend.margin = ggplot2::margin(grid::unit(0, "cm")),
                 legend.text = ggplot2::element_text(colour = "gray40",size = 7,face = "bold"),
                 legend.key.height = grid::unit(0.8, "cm"),
                 legend.key.width = grid::unit(0.2, "cm"),
                 axis.text.x = ggplot2::element_text(size = 10,colour = "gray40", angle = 90),
                 axis.text.y = ggplot2::element_text(vjust = 0.2,colour = "gray40"),
                 axis.ticks = ggplot2::element_line(size = 0.4),
                 plot.background = ggplot2::element_blank(),
                 panel.border= ggplot2::element_blank(),
                 plot.margin = ggplot2::margin(0.7, 0.4, 0.1, 0.2, "cm"),
                 plot.title = ggplot2::element_text(colour = "gray40", hjust = 0, size = 14, face = "bold"))


## SIGANUS RIVULATUS

heatmap_maxN_sr_cam <- ggplot2::ggplot(cam_heatmap_df_sr2, ggplot2::aes(x= x, y = y, fill = valuesfactor)) + 
  ggplot2::geom_tile(color = "gray80") +
  # remove x and y axis labels
  ggplot2::labs(x = "", y = "") +
  # remove extra space
  ggplot2::scale_x_discrete(labels = rev(c("18:10:00", "18:00:00", 
                                           "17:50:00", "17:40:00", "17:30:00", "17:20:00", "17:10:00", "17:00:00",
                                           "16:50:00", "16:40:00", "16:30:00", "16:20:00", "16:10:00", "16:00:00",
                                           "15:50:00", "15:40:00", "15:30:00", "15:20:00", "15:10:00", "15:00:00",
                                           "14:50:00", "14:40:00", "14:30:00", "14:20:00", "14:10:00", "14:00:00",
                                           "13:50:00", "13:40:00", "13:30:00", "13:20:00", "13:10:00", "13:00:00",
                                           "12:50:00", "12:40:00", "12:30:00", "12:20:00", "12:10:00", "12:00:00",
                                           "11:50:00", "11:40:00", "11:30:00", "11:20:00", "11:10:00", "11:00:00",
                                           "10:50:00", "10:40:00", "10:30:00", "10:20:00", "10:10:00", "10:00:00",
                                           "09:50:00", "09:40:00", "09:30:00", "09:20:00", "09:10:00", "09:00:00",
                                           "08:50:00", "08:40:00", "08:30:00", "08:20:00", "08:10:00",                                          "08:00:00"))) +
  
  ggplot2::scale_fill_manual(values = c("white", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000")) + 
  ggplot2::guides(fill = ggplot2::guide_legend(title = "Mean maxN / 4m² / seq")) +
  ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal",
                 legend.title = ggplot2::element_text(colour = "gray40"),
                 legend.margin = ggplot2::margin(grid::unit(0, "cm")),
                 legend.text = ggplot2::element_text(colour = "gray40",size = 7,face = "bold"),
                 legend.key.height = grid::unit(0.8, "cm"),
                 legend.key.width = grid::unit(0.2, "cm"),
                 axis.text.x = ggplot2::element_text(size = 10,colour = "gray40", angle = 90),
                 axis.text.y = ggplot2::element_text(vjust = 0.2,colour = "gray40"),
                 axis.ticks = ggplot2::element_line(size = 0.4),
                 plot.background = ggplot2::element_blank(),
                 panel.border= ggplot2::element_blank(),
                 plot.margin = ggplot2::margin(0.7, 0.4, 0.1, 0.2, "cm"),
                 plot.title = ggplot2::element_text(colour = "gray40", hjust = 0, size = 14, face = "bold"))



# save the plot for SS: 
ggplot2::ggsave(filename = here::here("outputs/Fig6A_maxN.pdf"),
                plot = heatmap_maxN_ss_cam,
                device = "pdf",
                scale = 1,
                height = 2000,
                width = 6000,
                units = "px",
                dpi = 600)


# save the plot for SR: 
ggplot2::ggsave(filename = here::here("outputs/Fig6B_maxN.pdf"),
                plot = heatmap_maxN_sr_cam,
                device = "pdf",
                scale = 1,
                height = 2000,
                width = 6000,
                units = "px",
                dpi = 600)
