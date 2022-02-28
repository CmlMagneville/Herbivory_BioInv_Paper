###############################################################################
##
## 11_Graz_vs_swimm_SI4.R
##
## Purpose: Prepare data and create the Supp Fig4 of the paper ... 
## ... illutrating the proportion of time where each species was grazing ...
## ...  Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 28/02/2022
##
###############################################################################



# 1 - Prepare data ####


# load data:
final_pres_abs_graz_tot <- readRDS(here::here("raw_data", "final_fd_behav_all_cam_all_days.rds"))

# create the new df:
bites_nb_cam_day_df <- as.data.frame(matrix(ncol = 3, nrow = 12*4))
colnames(bites_nb_cam_day_df) <- c("day_cam", "species", "prop_bites_fr_nb")

final_pres_abs_graz_tot2 <- final_pres_abs_graz_tot
final_pres_abs_graz_tot2$cam_day <- paste0(final_pres_abs_graz_tot2$recording_day, 
                                           sep = "_", 
                                           final_pres_abs_graz_tot2$cam_nm)

# fill the df:
n <- 1
for (dc in unique(final_pres_abs_graz_tot2$cam_day)) {
  
  data <- final_pres_abs_graz_tot2[which(final_pres_abs_graz_tot2$cam_day == dc), ]
  
  # compute proportions:
  prop_ss <- (nrow(data[which(data$Sarpa_salpa_grazing != 0), ]) / (nrow(data[which(data$Sarpa_salpa_swimming != 0), ]) + nrow(data[which(data$Sarpa_salpa_grazing != 0), ]) - nrow(data[which(data$Sarpa_salpa_swimming != 0 & data$Sarpa_salpa_grazing !=0 ), ] ))) * 100
  
  prop_sc <- (nrow(data[which(data$Sparisoma_cretense_grazing != 0), ]) / (nrow(data[which(data$Sparisoma_cretense_swimming != 0), ]) + nrow(data[which(data$Sparisoma_cretense_grazing != 0), ]) - 
                                                                             nrow(data[which(data$Sparisoma_cretense_swimming != 0 & data$Sparisoma_cretense_grazing !=0), ] ))) * 100
  
  prop_sr <- (nrow(data[which(data$Siganus_rivulatus_grazing != 0), ]) / (nrow(data[which(data$Siganus_rivulatus_swimming != 0), ]) + nrow(data[which(data$Siganus_rivulatus_grazing != 0), ]) -
                                                                            nrow(data[which(data$Siganus_rivulatus_swimming != 0 & data$Siganus_rivulatus_grazing !=0), ]))) * 100
  
  
  prop_sl <- (nrow(data[which(data$Siganus_luridus_grazing != 0), ]) / (nrow(data[which(data$Siganus_luridus_swimming != 0), ]) + nrow(data[which(data$Siganus_luridus_grazing != 0), ]) - 
                                                                          nrow(data[which(data$Siganus_luridus_swimming != 0 & data$Siganus_luridus_grazing !=0), ]))) * 100
  
  # fill the df:
  bites_nb_cam_day_df[c(n, n+1, n+2, n+3), "day_cam"] <- dc
  bites_nb_cam_day_df[c(n, n+1, n+2, n+3), "species"] <- c("Sarpa_salpa", 
                                                           "Sparisoma_cretense",
                                                           "Siganus_rivulatus",
                                                           "Siganus_luridus")
  bites_nb_cam_day_df[c(n, n+1, n+2, n+3), "prop_bites_fr_nb"] <- c(prop_ss, 
                                                                    prop_sc,
                                                                    prop_sr,
                                                                    prop_sl)
  print(paste0(dc, sep =" ", "done"))
  n <- n + 4
}

bites_nb_cam_day_df$prop_bites_fr_nb[which(bites_nb_cam_day_df$prop_bites_fr_nb == "NaN")] <- 0

# order species so ok for plotting:
bites_nb_cam_day_df$species <- factor(bites_nb_cam_day_df$species,       
                                      levels = c("Sparisoma_cretense", "Sarpa_salpa",  "Siganus_luridus", "Siganus_rivulatus"))

# save data so can be used in Info_SI4 script:
saveRDS(bites_nb_cam_day_df, here::here("transformed_data", "bites_nb_cam_day_df.rds"))

# 2 - Plot ####

# colors palette:
colours_pal <- c("#481567FF", "#DCE319FF", "#55C667FF", "#238A8DFF") 

# create comparisons from KW test to put on the graph: 
my_comparisons <- list( c("Siganus_luridus", "Sarpa_salpa"), 
                        c("Siganus_luridus", "Siganus_rivulatus"), 
                        c("Sparisoma_cretense", "Sarpa_salpa"), 
                        c("Sparisoma_cretense", "Siganus_rivulatus"))

# plot:
graph_bites_nb_prop <- ggpubr::ggboxplot(data = bites_nb_cam_day_df, x = "species", y = "prop_bites_fr_nb", fill = "species", alpha = 0.6,
                                         palette = colours_pal,
                                         legend = "none", outlier.shape = NA,
                                         add = "jitter", color = "species")+
  ggpubr::stat_compare_means(comparisons = my_comparisons, label = "p.signif") + # Add significance levels
  ggpubr::stat_compare_means(label.y = 65)  +      
  
  ggplot2::labs(y = "Proportion of grazing frames",
                x = "",
                title = "",
                subtitle = "") +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),                        panel.border = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(size = 0.5, linetype = "solid", 
                                                   colour = "black")) +
  ggplot2::scale_x_discrete(labels = c("Sparisoma cretense", 
                                       "Sarpa salpa",
                                       "Siganus luridus", 
                                       "Siganus rivulatus")) 

  # Comment following lines because fishualize did not work while coding on February 2022:

  # fishualize::add_fishape(family = "Siganidae",
  #                         option = "Siganus_rivulatus",
  #                         xmin = 3.6, xmax = 4.4, ymin = 57, ymax = 62,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3) +
  # fishualize::add_fishape(family = "Sparidae",
  #                         option = "Sarpa_salpa",
  #                         xmin = 1.6, xmax = 2.4, ymin = 57, ymax = 62,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3) +
  # fishualize::add_fishape(family = "Labridae",
  #                         option = "Sparisoma_cretense",
  #                         xmin = 0.6, xmax = 1.4, ymin = 57, ymax = 62,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3)  +
  # fishualize::add_fishape(family = "Siganidae",
  #                         option = "Siganus_luridus",
  #                         xmin = 2.6, xmax = 3.4,  ymin = 57, ymax = 62,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3)


# save the plot:
ggplot2::ggsave(filename = here::here("outputs/SI4.pdf"),
                plot = graph_bites_nb_prop,
                device = "pdf",
                scale = 1,
                height = 5500,
                width = 5800,
                units = "px",
                dpi = 600)
