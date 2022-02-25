###############################################################################
##
## 3_1_Bites_distrib_Fig2.R
##
## Purpose: Prepare data and create the Fig2 of the paper ... 
## ... illutrating the distribution of bite rate across the four species ...
## ... per day and per spot ...
## ...  Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 25/02/2022
##
###############################################################################


# 1 - Prepare data ####

# load needed dataset:
final_all_info_df <- readRDS(here::here("transformed_data/final_all_info_df.rds"))

# create new data:
bites_sp_prop_df <- as.data.frame(matrix(ncol = 4, nrow = 4*4*3))
colnames(bites_sp_prop_df) <- c("day_cam", "species", "bites_nb_sp", "bites_nb_tot")

# fill the df:
n <- 1

for (dc in unique(final_all_info_df$day_cam)) {
  
  data <- final_all_info_df[which(final_all_info_df$day_cam == dc), ]
  
  # compute bites nb for each species for this dc:
  ss <- sum(data$tot_bites_nb[which(data$species == "Sarpa_salpa")])
  
  sc <- sum(data$tot_bites_nb[which(data$species == "Sparisoma_cretense")])
  
  sl <- sum(data$tot_bites_nb[which(data$species == "Siganus_luridus")])
  
  sr <- sum(data$tot_bites_nb[which(data$species == "Siganus_rivulatus")])
  
  tot <- ss + sc + sl + sr
  
  # fill the new df:
  bites_sp_prop_df$day_cam[c(n, n+1, n+2, n+3)] <- rep(dc, 4)
  bites_sp_prop_df$species[c(n, n+1, n+2, n+3)] <- c("Sarpa_salpa", "Sparisoma_cretense", 
                                                     "Siganus_luridus", "Siganus_rivulatus")
  bites_sp_prop_df$bites_nb_sp[c(n, n+1, n+2, n+3)] <- c(ss, sc, sl, sr)
  bites_sp_prop_df$bites_nb_tot[c(n, n+1, n+2, n+3)] <- rep(tot, 4)
  
  n <- n + 4
}



# 2 - Order for plotting ####

# ordonner les especes dans le df pour que ok dans le plot:
bites_sp_prop_df$species <- factor(bites_sp_prop_df$species,       
                                   levels = c("Sparisoma_cretense", "Sarpa_salpa",  "Siganus_luridus", "Siganus_rivulatus"))

# correct cam B1 and B2 inversion (because backgrounds were exchanged between B1 and B2 these days):
bites_sp_prop_df$day_cam[c(33, 34, 35, 36)] <- "04-10-2019_cam_B2"
bites_sp_prop_df$day_cam[c(45, 46, 47, 48)] <- "04-10-2019_cam_B1"

bites_sp_prop_df[c(49, 50, 51, 52), ] <- bites_sp_prop_df[c(33, 34, 35, 36), ]
bites_sp_prop_df[c(33, 34, 35, 36), ] <- bites_sp_prop_df[c(45, 46, 47, 48), ]
bites_sp_prop_df[c(45, 46, 47, 48), ] <- bites_sp_prop_df[c(49, 50, 51, 52), ]
bites_sp_prop_df <- bites_sp_prop_df[- c(49, 50, 51, 52), ]

# save into transformed data folder:
saveRDS(bites_sp_prop_df, here::here("transformed_data/bites_sp_prop_df.rds"))


# 3 - Plot ####


# donner la palette de couleurs:
colours_pal <- c("#481567FF", "#DCE319FF", "#55C667FF", "#238A8DFF") 

# preparer l'annotation:
plot_labels <- data.frame(y1 = c(4.5, 4.5, 4.5), 
                          x1 = c(2.5, 6.5, 10.5), 
                          lab = c("02-10-2019", "03-10-2019", 
                                  "04-10-2019"))

# creer le plot:

graph_sp_bites <-  ggplot2::ggplot() +
  ggplot2::geom_bar(data = bites_sp_prop_df, 
                    ggplot2::aes(y = bites_nb_sp/103, x = day_cam, 
                                 fill = species),
                    color = "grey40", alpha = 0.7, stat = "identity", position = "stack") +
  ggplot2::labs(y = "Number of bites/min/m²",
                x = "",
                title = "") +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
                 panel.border = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(size = 0.5, linetype = "solid", 
                                                   colour = "black"),
                 axis.text.x = ggplot2::element_text(size = 10,colour = "gray40", angle = 90), 
                 legend.position = "bottom") +
  ggplot2::scale_x_discrete(labels = c("spot A1", "spot A2", 
                                       "spot B1", "spot B2",
                                       "spot A1", "spot A2", 
                                       "spot B1", "spot B2",
                                       "spot A1", "spot A2", 
                                       "spot B1", "spot B2")) + 
  
  ggplot2::scale_fill_manual(values = colours_pal,
                             labels = c("Sparisoma cretense", "Sarpa salpa",  "Siganus luridus", "Siganus rivulatus")) +
  ggplot2::geom_vline(xintercept=c(4.5,8.5), linetype="longdash", colour = "grey40") +
  
  ggplot2:: geom_text(data = plot_labels, ggplot2::aes(x = x1,  y = y1, label = lab, size = 0.2), show.legend = FALSE) 


# save the plot: 
ggplot2::ggsave(filename = here::here("outputs/Fig2.pdf"),
                plot = graph_sp_bites,
                device = "pdf",
                scale = 1,
                height = 5200,
                width = 5000,
                units = "px",
                dpi = 600)
