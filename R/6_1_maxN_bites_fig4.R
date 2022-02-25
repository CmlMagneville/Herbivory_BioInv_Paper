###############################################################################
##
## 6_1_maxN_bites_fig4.R
##
## Purpose: Prepare data and create the Fig4 of the paper ... 
## ... illutrating the correlation between maximal number of individuals ...
## ... and bites number in 1min40s sequences ...
## ...  Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 25/02/2022
##
###############################################################################


# 1 - Prepare data ####


# load:
final_all_info_df2 <- readRDS(here::here("transformed_data/final_all_info_df.rds"))

# order species:
final_all_info_df2$species <- factor(final_all_info_df2$species,       
                      levels = c("Sparisoma_cretense", "Sarpa_salpa",  "Siganus_luridus", "Siganus_rivulatus"))

# change species names when maxN != 0 so at least one individual present:
length(final_all_info_df2[which(final_all_info_df2$species == "Sparisoma_cretense" & final_all_info_df2$maxN !=0), 
                          "seq_id"]) 
final_all_info_df2$species <- factor(final_all_info_df2$species, 
                                     labels = c("Sparisoma cretense (n = 126)", 
                                                "Sarpa salpa (n = 181)", 
                                                "Siganus luridus (n = 7)", 
                                                "Siganus rivulatus (n = 246)"))
final_all_info_df2$species <- factor(final_all_info_df2$species, 
                                     levels =  c("Sparisoma cretense (n = 126)", 
                                                 "Sarpa salpa (n = 181)", 
                                                 "Siganus luridus (n = 7)", 
                                                 "Siganus rivulatus (n = 246)"))



# compute correlation for each species (without seq with 0 individuals) (and nb of sequences with individuals:
# H0 coeff is null

## sc : 0.17, pvalue > 5% accept H0, coeff null

# cor:
cor.test(x = final_all_info_df[which(final_all_info_df$species == "Sparisoma_cretense" & final_all_info_df$maxN != 0), "maxN"], 
         y = final_all_info_df[which(final_all_info_df$species == "Sparisoma_cretense" & final_all_info_df$maxN !=0), "tot_bites_nb"], method = "spearman")

# nb seq:
nrow(final_all_info_df[which(final_all_info_df$species == "Sparisoma_cretense" & final_all_info_df$maxN != 0), ])

## sr : 0.44, pvalue < 5% reject H0, coeff diff 0 but really low

# cor:
cor.test(x = final_all_info_df[which(final_all_info_df$species == "Siganus_rivulatus" & final_all_info_df$maxN != 0), "maxN"], 
         y = final_all_info_df[which(final_all_info_df$species == "Siganus_rivulatus" & final_all_info_df$maxN !=0), "tot_bites_nb"], method = "spearman")

# nb seq:
nrow(final_all_info_df[which(final_all_info_df$species == "Siganus_rivulatus" & final_all_info_df$maxN != 0), ])

## sl : 0.65, pvalue > 5% accept H0, coeff null

# cor: 

cor.test(x = final_all_info_df[which(final_all_info_df$species == "Siganus_luridus" & final_all_info_df$maxN != 0), "maxN"], 
         y = final_all_info_df[which(final_all_info_df$species == "Siganus_luridus" & final_all_info_df$maxN !=0), "tot_bites_nb"], method = "spearman")

# nb seq:
nrow(final_all_info_df[which(final_all_info_df$species == "Siganus_luridus" & final_all_info_df$maxN != 0), ])

## ss : 0.34, pvalue < 5% reject H0, coeff different from 0 but low

# cor:
cor.test(x = final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa" & final_all_info_df$maxN != 0), "maxN"], 
         y = final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa" & 
                                       final_all_info_df$maxN !=0), "tot_bites_nb"], method = "spearman")

# nb seq:
nrow(final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa" & final_all_info_df$maxN != 0), ])


# S. luridus has only 7 points on 3 places so do the plot without SL


# donner la palette de couleurs:
colours_pal <- c("#481567FF", "#DCE319FF", "#238A8DFF", "grey") 

# create a df that contains R2 and p value
plot_labels2 <- data.frame(x1 = c(25, 9, 17), 
                           y1 = c(17.5, 100, 60), 
                           lab = c("rho = 0.17", "rho = 0.34 ***", 
                                   "rho = 0.44 ***"),
                           species = c("Sparisoma cretense (n = 126)", 
                                       "Sarpa salpa (n = 181)", 
                                       "Siganus rivulatus (n = 246)"))

# sample data:
data_plot_maxN2 <- final_all_info_df2[which(final_all_info_df2$maxN != 0 &
                                              final_all_info_df2$species != "Siganus luridus (n = 7)"), ]



# 2 - Plot ####


maxN_graze_graph <- ggplot2::ggplot(data = data_plot_maxN2, 
                                     ggplot2::aes(x = maxN, y = tot_bites_nb)) +
  ggplot2::geom_point(ggplot2::aes(colour = species), show.legend = FALSE) + 
  
  ggplot2::scale_color_manual(values = colours_pal) +
  
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7,colour = "gray40", angle = 90, hjust = 0.5, vjust = 0)) +
  
  ggplot2::theme_light() +
  
  ggplot2::ylab("Bites number per sequence") +
  ggplot2::xlab("MaxN per sequence") + 
  ggplot2::facet_wrap(~ species, scales = "free") +
  
  ggplot2:: geom_text(data = plot_labels2, ggplot2::aes(x = x1,  y = y1, label = lab, size = 4), show.legend = FALSE) 


# save the plot: 
ggplot2::ggsave(filename = here::here("outputs/Fig4.pdf"),
                plot = maxN_graze_graph,
                device = "pdf",
                scale = 1,
                height = 2000,
                width = 6000,
                units = "px",
                dpi = 600)
