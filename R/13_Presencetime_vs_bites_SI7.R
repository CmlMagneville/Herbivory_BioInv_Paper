###############################################################################
##
## 13_Presencetime_vs_bites_SI7.R
##
## Purpose: Prepare data and create the Supp Fig7 of the paper ... 
## ... illutrating Correlation between bites number and duration of presence ...
## ... (i.e. number of frames where a given species was seen within ...
## ... a sequence) in sequences where each species occurs...
## ...  Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 28/02/2022
##
###############################################################################



# 1 - Prepare data ####

# load data:
final_all_info_df <- readRDS(here::here("transformed_data", "final_all_info_df.rds"))

# order species:
final_all_info_df2 <- final_all_info_df
final_all_info_df2$species <- factor(final_all_info_df2$species,       
                                     levels = c("Sparisoma_cretense", "Sarpa_salpa",  "Siganus_luridus", "Siganus_rivulatus"))

# give new names to species with the number of sequences which have maxN != 0 ...
# ... so at least one individual present:
length(final_all_info_df2[which(final_all_info_df2$species == "Sparisoma_cretense" & final_all_info_df2$maxN !=0), "seq_id"]) #  adapter pour avoir les autres
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

# compute correlation for each species (without seq with 0 individuals):
# H0 coeff = 0

## sc : 0.54, pvalue < 5% reject H0, coeff != 0

cor.test(x = final_all_info_df[which(final_all_info_df$species == "Sparisoma_cretense" & final_all_info_df$maxN != 0), "nb_frames_presence"], 
         y = final_all_info_df[which(final_all_info_df$species == "Sparisoma_cretense" & final_all_info_df$maxN !=0), "tot_bites_nb"], method = "spearman")

## sr : 0.76 pvalue < 5% reject H0, coeff != 0

cor.test(x = final_all_info_df[which(final_all_info_df$species == "Siganus_rivulatus" & final_all_info_df$maxN != 0), "nb_frames_presence"], 
         y = final_all_info_df[which(final_all_info_df$species == "Siganus_rivulatus" & final_all_info_df$maxN !=0), "tot_bites_nb"], method = "spearman")

## sl : 0.64, pvalue > 5% accept H0, coeff null

cor.test(x = final_all_info_df[which(final_all_info_df$species == "Siganus_luridus" & final_all_info_df$maxN != 0), "nb_frames_presence"], 
         y = final_all_info_df[which(final_all_info_df$species == "Siganus_luridus" & final_all_info_df$maxN !=0), "tot_bites_nb"], method = "spearman")

## ss : 0.80, pvalue < 5% rejette H0, coeff non nul

cor.test(x = final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa" & final_all_info_df$maxN != 0), "nb_frames_presence"], 
         y = final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa" & 
                                       final_all_info_df$maxN !=0), "tot_bites_nb"], method = "spearman")

# create a df that contains R2 and p value
plot_labels <- data.frame(x1 = c(22, 25, 10, 25), 
                          y1 = c(17, 100, 0.75, 60), 
                          lab = c("rhô = 0.54, ***", "rhô = 0.80, ***", 
                                  "rhô = 0.64", "rhô = 0.76, ***"),
                          species = c("Sparisoma cretense (n = 126)", 
                                      "Sarpa salpa (n = 181)", 
                                      "Siganus luridus (n = 7)", 
                                      "Siganus rivulatus (n = 246)"))


# 2- Plot ####


# donner la palette de couleurs:
colours_pal <- c("#481567FF", "#DCE319FF", "#55C667FF", "#238A8DFF") 


presence_graze_graph <- ggplot2::ggplot(data = final_all_info_df2[which(final_all_info_df2$maxN != 0), ], 
                                        ggplot2::aes(x = nb_frames_presence, y = tot_bites_nb)) +
  ggplot2::geom_point(ggplot2::aes(colour = species), show.legend = FALSE) + 
  
  ggplot2::scale_color_manual(values = colours_pal) +
  
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7,colour = "gray40", angle = 90, hjust = 0.5, vjust = 0)) +
  
  ggplot2::ylab("Bites number per sequence") +
  ggplot2::xlab("Presence time per sequence (s)") + 
  ggplot2::facet_wrap(~ species, scales = "free") +
  
  ggplot2:: geom_text(data = plot_labels, ggplot2::aes(x = x1,  y = y1, label = lab)) 

# save the plot: 
ggplot2::ggsave(filename = here::here("outputs/SI7.pdf"),
                plot = presence_graze_graph,
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 5000,
                units = "px",
                dpi = 600)

