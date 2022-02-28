###############################################################################
##
## 2_1_Occ_Fig1.R
##
## Purpose: Prepare data and create the Fig1 of the paper ... 
## ... illutrating the occurrence and co-occ of the four herbivores ...
## ...  Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 24/02/2022
##
###############################################################################


# Figure 1A ####

## 1 - Prepare data ####

# load the df:
final_all_info_df <- readRDS(here::here("transformed_data/final_all_info_df.rds"))


# sarpa salpa:
ss_seq <- nrow(final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa" & final_all_info_df$maxN != 0), ])
ss_seq

# sparisoma cretense:
sc_seq <- nrow(final_all_info_df[which(final_all_info_df$species == "Sparisoma_cretense" & final_all_info_df$maxN != 0), ])
sc_seq

# siganus luridus:
sl_seq <- nrow(final_all_info_df[which(final_all_info_df$species == "Siganus_luridus" & final_all_info_df$maxN != 0), ])
sl_seq

# siganus rivulatus:
sr_seq <- nrow(final_all_info_df[which(final_all_info_df$species == "Siganus_rivulatus" & final_all_info_df$maxN != 0), ])
sr_seq

# 0 species:
zero_seq_ss <- final_all_info_df$seq_id[which(final_all_info_df$maxN == 0 & 
                                                final_all_info_df$species == "Sarpa_salpa")]
zero_seq_sr <- final_all_info_df$seq_id[which(final_all_info_df$maxN == 0 & 
                                                final_all_info_df$species == "Siganus_rivulatus")]
zero_seq_sl <- final_all_info_df$seq_id[which(final_all_info_df$maxN == 0 & 
                                                final_all_info_df$species == "Siganus_luridus")]
zero_seq_sc <- final_all_info_df$seq_id[which(final_all_info_df$maxN == 0 & 
                                                final_all_info_df$species == "Sparisoma_cretense")]
zero_seq <- Reduce(intersect, list(zero_seq_ss, zero_seq_sc, zero_seq_sl, 
                                   zero_seq_sr))
zero_seq_nb <- length(zero_seq)

# dataframe to gather information:
seq_present_df <- data.frame(species = c("Sarpa_salpa", "Sparisoma_cretense", 
                                         "Siganus_luridus", "Siganus_rivulatus", 
                                         "No_species"),
                             seq_prop = c(ss_seq/nrow(final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa"), ]), 
                                          sc_seq/nrow(final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa"), ]),               sl_seq/nrow(final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa"), ]),                sr_seq/nrow(final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa"), ]),
                                          zero_seq_nb/nrow(final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa"), ])))
seq_present_df$seq_prop <- 100*seq_present_df$seq_prop

# order species in the df so that ok in the plot:
seq_present_df$species <- factor(seq_present_df$species,       
                                 levels = c("No_species", "Sparisoma_cretense", "Sarpa_salpa",  "Siganus_luridus", "Siganus_rivulatus"))



## 2 - Plot ####

# give colour palette:
colours_pal <- c("grey", "#481567FF", "#DCE319FF", "#55C667FF", "#238A8DFF") 

# create the plot (fishualize does not work (24/11/2021) so add shapes by hand):

graph_seq_present <- ggplot2::ggplot(data = seq_present_df, 
                                     ggplot2::aes(y = seq_prop, x = species))+
  ggplot2::geom_bar(stat = "identity", show.legend = FALSE) +
  ggplot2::aes(fill = species) +
  ggplot2::scale_fill_manual(values = colours_pal,
                             labels = c("Sparisoma_cretense", "Sarpa_salpa",  "Siganus_luridus", "Siganus_rivulatus", "No species")) +
  ggplot2::labs(y = "Temporal occurrence (proportion of the total number of sequences)",
                x = "",
                title = "",
                subtitle = "") +
  
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"), 
                 panel.border = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(size = 0.5, linetype = "solid", 
                                                   colour = "black"),
                 axis.title.y = ggplot2::element_text(size=13), 
                 axis.text = ggplot2::element_text(size=13, colour = "black")) +
  ggplot2::geom_text(ggplot2::aes(label=  round(seq_prop, 2)), 
                     position = ggplot2::position_dodge(width = 0.9), vjust = - 0.25) +
  ggplot2::scale_x_discrete(labels = c("No species", "Sparisoma cretense",
                                       "Sarpa salpa", "Siganus luridus",
                                       "Siganus rivulatus"),) + 
  
  # fishalize package did not work on 24/02/2022 so the following lines too
  #
  # fishualize::add_fishape(family = "Siganidae",
  #                         option = "Siganus_rivulatus",
  #                         xmin = 4.6, xmax = 5.4, ymin = 40, ymax = 45,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3) +
  # fishualize::add_fishape(family = "Sparidae",
  #                         option = "Sarpa_salpa",
  #                         xmin = 2.6, xmax = 3.4, ymin = 30, ymax = 35,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3) +
  # fishualize::add_fishape(family = "Labridae",
  #                         option = "Sparisoma_cretense",
  #                         xmin = 1.6, xmax = 2.4, ymin = 23, ymax = 28,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3)  +
  # fishualize::add_fishape(family = "Siganidae",
  #                         option = "Siganus_luridus",
  #                         xmin = 3.6, xmax = 4.4, ymin = 4, ymax = 9,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3) +
  
  ggplot2::ylim(0, 50)

# save the plot: 
ggplot2::ggsave(filename = here::here("outputs/Fig1A.pdf"),
                plot = graph_seq_present,
                device = "pdf",
                scale = 1,
                height = 4800,
                width = 5000,
                units = "px",
                dpi = 600)


####


# Figure 1B ####


# 1 - Prepare data ####

# create the matrix used to compute the diagram:
matrix_seq <- final_all_info_df[, c(1, 7, 8)]
matrix_upset_seq <- data.frame(seq_id = unique(matrix_seq$seq_id), 
                               Sarpa_salpa = rep(NA, length(unique(matrix_seq$seq_id))),
                               Siganus_rivulatus = rep(NA, length(unique(matrix_seq$seq_id))),
                               Sparisoma_cretense = rep(NA, length(unique(matrix_seq$seq_id))), 
                               Siganus_luridus = rep(NA, length(unique(matrix_seq$seq_id))))
rownames(matrix_upset_seq) <- matrix_upset_seq$seq_id

# fill the matrix:
for (n in rownames(matrix_upset_seq)) {
  
  ss <- matrix_seq$maxN[which(matrix_seq$species == "Sarpa_salpa" & matrix_seq$seq_id == n)]
  matrix_upset_seq[n, "Sarpa_salpa"] <- ss
  
  sr <- matrix_seq$maxN[which(matrix_seq$species == "Siganus_rivulatus" & matrix_seq$seq_id == n)]
  matrix_upset_seq[n, "Siganus_rivulatus"] <- sr
  
  sc <- matrix_seq$maxN[which(matrix_seq$species == "Sparisoma_cretense" & matrix_seq$seq_id == n)]
  matrix_upset_seq[n, "Sparisoma_cretense"] <- sc
  
  sl <- matrix_seq$maxN[which(matrix_seq$species == "Siganus_luridus" & matrix_seq$seq_id == n)]
  matrix_upset_seq[n, "Siganus_luridus"] <- sl
  
}

# remove seq_id column:
matrix_upset_seq <- matrix_upset_seq[, -1]

# remove 0 lines:
matrix_upset_seq <- matrix_upset_seq[rowSums(matrix_upset_seq[]) > 0, ]

# change numbers > 0 into 1:
for (i in (1:ncol(matrix_upset_seq))) {
  for (j in (1:nrow(matrix_upset_seq))) {
    if (matrix_upset_seq[j, i] > 0) {
      matrix_upset_seq[j, i] <- 1
    }
  }
}


# retrieve combination matrix :
comb_matrix_upset_seq <- ComplexHeatmap::make_comb_mat(matrix_upset_seq)
rownames(comb_matrix_upset_seq) <- c("Sarpa salpa", "Siganus rivulatus", "Sparisoma cretense",
                                     "Siganus luridus")


# 2 - Plot ####

# prepare for saving  the plot: 
pdf(file = here::here("outputs/Fig1B.pdf"),
    height = 50,
    width = 52)

# plot
upset_graph_seq <- ComplexHeatmap::UpSet(comb_matrix_upset_seq, 
                         comb_order = order(ComplexHeatmap::comb_size(comb_matrix_upset_seq)),
                         pt_size = grid::unit(5, "mm"), lwd = 4,
                         comb_col = c("#018571","#80cdc1", 
                                      'tan1', "#a6611a")[ComplexHeatmap::comb_degree(comb_matrix_upset_seq)],
                         bg_pt_col = "grey85",
                         right_annotation = ComplexHeatmap::upset_right_annotation(comb_matrix_upset_seq, 
                                      gp = grid::gpar(fill = "gray40", color = "gray40")))
upset_graph_seq



# clean plot window:
dev.off()

