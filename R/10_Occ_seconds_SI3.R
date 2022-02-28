###############################################################################
##
## 10_Occ_seconds_SI3.R
##
## Purpose: Prepare data and create the Supp Fig3 of the paper ... 
## ... illutrating the occurrence and co-occurrence of the four herbivorous...
## ... species on the 74 144 annotated seconds ...
## ...  Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 28/02/2022
##
###############################################################################


# 1 - Prepare data ####


# load data:
final_pres_abs_tot <- readRDS(here::here("raw_data", "final_df_abund_all_cam_all_days.rds"))

# create a list that contain frames where each species is present:
final_pres_abs_tot2 <- final_pres_abs_tot
final_pres_abs_tot2$sec_id <- paste0(final_pres_abs_tot2$recording_day, sep = "_", 
                                     final_pres_abs_tot2$cam_nm, sep = "_",
                                     final_pres_abs_tot2$time)
venn_plot_list_sec <- list(Sarpa_salpa = c(final_pres_abs_tot2$sec_id[which(final_pres_abs_tot2$Sarpa_salpa != 0)]), 
                           Sparisoma_cretense = c(final_pres_abs_tot2$sec_id[which(final_pres_abs_tot2$Sparisoma_cretense != 0)]),
                           Siganus_rivulatus = c(final_pres_abs_tot2$sec_id[which(final_pres_abs_tot2$Siganus_rivulatus != 0)]),
                           Siganus_luridus = c(final_pres_abs_tot2$sec_id[which(final_pres_abs_tot2$Siganus_luridus != 0)]))

# create the matrix used to compute the diagram:
matrix_upset <- final_pres_abs_tot2[, c(4:8)]
matrix_upset <- as.data.frame(matrix_upset)
rownames(matrix_upset) <- matrix_upset$sec_id
matrix_upset <- matrix_upset[, -5]
# remove 0 lines:
matrix_upset <- matrix_upset[rowSums(matrix_upset[]) > 0, ]
# change numbers > 0 into 1:
for (i in (1:ncol(matrix_upset))) {
  for (j in (1:nrow(matrix_upset))) {
    if (matrix_upset[j, i] > 0) {
      matrix_upset[j, i] <- 1
    }
  }
}


# retrieve combination matrix :
comb_matrix_upset <- ComplexHeatmap::make_comb_mat(matrix_upset)
rownames(comb_matrix_upset) <- c("Siganus rivulatus", "Sparisoma cretense","Sarpa salpa",
                                 "Siganus luridus")



# 2 - Plot ####

# prepare for saving the plot: 
pdf(file = here::here("outputs/SI3.pdf"),
    height = 50,
    width = 52)

# plot
upset_graph <- ComplexHeatmap::UpSet(comb_matrix_upset, 
                     comb_order = order(ComplexHeatmap::comb_size(comb_matrix_upset)),
                     pt_size = grid::unit(5, "mm"), lwd = 4,
                     comb_col = c("#018571","#80cdc1", 'tan1', "#a6611a")[ComplexHeatmap::comb_degree(comb_matrix_upset)],
                     bg_pt_col = "grey85",
                     right_annotation = ComplexHeatmap::upset_right_annotation(comb_matrix_upset, 
                          gp = grid::gpar(fill = "gray40", color = "gray40")))
upset_graph

# clean plot window:
dev.off()


