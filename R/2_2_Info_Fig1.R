###############################################################################
##
## 2_2_Info_Fig1.R
##
## Purpose: Compute some figures related to the fig1 given in the paper
##
## Camille Magneville
##
## cleaned version: 24/02/2022
##
###############################################################################


# Figure 1A ####

final_pres_abs_tot <- readRDS(here::here("raw_data/final_df_abund_all_cam_all_days.rds"))

# occurence in seconds of Siganus luridus:
nrow(final_pres_abs_tot[which(final_pres_abs_tot$Siganus_luridus != 0), ])

# occurence in seconds of Siganus rivulatus:
nrow(final_pres_abs_tot[which(final_pres_abs_tot$Siganus_rivulatus != 0), ])

# occurence in seconds of Sarpa salpa:
nrow(final_pres_abs_tot[which(final_pres_abs_tot$Sarpa_salpa != 0), ])

# occurence in seconds of Sparisoma cretense:
nrow(final_pres_abs_tot[which(final_pres_abs_tot$Sparisoma_cretense != 0), ])


####


# Figure 1B ####


## 1 - Compute values of some values of percentage of sequences ####


# compute a matrix that is a copy of matrix_upset and will contain sum of each row:
matrix_upset_seq2 <- matrix_upset_seq
matrix_upset_seq2$sum <- apply(matrix_upset_seq2, 1, sum)

# compute the number of frames on which there is only one species:
nrow(matrix_upset_seq2[which(matrix_upset_seq2$sum == 1), ])

# compute the proportion of frames having one species regarding the total number of frames where species are seen:
(nrow(matrix_upset_seq2[which(matrix_upset_seq2$sum == 1), ]) / nrow(matrix_upset_seq2)) * 100

# compute the proportion of occurrence of SS and SR:
(nrow(matrix_upset_seq2[which(matrix_upset_seq2$Siganus_rivulatus == 1 & matrix_upset_seq2$Sarpa_salpa == 1), ]) / nrow(matrix_upset_seq2)) * 100

# compute  the proportion of occurrence of SS and SR based on frames where we see SS:
(nrow(matrix_upset_seq2[which(matrix_upset_seq2$Siganus_rivulatus == 1 & matrix_upset_seq2$Sarpa_salpa == 1), ]) / nrow(matrix_upset_seq2[which(matrix_upset_seq2$Sarpa_salpa == 1), ])) * 100

# compute  the proportion of occurrence of SS and SR based on frames where we see SR:
(nrow(matrix_upset_seq2[which(matrix_upset_seq2$Siganus_rivulatus == 1 & matrix_upset_seq2$Sarpa_salpa == 1), ]) / nrow(matrix_upset_seq2[which(matrix_upset_seq2$Siganus_rivulatus == 1), ])) * 100

# compute the proportion of occurrence of natives and SR:
(nrow(matrix_upset_seq2[which(matrix_upset_seq2$Siganus_rivulatus == 1 & matrix_upset_seq2$Sarpa_salpa == 1 & matrix_upset_seq2$Sparisoma_cretense == 1), ]) / nrow(matrix_upset_seq2)) * 100


## 2 - Create new matrix for step 3 ####


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


## 3 - Compute values such as the percentage of frames where species are occuring alone ####


# compute a matrix taht is a copy of matrix_upset and will contain sum of each row:
matrix_upset2 <- matrix_upset
matrix_upset2$sum <- apply(matrix_upset2, 1, sum)

# compute the number of frames on which there is only one species:
nrow(matrix_upset2[which(matrix_upset2$sum == 1), ])

# compute the proportion of frames having one species regarding the total number of frames where species are seen:
(nrow(matrix_upset2[which(matrix_upset2$sum == 1), ]) / nrow(matrix_upset2)) * 100

# compute the proportion of occurrence of SS and SR:
(nrow(matrix_upset2[which(matrix_upset2$Siganus_rivulatus == 1 & matrix_upset2$Sarpa_salpa == 1), ]) / nrow(matrix_upset2)) * 100

# compute  the proportion of occurrence of SS and SR based on frames where we see SS:
(nrow(matrix_upset2[which(matrix_upset2$Siganus_rivulatus == 1 & matrix_upset2$Sarpa_salpa == 1), ]) / nrow(matrix_upset2[which(matrix_upset2$Sarpa_salpa == 1), ])) * 100

# compute  the proportion of occurrence of SS and SR based on frames where we see SR:
(nrow(matrix_upset2[which(matrix_upset2$Siganus_rivulatus == 1 & matrix_upset2$Sarpa_salpa == 1), ]) / nrow(matrix_upset2[which(matrix_upset2$Siganus_rivulatus == 1), ])) * 100

# compute the proportion of occurrence of natives and SR:
(nrow(matrix_upset2[which(matrix_upset2$Siganus_rivulatus == 1 & matrix_upset2$Sarpa_salpa == 1 & matrix_upset2$Sparisoma_cretense == 1), ]) / nrow(matrix_upset2)) * 100

