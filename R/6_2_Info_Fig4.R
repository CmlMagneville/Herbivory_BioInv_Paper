###############################################################################
##
## 6_2_Info_Fig4.R
##
## Purpose: Compute some figures related to the Fig4 given in the paper
##
## Camille Magneville
##
## cleaned version: 25/02/2022
##
###############################################################################



# 1 - Mean range of bites nb for a given nb of individuals:

# Load data:
final_all_info_df2 <- readRDS(here::here("transformed_data/final_all_info_df2.rds"))

# retrieve a list of range span for each number of individuals:
list_range_bites <- list()

# remove when no species:
final_all_info_df3 <- final_all_info_df2[which(final_all_info_df2$maxN != 0), ]

# loop on maxN values and get range span of bites nb for each maxN value:
for (i in unique(final_all_info_df3$maxN)){ 
  range <- max(final_all_info_df2$tot_bites_nb[which(final_all_info_df3$maxN == i)]) - 
    min(final_all_info_df2$tot_bites_nb[which(final_all_info_df3$maxN == i)])
  list_range_bites <- append(list_range_bites, range)
} 

# thus get the mean range value:
(7 + 11 + 5 + 80 + 21 + 8)  / 14



### mean range of individuals for a given number of bites:
# retrieve a list of range span for each number of bites:
list_range_ind <- list()

# remove when no species:
final_all_info_df3 <- final_all_info_df2[which(final_all_info_df2$maxN != 0), ]

# loop on maxN values and get range span of bites nb for each maxN value:
for (i in unique(final_all_info_df3$tot_bites_nb)){ 
  range <- max(final_all_info_df2$maxN[which(final_all_info_df3$tot_bites_nb == i)]) - min(final_all_info_df2$maxN[which(final_all_info_df3$tot_bites_nb == i)])
  list_range_ind <- append(list_range_ind, range)
} 

# thus get the mean value:
(8+8+2+5+1+1+3+1+2+1+1+2+1+2+2+1)  / 38
