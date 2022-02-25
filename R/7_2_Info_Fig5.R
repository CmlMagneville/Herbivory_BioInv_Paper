###############################################################################
##
## 7_2_Info_Fig5.R
##
## Purpose: Compute some figures related to the Fig5 given in the paper
##
## Camille Magneville
##
## cleaned version: 25/02/2022
##
###############################################################################


# 1 - Total nb of bites and proportion if homogeneous ####

# Load data:
bites_nb_all_sp_top_df <- readRDS(here::here("transformed_data", "bites_nb_all_sp_top_df.rds"))

# total number of bites for the 3 days and 4 cam:
sum(bites_nb_all_sp_top_df$bites_nb)
# so total number of bites PER SEQUENCE if herb is an homogeneous process:
1978/(62*3)
# so prop:
(13.36/1978)*100


# 2 - Proportion of grazing activity per day #### 

# compute the proportion of grazing activity for each day:
unique(bites_nb_all_sp_top_df$day)

prop_02 <- ((sum(bites_nb_all_sp_top_df$bites_nb[which(bites_nb_all_sp_top_df$day == unique(bites_nb_all_sp_top_df$day)[1])])) /  sum(bites_nb_all_sp_top_df$bites_nb))*100

prop_03 <- ((sum(bites_nb_all_sp_top_df$bites_nb[which(bites_nb_all_sp_top_df$day == unique(bites_nb_all_sp_top_df$day)[2])])) /  sum(bites_nb_all_sp_top_df$bites_nb))*100

prop_04 <- ((sum(bites_nb_all_sp_top_df$bites_nb[which(bites_nb_all_sp_top_df$day == unique(bites_nb_all_sp_top_df$day)[3])])) /  sum(bites_nb_all_sp_top_df$bites_nb))*100

# compute prop of sequences with no bites:
prop_0 <- ((nrow(bites_nb_all_sp_top_df[which(bites_nb_all_sp_top_df$bites_nb == 0), ])) /                            nrow(bites_nb_all_sp_top_df))*100


# 3 - Other values ####


# sequence with the highest number of bites accounts for ... % of total herb:
(189 / sum(bites_nb_all_sp_top_df$bites_nb))*100

# five first sequence with the highest number of bites accounts for ... % of total herb:
((189 + 87 + 76 + 63 + 53) / sum(bites_nb_all_sp_top_df$bites_nb))*100

## the 1st five grazing events correspond to the ... weakest events:
bites_5 <- (189 + 87 + 76 + 63 + 53)

# create a new table that only contains grazing events (remove 0) and is ordered according to bites nb:
bites_nb_all_cam_top_df2 <- dplyr::arrange(bites_nb_all_sp_top_df, bites_nb)
bites_nb_all_cam_top_df2 <- bites_nb_all_cam_top_df2[which(bites_nb_all_cam_top_df2$bites_nb != 0), ]

# count the nb of sequences need to go to a bite nb == bites_5:

n <- 0
seq <- 1

for (i in (1:nrow(bites_nb_all_cam_top_df2))) {
  if (n >= 468) {
    print(paste0("Need", sep = " ", seq, sep = " ", "sequences"))
  }
  else {
    n <- n + bites_nb_all_cam_top_df2$bites_nb[i]
    seq <- seq + 1
  }
}
