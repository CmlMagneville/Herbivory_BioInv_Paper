###############################################################################
##
## 4_2_Info_Fig3.R
##
## Purpose: Compute some figures related to the fig3 given in the paper
##
## Camille Magneville
##
## cleaned version: 25/02/2022
##
###############################################################################


# 1 - Test significative difference of grazing ####

# load data:
grazing_ss_sr2 <- readRDS(here::here("transformed_data/grazing_ss_sr2.rds"))

# test significative difference or not: Wilcoxon pour chaque categorie de maxN entre ss et ss_sr:

# maxN = 1
wilcox.test(x = grazing_ss_sr2[which(grazing_ss_sr2$category == "ss" & 
                                       grazing_ss_sr2$maxN_cat == "1"), "bites_nb_ss"], 
            y = grazing_ss_sr2[which(grazing_ss_sr2$category == "ss_sr" & 
                                       grazing_ss_sr2$maxN_cat == "1"), "bites_nb_ss"])
# not significant thus there is no significative difference between the medians of bites number whether ss are alone or with sr

# maxN = 2-5
wilcox.test(x = grazing_ss_sr2[which(grazing_ss_sr2$category == "ss" & 
                                       grazing_ss_sr2$maxN_cat == "2-5"), "bites_nb_ss"], 
            y = grazing_ss_sr2[which(grazing_ss_sr2$category == "ss_sr" & 
                                       grazing_ss_sr2$maxN_cat == "2-5"), "bites_nb_ss"])
# not significant thus there is no significative difference between the medians of bites number whether ss are alone or with sr

# maxN = 6-10
wilcox.test(x = grazing_ss_sr2[which(grazing_ss_sr2$category == "ss" & 
                                       grazing_ss_sr2$maxN_cat == "6-10"), "bites_nb_ss"], 
            y = grazing_ss_sr2[which(grazing_ss_sr2$category == "ss_sr" & 
                                       grazing_ss_sr2$maxN_cat == "6-10"), "bites_nb_ss"])
# not significant thus there is no significative difference between the medians of bites number whether ss are alone or with sr



# 2 - Number of replicates:


# number of replicates for maxN = 1
## SS
nrow(grazing_ss_sr2[which(grazing_ss_sr2$category == "ss" & grazing_ss_sr2$maxN_cat == 1), ])

## SS + SR
nrow(grazing_ss_sr2[which(grazing_ss_sr2$category == "ss_sr" & grazing_ss_sr2$maxN_cat == 1), ])

# number of replicates for maxN = 2-5
## SS
nrow(grazing_ss_sr2[which(grazing_ss_sr2$category == "ss" & grazing_ss_sr2$maxN_cat == "2-5"), ])

## SS + SR
nrow(grazing_ss_sr2[which(grazing_ss_sr2$category == "ss_sr" & grazing_ss_sr2$maxN_cat == "2-5"), ])

# number of replicates for maxN = 6 - 10
## SS
nrow(grazing_ss_sr2[which(grazing_ss_sr2$category == "ss" & grazing_ss_sr2$maxN_cat == "6-10"), ])

## SS + SR
nrow(grazing_ss_sr2[which(grazing_ss_sr2$category == "ss_sr" & grazing_ss_sr2$maxN_cat == "6-10"), ])


## Then added by hand using Inkscape on the Fig3 to have the final paper figure
