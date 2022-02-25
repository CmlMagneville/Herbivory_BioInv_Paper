###############################################################################
##
## 5_2_Info_SI5.R
##
## Purpose: Compute some figures related to the Supp Inf 5 given in the paper
##
## Camille Magneville
##
## cleaned version: 25/02/2022
##
###############################################################################


# 1 - Test significative difference of grazing ####


# load the data:
grazing_sc_sr2 <- readRDS(here::here("transformed_data/grazing_sc_sr2.rds"))

# test significative difference or not: Wilcoxon pour chaque categorie de maxN entre sc et sc_sr:


# maxN = 2-5
wilcox.test(x = grazing_sc_sr2[which(grazing_sc_sr2$category == "sc" & 
                                       grazing_sc_sr2$maxN_cat == "2-5"), "bites_nb_sc"], 
            y = grazing_sc_sr2[which(grazing_sc_sr2$category == "sc_sr" & 
                                       grazing_sc_sr2$maxN_cat == "2-5"), "bites_nb_sc"])
# not significant thus there is no significative difference between the medians of bites ...
# ... number whether sc are alone or with sr


# 2 -  Number of replicates:

# number of replicates for maxN = 2-5
## SC
nrow(grazing_sc_sr2[which(grazing_sc_sr2$category == "sc" & grazing_sc_sr2$maxN_cat == "2-5"), ])

## SC + SR
nrow(grazing_sc_sr2[which(grazing_sc_sr2$category == "sc_sr" & grazing_sc_sr2$maxN_cat == "2-5"), ])

## Then added by hand using Inkscape on the Fig3 to have the final paper figure
