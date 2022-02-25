###############################################################################
##
## Script to execute the whole project
##
## make.R
##
## 25/02/2022
##
## Camille Magneville
##
###############################################################################


# load needed packages:
library(dplyr)

# Run analysis script in the right order

# basic df (/!\ run once because take some time)
source(here::here("R", "1_Create_basic_df.R"))

# fig 1
source(here::here("R", "2_1_Occ_Fig1.R"))

# info fig 1
source(here::here("R", "2_2_Info_Fig1.R"))

# fig 2
source(here::here("R", "3_1_Bites_distrib_Fig2.R"))

# info fig 2
source(here::here("R", "3_2_Info_Fig2.R"))

# fig 3
source(here::here("R", "4_1_SS_SR_grazing_Fig3_.R"))

# info fig 3
source(here::here("R", "4_2_Info_Fig3.R"))


