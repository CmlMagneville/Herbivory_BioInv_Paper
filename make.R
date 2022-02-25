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
library(tidyverse)

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
source(here::here("R", "4_1_SS_SR_grazing_Fig3.R"))

# info fig 3
source(here::here("R", "4_2_Info_Fig3.R"))

# SI5
source(here::here("R", "5_1_SC_SR_grazing_SI5.R"))

# SI5
source(here::here("R", "5_2_Info_SI5.R"))

# fig 4
source(here::here("R", "6_1_maxN_bites_Fig4.R"))

# info fig 4
source(here::here("R", "6_2_Info_Fig4.R"))

# fig 5
source(here::here("R", "7_1_Temp_var_Fig5.R"))

# info fig 5
source(here::here("R", "7_2_Info_Fig5.R"))

# fig 6


# info fig6


# fig 7
source(here::here("R", "9_Cum_graz_Fig7.R"))


