
# Herbivory_BioInv R Project

<!-- badges: start -->

[![License:
GPL-2)](https://img.shields.io/badge/License-GPL%202-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

This project contains the code and data for the paper “…” (202..).

:package: It is organised as follow:

-   the `outputs` folder contains high quality PDF files of each Figure
    and Supplementary igure of the paper

-   the `R` folder contains the R scripts used to prepare the basic
    dataframes, compute each plot and diverse values used in the paper.
    Most Figure has one script for preparing data for the plot and
    plotting data and another one for computing interesting values about
    these plots.

-   the `raw_data` folder contains the raw data used in the paper. The
    precise content of each file is described in the following <span
    style="color: aquamarine;">Raw Data section</span>.

-   the `transformed_data` folder contains the data build from the
    `raw data files` computed to do the plots. The precise content of
    each file is described in the following <span
    style="color: aquamarine;">Transformed Data section</span>.

:sparkles: To get all the figures and analysis of the paper, just run
the `make.R` file

## <span style="color: aquamarine;">Goals of the Project</span>

The goal of this project is to study the **herbivory activity** and
**its temporal variation within a day and across days** in an **invasive
context** using **long-duration remote underwater cameras**.

To learn more \>\>\> [add article](link_to_add)

## <span style="color: aquamarine;">Raw Data</span>

The `raw_data` folder contains:

-   `annot_data.rds`: a dataframe containing informations about when
    annotations has been realised. :wrench: It has **four columns** :
    `frame_nb_start` -> the frame number of the beginning of the
    annotation sequence, `frame_nb_stop` -> the frame number of the end
    of the annotation sequence, `real_time_start` -> the time
    corresponding to the beginning of the annotation sequence,
    `real_time_stop` -> the time corresponding to the end of the
    annotation sequence

-   `final_0_1_all_cam_all_days.rds`: the presence absence dataframe of
    the four species of interest. :wrench: It has **seven columns** :
    `time` -> the time for each second of each annotation sequence (thus
    rows of this dataframe does not represent continuous time but time
    of annotation sequences *i.e.* 1min40s each 10min from 8:00:00 to
    18:11:40), `cam_nm` -> the name of the camera on which annotations
    has been realised (four cameras), `recording_day` -> the days on
    which annotations has been realised (three cameras), Then one column
    for each species of interest filled with 1 or 0 if species annotated
    or not.

:bulb: The dataframe has 75 144 row for 101 seconds of annotations \* 62
sequences of annotation \* 3 days \* 4 cameras

-   `final_df_abund_all_cam_all_days.rds`: organised as the
    `final_0_1_all_cam_all_days` dataframe but shows the abundance of
    species for each annotated second.

-   `final_fd_behav_all_cam_all_days.rds`: organised as the
    `final_0_1_all_cam_all_days` dataframe but shows the bites (grazing
    activity) of species for each annotated second.

-   `fish_mouth_size.csv` and `metadata_fish_mouth_size.csv` are data
    collected on several sites thanks to the EXOFISHMED campaign
    (<https://exofishmed.cnrs.fr/>). Only data collected in Crete -
    October 2019 was used.

## <span style="color: aquamarine;">Transformed Data</span>

The `tranformed_data` folder contains:

-   `all_info_df.rds`: organised as the `final_0_1_all_cam_all_days`
    dataframe but Species columns are divided into *grazing* and
    *swimming* behaviour and two columns have been added for sequence
    number (from 1 to 62 per day and camera) and a “day-camera” string.

-   `bites_nb_all_cam_top_df.rds`: shows the number of bites of **each
    species** per sequence and mean maxN across the four spots of each
    species per sequence.

-   `bites_nb_all_sp_top_df.rds`: shows the number of bites for **the
    four species altogether** per sequence and mean maxN across the four
    spots of each species per sequence.

-   `bites_nb_cam_day_df.rds` : shows the proportion of time where each
    species is annotated grazing for one day and one camera compared to
    the amount of time where the species is seen.

-   `bites_nb_seq_model_df.rds`: shows the data used for the generalized
    linear model.

-   `bites_sp_prop_df.rds`: shows the number of bites given by each
    species for one day and one camera and the total number of bites
    done for one day and one camera.

-   `final_all_info_df.rds`: shows for each species and each sequence of
    each day and camera: the maxN, the total bites number, the maximal
    number of individuals seen grazing, the mean number of individuals
    seen grazing, the number of frame where individual are seen grazing
    and the number of frame where individual are seen.

-   `final_all_info_df2.rds`: is the same as the `final_all_info_df` but
    the same of species is changed.

-   `grazing_sc_sr2.rds`: shows the number of bites of *Sparisoma
    cretense* in sequences where it is seen grazing and the number of
    individual of *Sparisoma cretense* grazing. A column indicate
    whether *Sparisoma cretense* individual(s) is/are grazing alone or
    with *Siganus rivulatus* and the last column refers to the category
    of abundance used for the plot.

-   `grazing_ss_sr2.rds`: shows the number of bites of *Sarpa salpa* in
    sequences where it is seen grazing and the number of individual of
    *Sarpa salpa* grazing. A column indicate whether *Sarpa salpa*
    individual(s) is/are grazing alone or with *Siganus rivulatus* and
    the last column refers to the category of abundance used for the
    plot.

## Contributions

CM, SV and TC conceived the ideas and designed methodology; MLLB, TD,
GS, SV and CM collected the data; MLLB and CM analysed the data; CM and
SV led the writing of the manuscript. All authors contributed critically
to the drafts and gave final approval for publication.
