
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

## <span style="color: aquamarine;">Raw Data</span>

The `raw_data` folder contains:

-   `annot_data.rds`: a dataframe containing informations about when
    annotations has been realised. :wrench: It has **four columns**:
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
