###############################################################################
##
## 12_maxN_passing_SI6.R
##
## Purpose: Prepare data and create the Supp Fig6 of the paper ... 
## ... illutrating the Log transformed maximal number of individuals ...
## ... per time-occurence ...
## ...  Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 28/02/2022
##
###############################################################################


# 1 - Prepare data:


# load data:
final_pres_abs_tot <- readRDS(here::here("raw_data", "final_df_abund_all_cam_all_days.rds"))

# create a dataframe measuring for each passing time, the max nb of ind:
passing_time_size <- data.frame(Siganus_rivulatus_time = NA, Siganus_rivulatus_nb = NA, Sarpa_salpa_time = NA,
                                Sarpa_salpa_nb = NA, Siganus_luridus_time = NA,
                                Siganus_luridus_nb = NA, Sparisoma_cretense_time = NA,
                                Sparisoma_cretense_nb = NA, time = NA, daycam = NA)


# loop to fill the df:
for (j in (4:ncol(final_pres_abs_tot))) {
  
  # initialize counters:
  n <- 0
  m <- 1
  max <- 0
  
  species_nm <- colnames(final_pres_abs_tot)[j]
  
  for (i in (1:nrow(final_pres_abs_tot))) {
    
    value <- final_pres_abs_tot[i, j]
    
    if (value == 0 & n == 0) {
      n <- 0
    }
    
    if (value != 0 & n!= 0 & max != 0) {
      n <- n + 1
      if (final_pres_abs_tot[i, j] > max) {
        max <- value
      }
    }
    
    if (value != 0 & n == 0 & max == 0) {
      n <- n + 1
      max<-value
      time <- final_pres_abs_tot[i, "time"]
    }
    
    if (value == 0 & n != 0) {
      passing_time_size[m, paste0(species_nm, rep = "_", "time")] <- n
      passing_time_size[m, paste0(species_nm, rep = "_", "nb")] <- max
      passing_time_size[m, "time"] <- time
      passing_time_size[m, 'daycam'] <- paste0(final_pres_abs_tot$recording_day[i], sep = "_", final_pres_abs_tot$cam_nm[i])
      n <- 0
      max<-0
      m <- m + 1
    }
    
  }
}

passing_time_size$time <- hms::as_hms(passing_time_size$time)

# compute the number of passing sequences that we have for each species:
nb_passage_s_r <- nrow(passing_time_size[which(! is.na(passing_time_size$Siganus_rivulatus_nb)), ])
nb_passage_s_l <- nrow(passing_time_size[which(! is.na(passing_time_size$Siganus_luridus_nb)), ])
nb_passage_s_s <- nrow(passing_time_size[which(! is.na(passing_time_size$Sarpa_salpa_nb)), ])
nb_passage_s_c <- nrow(passing_time_size[which(! is.na(passing_time_size$Sparisoma_cretense_nb)), ])

# build the dataframe:
maxN_graph <- data.frame(species = c(rep("Siganus_rivulatus", nb_passage_s_r),
                                     rep("Siganus_luridus", nb_passage_s_l),
                                     rep("Sarpa_salpa", nb_passage_s_s),
                                     rep("Sparisoma_cretense", nb_passage_s_c)),
                         maxN_passing = c(passing_time_size[which(!is.na(passing_time_size$Siganus_rivulatus_nb)), "Siganus_rivulatus_nb"],
                                          passing_time_size[which(!is.na(passing_time_size$Siganus_luridus_nb)), "Siganus_luridus_nb"],       passing_time_size[which(!is.na(passing_time_size$Sarpa_salpa_nb)), "Sarpa_salpa_nb"],               passing_time_size[which(!is.na(passing_time_size$Sparisoma_cretense_nb)), "Sparisoma_cretense_nb"]))


# ordonner les especes dans le df pour que ok dans le plot:
maxN_graph$species <- factor(maxN_graph$species,       
                             levels = c("Sparisoma_cretense", "Sarpa_salpa",  "Siganus_luridus", "Siganus_rivulatus"))



# 2 - Plot #####


# plot the boxplots + violin plots:
colours_pal <- c("#481567FF", "#DCE319FF", "#55C667FF", "#238A8DFF") 

graph_maxN_passing <- ggplot2::ggplot(data = maxN_graph, 
                                      ggplot2::aes(x = species, y = log2(maxN_passing), fill = species)) +
  ggplot2::geom_violin(width = 1.4, show.legend = FALSE) +
  ggplot2::geom_boxplot(width = 0.1, color = "grey", alpha = 0.2, show.legend = FALSE) +
  ggplot2::scale_fill_manual(values = colours_pal,
                             labels = c("Sparisoma cretense", "Sarpa salpa",  "Siganus luridus", "Siganus rivulatus")) +
  ggplot2::scale_x_discrete(labels = c("Sparisoma cretense",
                                       "Sarpa salpa", "Siganus luridus",
                                       "Siganus rivulatus")) +
  ggplot2::labs(y = "log2(MaxN per passing)",
                x = "",
                title = "",
                subtitle = "") +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),                        panel.border = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(size = 0.5, linetype = "solid", 
                                                   colour = "black")) +
  ggplot2::ylim(0, 6) 
  
# Comment fishualize lines because did not work while coding in February 2022:
  # fishualize::add_fishape(family = "Siganidae",
  #                         option = "Siganus_rivulatus",
  #                         xmin = 3.6, xmax = 4.4, ymin = 4.5, ymax = 5.5,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3) +
  # fishualize::add_fishape(family = "Sparidae",
  #                         option = "Sarpa_salpa",
  #                         xmin = 1.6, xmax = 2.4, ymin = 4, ymax = 5,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3) +
  # fishualize::add_fishape(family = "Labridae",
  #                         option = "Sparisoma_cretense",
  #                         xmin = 0.5, xmax = 1.4, ymin = 5, ymax = 6,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3)  +
  # fishualize::add_fishape(family = "Siganidae",
  #                         option = "Siganus_luridus",
  #                         xmin = 2.6, xmax = 3.4, ymin = 2, ymax = 3,
  #                         scaled = FALSE,
  #                         fill = "grey38",
  #                         alpha = 0.3)

# save the plot:
ggplot2::ggsave(filename = here::here("outputs/SI6.pdf"),
                plot = graph_maxN_passing,
                device = "pdf",
                scale = 1,
                height = 5500,
                width = 5800,
                units = "px",
                dpi = 600)

