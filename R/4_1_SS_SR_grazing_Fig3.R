###############################################################################
##
## 4_1_SS_SR_grazing_Fig3.R
##
## Purpose: Prepare data and create the Fig3 of the paper ... 
## ... illutrating the variation of S. salpa grazing rate according to ...
## ... the maximal number of S. salpa depending on whether S. rivulatus was ...
## ... present or absent.Only sequences where S. salpa was grazing were kept ...
## ...  Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 25/02/2022
##
###############################################################################


# 1 - Prepare data ####


# Load data:
final_all_info_df <- readRDS(here::here("transformed_data/final_all_info_df.rds"))

# New df:
data <- final_all_info_df[which(final_all_info_df$species == "Sarpa_salpa" &
                                  final_all_info_df$tot_bites_nb != 0), ]

# create one new df:
grazing_ss_sr <- as.data.frame(matrix(ncol = 5, nrow = nrow(data)))

# colnames:
colnames(grazing_ss_sr) <- c("seq_id", "seq", "bites_nb_ss", "ind_nb_ss", "category")

n <- 1

for (i in (1:nrow(data))) {
  
  bites_nb <- data$tot_bites_nb[i]
  seq_id <- data$seq_id[i]
  
  # check if siganus rivlatus is here or not and fill category:
  category <- "ss"
  if (final_all_info_df[which(final_all_info_df$seq_id == seq_id &
                              final_all_info_df$species == "Siganus_rivulatus"), "tot_bites_nb"] != 0) {
    category <- "ss_sr"
  }
  
  
  grazing_ss_sr$seq_id[n] <- seq_id
  grazing_ss_sr$seq[n] <- data$seq_nb[i]
  grazing_ss_sr$bites_nb_ss[n] <- bites_nb
  grazing_ss_sr$category[n] <- category
  
  # get individual number of ss in the seq:
  ind_ss <- final_all_info_df[which(final_all_info_df$seq_id == seq_id &
                                      final_all_info_df$species == "Sarpa_salpa"), "maxN"]
  grazing_ss_sr$ind_nb_ss[n] <- ind_ss
  
  print(n)
  n <- n + 1
}

# creer les comparsions liees au test de KW pour mettre sur le graph:
my_comparisons <- list( c("ss", "ss_sr"))


# I am going to gather maxN of ss into categorie for better visualisation:
grazing_ss_sr2 <- grazing_ss_sr %>%
  # create a new variable from count
  mutate(maxN_cat = cut(ind_nb_ss, breaks=c(0,1,5,max(ind_nb_ss, na.rm = T)),
                        labels=c("1","2-5","6-10")))

# save df because needed in another script:
saveRDS(grazing_ss_sr2, here::here("transformed_data/grazing_ss_sr2.rds"))

# arrange category level so right label:
grazing_sssr_mod <- grazing_ss_sr2 %>%
  # Rename 4 to 4wd, f to Front, r to Rear
  dplyr::mutate(category = dplyr::recode(category, "ss" = "S. salpa", "ss_sr" = "S. salpa + S. rivulatus"))


# 2 - Plot ####


graph_sssr_boxplots <- ggpubr::ggboxplot(data = grazing_sssr_mod, x = "category", y = "bites_nb_ss", alpha = 0.6,
                                         outlier.shape = NA,
                                         palette = c("#dfc27d", "#80cdc1"),
                                         add = "jitter", add.params = list(size = 3, color = "category"))+
  
  ggplot2::labs(y = "Number of bites per 100 seconds",
                x = "",
                title = "",
                color = "Presence or absence of S. rivulatus") +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
                 panel.border = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(size = 0.5, linetype = "solid", 
                                                   colour = "black"),
                 legend.position = "bottom") +
  ggplot2::scale_x_discrete(labels = c("S. salpa", "S. salpa + S. rivulatus")) +
  ggplot2::facet_wrap(~ maxN_cat)


# save the plot: 
ggplot2::ggsave(filename = here::here("outputs/Fig3.pdf"),
                plot = graph_sssr_boxplots,
                device = "pdf",
                scale = 1,
                height = 5500,
                width = 5800,
                units = "px",
                dpi = 600)
