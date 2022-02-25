###############################################################################
##
## 5_1_SC_SR_grazing_SI5.R
##
## Purpose: Prepare data and create the SI5 of the paper ... 
## ... illutrating the variation of S. cretense grazing rate according to ...
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
data <- final_all_info_df[which(final_all_info_df$species == "Sparisoma_cretense" &
                                  final_all_info_df$tot_bites_nb != 0), ]

# create one new df:
grazing_sc_sr <- as.data.frame(matrix(ncol = 5, nrow = nrow(data)))

# colnames:
colnames(grazing_sc_sr) <- c("seq_id", "seq", "bites_nb_sc", "ind_nb_sc", "category")

n <- 1

for (i in (1:nrow(data))) {
  
  bites_nb <- data$tot_bites_nb[i]
  seq_id <- data$seq_id[i]
  
  # check if siganus rivlatus is here or not and fill category:
  category <- "sc"
  if (final_all_info_df[which(final_all_info_df$seq_id == seq_id &
                              final_all_info_df$species == "Siganus_rivulatus"), "tot_bites_nb"] != 0) {
    category <- "sc_sr"
  }
  
  
  grazing_sc_sr$seq_id[n] <- seq_id
  grazing_sc_sr$seq[n] <- data$seq_nb[i]
  grazing_sc_sr$bites_nb_sc[n] <- bites_nb
  grazing_sc_sr$category[n] <- category
  
  # get individual number of ss in the seq:
  ind_sc <- final_all_info_df[which(final_all_info_df$seq_id == seq_id &
                                      final_all_info_df$species == "Sparisoma_cretense"),                                                                            "maxN"]
  grazing_sc_sr$ind_nb_sc[n] <- ind_sc
  
  print(n)
  n <- n + 1
}

# creer les comparsions liees au test de KW pour mettre sur le graph:
my_comparisons <- list( c("sc", "sc_sr"))


# I am going to gather maxN of ss into categorie for better visualisation:
# unlike the analysis with ss we will here only consider 2-5 maxN ...
# ... as there is only one maxN above 5 (30) with sc alone but not with ...
# ... sc and sr
# ... and as for maxN = 1, there is only 1 moment where sc and sr are together.
grazing_sc_sr2 <- grazing_sc_sr %>%
  # create a new variable from count
  mutate(maxN_cat = cut(ind_nb_sc, breaks=c(1,max(ind_nb_sc, na.rm = T)),
                        labels=c("2-5")))

# save grazing_ss_ssr2 because needed in the info script after:
saveRDS(grazing_sc_sr2, file = here::here("transformed_data/grazing_sc_sr2.rds"))

# arrange category level so right label:
grazing_scsr_mod <- grazing_sc_sr2 %>%
  # Rename 4 to 4wd, f to Front, r to Rear
  dplyr::mutate(category = dplyr::recode(category, "sc" = "S. cretense", "sc_sr" = "S. cretense + S. rivulatus"))


# 2 - Plot ####


graph_scsr_boxplots <- ggpubr::ggboxplot(data = grazing_scsr_mod, x = "category", y = "bites_nb_sc", alpha = 0.6,
                                         outlier.shape = NA,
                                         palette = c("#dfc27d", "#80cdc1"),
                                         add = "jitter", add.params = list(size = 3, color = "category"))+
  
  ggplot2::labs(y = "Number of bites per 100 seconds",
                x = "",
                title = "",
                color = "Presence or absence of S. rivulatus") +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),                        panel.border = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(size = 0.5, linetype = "solid", 
                                                   colour = "black"),
                 legend.position = "bottom") +
  ggplot2::scale_x_discrete(labels = c("S. cretense", "S. cretense + S. rivulatus"))


# save the plot: 
ggplot2::ggsave(filename = here::here("outputs/SI5.pdf"),
                plot = graph_scsr_boxplots,
                device = "pdf",
                scale = 1,
                height = 5500,
                width = 5800,
                units = "px",
                dpi = 600)
