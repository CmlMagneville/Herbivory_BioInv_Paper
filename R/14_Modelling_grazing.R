###############################################################################
##
## 14_Modelling_grazing.R.R
##
## Purpose: tested for difference in the timing of grazing activity between ...
## ... species using a generalised mixed model with random effects ...
## ... and zero inflation correction
## ...  Figures are saved in the output folder 
##
## Camille Magneville
##
## cleaned version: 28/02/2022
##
###############################################################################


# 1 - Explanations ####


# We have collected bites number of four herbivorous species on videos in Crete ...
# (two native species: *Sarpa salpa*, *Sparisoma cretense* and two invasive ...
# species: *Siganus_luridus* and *Siganus rivulatus*) for three days and ...
# four cameras. However bites number were not collected for the entire day but ...
# for sequences of 100 secondes per 10 minutes (1min40 per 10min *ie* 10 minutes ...
# per hour *ie* 1/6 of the total video time). Each camera belongs to a system.

# We want to see if the variation in bites number can be explained by the ...
# variation observed between species or/and the variation over time ...
# (according to sequence number). Thus, we thought about doing a ...
# regression model. 



####



# 2 - Prepare data for the model ####


# load data:
annot_data <- readRDS(here::here("raw_data", "annot_data.rds"))
final_pres_abs_graz_tot <-  readRDS(here::here("raw_data", "final_fd_behav_all_cam_all_days.rds"))

## 2. 1-  Create a matrix with sequence equivalence (add sequence column)... ####
# ... and add sequence nb:

graz_time_seq <- final_pres_abs_graz_tot

# add new column for sequence number:
graz_time_seq$seq <- rep(NA, nrow(graz_time_seq))

# add new column for "daycam":
graz_time_seq$daycam <- rep(NA, nrow(graz_time_seq))

## fill the "seq" column with sequences number (row number of annot_data df)...
# ... and complete

# first order the passing time df with increasing time:
graz_time_seq <- graz_time_seq[order(graz_time_seq$time), ]

# then remove rows filled only with 0:
graz_time_seq <- graz_time_seq[apply(graz_time_seq[, c(4:11)], 1, function(x) !all(x == 0)), ]


# then for each line, complete the sequence it belongs to:

for (i in (1:nrow(graz_time_seq))) {
  time <- graz_time_seq$time[i]
  
  for (j in (1:nrow(annot_data))) {
    annot_start_hour <- hms::as_hms(annot_data$real_time_start[j])
    annot_stop_hour <- hms::as_hms(annot_data$real_time_stop[j])
    
    if (annot_start_hour <= time && time <= annot_stop_hour) {
      seq <- paste0("seq", sep = "_", rownames(annot_data)[j])
      break
    }
    
  }
  
  graz_time_seq$seq[i] <- seq
  print(paste0("on en est a la seq", sep = " ", seq))
  graz_time_seq$daycam[i] <- paste0(graz_time_seq$recording_day[i], sep = "_", graz_time_seq$cam_nm[i])
  print(paste0("on en est au daycam", sep = " ", graz_time_seq$daycam[i]))
  
  
}


# then remove time, cam_nm and recording_day columns:
graz_time_seq2 <- graz_time_seq[, -c(1, 2, 3)]


## 2.2 - Bites number per timeslot per species and day and camera ####


## create df containing link with timeslots:
graz_time_seq_timeslot <- tibble::add_column(graz_time_seq2, timeslot = NA)

# create vectors containing the sequences of each timeslot:
seq_vect1 <- paste0("seq", sep = "_", 1:15)
seq_vect2 <- paste0("seq", sep = "_", 16:30)
seq_vect3 <- paste0("seq", sep = "_", 31:45)
seq_vect4 <- paste0("seq", sep = "_", 46:60)


for(i in (1:nrow(graz_time_seq_timeslot))) {
  seq <- graz_time_seq_timeslot$seq[i]
  
  if (seq %in% seq_vect1) {
    graz_time_seq_timeslot$timeslot[i] <- "08:00:00 - 10:29:59"
  }
  
  if (seq %in% seq_vect2) {
    graz_time_seq_timeslot$timeslot[i] <- "10:30:00 - 12:59:59"
  }
  
  if (seq %in% seq_vect3) {
    graz_time_seq_timeslot$timeslot[i] <- "13:00:00 - 15:29:59"
  }
  
  if (seq %in% seq_vect4) {
    graz_time_seq_timeslot$timeslot[i] <- "15:30:00 - 17:59:59"
  }
  
}


## create cleaned df with species, timeslot, day_cam_bites_nb and seq data:

# create df::
bites_nb_df <- data.frame(species = NA, timeslot = NA, day_cam = NA, bites_nb = NA, seq = NA)

# remove swimming individuals from graz_time_timeslot df:
graz_time_seq_timeslot2 <- graz_time_seq_timeslot[, - c(5,6,7,8)]

# COMPLETE FIRST TIMESLOT: sequences 1-15:
seq_vect <- paste0("seq", sep = "_", 1:15)

m <- 1
sum <- 0

for (j in (1:ncol(graz_time_seq_timeslot2[, -c(5, 6, 7)]))) {
  species_nm <- colnames(graz_time_seq_timeslot2)[j]
  
  n <- 0
  
  # loop on days and cam
  for (d in unique(graz_time_seq_timeslot2$daycam)) {
    
    # loop on sequences
    for (s in seq_vect){
      
      # create a df for the day d cam c and sequence s:
      df <- graz_time_seq_timeslot2[which(graz_time_seq_timeslot2$daycam == d & graz_time_seq_timeslot2$seq == s), ]
      
      # check that at least one individual in the sequence or not:
      if(sum(df[, species_nm])  != 0 & s %in% seq_vect) {
        sum <- sum + sum(df[, species_nm])
      }
      
      bites_nb_df[m, "species"] <- species_nm
      bites_nb_df[m, "timeslot"] <- "08:00:00 - 10:29:59"
      bites_nb_df[m, "day_cam"] <- d
      bites_nb_df[m, "bites_nb"] <- sum
      bites_nb_df[m, "seq"] <- s
      sum <- 0
      
      m <- m + 1
      
    }
  }
}

# COMPLETE SECOND TIMESLOT: sequences 16-30:
seq_vect <- paste0("seq", sep = "_", 16:30)

m <- nrow(bites_nb_df) + 1
sum <- 0

for (j in (1:ncol(graz_time_seq_timeslot2[, -c(5, 6, 7)]))) {
  species_nm <- colnames(graz_time_seq_timeslot2)[j]
  
  n <- 0
  
  # loop on days and cam:
  for (d in unique(graz_time_seq_timeslot2$daycam)) {
    
    # loop on sequences:
    for (s in seq_vect){
      
      # create a df for the day d cam c and sequence s:
      df <- graz_time_seq_timeslot2[which(graz_time_seq_timeslot2$daycam == d & graz_time_seq_timeslot2$seq == s), ]
      
      # check that at least one individual in the sequence or not:
      if(sum(df[, species_nm])  != 0 & s %in% seq_vect) {
        sum <- sum + sum(df[, species_nm])
      }
      
      bites_nb_df[m, "species"] <- species_nm
      bites_nb_df[m, "timeslot"] <- "10:30:00 - 12:59:59"
      bites_nb_df[m, "day_cam"] <- d
      bites_nb_df[m, "bites_nb"] <- sum
      bites_nb_df[m, "seq"] <- s
      
      sum <- 0
      
      m <- m + 1
      
    }
  }
}


# COMPLETE THIRD TIMESLOT: sequences 31-45:

seq_vect <- paste0("seq", sep = "_", 31:45)

m <- nrow(bites_nb_df) + 1
sum <- 0

for (j in (1:ncol(graz_time_seq_timeslot2[, -c(5, 6, 7)]))) {
  species_nm <- colnames(graz_time_seq_timeslot2)[j]
  
  n <- 0
  
  # loop on days and cam:
  for (d in unique(graz_time_seq_timeslot2$daycam)) {
    
    # loop on sequences:
    for (s in seq_vect){
      
      # create a df for the day d cam c and sequence s:
      df <- graz_time_seq_timeslot2[which(graz_time_seq_timeslot2$daycam == d & graz_time_seq_timeslot2$seq == s), ]
      
      # check that at least one individual in the sequence or not:
      if(sum(df[, species_nm])  != 0 & s %in% seq_vect) {
        sum <- sum + sum(df[, species_nm])
      }
      
      bites_nb_df[m, "species"] <- species_nm
      bites_nb_df[m, "timeslot"] <- "13:00:00 - 15:29:59"
      bites_nb_df[m, "day_cam"] <- d
      bites_nb_df[m, "bites_nb"] <- sum
      bites_nb_df[m, "seq"] <- s
      
      sum <- 0
      
      m <- m + 1
    }
    
  }
}

# COMPLETE FOURTH TIMESLOT: sequences 46-60:
seq_vect <- paste0("seq", sep = "_", 46:60)

m <- nrow(bites_nb_df) + 1
sum <- 0

for (j in (1:ncol(graz_time_seq_timeslot2[, -c(5, 6, 7)]))) {
  species_nm <- colnames(graz_time_seq_timeslot2)[j]
  
  n <- 0
  
  # loop on days and cam:
  for (d in unique(graz_time_seq_timeslot2$daycam)) {
    
    # loop on sequences:
    for (s in seq_vect){
      
      # create a df for the day d cam c and sequence s:
      df <- graz_time_seq_timeslot2[which(graz_time_seq_timeslot2$daycam == d & graz_time_seq_timeslot2$seq == s), ]
      
      # check that at least one individual in the sequence or not:
      if(sum(df[, species_nm])  != 0 & s %in% seq_vect) {
        sum <- sum + sum(df[, species_nm])
      }
      
      bites_nb_df[m, "species"] <- species_nm
      bites_nb_df[m, "timeslot"] <- "15:30:00 - 17:59:59"
      bites_nb_df[m, "day_cam"] <- d
      bites_nb_df[m, "bites_nb"] <- sum
      bites_nb_df[m, "seq"] <- s
      
      sum <- 0
      
      m <- m + 1
      
    }
  }
}


## 2.3 - Create final table for the model ####

## Step1: add mixt effect in the df bites_nb_df with day/cam/syst/columns: 

bites_nb_seq_df2 <- bites_nb_df
bites_nb_seq_df2 <- tibble::add_column(bites_nb_seq_df2, day = NA)
bites_nb_seq_df2 <- tibble::add_column(bites_nb_seq_df2, cam = NA)
bites_nb_seq_df2 <- tibble::add_column(bites_nb_seq_df2, syst = NA)

# complete the df:

for (i in (1:nrow(bites_nb_seq_df2))) {
  
  bites_nb_seq_df2$day[i] <- substr(bites_nb_seq_df2$day_cam[i], 1, 10)  
  bites_nb_seq_df2$cam[i] <- substr(bites_nb_seq_df2$day_cam[i], 12, 17)  
  
  if (bites_nb_seq_df2$cam[i] == "cam_A1" | bites_nb_seq_df2$cam[i] == "cam_A2") {
    bites_nb_seq_df2$syst[i] <- "syst_A"
  }
  
  if (bites_nb_seq_df2$cam[i] == "cam_B1" | bites_nb_seq_df2$cam[i] == "cam_B2") {
    bites_nb_seq_df2$syst[i] <- "syst_B"
  }
  
}


# add new column with sequences nb in numeric format:
bites_nb_seq_df2 <- tibble::add_column(bites_nb_seq_df2, seq_num = NA)

# fill it:
for (i in (1:nrow(bites_nb_seq_df2))) {
  bites_nb_seq_df2$seq_num[i] <- stringr::str_sub(bites_nb_seq_df2$seq[i], 5)
}
bites_nb_seq_df2$seq_num <- as.numeric(bites_nb_seq_df2$seq_num)


# exchange cam B1 and camB2 on 04/10 because exchange during fieldwork:
bites_nb_seq_df2[which(bites_nb_seq_df2$day == "04-10-2019" & 
                         bites_nb_seq_df2$cam == "cam_B1"), "cam"] <- rep("cam_B3", 
                                nrow(bites_nb_seq_df2[which(bites_nb_seq_df2$day == "04-10-2019" & 
                                    bites_nb_seq_df2$cam == "cam_B1"), ]))

bites_nb_seq_df2[which(bites_nb_seq_df2$day == "04-10-2019" & 
                         bites_nb_seq_df2$cam == "cam_B2"), "cam"] <- rep("cam_B1", 
                         nrow(bites_nb_seq_df2[which(bites_nb_seq_df2$day == "04-10-2019" & 
                              bites_nb_seq_df2$cam == "cam_B2"), ]))

bites_nb_seq_df2[which(bites_nb_seq_df2$day == "04-10-2019" & 
                         bites_nb_seq_df2$cam == "cam_B2"), "day_cam"] <- rep("04-10-2019-cam_B1", 
                         nrow(bites_nb_seq_df2[which(bites_nb_seq_df2$day == "04-10-2019" & 
                         bites_nb_seq_df2$cam == "cam_B2"), ]))

bites_nb_seq_df2[which(bites_nb_seq_df2$day == "04-10-2019" & 
                         bites_nb_seq_df2$cam == "cam_B3"), "cam"] <- rep("cam_B2",
                        nrow(bites_nb_seq_df2[which(bites_nb_seq_df2$day == "04-10-2019" & 
                        bites_nb_seq_df2$cam == "cam_B3"), ]))


bites_nb_seq_df2[which(bites_nb_seq_df2$day == "04-10-2019" & 
                         bites_nb_seq_df2$cam == "cam_B3"), "day_cam"] <- rep("04-10-2019-cam_B2", 
                        nrow(bites_nb_seq_df2[which(bites_nb_seq_df2$day == "04-10-2019" & 
                        bites_nb_seq_df2$cam == "cam_B3"), ]))

# convert data into factors and not caracter strings:
bites_nb_seq_df2$species <- as.factor(bites_nb_seq_df2$species)
bites_nb_seq_df2$day <- as.factor(bites_nb_seq_df2$day)
bites_nb_seq_df2$cam <- as.factor(bites_nb_seq_df2$cam)
bites_nb_seq_df2$syst <- as.factor(bites_nb_seq_df2$syst)
bites_nb_seq_df2$seq <- as.factor(bites_nb_seq_df2$seq)

# save data:
saveRDS(bites_nb_seq_df2, file = here::here("transformed_data", "bites_nb_seq_model_df.rds"))



#####



# 3 - Visualise data ####


hist(bites_nb_seq_df2$bites_nb)
hist(log10(bites_nb_seq_df2$bites_nb + 1))

# We can see that we have a lot of 0 in our datasets:  ...
# ... in fact, there is a lot of sequences where there was no herbivory...
# ... Even when log transforming.



#####



# 4 - Data consistent with Poisson distribution?


## POISSON:

# visualisation:
theoretic_count <-rpois(1804, mean(bites_nb_seq_df2$bites_nb))

# add theorical counts in a df: 
tc_df <-data.frame(theoretic_count)

# plot simulteaneously observed and theorical countings:

ggplot2::ggplot(bites_nb_seq_df2, ggplot2::aes(bites_nb)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theoretic_count,fill = "#1E90FF", alpha = 0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")

# can see zero inflation compared to classical Poisson distribution

# another way to test Poisson distrib:
# ... on utilise le test du Chisquared "Goodness of fit" du pkg vcd:
# H0: it follows the Poisson distrib
gf <-  vcd::goodfit(bites_nb_seq_df2$bites_nb, type = "poisson", method = "ML")
plot(gf, main = "Count data vs Poisson distribution")
summary(gf)



## CONCLUSION:

# No (H0: fits well), H0 rejected: So we will build a regression ...
# ... model with a correction for 0 inflation through the *glmmTMB* package ...
# ... (read code comments)



#####



# 5 - Build the model and parameters computation ####


## Explanations:

# build the model
# idea: bites_nb repartition doesn't follow a Normal distribution -> generalized linear model 
# ... what's more: camera ID/system IS,  atudied day can have an impact ...
# ... on bites nb: need to take them into account in the model!
# ... use a generalized mixed linear model which will take into account ...
# ... the possible effects of cam/syst/day ...
# ... What's more: a lot of sequences with no bites so ...
# ... when visualising bites_nb data, it has a lot of 0:
# ... use a generalized mixed model with correction for zero inflation


model1 <- glmmTMB::glmmTMB(bites_nb ~ species*seq_num + (1|syst/cam) + (1 + seq_num|day), 
                  data = bites_nb_seq_df2, ziformula = ~ 1, family = poisson,
                  control = glmmTMB::glmmTMBControl(parallel = 2))

summary(model1)

# get ANOVA table
glmmTMB:::Anova.glmmTMB(model1)
# ... species effect (bites number varies according to which species eats)
# ... seq_nb effect (bites number varies according to the sequence number ie time of the day)


# utilisation du package DAHRMA pour regarder si ok:
simulationOutput <- DHARMa::simulateResiduals(fittedModel = model1, plot = TRUE)
DHARMa::plotResiduals(simulationOutput) 

# compute pseudo R2:
performance::r2_nakagawa(model1, by_group = FALSE, tolerance = 1e-8)
