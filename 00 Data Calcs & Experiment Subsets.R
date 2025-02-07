cat("\014") #clears console
rm(list=ls()) 
graphics.off()

#### load relevant packages ####
library("readxl")
library("tidyverse")
library("reshape")

#### data input ####
dat <- read_excel("00_DataInputs/02_BP_Iso_Data/Saci_environmentalConditions_d2H_rawData.xlsx",
                  sheet = "Data")

#### SYMBOLOGY ####
dat$pch = 21
dat$Col = "black"

#### CALCULATIONS ####
##### Abundance-weighted mean d2H #####
# Check if BP3_d2H contains a number
include_BP3 <- !is.na(dat$BP3_d2H_mean) & !is.nan(dat$BP3_d2H_mean)

# Calculate d2H_wt_mean
# Will include BP3 term if d2H_BP3_mean is a number
# Will return NA if d2H_BP0, d2H_BP1, or d2H_BP2 is NA
dat$d2H_wt_mean <- ifelse(include_BP3,
                      rowSums(dat[, c("BP0_d2H_mean", "BP1_d2H_mean", "BP2_d2H_mean", "BP3_d2H_mean")] *
                                dat[, c("BP0_relAbun_mean", "BP1_relAbun_mean", "BP2_relAbun_mean", "BP3_relAbun_mean")]) /
                        rowSums(dat[, c("BP0_relAbun_mean", "BP1_relAbun_mean", "BP2_relAbun_mean", "BP3_relAbun_mean")]),
                      rowSums(dat[, c("BP0_d2H_mean", "BP1_d2H_mean", "BP2_d2H_mean")] *
                                dat[, c("BP0_relAbun_mean", "BP1_relAbun_mean", "BP2_relAbun_mean")]) /
                        rowSums(dat[, c("BP0_relAbun_mean", "BP1_relAbun_mean", "BP2_relAbun_mean")]))

##### Propagate error for weighted mean d2H #####
# Propagate errors and calculate d2H_wt_mean_sd
dat$d2H_wt_mean_sd <- NA
# i = 1
# loop through all rows
for (i in 1:nrow(dat)) {
  # Check if replicate has d2H measurements for BPs 0 1 and 2
  if (!anyNA(c(dat$BP0_d2H_mean[i], dat$BP1_d2H_mean[i], dat$BP2_d2H_mean[i]))) {
    # Standard deviations of d2H and rel abundance
    sigma_d2H_BP0 <-  dat$BP0_d2H_error[i]
    sigma_d2H_BP1 <-  dat$BP1_d2H_error[i]
    sigma_d2H_BP2 <-  dat$BP2_d2H_error[i]
    sigma_d2H_BP3 <-  dat$BP3_d2H_error[i]
    sigma_Abundance_BP0 <-  dat$BP0_relAbun_error[i]
    sigma_Abundance_BP1 <-  dat$BP1_relAbun_error[i]
    sigma_Abundance_BP2 <-  dat$BP2_relAbun_error[i]
    sigma_Abundance_BP3 <-  dat$BP3_relAbun_error[i]
    
    # Weight sigmas by rel abundance mean
    w_Abundance_BP0 <- dat$BP0_relAbun_mean[i]
    w_Abundance_BP1 <- dat$BP1_relAbun_mean[i]
    w_Abundance_BP2 <- dat$BP2_relAbun_mean[i]
    w_Abundance_BP3 <- dat$BP3_relAbun_mean[i]
    
    # error propagation calculation
    dat$d2H_wt_mean_sd[i] <- sqrt(
      sum(
        ((w_Abundance_BP0 * ((ifelse(is.na(sigma_d2H_BP0), 0, sigma_d2H_BP0))^2))),
        ((w_Abundance_BP1 * ((ifelse(is.na(sigma_d2H_BP1), 0, sigma_d2H_BP1))^2))),
        ((w_Abundance_BP2 * ((ifelse(is.na(sigma_d2H_BP2), 0, sigma_d2H_BP2))^2))),
        ((w_Abundance_BP3 * ((ifelse(is.na(sigma_d2H_BP3), 0, sigma_d2H_BP3))^2))),
        ((w_Abundance_BP0 * ifelse(is.na(sigma_Abundance_BP0), 0, sigma_Abundance_BP0))^2),
        ((w_Abundance_BP1 * ifelse(is.na(sigma_Abundance_BP1), 0, sigma_Abundance_BP1))^2),
        ((w_Abundance_BP2 * ifelse(is.na(sigma_Abundance_BP2), 0, sigma_Abundance_BP2))^2),
        ((w_Abundance_BP3 * ifelse(is.na(sigma_Abundance_BP3), 0, sigma_Abundance_BP3))^2)
      )
    )
  }
}

# summary(dat$d2H_wt_mean_sd)

##### Eps_L/W values for each BP #####
dat$BP0_eps_LW_mean <- ((dat$BP0_d2H_mean/1000 + 1) / 
                               (dat$Water_d2H/1000 + 1) - 1) * 1000
dat$BP0_eps_LW_error <- sqrt((1/1000/(dat$Water_d2H/1000 + 1) * 1000 * dat$BP0_d2H_error)^2 + (-((dat$BP0_d2H_mean/1000 + 1) * (1/1000)/(dat$Water_d2H/1000 + 1)^2 * 1000) * 0.2)^2)  

dat$BP1_eps_LW_mean <- ((dat$BP1_d2H_mean/1000 + 1) / 
                               (dat$Water_d2H/1000 + 1) - 1) * 1000
dat$BP1_eps_LW_error <- sqrt((1/1000/(dat$Water_d2H/1000 + 1) * 1000 * dat$BP1_d2H_error)^2 + (-((dat$BP1_d2H_mean/1000 + 1) * (1/1000)/(dat$Water_d2H/1000 + 1)^2 * 1000) * 0.2)^2)  

dat$BP2_eps_LW_mean <- ((dat$BP2_d2H_mean/1000 + 1) / 
                               (dat$Water_d2H/1000 + 1) - 1) * 1000
dat$BP2_eps_LW_error <- sqrt((1/1000/(dat$Water_d2H/1000 + 1) * 1000 * dat$BP2_d2H_error)^2 + (-((dat$BP2_d2H_mean/1000 + 1) * (1/1000)/(dat$Water_d2H/1000 + 1)^2 * 1000) * 0.2)^2)  

dat$BP3_eps_LW_mean <- ((dat$BP3_d2H_mean/1000 + 1) / 
                               (dat$Water_d2H/1000 + 1) - 1) * 1000
dat$BP3_eps_LW_error <- sqrt((1/1000/(dat$Water_d2H/1000 + 1) * 1000 * dat$BP3_d2H_error)^2 + (-((dat$BP3_d2H_mean/1000 + 1) * (1/1000)/(dat$Water_d2H/1000 + 1)^2 * 1000) * 0.2)^2)  


##### Abundance weighted mean E_L/W #####
dat$EpsLW_wt_mean <- ifelse(include_BP3,
                               rowSums(dat[, c("BP0_eps_LW_mean", "BP1_eps_LW_mean", "BP2_eps_LW_mean", "BP3_eps_LW_mean")] *
                                         dat[, c("BP0_relAbun_mean", "BP1_relAbun_mean", "BP2_relAbun_mean", "BP3_relAbun_mean")]) /
                                 rowSums(dat[, c("BP0_relAbun_mean", "BP1_relAbun_mean", "BP2_relAbun_mean", "BP3_relAbun_mean")]),
                               rowSums(dat[, c("BP0_eps_LW_mean", "BP1_eps_LW_mean", "BP2_eps_LW_mean")] *
                                         dat[, c("BP0_relAbun_mean", "BP1_relAbun_mean", "BP2_relAbun_mean")]) /
                                 rowSums(dat[, c("BP0_relAbun_mean", "BP1_relAbun_mean", "BP2_relAbun_mean")]))

##### Propagate error for mean weighted E_L/W #####
# propagate error for mean weighted E_L/W
dat$Wt_mean_Eps_sd <- NA
# i=1
# loop through all rows
for (i in 1:nrow(dat)) {
  # Check if replicate has d2H values for BPs 0 1 and 2
  if (!anyNA(c(dat$BP0_d2H_mean[i], dat$BP1_d2H_mean[i], dat$BP2_d2H_mean[i]))) {
    # Standard deviations of d2H values and rel abundances
    sigma_d2H_water <-  dat$Water_d2H_error[i]
    sigma_d2H_BP0 <-  dat$BP0_d2H_error[i]
    sigma_d2H_BP1 <-  dat$BP1_d2H_error[i]
    sigma_d2H_BP2 <-  dat$BP2_d2H_error[i]
    sigma_d2H_BP3 <-  dat$BP3_d2H_error[i]
    sigma_Abundance_BP0 <-  dat$BP0_relAbun_error[i]
    sigma_Abundance_BP1 <-  dat$BP1_relAbun_error[i]
    sigma_Abundance_BP2 <-  dat$BP2_relAbun_error[i]
    sigma_Abundance_BP3 <-  dat$BP3_relAbun_error[i]
    
    # Weight by rel abundance
    w_Abundance_BP0 <- dat$BP0_relAbun_mean[i]
    w_Abundance_BP1 <- dat$BP1_relAbun_mean[i]
    w_Abundance_BP2 <- dat$BP2_relAbun_mean[i]
    w_Abundance_BP3 <- dat$BP3_relAbun_mean[i]
    
    # error propagation calculation
    dat$Wt_mean_Eps_sd[i] <- sqrt(
      sum(
        ((w_Abundance_BP0 * ((ifelse(is.na(sigma_d2H_BP0), 0, sigma_d2H_BP0))^2))),
        ((w_Abundance_BP1 * ((ifelse(is.na(sigma_d2H_BP1), 0, sigma_d2H_BP1))^2))),
        ((w_Abundance_BP2 * ((ifelse(is.na(sigma_d2H_BP2), 0, sigma_d2H_BP2))^2))),
        ((w_Abundance_BP3 * ((ifelse(is.na(sigma_d2H_BP3), 0, sigma_d2H_BP3))^2))),
        ((ifelse(is.na(sigma_d2H_water), 0, sigma_d2H_water))^2),  
        ((w_Abundance_BP0 * ifelse(is.na(sigma_Abundance_BP0), 0, sigma_Abundance_BP0))^2),
        ((w_Abundance_BP1 * ifelse(is.na(sigma_Abundance_BP1), 0, sigma_Abundance_BP1))^2),
        ((w_Abundance_BP2 * ifelse(is.na(sigma_Abundance_BP2), 0, sigma_Abundance_BP2))^2),
        ((w_Abundance_BP3 * ifelse(is.na(sigma_Abundance_BP3), 0, sigma_Abundance_BP3))^2)
      )
    )
  }
}


##### Ring Index - BP #####
# if either BP0, BP1, or BP2 are missing a relative abundance, return NA
# Only include the BP3 term if the BP3 rel abundance is not NA

dat$RingIndex_BP <- NA

# Loop through each row
for (i in 1:nrow(dat)) {
  # Check if any of BP0, BP1, or BP2 is NA
  if (any(is.na(c(dat$BP0_relAbun_mean[i], dat$BP1_relAbun_mean[i], dat$BP2_relAbun_mean[i])))) {
    # Skip this row if any of BP0, BP1, or BP2 is NA
    dat$RingIndex_BP[i] <- NA
    next
  }
  
  # Check if BP3_relAbun_mean contains NA
  if (is.na(dat$BP3_relAbun_mean[i])) {
    # Calculate RingIndex_BP without BP3
    dat$RingIndex_BP[i] <- ((dat$BP0_relAbun_mean[i] * 0) + 
                               (dat$BP1_relAbun_mean[i] * 1) + 
                               (dat$BP2_relAbun_mean[i] * 2)) /
      sum(dat$BP0_relAbun_mean[i], dat$BP1_relAbun_mean[i], dat$BP2_relAbun_mean[i], na.rm = TRUE)
  } else {
    # Calculate RingIndex_BP with BP3
    dat$RingIndex_BP[i] <- ((dat$BP0_relAbun_mean[i] * 0) + 
                               (dat$BP1_relAbun_mean[i] * 1) + 
                               (dat$BP2_relAbun_mean[i] * 2) + 
                               (dat$BP3_relAbun_mean[i] * 3)) /
      sum(dat$BP0_relAbun_mean[i], dat$BP1_relAbun_mean[i], dat$BP2_relAbun_mean[i], dat$BP3_relAbun_mean[i], na.rm = TRUE)
  }
}

##### RI-BP error #####
# Calculate error propagation terms
term1 <- (dat$BP1_relAbun_error * 1)^2
term2 <- (dat$BP2_relAbun_error * 2)^2 # weighted
term3 <- (dat$BP3_relAbun_error * 3)^2 # weighted

# Calculate total error
dat$RingIndex_BP_error <- sqrt(term1 + term2 + term3)

##### Ring Index - GDGT #####
dat$RingIndex_GDGT <- ((dat$GDGT0_RelAbun * 0) +
                              (dat$GDGT1_RelAbun * 1) +
                              (dat$GDGT2_RelAbun * 2) +
                              (dat$GDGT3_RelAbun * 3) +
                              (dat$GDGT4_RelAbun * 4) +
                              (dat$GDGT5_RelAbun * 5) +
                              (dat$GDGT6_RelAbun * 6) +
                              (dat$GDGT7_RelAbun * 7) +
                              (dat$GDGT8_RelAbun * 8) /100)/100
  
  
##### Ring Difference from d2H and EpsLW values #####  
# calculate Δε/ring after Leavitt et al., 2023 (GCA)
# Changes in εL/W per ring (Δε/ring) were calculated as the average of 
# isotope ratios for all combinations of (δBP(x) – δBP(y < x))/ (ring difference x-y)
# if/else statement to only includes BP3 terms if BP3_d2H_mean has a value

##### calculate from d2H values #####
# dat <- dat %>%
#   mutate(RingDiff_BP1 = ((BP1_d2H_mean - BP0_d2H_mean) / (1 - 0)),
#          RingDiff_BP2 = ((BP2_d2H_mean - BP1_d2H_mean) / (2 - 1) +
#                            (BP2_d2H_mean - BP0_d2H_mean) / (2 - 0)) / 2,
#          RingDiff_BP3 = ((BP3_d2H_mean - BP2_d2H_mean) / (3 - 2) +
#                            (BP3_d2H_mean - BP1_d2H_mean) / (3 - 1) +
#                            (BP3_d2H_mean - BP0_d2H_mean) / (3 - 0)) / 3,
#          RingDiff_all = ifelse(is.na(BP3_d2H_mean),
#                                (((BP1_d2H_mean - BP0_d2H_mean) / (1 - 0) +
#                                    (BP2_d2H_mean - BP1_d2H_mean) / (2 - 1) +
#                                    (BP2_d2H_mean - BP0_d2H_mean) / (2 - 0)) / 3),
#                                (((BP1_d2H_mean - BP0_d2H_mean) / (1 - 0) +
#                                    (BP3_d2H_mean - BP2_d2H_mean) / (3 - 2) +
#                                    (BP3_d2H_mean - BP1_d2H_mean) / (3 - 1) +
#                                    (BP3_d2H_mean - BP0_d2H_mean) / (3 - 0) +
#                                    (BP2_d2H_mean - BP1_d2H_mean) / (2 - 1) +
#                                    (BP2_d2H_mean - BP0_d2H_mean) / (2 - 0))) / 6))
# 
# # Propagate error for Ring Diff calcs
# # Create empty columns to store the calculated errors
# dat$RingDiff_BP1_error <- NA
# dat$RingDiff_BP2_error <- NA
# dat$RingDiff_BP3_error <- NA
# dat$RingDiff_all_error <- NA
# 
# # Iterate through each row of the dataframe
# for (i in 1:nrow(dat)) {
#   # Calculate error for each term
#   diff_10_error <- sqrt((dat$BP1_d2H_error[i]^2 + dat$BP0_d2H_error[i]^2) / (1 - 0)^2)
#   diff_21_error <- sqrt((dat$BP2_d2H_error[i]^2 + dat$BP1_d2H_error[i]^2) / (2 - 1)^2)
#   diff_20_error <- sqrt((dat$BP2_d2H_error[i]^2 + dat$BP0_d2H_error[i]^2) / (2 - 0)^2)
#   diff_32_error <- sqrt((dat$BP3_d2H_error[i]^2 + dat$BP2_d2H_error[i]^2) / (3 - 2)^2)
#   diff_31_error <- sqrt((dat$BP3_d2H_error[i]^2 + dat$BP1_d2H_error[i]^2) / (3 - 1)^2)
#   diff_30_error <- sqrt((dat$BP3_d2H_error[i]^2 + dat$BP0_d2H_error[i]^2) / (3 - 0)^2)
#   
#   # Propagate error
#   dat$RingDiff_BP1_error[i] <- sqrt((diff_10_error^2) / 1^2)
#   dat$RingDiff_BP2_error[i] <- sqrt((diff_21_error^2 + diff_20_error^2) / 2^2)
#   dat$RingDiff_BP3_error[i] <- sqrt((diff_32_error^2 + diff_31_error^2 + diff_30_error^2) / 3^2)
#   dat$RingDiff_all_error[i] <- sqrt((diff_32_error^2 + diff_31_error^2 + diff_30_error^2 + diff_32_error^2 + diff_31_error^2 + diff_10_error^2) / 6^2)
# }


###### calculate using Eps values #####
dat <- dat %>%
  mutate(RingDiff_BP1 = ((BP1_eps_LW_mean - BP0_eps_LW_mean) / (1 - 0)),
         RingDiff_BP2 = ((BP2_eps_LW_mean - BP1_eps_LW_mean) / (2 - 1) +
                               (BP2_eps_LW_mean - BP0_eps_LW_mean) / (2 - 0)) / 2,
         RingDiff_BP3 = ((BP3_eps_LW_mean - BP2_eps_LW_mean) / (3 - 2) +
                               (BP3_eps_LW_mean - BP1_eps_LW_mean) / (3 - 1) +
                               (BP3_eps_LW_mean - BP0_eps_LW_mean) / (3 - 0)) / 3,
         RingDiff_all = ifelse(is.na(BP3_eps_LW_mean),
                                   (((BP1_eps_LW_mean - BP0_eps_LW_mean) / (1 - 0) +
                                       (BP2_eps_LW_mean - BP1_eps_LW_mean) / (2 - 1) +
                                       (BP2_eps_LW_mean - BP0_eps_LW_mean) / (2 - 0)) / 3),
                                   (((BP1_eps_LW_mean - BP0_eps_LW_mean) / (1 - 0) +
                                       (BP3_eps_LW_mean - BP2_eps_LW_mean) / (3 - 2) +
                                       (BP3_eps_LW_mean - BP1_eps_LW_mean) / (3 - 1) +
                                       (BP3_eps_LW_mean - BP0_eps_LW_mean) / (3 - 0) +
                                       (BP2_eps_LW_mean - BP1_eps_LW_mean) / (2 - 1) +
                                       (BP2_eps_LW_mean - BP0_eps_LW_mean) / (2 - 0))) / 6))
# Iterate through each row of the dataframe
for (i in 1:nrow(dat)) {
  # Calculate error for each term
  diff_10_error <- sqrt((dat$BP1_eps_LW_error[i]^2 + dat$BP0_eps_LW_error[i]^2) / (1 - 0)^2)
  diff_21_error <- sqrt((dat$BP2_eps_LW_error[i]^2 + dat$BP1_eps_LW_error[i]^2) / (2 - 1)^2)
  diff_20_error <- sqrt((dat$BP2_eps_LW_error[i]^2 + dat$BP0_eps_LW_error[i]^2) / (2 - 0)^2)
  diff_32_error <- sqrt((dat$BP3_eps_LW_error[i]^2 + dat$BP2_eps_LW_error[i]^2) / (3 - 2)^2)
  diff_31_error <- sqrt((dat$BP3_eps_LW_error[i]^2 + dat$BP1_eps_LW_error[i]^2) / (3 - 1)^2)
  diff_30_error <- sqrt((dat$BP3_eps_LW_error[i]^2 + dat$BP0_eps_LW_error[i]^2) / (3 - 0)^2)
  
  # Propagate error
  dat$RingDiff_BP1_error[i] <- sqrt((diff_10_error^2) / 1^2)
  dat$RingDiff_BP2_error[i] <- sqrt((diff_21_error^2 + diff_20_error^2) / 2^2)
  dat$RingDiff_BP3_error[i] <- sqrt((diff_32_error^2 + diff_31_error^2 + diff_30_error^2) / 3^2)
  dat$RingDiff_all_error[i] <- sqrt((diff_32_error^2 + diff_31_error^2 + diff_30_error^2 + diff_32_error^2 + diff_31_error^2 + diff_10_error^2) / 6^2)
}

##### Ring Enrichment from d2H values #####  
# calculate the difference in magnitude of d2H_BP vs d2HBP0
dat$RingEnrichBP1 <- dat$BP1_d2H_mean - dat$BP0_d2H_mean
dat$RingEnrichBP2 <- dat$BP2_d2H_mean - dat$BP0_d2H_mean
dat$RingEnrichBP3 <- dat$BP3_d2H_mean - dat$BP0_d2H_mean

# #### Treatment means ####
# # Group by experiment type, treatment, and experiment replicate
# # Calculate the mean and error of d2H_wt_mean and EpsLW_wt_mean
# # Calculate the sd among replicates and the propagated error, use whichever value is larger
# 
# treatment_means <- dat %>%
#   filter(!is.na(EpsLW_wt_mean)) %>%  # Filter out rows with missing values
#   # Group by experiment type and treatment
#   group_by(ExpType2, Treatment) %>%
#   # Calculate the mean and error across replicates
#   summarise(d2H_wt_mean_all = mean(d2H_wt_mean, na.rm = TRUE),
#             d2H_wt_mean_error = sqrt(sum(d2H_wt_mean_sd^2, na.rm = TRUE)),
#             d2H_wt_mean_stdev = sd(d2H_wt_mean, na.rm = TRUE), 
#             EpsLW_wt_mean_all = mean(EpsLW_wt_mean, na.rm = TRUE),
#             EpsLW_wt_mean_error = sqrt(sum(Wt_mean_Eps_sd^2, na.rm = TRUE)),
#             EpsLW_wt_mean_stdev = sd(EpsLW_wt_mean, na.rm = TRUE), 
#             n = sum(!is.na(EpsLW_wt_mean))) %>%
#   # Calculate the maximum between wt_mean_error and wt_mean_stdev for d2H and eps
#   # NAs are interpretted as 0s
#   mutate(d2H_wt_mean_error_all = pmax(ifelse(is.na(d2H_wt_mean_error), -Inf, d2H_wt_mean_error),
#                                       ifelse(is.na(d2H_wt_mean_stdev), -Inf, d2H_wt_mean_stdev)),
#          EpsLW_wt_mean_error_all = pmax(ifelse(is.na(EpsLW_wt_mean_error), -Inf, EpsLW_wt_mean_error),
#                                         ifelse(is.na(EpsLW_wt_mean_stdev), -Inf, EpsLW_wt_mean_stdev))) %>%
#   # drop columns, only keep error_all columns
#   # select(-c(d2H_wt_mean_error, d2H_wt_mean_stdev, EpsLW_wt_mean_error, EpsLW_wt_mean_stdev)) %>%
#   arrange(ExpType2, Treatment) %>%
#   ungroup()


#### Create subset dataframes for each experiment ####
enviro <- subset(dat, dat$ExpType == "Enviro")
Temp <- subset(enviro, enviro$ExpType2 == "Temp")
pH <- subset(enviro, enviro$ExpType2 == "pH")
RPM <- subset(enviro, enviro$ExpType2 == "RPM")
DO <- subset(enviro, enviro$ExpType2 == "DO")
Flux <- subset(enviro, enviro$ExpType2 == "Flux")
Water <- subset(dat, dat$ExpType2 == "Water")

##### unique symbology for experiments #####
Temp$Col_exp <- "red"
pH$Col_exp <- "darkgoldenrod1"
RPM$Col_exp <- "blueviolet"
DO$Col_exp <- "cyan2"
Flux$Col_exp <- "deeppink2"
Water$Col_exp <- "blue"

Temp$pch_exp <- 21
pH$pch_exp = 22
RPM$pch_exp <- 23
DO$pch_exp <- 24
Flux$pch_exp <- 25
Water$pch_exp <- 21

# merge datasets 
enviro_merge <- Reduce(function(x, y) merge(x, y, all = TRUE), 
                       list(Temp, 
                            RPM, 
                            pH, 
                            DO,
                            Flux))
enviro_merge <- enviro_merge %>%
  arrange(Treatment, Replicate)

all_merge <- Reduce(function(x, y) merge(x, y, all = TRUE), 
                      list(enviro_merge,
                        Water))


#### Save as .RDS files ####
# save as RDS files
saveRDS(all_merge, file = "00_Experiment Dataframes/all_merge.RDS")
saveRDS(enviro_merge, file = "00_Experiment Dataframes/enviro_merge.RDS")
saveRDS(Temp, file = "00_Experiment Dataframes/temp.RDS")
saveRDS(pH, file = "00_Experiment Dataframes/pH.RDS")
saveRDS(RPM, file = "00_Experiment Dataframes/RPM.RDS")
saveRDS(DO, file = "00_Experiment Dataframes/DO.RDS" )
saveRDS(Flux, file = "00_Experiment Dataframes/flux.RDS")
saveRDS(Water, file = "00_Experiment Dataframes/water.RDS")
