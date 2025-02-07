cat("\014") #clears console
rm(list=ls()) 
graphics.off()

#### load relevant packages ####
library("readxl")
library("dplyr")
library("tidyverse")
library("reshape")
library("gt")

##### data input ####
enviro <- readRDS("00_Experiment Dataframes/all_merge.rds")

#### data cleanup ####
# Remove all rows that don't have any BP or d2H data
enviro_iso <- enviro[complete.cases(enviro$n_analyses), ]

#### Experimental conditions & growth summary table ####
# Columns to summarize
columns_to_summarize <- c("GrowthRate", 
                          "DoublingTime", 
                          "SacrificeOD",
                          "Water_d2H")
# Create summary table
enviro_summary_growth <- enviro %>%
  # set order of experiments and treatments
  mutate(
    ExpType2 = factor(ExpType2, 
                      levels = c("Temp", "pH", "RPM", "DO", "Flux", "Water")), 
    Treatment = factor(Treatment, 
                       levels = c("Temp_65", "Temp_70", "Temp_75", "Temp_80",
                                  "pH_2", "pH_3", "pH_4",
                                  "RPM_50", "RPM_125", "RPM_300",
                                  "DO_0.2","DO_0.5", "DO_2", "DO_20", 
                                  "TD_7", "TD_21", "TD_44",
                                  "Water_-400", "Water_-50", "Water_+400"))
  ) %>%
  group_by(ExpType2, Treatment) %>%
  summarize(Temperature = first(Temp),
            pH = first(pH),
            RPM = first(RPM),
            DO = first(DO),
            Substrate = first(Energy_Source),
            N = n(),
            across(all_of(columns_to_summarize), list(Mean = ~mean(., na.rm = TRUE))),
            across(all_of(columns_to_summarize), list(SD = ~sd(., na.rm = TRUE)))) %>%
  # sort by experiment and treatment
  arrange(ExpType2, Treatment) %>%
  ungroup()

# replace NaN with NA
for (col in names(enviro_summary_growth)) {
  enviro_summary_growth[[col]][is.nan(enviro_summary_growth[[col]])] <- NA
}

# rearrange order
enviro_summary_growth <- enviro_summary_growth %>%
  select(ExpType2,
         Treatment,
         Temperature,
         pH,
         RPM,
         DO,
         Water_d2H_Mean,
         Substrate,
         N,
         GrowthRate_Mean,
         GrowthRate_SD,
         DoublingTime_Mean,
         DoublingTime_SD,
         SacrificeOD_Mean,
         SacrificeOD_SD)

# make dataframe
enviro_summary_growth <- as.data.frame(enviro_summary_growth)

#### Enviro growth conditions gt ####
enviro_growth_gt <- enviro_summary_growth %>%
  gt() %>%
  # tab_row_group(ExpType2) %>%
  tab_stubhead("Experiment") %>%
  tab_header(title = "Table X",
             subtitle = "Growth conditions for environmental control experiments.") %>%
  tab_spanner(label = "Experiment", columns = c(ExpType2, Treatment)) %>%
  tab_spanner(label = "Growth Conditions", columns = c(Temperature, 
                                                       pH,
                                                       RPM,
                                                       DO,
                                                       Water_d2H_Mean)) %>%
  tab_spanner(label = "Growth Media", columns = c(Substrate)) %>%
  tab_spanner(label = html("Growth Rate (Hour<sup>-1</sup>)"), columns = starts_with("GrowthRate")) %>%
  tab_spanner(label = html("T<sub>D</sub> (Hours)"), columns = starts_with("Doubling")) %>%
  tab_spanner(label = "Max OD", columns = starts_with("Sacrifice")) %>%
  tab_footnote(footnote = "Note: Results from T, pH, RPM, and pO2 experiments originally published in Cobban et al., 2020.") %>%
  cols_label(
    ExpType2 = "Experiment",
    Treatment = "Treatment",
    Temperature = "T (°C)",
    pH = "pH",
    RPM = "Shaking (RPM)",
    DO = html("pO2 (%)"),
    Water_d2H_Mean = html("δ<sup>2</sup>H<sub>W-1</sub> (‰)"), 
    Substrate = "Substrate",
    N = "N",
    GrowthRate_Mean = "Mean",
    GrowthRate_SD = "sd",
    DoublingTime_Mean = "Mean",
    DoublingTime_SD = "sd",
    SacrificeOD_Mean = "Mean",
    SacrificeOD_SD = "sd"
  ) %>%
  fmt_number(
    columns = c(GrowthRate_Mean,
                GrowthRate_SD,
                DoublingTime_Mean,
                DoublingTime_SD,
                SacrificeOD_Mean,
                SacrificeOD_SD),
    decimals = 2
  ) %>%
  fmt_missing(columns = everything(),
              missing_text = "") 

# Print the table
enviro_growth_gt

# Save the table to a CSV file
write.csv(enviro_summary_growth, "01_SummaryTables/Results_enviro_growth.csv", row.names = FALSE)
gtsave(enviro_growth_gt, file = "01_SummaryTables/Results_enviro_growth_gt.html")


#### BP/ iso summary table by experiment & treatment ####
# group by experiment and treatment
# add if/else for error calcs
# if n.iso > 1, calculate sd of exp replicates
# if n.iso = 1, report sd of the single replicate
# for d2H and Eps values, calculate sd of exp replicates and sd propagated
# error, keep whichever value is larger
# normalizes mean BP Rel Abun so they sum to 1
enviro_summary_iso <- enviro %>%
  # set order of experiments and treatments
  mutate(
    ExpType2 = factor(ExpType2, 
                      levels = c("Temp", "pH", "RPM", "DO", "Flux", "Water")), 
    Treatment = factor(Treatment, 
                       levels = c("Temp_65", "Temp_70", "Temp_75", "Temp_80",
                                  "pH_2", "pH_3", "pH_4",
                                  "RPM_50", "RPM_125", "RPM_300",
                                  "DO_0.2","DO_0.5", "DO_2", "DO_20", 
                                  "TD_7", "TD_21", "TD_44",
                                  "Water_-400", "Water_-50", "Water_+400"))
  ) %>%
  group_by(ExpType2, Treatment) %>%
  # calculate mean and sd for biological reps, calculate propogated error
  summarise(n = sum(!is.na(DoublingTime)),
            n.iso = sum(!is.na(EpsLW_wt_mean)),
            Water_d2H_mean = mean(Water_d2H, na.rm = TRUE),
            Water_d2H_error = sqrt(sum(Water_d2H_error^2, na.rm = TRUE)),
            Water_d2H_sd = sd(Water_d2H, na.rm = TRUE),
            BP0_RAb = mean(BP0_relAbun_mean, na.rm = TRUE),
            BP0_RAb_sd = ifelse(n.iso > 1, sd(BP0_relAbun_mean, na.rm = TRUE), mean(BP0_relAbun_error, na.rm = TRUE)),
            BP0_d2H = mean(BP0_d2H_mean, na.rm = TRUE),
            BP0_d2H_sd = ifelse(n.iso > 1, sd(BP0_d2H_mean, na.rm = TRUE), mean(BP0_d2H_error, na.rm = TRUE)),
            BP0_Eps = mean(BP0_eps_LW_mean, na.rm = TRUE),
            BP0_Eps_sd = ifelse(n.iso > 1, sd(BP0_eps_LW_mean, na.rm = TRUE), mean(BP0_eps_LW_error, na.rm = TRUE)),
            BP1_RAb = mean(BP1_relAbun_mean, na.rm = TRUE),
            BP1_RAb_sd = ifelse(n.iso > 1, sd(BP1_relAbun_mean, na.rm = TRUE), mean(BP1_relAbun_error, na.rm = TRUE)),
            BP1_d2H = mean(BP1_d2H_mean, na.rm = TRUE),
            BP1_d2H_sd = ifelse(n.iso > 1, sd(BP1_d2H_mean, na.rm = TRUE), mean(BP1_d2H_error, na.rm = TRUE)),
            BP1_Eps = mean(BP1_eps_LW_mean, na.rm = TRUE),
            BP1_Eps_sd = ifelse(n.iso > 1, sd(BP1_eps_LW_mean, na.rm = TRUE), mean(BP1_eps_LW_error, na.rm = TRUE)),
            BP1_RingDiff = mean(RingDiff_BP1, na.rm = TRUE),
            BP1_RingDiff_sd = ifelse(n.iso > 1, sd(RingDiff_BP1_error, na.rm = TRUE), mean(RingDiff_BP1_error, na.rm = TRUE)), 
            BP2_RAb = mean(BP2_relAbun_mean, na.rm = TRUE),
            BP2_RAb_sd = ifelse(n.iso > 1, sd(BP2_relAbun_mean, na.rm = TRUE), mean(BP2_relAbun_error, na.rm = TRUE)),
            BP2_d2H = mean(BP2_d2H_mean, na.rm = TRUE),
            BP2_d2H_sd = ifelse(n.iso > 1, sd(BP2_d2H_mean, na.rm = TRUE), mean(BP2_d2H_error, na.rm = TRUE)),
            BP2_Eps = mean(BP2_eps_LW_mean, na.rm = TRUE),
            BP2_Eps_sd = ifelse(n.iso > 1, sd(BP2_eps_LW_mean, na.rm = TRUE), mean(BP2_eps_LW_error, na.rm = TRUE)),
            BP2_RingDiff = mean(RingDiff_BP2, na.rm = TRUE),
            BP2_RingDiff_sd = ifelse(n.iso > 1, sd(RingDiff_BP2_error, na.rm = TRUE), mean(RingDiff_BP2_error, na.rm = TRUE)),
            BP3_RAb = mean(BP3_relAbun_mean, na.rm = TRUE),
            BP3_RAb_sd = ifelse(n.iso > 1, sd(BP3_relAbun_mean, na.rm = TRUE), mean(BP3_relAbun_error, na.rm = TRUE)),
            BP3_d2H = mean(BP3_d2H_mean, na.rm = TRUE),
            BP3_d2H_sd = ifelse(n.iso > 1, sd(BP3_d2H_mean, na.rm = TRUE), mean(BP3_d2H_error, na.rm = TRUE)),
            BP3_Eps = mean(BP3_eps_LW_mean, na.rm = TRUE),
            BP3_Eps_sd = ifelse(n.iso > 1, sd(BP3_eps_LW_mean, na.rm = TRUE), mean(BP3_eps_LW_error, na.rm = TRUE)),
            BP3_RingDiff = mean(RingDiff_BP3, na.rm = TRUE),
            BP3_RingDiff_sd = ifelse(n.iso > 1, sd(RingDiff_BP3_error, na.rm = TRUE), mean(RingDiff_BP3_error, na.rm = TRUE)),
            Sum_BP = sum(coalesce(BP0_RAb, 0), coalesce(BP1_RAb, 0), coalesce(BP2_RAb, 0), coalesce(BP3_RAb, 0), na.rm = TRUE),
            RingIndex_BP = mean(RingIndex_BP, na.rm = TRUE),
            RingIndex_BP_error = sqrt(sum(RingIndex_BP_error^2, na.rm = TRUE)),
            RingIndex_BP_stdev = sd(RingIndex_BP_error, na.rm = TRUE),
            AllBP_d2H_wt_mean = mean(d2H_wt_mean, na.rm = TRUE),
            AllBP_d2H_wt_mean_error = sqrt(sum(d2H_wt_mean_sd^2, na.rm = TRUE)),
            AllBP_d2H_wt_mean_stdev = sd(d2H_wt_mean, na.rm = TRUE), 
            AllBP_EpsLW_wt_mean = mean(EpsLW_wt_mean, na.rm = TRUE),
            AllBP_EpsLW_wt_mean_error = sqrt(sum(Wt_mean_Eps_sd^2, na.rm = TRUE)),
            AllBP_EpsLW_wt_mean_stdev = sd(EpsLW_wt_mean, na.rm = TRUE),
            AllBP_RingDiff_mean = mean(RingDiff_all, na.rm = TRUE),
            AllBP_RingDiff_error = sqrt(sum(RingDiff_all_error^2, na.rm = TRUE)),
            AllBP_RingDiff_sd = sd(RingDiff_all, na.rm = TRUE)) %>%
  # keeo larger error term (sd or prop. error) for RI-BP, water_d2H, BP_d2H, BP_Eps, and Ring Diff 
  mutate(RingIndex_BP_error = pmax(ifelse(is.na(RingIndex_BP_error), -Inf, RingIndex_BP_error),
                                   ifelse(is.na(RingIndex_BP_stdev), -Inf, RingIndex_BP_stdev)),
         Water_d2H_error = pmax(ifelse(is.na(Water_d2H_error), -Inf, Water_d2H_error),
                                   ifelse(is.na(Water_d2H_sd), -Inf, Water_d2H_sd)),
         AllBP_RingDiff_error = pmax(ifelse(is.na(AllBP_RingDiff_error), -Inf, AllBP_RingDiff_error),
                                ifelse(is.na(AllBP_RingDiff_sd), -Inf, AllBP_RingDiff_sd)),
         AllBP_d2H_wt_error = pmax(ifelse(is.na(AllBP_d2H_wt_mean_error), -Inf, AllBP_d2H_wt_mean_error),
                                   ifelse(is.na(AllBP_d2H_wt_mean_stdev), -Inf, AllBP_d2H_wt_mean_stdev)),
         AllBP_EpsLW_wt_error = pmax(ifelse(is.na(AllBP_EpsLW_wt_mean_error), -Inf, AllBP_EpsLW_wt_mean_error),
                                     ifelse(is.na(AllBP_EpsLW_wt_mean_stdev), -Inf, AllBP_EpsLW_wt_mean_stdev))) %>%
  mutate(RingIndex_BP_error = ifelse(n.iso == 0, NA, RingIndex_BP_error),
         AllBP_RingDiff_error = ifelse(n.iso == 0, NA, AllBP_RingDiff_error),
         AllBP_d2H_wt_error = ifelse(n.iso == 0, NA, AllBP_d2H_wt_mean_error),
         AllBP_EpsLW_wt_error = ifelse(n.iso == 0, NA, AllBP_EpsLW_wt_mean_error)) %>%
  mutate(BP0_RAb_norm = (BP0_RAb /Sum_BP),
         BP1_RAb_norm = (BP1_RAb /Sum_BP),
         BP2_RAb_norm = (BP2_RAb /Sum_BP),
         BP3_RAb_norm = (BP3_RAb /Sum_BP)) %>%
  # # remove extraneous columns
  select(-c(BP0_RAb, BP1_RAb, BP2_RAb, BP3_RAb, RingIndex_BP_stdev, AllBP_RingDiff_sd, AllBP_d2H_wt_mean_error, AllBP_d2H_wt_mean_stdev, AllBP_EpsLW_wt_mean_error, AllBP_EpsLW_wt_mean_stdev)) %>%
  # sort by experiment and treatment
  arrange(ExpType2, Treatment) %>%
  ungroup()

# replace NaN with nA
# enviro_summary_isodata[is.nan(enviro_summary_isodata)] <- NA
for (col in names(enviro_summary_iso)) {
  enviro_summary_iso[[col]][is.nan(enviro_summary_iso[[col]])] <- NA
}

# rearrange order
enviro_summary_iso <- enviro_summary_iso %>%
  select(ExpType2, 
         Treatment, 
         n, n.iso, 
         Water_d2H_mean, Water_d2H_error, 
         BP0_RAb_norm, BP0_RAb_sd, 
         BP0_d2H, BP0_d2H_sd, 
         BP0_Eps, BP0_Eps_sd, 
         BP1_RAb_norm, BP1_RAb_sd, 
         BP1_d2H, BP1_d2H_sd, 
         BP1_Eps, BP1_Eps_sd, 
         BP1_RingDiff, BP1_RingDiff_sd,
         BP2_RAb_norm, BP2_RAb_sd, 
         BP2_d2H, BP2_d2H_sd, 
         BP2_Eps, BP2_Eps_sd, 
         BP2_RingDiff, BP2_RingDiff_sd,
         BP3_RAb_norm, BP3_RAb_sd, 
         BP3_d2H, BP3_d2H_sd, 
         BP3_Eps, BP3_Eps_sd, 
         BP3_RingDiff, BP3_RingDiff_sd,
         RingIndex_BP, RingIndex_BP_error,
         AllBP_d2H_wt_mean, AllBP_d2H_wt_error, 
         AllBP_EpsLW_wt_mean, AllBP_EpsLW_wt_error, 
         AllBP_RingDiff_mean, AllBP_RingDiff_error)

# make dataframe
enviro_summary_iso <- as.data.frame(enviro_summary_iso)

#### Enviro results gt ####
enviro_results_gt <- enviro_summary_iso %>%
  gt() %>%
  # tab_row_group(ExpType2) %>%
  tab_header(title = "Table X",
             subtitle = "Data for individual biphytanes. Treatment mean and sd for is shown.") %>%
  tab_spanner(label = "Experiment", columns = c(ExpType2, Treatment)) %>%
  tab_spanner(label = html("T<sub>D</sub> (Hours)"), columns = starts_with("Doubling")) %>%
  tab_spanner(label = html("δ<sup>2</sup>H<sub>Water</sub> (‰)"), columns = starts_with("Water")) %>%
  tab_spanner(label = "BP-0", columns = starts_with("BP0")) %>%
  tab_spanner(label = "BP-1", columns = starts_with("BP1")) %>%
  tab_spanner(label = "BP-2", columns = starts_with("BP2")) %>%
  tab_spanner(label = "BP-3", columns = starts_with("BP3")) %>%
  tab_spanner(label = "Wt Mean BP", columns = starts_with("AllBP")) %>%
  tab_footnote(footnote = "N = number replicates in growth calculations, n = number replicates in biphytane and isotope calculations.") %>%
  cols_label(
    ExpType2 = "Experiment",
    n = "N",
    n.iso = "n",
    Water_d2H_mean = "mean",
    Water_d2H_error = "sd",
    BP0_RAb_norm = "Rel. Abun.",
    BP0_RAb_sd = "sd",
    BP0_d2H = html("δ<sup>2</sup>H<sub>BP-0</sub> (‰)"), 
    BP0_d2H_sd = "sd",
    BP0_Eps = html("&#178;&epsilon;<sub>L/W</sub> (‰)"),
    BP0_Eps_sd = "sd",
    BP1_RAb_norm = "Rel. Abun.",
    BP1_RAb_sd = "sd",
    BP1_d2H = html("δ<sup>2</sup>H<sub>BP-1</sub> (‰)"), 
    BP1_d2H_sd = "sd",
    BP1_Eps = html("&#178;&epsilon;<sub>L/W</sub> (‰)"),
    BP1_Eps_sd = "sd",
    BP1_RingDiff = "Δε/ring",
    BP1_RingDiff_sd = "sd",
    BP2_RAb_norm = "Rel. Abun.",
    BP2_RAb_sd = "sd",
    BP2_d2H = html("δ<sup>2</sup>H<sub>BP-2</sub> (‰)"), 
    BP2_d2H_sd = "sd",
    BP2_Eps = html("&#178;&epsilon;<sub>L/W</sub> (‰)"),
    BP2_Eps_sd = "sd",
    BP2_RingDiff = "Δε/ring",
    BP2_RingDiff_sd = "sd",
    BP3_RAb_norm = "Rel. Abun.",
    BP3_RAb_sd = "sd",
    BP3_d2H = html("δ<sup>2</sup>H<sub>BP-3</sub> (‰)"), 
    BP3_d2H_sd = "sd",
    BP3_Eps = html("&#178;&epsilon;<sub>L/W</sub> (‰)"),
    BP3_Eps_sd = "sd",
    BP3_RingDiff = "Δε/ring",
    BP3_RingDiff_sd = "sd",
    RingIndex_BP = "RI-BP",
    RingIndex_BP_error = "sd",
    AllBP_d2H_wt_mean = html("δ<sup>2</sup>H<sub>Wt.Mean</sub> (‰)"), 
    AllBP_d2H_wt_error = "sd",
    AllBP_EpsLW_wt_mean = html("&#178;&epsilon;<sub>L/W</sub> (‰)"),
    AllBP_EpsLW_wt_error = "sd",
    AllBP_RingDiff_mean = "Δε/ring",
    AllBP_RingDiff_error = "sd") %>%
  fmt_number(
    columns = everything(),
    decimals = 2
  ) %>%
  fmt_number(
    columns = vars(n, n.iso),
    decimals = 0
  ) %>%
  fmt_missing(columns = everything(),
              missing_text = "") 
  # text_transform(
  #   locations = cells_body(columns = Water_d2H_mean),
  #   fn = function(x) {
  #     mean_val <- x
  #     sd_val <- enviro_summary_iso$Water_d2H_sd[which(enviro_summary_iso$Water_d2H_mean == mean_val)]
  #     return(paste0(format(mean_val, digits = 2), " ± ", format(sd_val, digits = 2)))
  #   }
  # )

# Print the table
enviro_results_gt

# Save the table to a CSV file
write.csv(enviro_summary_iso, "01_SummaryTables/Results_enviro_BPiso.csv", row.names = FALSE)
gtsave(enviro_results_gt, file = "01_SummaryTables/Results_enviro_BPiso_gt.html")
