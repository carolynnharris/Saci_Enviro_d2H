cat("\014") #clears console
rm(list=ls()) 
graphics.off()

#### load relevant packages ####
library("readxl")
library("tidyverse")
library("reshape2")
library("tidyr")


##### data input ####
# summary tables from paper
growth <- read.csv("01_SummaryTables/Results_enviro_growth.csv")
BPiso <- read.csv("01_SummaryTables/Results_enviro_BPiso.csv")
dat <- full_join(growth, 
                 BPiso, 
                 by = intersect(names(growth), names(BPiso)))
# ring diff data from other papers
other <- read_xlsx("00_DataInputs/04_Ring_Difference/RingEffectData_v01.xlsx",
                   sheet = "SummaryForR")

##### data manipulation #####
# subset for each enviro condition 
Temp <- dat %>%
  filter(str_detect(ExpType2, "Temp"))
pH <- dat %>%
  filter(str_detect(ExpType2, "pH"))
RPM <- dat %>%
  filter(str_detect(ExpType2, "RPM"))
DO <- dat %>%
  filter(str_detect(ExpType2, "DO"))
Flux <- dat %>%
  filter(str_detect(ExpType2, "Flux"))

##### unique symbology for enviro exps #####
Temp$Col_exp <- "red"
pH$Col_exp <- "darkgoldenrod1"
RPM$Col_exp <- "blueviolet"
DO$Col_exp <- "cyan2"
Flux$Col_exp <- "deeppink2"

Temp$pch_exp <- 21
pH$pch_exp = 22
RPM$pch_exp <- 23
DO$pch_exp <- 24
Flux$pch_exp <- 25

enviro <- rbind(Temp, pH, RPM, DO, Flux)




#### ring difference vs. BP ring number ####
# set BP0_RingDiff to 0
enviro$BP0_RingDiff <- 0
Temp$BP0_RingDiff <- 0
pH$BP0_RingDiff <- 0
RPM$BP0_RingDiff <- 0
DO$BP0_RingDiff <- 0
Flux$BP0_RingDiff <- 0

# Reshape the data to long format
enviro_RD <- subset(enviro, 
                  select = c("Treatment", 
                             "BP0_RingDiff", 
                             "BP1_RingDiff", 
                             "BP2_RingDiff", 
                             "BP3_RingDiff", 
                             "Col_exp", 
                             "pch_exp"))
enviro_long <- pivot_longer(enviro_RD, 
                            cols = ends_with("_RingDiff"),
                            names_to = "BP",
                            values_to = "RingDiff")
# Remove all non-numeric characters from the BP column
enviro_long$BP <- gsub("\\D", "", enviro_long$BP)

# do same process but for ring difference error estimates 
# set BP0_RingDiff_sd to 0
enviro$BP0_RingDiff_sd <- 0
Temp$BP0_RingDiff_sd <- 0
pH$BP0_RingDiff_sd <- 0
RPM$BP0_RingDiff_sd <- 0
DO$BP0_RingDiff_sd <- 0
Flux$BP0_RingDiff_sd <- 0
enviro_RD_error <- subset(enviro, 
                    select = c("Treatment", 
                               "BP0_RingDiff_sd", 
                               "BP1_RingDiff_sd", 
                               "BP2_RingDiff_sd", 
                               "BP3_RingDiff_sd"))
enviro_long_error <- pivot_longer(enviro_RD_error, 
                                  cols = ends_with("_RingDiff_sd"),
                                  names_to = "BP",
                                  values_to = "RD_Error")
# Remove all non-numeric characters from the BP column
enviro_long_error$BP <- gsub("\\D", "", enviro_long_error$BP)

# merge ring diff and error data frames together
enviro_merge <- full_join(enviro_long, 
                           enviro_long_error, 
                           by = c("Treatment", "BP"))
enviro_merge$BP <- as.numeric(enviro_merge$BP)

enviro_merge <- enviro_merge %>%
  group_by(Treatment, BP, Col_exp, pch_exp) %>%
  summarize(RingDiff = mean(RingDiff, na.rm = T,),
            RD_Error = mean(RD_Error, na.rm = T), .groups = 'drop')

# calculate LL and UL for each data point
enviro_merge$RingDiff_LL <- enviro_merge$RingDiff - enviro_merge$RD_Error
enviro_merge$RingDiff_UL <- enviro_merge$RingDiff + enviro_merge$RD_Error

# change NaN to NA
enviro_merge <- enviro_merge %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

# subset for indiv experiments
Temp_long <- enviro_merge %>%
  filter(str_detect(Treatment, "Temp"))

pH_long <- enviro_merge %>%
  filter(str_detect(Treatment, "pH"))

RPM_long <- enviro_merge %>%
  filter(str_detect(Treatment, "RPM"))

DO_long <- enviro_merge %>%
  filter(str_detect(Treatment, "DO"))

Flux_long <- enviro_merge %>%
  filter(str_detect(Treatment, "TD"))


##### assign unique symbology ####
pH_long <- pH_long %>%
  mutate(Col = case_when(
    Treatment == "pH_2" ~ "darkorange",
    Treatment == "pH_3" ~ "goldenrod1",
    Treatment == "pH_4" ~ "gold1",
    TRUE ~ "black"  # Default color 
  ),
  pch = case_when(
    Treatment == "pH_2" ~ 15,
    Treatment == "pH_3" ~ 16,
    Treatment == "pH_4" ~ 17,
    TRUE ~ 15  # Default pch 
  )
  )

Temp_long <- Temp_long %>%
  mutate(Col = case_when(
    Treatment == "Temp_65" ~ "black",
    Treatment == "Temp_70" ~ "red4",
    Treatment == "Temp_75" ~ "red3",
    Treatment == "Temp_80" ~ "red",
    TRUE ~ "black"  # Default color 
  ),
  pch = case_when(
    Treatment == "Temp_65" ~ 15,
    Treatment == "Temp_70" ~ 16,
    Treatment == "Temp_75" ~ 17,
    Treatment == "Temp_80" ~ 18,
    TRUE ~ 16  # Default pch 
  )
  )

RPM_long <- RPM_long %>%
  mutate(Col = case_when(
    Treatment == "RPM_50" ~ "darkorchid1",
    Treatment == "RPM_125" ~ "darkorchid4",
    Treatment == "RPM_300" ~ "black",
    TRUE ~ "black"  # Default color 
  ),
  pch = case_when(
    Treatment == "RPM_50" ~ 18,
    Treatment == "RPM_125" ~ 15,
    Treatment == "RPM_300" ~ 17,
    TRUE ~ 16  # Default pch 
  )
  )
DO_long <- DO_long %>%
  mutate(Col = case_when(
    Treatment == "DO_0.2" ~ "darkslategray1",
    Treatment == "DO_0.5" ~ "cyan",
    Treatment == "DO_2" ~ "cyan3",
    Treatment == "DO_20" ~ "cyan4",
    TRUE ~ "black"  # Default color 
  ),
  pch = case_when(
    Treatment == "DO_0.2" ~ 16,
    Treatment == "DO_0.5" ~ 15,
    Treatment == "DO_2" ~ 18,
    Treatment == "DO_20" ~ 16,
    TRUE ~ 16  # Default pch 
  )
  )


Flux_long <- Flux_long %>%
  mutate(Col = case_when(
    Treatment == "TD_7" ~ "deeppink",
    Treatment == "TD_21" ~ "deeppink3",
    Treatment == "TD_44" ~ "deeppink4",
    TRUE ~ "black"  # Default color 
  ),
  pch = case_when(
    Treatment == "TD_7" ~ 16,
    Treatment == "TD_21" ~ 17,
    Treatment == "TD_44" ~ 18,
    TRUE ~ 16  # Default pch 
  )
  )





###### do for "other" studies ####
other_RD <- subset(other, 
                    select = c("Organism", 
                               "Type",
                               "BP0_RingDiff", 
                               "BP1_RingDiff", 
                               "BP2_RingDiff", 
                               "BP3_RingDiff", 
                               "Col", 
                               "pch"))
other_long <- pivot_longer(other_RD, cols = ends_with("_RingDiff"), 
                            names_to = "BP", 
                            values_to = "RingDiff")
# Remove all non-numeric characters from the BP column
other_long$BP <- gsub("\\D", "", other_long$BP)


#### ENVIRO EXPERIMENTS - 2 x 3 panel ####  

png("02_SummaryFigs/Fig3_RingDiff_vs_BPRingNumber.png",
    width = 130, height = 80, units = 'mm', res = 300)
      # width = 170, height = 100, units = 'mm', res = 300)
par(mfrow=c(2, 3),
    mar=c(2.5,2,1.5,0),
    oma = c(1, 1, 0.5, 1),
    mgp = c(2, 0.5, 0)) 
  
  ##### Plot 1 Temp #### 
  maxRingDiff <- 40
  minRingDiff <- -40
  plot(Temp_long$BP, Temp_long$RingDiff,
       las = 1,
       cex = 1.5,
       type = "p",
       pch = Temp_long$pch,
       col = Temp_long$Col,
       bg = "white",
       ylim = c(minRingDiff, maxRingDiff),
       xlim = c(0, 3),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  abline(h = 0,
         lty = 2,
         lwd = 1)
  axis(1, at = seq(0, 3, 1),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)

  axis(2, at = seq(-40, 40, 20),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  mtext("A", 
        side = 1, 
        line = -1.5, 
        adj = 0.9,
        font = 2, 
        cex = 0.9)
  title(main = "Temperature", 
        line = 0.5, 
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)
  title(ylab = " Δε/ring (‰)",
        line = -0.4, 
        cex.lab = 1,
        outer = T)
  
  
  # add lines for each experiment
  treatments <- unique(Temp_long$Treatment)
  for (i in 1:length(treatments)){
    Temp2 <- Temp_long[Temp_long$Treatment == treatments[i], ]
    Temp2 <- Temp2[with(Temp2, order(BP)), ] # sort by elapsed time
    Temp2 <- Temp2[!is.na(Temp2$RingDiff),] # remove NAs
    lines(Temp2$BP, Temp2$RingDiff,
          col = Temp2$Col,
          lwd = 1.5)
    arrows(x0 = Temp2$BP,
           y0= Temp2$RingDiff_LL,
           x1 = Temp2$BP,
           y1 = Temp2$RingDiff_UL,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 1.5,
           col = Temp2$Col)
    # add points for each experiment (so points are on top of lines)
    points(Temp_long$BP, Temp_long$RingDiff,
           col = Temp_long$Col,
           pch = Temp_long$pch,
           cex = 1.5)
  }


  # symbology for legend
  legend("topright",
         xpd = NA, # plots in outer margin area
         cex = 0.7,
         # inset = c(-0.85, 0),
         bty = "n",
         legend = c("65°C", "70°C", "75°C", "80°C"),
         text.col = "black",
         col = c("black", "red4", "red3", "red"),
         pt.cex = 1.2,
         pch = c(15, 16, 17, 18))
      
  
  
  ##### Plot 2 pH #### 
  maxRingDiff <- 40
  minRingDiff <- -40
  plot(pH_long$BP, pH_long$RingDiff,
       las = 1,
       cex = 1.5,
       type = "p",
       pch = pH_long$pch,
       col = pH_long$Col,
       bg = "white",
       ylim = c(minRingDiff, maxRingDiff),
       xlim = c(0, 3),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  abline(h = 0,
         lty = 2,
         lwd = 1)
  axis(1, at = seq(0, 3, 1),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  
  axis(2, at = seq(-40, 40, 20),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  mtext("B", 
        side = 1, 
        line = -1.5, 
        adj = 0.9,
        font = 2, 
        cex = 0.9)
  title(main = "pH", 
        line = 0.5, 
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)

  # add lines for each experiment
  treatments <- unique(pH_long$Treatment)
  for (i in 1:length(treatments)){
    pH2 <- pH_long[pH_long$Treatment == treatments[i], ]
    pH2 <- pH2[with(pH2, order(BP)), ] # sort by elapsed time
    pH2 <- pH2[!is.na(pH2$RingDiff),] # remove NAs
    lines(pH2$BP, pH2$RingDiff,
          col = pH2$Col,
          lwd = 1.5)
    arrows(x0 = pH2$BP,
           y0= pH2$RingDiff_LL,
           x1 = pH2$BP,
           y1 = pH2$RingDiff_UL,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 1.5,
           col = pH2$Col)
    # add points for each experiment (so points are on top of lines)
    points(pH_long$BP, pH_long$RingDiff,
           col = pH_long$Col,
           pch = pH_long$pch,
           cex = 1.5)
  }
  

  legend("topleft",
         xpd = NA, # plots in outer margin area
         cex = 0.7,
         bty = "n",
         legend = c("pH 2", "pH 3", "pH 4"),
         text.col = "black",
         col = c("darkorange", "goldenrod1", "gold1"),
         pt.cex = 1.2,
         pch = c(15,16, 17))
  
  
  
  ##### Plot 3 RPM #### 
  maxRingDiff <- 40
  minRingDiff <- -40
  plot(RPM_long$BP, RPM_long$RingDiff,
       las = 1,
       cex = 1.5,
       type = "p",
       pch = RPM_long$pch,
       col = RPM_long$Col,
       bg = "white",
       ylim = c(minRingDiff, maxRingDiff),
       xlim = c(0, 3),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  abline(h = 0,
         lty = 2,
         lwd = 1)
  axis(1, at = seq(0, 3, 1),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  
  axis(2, at = seq(-40, 40, 20),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  mtext("C", 
        side = 1, 
        line = -1.5, 
        adj = 0.9,
        font = 2, 
        cex = 0.9)
  title(main = "Aeration Rate", 
        line = 0.5, 
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)

  # add lines for each experiment
  treatments <- unique(RPM_long$Treatment)
  for (i in 1:length(treatments)){
    RPM2 <- RPM_long[RPM_long$Treatment == treatments[i], ]
    RPM2 <- RPM2[with(RPM2, order(BP)), ] # sort by elapsed time
    RPM2 <- RPM2[!is.na(RPM2$RingDiff),] # remove NAs
    lines(RPM2$BP, RPM2$RingDiff,
          col = RPM2$Col,
          lwd = 1.5)
    arrows(x0 = RPM2$BP,
           y0= RPM2$RingDiff_LL,
           x1 = RPM2$BP,
           y1 = RPM2$RingDiff_UL,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 1.5,
           col = RPM2$Col)
    # add points for each experiment (so points are on top of lines)
    points(RPM_long$BP, RPM_long$RingDiff,
           col = RPM_long$Col,
           pch = RPM_long$pch,
           cex = 1.5)
  }
  
  legend("topleft",
         xpd = NA, # plots in outer margin area
         cex = 0.65,
         bty = "n",
         legend = c("RPM 50", "RPM 125", "RPM 300"),
         text.col = "black",
         col = c("darkorchid1", "darkorchid4", "black"),
         pt.cex = 1.2,
         pch = c(18, 15, 17))
  
  
  
  ##### Plot 4 DO Sparge Gas #### 
  maxRingDiff <- 40
  minRingDiff <- -40
  plot(DO_long$BP, DO_long$RingDiff,
       las = 1,
       cex = 1.5,
       type = "p",
       pch = DO_long$pch,
       col = DO_long$Col,
       bg = "white",
       ylim = c(minRingDiff, maxRingDiff),
       xlim = c(0, 3),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  abline(h = 0,
         lty = 2,
         lwd = 1)
  axis(1, at = seq(0, 3, 1),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  
  axis(2, at = seq(-40, 40, 20),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  mtext("D", 
        side = 1, 
        line = -1.5, 
        adj = 0.9,
        font = 2, 
        cex = 0.9)
  title(main = expression("pO"[2]), 
        line = 0.7, 
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)

  # add lines for each experiment
  treatments <- unique(DO_long$Treatment)
  for (i in 1:length(treatments)){
    DO2 <- DO_long[DO_long$Treatment == treatments[i], ]
    DO2 <- DO2[with(DO2, order(BP)), ] # sort by elapsed time
    DO2 <- DO2[!is.na(DO2$RingDiff),] # remove NAs
    lines(DO2$BP, DO2$RingDiff,
          col = DO2$Col,
          lwd = 1.5)
    arrows(x0 = DO2$BP,
           y0= DO2$RingDiff_LL,
           x1 = DO2$BP,
           y1 = DO2$RingDiff_UL,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 1.5,
           col = DO2$Col)
    # add points for each experiment (so points are on top of lines)
    points(DO_long$BP, DO_long$RingDiff,
           col = DO_long$Col,
           pch = DO_long$pch,
           cex = 1.5)
  }
  
  legend("topleft",
         cex = 0.7,
         bty = "n",
         legend = c("0.2%" , "0.5%", "2%", "20%"),
         text.col = "black",
         col = c("darkslategray1", "cyan", "cyan3", "cyan4"),
         pt.cex = 1.2,
         pch = c(16, 15,18, 16))
  
  
  ##### Plot 5 Flux  #### 
  maxRingDiff <- 40
  minRingDiff <- -40
  plot(Flux_long$BP, Flux_long$RingDiff,
       las = 1,
       cex = 1.5,
       type = "p",
       pch = Flux_long$pch,
       col = Flux_long$Col,
       bg = "white",
       ylim = c(minRingDiff, maxRingDiff),
       xlim = c(0, 3),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  abline(h = 0,
         lty = 2,
         lwd = 1)
  axis(1, at = seq(0, 3, 1),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  
  axis(2, at = seq(-40, 40, 20),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  mtext("E", 
        side = 1, 
        line = -1.5, 
        adj = 0.9,
        font = 2, 
        cex = 0.9)
  title(main = "e- donor flux", 
        line = 0.5, 
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)
  title(xlab = "BP Ring Number",
        line = 1.3,
        cex.lab = 1,
        outer = F)
  
  # add lines for each experiment
  treatments <- unique(Flux_long$Treatment)
  for (i in 1:length(treatments)){
    Flux2 <- Flux_long[Flux_long$Treatment == treatments[i], ]
    Flux2 <- Flux2[with(Flux2, order(BP)), ] # sort by elapsed time
    Flux2 <- Flux2[!is.na(Flux2$RingDiff),] # remove NAs
    lines(Flux2$BP, Flux2$RingDiff,
          col = Flux2$Col,
          lwd = 1.5)
    arrows(x0 = Flux2$BP,
           y0= Flux2$RingDiff_LL,
           x1 = Flux2$BP,
           y1 = Flux2$RingDiff_UL,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 1.5,
           col = Flux2$Col)
    # add points for each experiment (so points are on top of lines)
    points(Flux_long$BP, Flux_long$RingDiff,
           col = Flux_long$Col,
           pch = Flux_long$pch,
           cex = 1.5)
  }
  
  
  
  
  legend("topleft",
         cex = 0.7,
         bty = "n",
         legend = c("7 hr",  "21 hr" , "44 hr"),
         col = c("deeppink", "deeppink3", "deeppink4"),
         pt.cex = 1.2,
         pch = c(16, 17, 18))
  
  ##### Plot 6 - other studies ####
  maxRingDiff <- 40
  minRingDiff <- -40
  plot(other_long$RingDiff ~ other_long$BP,
       las = 1,
       cex = 1.5,
       type = "p",
       pch = other_long$pch,
       col = other_long$Col,
       bg = "white",
       ylim = c(minRingDiff, maxRingDiff),
       xlim = c(0, 3),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  abline(h = 0,
         lty = 2,
         lwd = 1)
  # add lines for each experiment
  treatments <- unique(other_long$Type)
  for (i in 1:length(treatments)){
    other2 <- other_long[other_long$Type == treatments[i], ]
    other2 <- other2[with(other2, order(BP)), ] # sort by elapsed time
    other2 <- other2[!is.na(other2$RingDiff),] # remove NAs
    lines(other2$BP, other2$RingDiff,
          col = other2$Col,
          lwd = 1.5)
  }
  axis(1, at = seq(0, 3, 1),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  
  axis(2, at = seq(-40, 40, 20),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  mtext("F", 
        side = 1, 
        line = -1.5, 
        adj = 0.9,
        font = 2, 
        cex = 0.9)
  title(main = "Other studies", 
        line = 0.5, 
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)
  legend("bottomleft",
         bty = "n",
         cex = 0.7,
         legend = c("Sulfolobus spp.", 
                    "N. mari - 31 hr",
                    "N. mari - 46 hr",
                    "N. mari - 93 hr"),
         col = c("red", "cyan", "cyan3", "cyan4"),
         pt.lwd = c(2, 2, 2, 2),
         pch = c(16,  18, 18, 18),
         pt.cex = c(0.9,  1, 1, 1))
  
  dev.off()
  