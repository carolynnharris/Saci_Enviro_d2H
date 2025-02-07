cat("\014") #clears console
rm(list=ls()) 
graphics.off()

#### load relevant packages ####
library("readxl")
library("tidyverse")
library("reshape")
library("RColorBrewer")
library("colorspace")
library("yarrr")
require("mosaic")
require("car")
require("effects")
library("multcomp")

##### data input ####
# summary tables from paper
growth <- read.csv("01_SummaryTables/Results_enviro_growth.csv")
BPiso <- read.csv("01_SummaryTables/Results_enviro_BPiso.csv")
dat <- full_join(growth, 
                 BPiso, 
                 by = intersect(names(growth), names(BPiso)))

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

####  PLOT OF T_D vs. Eps for each indiv experiment   - 2 x 3 ####
  minEps = -250
  maxEps = -150
  
  png("02_SummaryFigs/Fig5_WtMeanEps_vs_Td.png",
      width = 130, height = 80, units = 'mm', res = 300)
  par(mfrow=c(2, 3),
      mar=c(2.5,2,1.5,0),
      oma = c(1, 1, 0.5, 1),
      mgp = c(2, 0.3, 0)) 

  
  ##### Plot 1 Temp ##### 
  # lm_temp <- lm(AllBP_EpsLW_wt_mean ~ DoublingTime_Mean, data = Temp)
  # summary(lm_temp) #r2 = 0.53, p = 0.26
  plot(Temp$DoublingTime_Mean, Temp$AllBP_EpsLW_wt_mean,
       las = 1,
       cex = 1.5,
       type = "p",
       pch = Temp$pch_exp,
       col = Temp$Col_exp,
       lwd = 1.5,
       bg = "white",
       ylim = c(minEps, maxEps),
       xlim = c(0, 15),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  axis(1, at = seq(0, 20, 5),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  # add minor Y ticks
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = F,
       tck = -0.02,
       cex.axis = 0.75,
       line = 0)
  mtext("A", 
        side = 1, 
        line = -1.5, 
        adj = 0.1,
        font = 2, 
        cex = 0.9)
  title(main = "Temperature", 
        line = 0.4, 
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)
  title(ylab = expression(paste("Weighted Mean", phantom(0)^2, epsilon[L/W], " (‰)")),        
        line = -0.4, 
        cex.lab = 1,
        outer = TRUE)
  
  # add error bars for each experiment
  treatments <- unique(Temp$Treatment)
  for (i in 1:length(treatments)){
    temp2 <- Temp[Temp$Treatment == treatments[i], ]
    arrows(x0 = temp2$DoublingTime_Mean,
           y0= temp2$AllBP_EpsLW_wt_mean - temp2$AllBP_EpsLW_wt_error,
           x1 = temp2$DoublingTime_Mean,
           y1 = temp2$AllBP_EpsLW_wt_mean + temp2$AllBP_EpsLW_wt_error,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = temp2$Col_exp)
    arrows(y0 = temp2$AllBP_EpsLW_wt_mean,
           x0= temp2$DoublingTime_Mean - temp2$DoublingTime_SD,
           y1 = temp2$AllBP_EpsLW_wt_mean,
           x1 = temp2$DoublingTime_Mean + temp2$DoublingTime_SD,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = temp2$Col_exp)
  }
  points(Temp$DoublingTime_Mean, Temp$AllBP_EpsLW_wt_mean,
         cex = 1.5,
         lwd = 1.5,
         bg = "white",
         pch = Temp$pch_exp,
         col = Temp$Col_exp)
  
  ##### Plot 2 pH #####
  # lm_pH <- lm(AllBP_EpsLW_wt_mean ~ DoublingTime_Mean, data = pH)
  # summary(lm_pH) #r2 = 0.25, p = 0.67
  plot(pH$DoublingTime_Mean, pH$AllBP_EpsLW_wt_mean,
       las = 1,
       cex = 1.5,
       type = "p",
       pch = pH$pch,
       col = pH$Col_exp,
       lwd = 1.5,
       bg = "white",
       ylim = c(minEps, maxEps),
       xlim = c(0,15),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  axis(1, at = seq(0,15,5),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  # add minor Y ticks
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = F,
       tck = -0.02,
       cex.axis = 0.75,
       line = 0)
  mtext("B", 
        side = 1, 
        line = -1.5, 
        adj = 0.1,
        font = 2, 
        cex = 0.9)
  title(main = "pH", 
        line = 0.4, 
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)
  
  # add error bars for each experiment
  treatments <- unique(pH$Treatment)
  for (i in 1:length(treatments)){
    pH2 <- pH[pH$Treatment == treatments[i], ]
    arrows(x0 = pH2$DoublingTime_Mean,
           y0= pH2$AllBP_EpsLW_wt_mean - pH2$AllBP_EpsLW_wt_error,
           x1 = pH2$DoublingTime_Mean,
           y1 = pH2$AllBP_EpsLW_wt_mean + pH2$AllBP_EpsLW_wt_error,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = pH2$Col_exp)
    arrows(y0 = pH2$AllBP_EpsLW_wt_mean,
           x0= pH2$DoublingTime_Mean - pH2$DoublingTime_SD,
           y1 = pH2$AllBP_EpsLW_wt_mean,
           x1 = pH2$DoublingTime_Mean + pH2$DoublingTime_SD,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = pH2$Col_exp)
  }
  points(pH$AllBP_EpsLW_wt_mean ~ pH$DoublingTime_Mean,
         pch = pH$pch,
         col = pH$Col_exp,
         cex = 1.5,
         bg = "white",
         lwd = 1.5)
  
  ##### Plot 3 RPM ##### 
  lm_RPM <- lm(AllBP_EpsLW_wt_mean ~ DoublingTime_Mean, data = RPM)
  summary(lm_RPM) #r2 = 0.83, p = 0.28
  plot(RPM$DoublingTime_Mean, RPM$AllBP_EpsLW_wt_mean,
       las = 1,
       cex = 1.5,
       lwd = 1.5,
       type = "p",
       pch = RPM$pch,
       col = RPM$Col_exp,
       bg = "white",
       ylim = c(minEps, maxEps),
       xlim = c(0, 50),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  axis(1, at = c(seq(0,50,10)),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  # add minor Y ticks
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = F,
       tck = -0.02,
       cex.axis = 0.75,
       line = 0)
  mtext("C", 
        side = 1, 
        line = -1.5, 
        adj = 0.1,
        font = 2, 
        cex = 0.9)
  title(main = "Aeration Rate", 
        line = 0.4, 
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)
  # add error bars for each experiment
  treatments <- unique(RPM$Treatment)
  for (i in 1:length(treatments)){
    RPM2 <- RPM[RPM$Treatment == treatments[i], ]
    arrows(x0 = RPM2$DoublingTime_Mean,
           y0= RPM2$AllBP_EpsLW_wt_mean - RPM2$AllBP_EpsLW_wt_error,
           x1 = RPM2$DoublingTime_Mean,
           y1 = RPM2$AllBP_EpsLW_wt_mean + RPM2$AllBP_EpsLW_wt_error,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = RPM2$Col_exp)
    arrows(y0 = RPM2$AllBP_EpsLW_wt_mean,
           x0= RPM2$DoublingTime_Mean - RPM2$DoublingTime_SD,
           y1 = RPM2$AllBP_EpsLW_wt_mean,
           x1 = RPM2$DoublingTime_Mean + RPM2$DoublingTime_SD,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = RPM2$Col_exp)
  }
  points(RPM$AllBP_EpsLW_wt_mean ~ RPM$DoublingTime_Mean,
         pch = RPM$pch,
         col = RPM$Col_exp,
         bg = "white",
         cex = 1.5,
         lwd = 1.5)
  
  ##### Plot 4 Dissolved O2% sparge ##### 
  lm_DO <- lm(AllBP_EpsLW_wt_mean ~ DoublingTime_Mean, data = DO)
  summary(lm_DO) #r2 = 0.7, p = 0.16
  plot(DO$DoublingTime_Mean, DO$AllBP_EpsLW_wt_mean,
       las = 1,
       cex = 1.5,
       type = "p",
       pch = DO$pch,
       col = DO$Col_exp,
       lwd = 1.5,
       bg = "white",
       ylim = c(minEps, maxEps),
       xlim = c(0, 50),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  axis(1, at = c(seq(0,50,10)),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  # add minor Y ticks
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = F,
       tck = -0.02,
       cex.axis = 0.75,
       line = 0)
  mtext("D", 
        side = 1, 
        line = -1.5, 
        adj = 0.1,
        font = 2, 
        cex = 0.9)
  title(main = expression("pO"[2]), 
        line = 0.6, 
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)
  # add error bars for each experiment
  treatments <- unique(DO$Treatment)
  for (i in 1:length(treatments)){
    DO2 <- DO[DO$Treatment == treatments[i], ]
    arrows(x0 = DO2$DoublingTime_Mean,
           y0= DO2$AllBP_EpsLW_wt_mean - DO2$AllBP_EpsLW_wt_error,
           x1 = DO2$DoublingTime_Mean,
           y1 = DO2$AllBP_EpsLW_wt_mean + DO2$AllBP_EpsLW_wt_error,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = DO2$Col_exp)
    arrows(y0 = DO2$AllBP_EpsLW_wt_mean,
           x0= DO2$DoublingTime_Mean - DO2$DoublingTime_SD,
           y1 = DO2$AllBP_EpsLW_wt_mean,
           x1 = DO2$DoublingTime_Mean + DO2$DoublingTime_SD,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = DO2$Col_exp)
  }
  points(DO$AllBP_EpsLW_wt_mean ~ DO$DoublingTime_Mean,
         pch = DO$pch,
         col = DO$Col_exp,
         cex = 1.5,
         bg = "white",
         lwd = 1.5)
  
  
  
  ##### Plot 5 Flux ##### 
  lm_Flux <- lm(AllBP_EpsLW_wt_mean ~ DoublingTime_Mean, data = Flux)
  summary(lm_Flux) #r2 = 0.85, p = 0.24
  plot(Flux$DoublingTime_Mean, Flux$AllBP_EpsLW_wt_mean,
       las = 1,
       cex = 1.5,
       lwd = 1.5,
       type = "p",
       pch = Flux$pch,
       col = Flux$Col_exp,
       bg = "white",
       ylim = c(minEps, maxEps),
       xlim = c(0, 50),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  axis(1, at = c(seq(0,50,10)),
       las = 1,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  # add minor Y ticks
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = F,
       tck = -0.02,
       cex.axis = 0.75,
       line = 0)
  mtext("E", 
        side = 1, 
        line = -1.5, 
        adj = 0.9,
        font = 2, 
        cex = 0.9)
  title(main = "e- donor flux", 
        line = 0.4, 
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)
  title(xlab = expression("T"[D]~"(hours)"), 
        line = -0.5, 
        cex.lab = 1,
        outer = T)
  
  # add error bars for each experiment
  treatments <- unique(Flux$Treatment)
  for (i in 1:length(treatments)){
    Flux2 <- Flux[Flux$Treatment == treatments[i], ]
    arrows(x0 = Flux2$DoublingTime_Mean,
           y0= Flux2$AllBP_EpsLW_wt_mean - Flux2$AllBP_EpsLW_wt_error,
           x1 = Flux2$DoublingTime_Mean,
           y1 = Flux2$AllBP_EpsLW_wt_mean + Flux2$AllBP_EpsLW_wt_error,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = Flux2$Col_exp)
    arrows(y0 = Flux2$AllBP_EpsLW_wt_mean,
           x0= Flux2$DoublingTime_Mean - Flux2$DoublingTime_SD,
           y1 = Flux2$AllBP_EpsLW_wt_mean,
           x1 = Flux2$DoublingTime_Mean + Flux2$DoublingTime_SD,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = Flux2$Col_exp)
  }
  points(Flux$AllBP_EpsLW_wt_mean ~ Flux$DoublingTime_Mean,
         pch = Flux$pch,
         col = Flux$Col_exp,
         cex = 1.5,
         bg = "white",
         lwd = 1.5)
  
  ##### Plot 6 All data - doubling time #####
  enviro <- as.data.frame(rbind(Temp, pH, RPM, DO, Flux))
  
  # subset rows that includes Eps value, order by Doubling Time
  enviro_subset <- enviro[grepl("\\d", enviro$AllBP_EpsLW_wt_mean), ]
  enviro_subset <- enviro_subset[order(enviro_subset$DoublingTime_Mean), ]
  # linear regression
  lm <- lm(enviro_subset$AllBP_EpsLW_wt_mean ~ enviro_subset$DoublingTime_Mean)
  summary_lm <- summary(lm) # r2 = 0.0035, p = 0.82
  
  plot(enviro_subset$DoublingTime_Mean, enviro_subset$AllBP_EpsLW_wt_mean,
       las = 1,
       cex = 1.5,
       lwd = 1.5,
       type = "p",
       pch = enviro_subset$pch,
       col = enviro_subset$Col_exp,
       bg = "white",
       ylim = c(minEps, maxEps),
       xlim = c(0, 50),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       cex.axis = 0.75)
  axis(1, at = c(seq(0,50,10)),
       las = 1,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = T,
       tck = -0.035,
       cex.axis = 0.75,
       line = 0)
  # add minor Y ticks
  axis(2, at = seq(minEps, maxEps, 50),
       las = 1,
       labels = F,
       tck = -0.02,
       cex.axis = 0.75,
       line = 0)
  mtext("F", 
        side = 1, 
        line = -1.5, 
        adj = 0.9,
        font = 2, 
        cex = 0.9)
  title(main = "All Data",
        line = 0.4,
        cex.main = 0.9,
        outer = F,
        adj = 0.5,
        font.main = 1)
  
  # add error bars for each experiment
  treatments <- unique(enviro_subset$Treatment)
  for (i in 1:length(treatments)){
    enviro2 <- enviro_subset[enviro_subset$Treatment == treatments[i], ]
    arrows(x0 = enviro2$DoublingTime_Mean,
           y0= enviro2$AllBP_EpsLW_wt_mean - enviro2$AllBP_EpsLW_wt_error,
           x1 = enviro2$DoublingTime_Mean,
           y1 = enviro2$AllBP_EpsLW_wt_mean + enviro2$AllBP_EpsLW_wt_error,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = enviro2$Col_exp)
    arrows(y0 = enviro2$AllBP_EpsLW_wt_mean,
           x0= enviro2$DoublingTime_Mean - enviro2$DoublingTime_SD,
           y1 = enviro2$AllBP_EpsLW_wt_mean,
           x1 = enviro2$DoublingTime_Mean + enviro2$DoublingTime_SD,
           code = 1, # no cap of bars
           angle = 90,
           length = 0,
           lwd = 2,
           col = enviro2$Col_exp)
    points(enviro2$AllBP_EpsLW_wt_mean ~ enviro2$DoublingTime_Mean,
           pch = enviro2$pch,
           col = enviro2$Col_exp,
           cex = 1.5,
           bg = "white",
           lwd = 1.5)
  }
  # Add the reg line
  abline(lm, 
         col = "black",
         lwd = 1.5,
         lty = 1)
  # Add 95% confidence interval
  enviro_subset <- enviro_subset %>%
    arrange(DoublingTime_Mean)
  ci <- predict(lm, enviro_subset, interval = "confidence", level = 0.95)
  # Shade the region between the lower and upper bounds of the CI
  polygon(c(enviro_subset$DoublingTime_Mean, rev(enviro_subset$DoublingTime_Mean)), 
          c(ci[, "lwr"], rev(ci[, "upr"])), 
          col = rgb(0.7, 0.7, 0.7, alpha = 0.4), border = NA)
  legend("topright", 
         bty = "n",
         cex = 0.8,
         legend = c("Linear", "95% CI"), 
         col = c("black", "grey80"), 
         lty = c(1, 1), 
         lwd = c(2,7))
  
  dev.off()