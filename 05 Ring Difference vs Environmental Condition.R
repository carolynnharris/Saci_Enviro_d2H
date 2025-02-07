cat("\014") #clears console
rm(list=ls()) 
graphics.off()

#### load relevant packages ####
library("readxl")
library("tidyverse")
library("reshape")
library("RColorBrewer")
library("colorspace")

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

enviro <- rbind(Temp, pH, RPM, DO, Flux)


#### Ring Difference - All BPs - 2 x 3 panel  ####
minRD = -10
maxRD = 20

png("02_SummaryFigs/FigS4_MeanRingDiff_vs_EnviroCondition.png",
    width = 170, height = 40, units = 'mm', res = 300)
par(mfrow=c(1, 5),
    mar=c(2.5,2,1.5,0),
    oma = c(1, 1, 0.5, 1),
    mgp = c(2, 0.3, 0)) 

##### Plot 1 Temp ##### 
plot(Temp$Temp, Temp$AllBP_RingDiff_mean,
     las = 1,
     cex = 1.5,
     type = "p",
     pch = Temp$pch_exp,
     col = Temp$Col_exp,
     lwd = 1.5,
     bg = "white",
     ylim = c(minRD, maxRD),
     xlim = c(63, 82),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
axis(1, at = seq(60, 80, 5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(minRD, maxRD, 10),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
mtext("A", 
      side = 1, 
      line = -6, 
      adj = 0.9,
      font = 2, 
      cex = 0.9)
title(main = "Temperature", 
      line = 0.5, 
      cex.main = 0.9,
      outer = F,
      adj = 0.5,
      font.main = 1)
title(xlab = "T (°C)", 
      line = 1.3, 
      cex.lab = 0.9,
      outer = F)
title(ylab = "Mean Δε/ring (‰)",
      line = -0.4, 
      cex.lab = 1,
      outer = TRUE)

# add error bars for each experiment
treatments <- unique(Temp$Treatment)
for (i in 1:length(treatments)){
  temp2 <- Temp[Temp$Treatment == treatments[i], ]
  arrows(x0 = temp2$Temp,
         y0= temp2$AllBP_RingDiff_mean - temp2$AllBP_RingDiff_error,
         x1 = temp2$Temp,
         y1 = temp2$AllBP_RingDiff_mean + temp2$AllBP_RingDiff_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 2,
         col = temp2$Col_exp)
}
points(Temp$Temp, Temp$AllBP_RingDiff_mean,
       cex = 1.5,
       lwd = 1.5,
       bg = "white",
       pch = Temp$pch_exp,
       col = Temp$Col_exp)

##### Plot 2 pH #####
plot(pH$pH, pH$AllBP_RingDiff_mean,
     las = 1,
     cex = 1.5,
     type = "p",
     pch = pH$pch,
     col = pH$Col_exp,
     lwd = 1.5,
     bg = "white",
     ylim = c(minRD, maxRD),
     xlim = c(1.5, 4.5),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
axis(1, at = seq(2, 4, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor X ticks
axis(2, at = seq(minRD, maxRD, 10),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
mtext("B", 
      side = 1, 
      line = -6, 
      adj = 0.9,
      font = 2, 
      cex = 0.9)
title(main = "pH", 
      line = 0.5, 
      cex.main = 0.9,
      outer = F,
      adj = 0.5,
      font.main = 1)
title(xlab = "pH", 
      line = 1.3, 
      cex.lab = 0.9,
      outer = F)

# add error bars for each experiment
treatments <- unique(pH$Treatment)
for (i in 1:length(treatments)){
  pH2 <- pH[pH$Treatment == treatments[i], ]
  arrows(x0 = pH2$pH,
         y0= pH2$AllBP_RingDiff_mean - pH2$AllBP_RingDiff_error,
         x1 = pH2$pH,
         y1 = pH2$AllBP_RingDiff_mean + pH2$AllBP_RingDiff_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 2,
         col = pH2$Col_exp)
}
points(pH$AllBP_RingDiff_mean ~ pH$pH,
     pch = pH$pch,
     col = pH$Col_exp,
     cex = 1.5,
     bg = "white",
     lwd = 1.5)

##### Plot 3 RPM ##### 
plot(RPM$RPM, RPM$AllBP_RingDiff_mean,
     # log = "y",
     las = 1,
     cex = 1.5,
     lwd = 1.5,
     type = "p",
     pch = RPM$pch,
     col = RPM$Col_exp,
     bg = "white",
     ylim = c(minRD, maxRD),
     xlim = c(0,  320),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
axis(1, at = c(seq(0,300,100)),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(minRD, maxRD, 10),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
mtext("C", 
      side = 1, 
      line = -6, 
      adj = 0.9,
      font = 2, 
      cex = 0.9)
title(main = "Aeration Rate", 
      line = 0.5, 
      cex.main = 0.9,
      outer = F,
      adj = 0.5,
      font.main = 1)
title(xlab = "RPM", 
      line = 1.3, 
      cex.lab = 0.9,
      outer = F)

# add error bars for each experiment
treatments <- unique(RPM$Treatment)
for (i in 1:length(treatments)){
  RPM2 <- RPM[RPM$Treatment == treatments[i], ]
  arrows(x0 = RPM2$RPM,
         y0= RPM2$AllBP_RingDiff_mean - RPM2$AllBP_RingDiff_error,
         x1 = RPM2$RPM,
         y1 = RPM2$AllBP_RingDiff_mean + RPM2$AllBP_RingDiff_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 2,
         col = RPM2$Col_exp)
}
points(RPM$AllBP_RingDiff_mean ~ RPM$RPM,
       pch = RPM$pch,
       col = RPM$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)


##### Plot 4 Dissolved O2% sparge ##### 
plot(DO$DO, DO$AllBP_RingDiff_mean,
     log = "x",
     las = 1,
     cex = 1.5,
     type = "p",
     pch = DO$pch,
     col = DO$Col_exp,
     lwd = 1.5,
     bg = "white",
     ylim = c(minRD, maxRD),
     xlim = c(0.1, 22),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
axis(1, at = c( 0.2, 0.5,    2,  20),
     las = 1,
     labels = c( "0.2",  "0.5", "2", "20"),
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(minRD, maxRD, 10),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
mtext("D", 
      side = 1, 
      line = -6, 
      adj = 0.9,
      font = 2, 
      cex = 0.9)

title(main = expression("pO"[2]), 
      line = 0.7, 
      cex.main = 0.9,
      outer = F,
      adj = 0.5,
      font.main = 1)
title(xlab = expression("log % O"[2]), 
      line = 1.3, 
      cex.lab = 0.9,
      outer = F)


# add error bars for each experiment
treatments <- unique(DO$Treatment)
for (i in 1:length(treatments)){
  DO2 <- DO[DO$Treatment == treatments[i], ]
  lines(DO2$DO, DO2$AllBP_RingDiff_mean,
        col = DO2$Col_exp,
        lwd = 1.5)
  arrows(x0 = DO2$DO,
         y0= DO2$AllBP_RingDiff_mean - DO2$AllBP_RingDiff_error,
         x1 = DO2$DO,
         y1 = DO2$AllBP_RingDiff_mean + DO2$AllBP_RingDiff_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 2,
         col = DO2$Col_exp)
}
points(DO$AllBP_RingDiff_mean ~ DO$DO,
       pch = DO$pch,
       col = DO$Col_exp,
       cex = 1.5,
       bg = "white",
       lwd = 1.5)

##### Plot 5 Flux ##### 
plot(Flux$DoublingTime_Mean, Flux$AllBP_RingDiff_mean,
     # log = "y",
     las = 1,
     cex = 1.5,
     lwd = 1.5,
     type = "p",
     pch = Flux$pch,
     col = Flux$Col_exp,
     bg = "white",
     ylim = c(minRD, maxRD),
     xlim = c(0, 60),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
axis(1, at = c(7, 21, 44),
     las = 1,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(minRD, maxRD, 10),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
mtext("E", 
      side = 1, 
      line = -6, 
      adj = 0.9,
      font = 2, 
      cex = 0.9)
title(main = "e- donor flux", 
      line = 0.5, 
      cex.main = 0.9,
      outer = F,
      adj = 0.5,
      font.main = 1)
title(xlab = expression("T"[D]~"(hours)"), 
      line = 1.3, 
      cex.lab = 0.9,
      outer = F)

# add error bars for each experiment
treatments <- unique(Flux$Treatment)
for (i in 1:length(treatments)){
  Flux2 <- Flux[Flux$Treatment == treatments[i], ]
  arrows(x0 = Flux2$DoublingTime_Mean,
         y0= Flux2$AllBP_RingDiff_mean - Flux2$AllBP_RingDiff_error,
         x1 = Flux2$DoublingTime_Mean,
         y1 = Flux2$AllBP_RingDiff_mean + Flux2$AllBP_RingDiff_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 2,
         col = Flux2$Col_exp)
}
points(Flux$AllBP_RingDiff_mean ~ Flux$DoublingTime_Mean,
       pch = Flux$pch,
       col = Flux$Col_exp,
       cex = 1.5,
       bg = "white",
       lwd = 1.5)

dev.off()



