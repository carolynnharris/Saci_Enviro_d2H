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
dat <- read.csv("00_DataInputs/03_GCFID_Chromatogram/FID01323_CH20_Chromatogram.csv")

##### data cleaning ####
colnames(dat) <- c("Time", "pA")

# subset for iGDGT RT area
subset <- dat[dat$Time > 19.7 & dat$Time < 21.9, ]


##### chromatogram plot ####
png("02_SummaryFigs/12_Fig1_SaciChromatogram_CH20.png",
    width = 5, height = 2.5, units = 'in', res = 300)
par(mfrow=c(1,1),
    mar=c(3,3,1,0),
    oma = c(0, 0, 1, 2),
    mgp = c(3, 0.4, 0)) 
plot(subset$pA ~ subset$Time,
     las = 1,
     cex = 1.2,
     type = "l",
     lwd = 1.5,
     col = "grey40",
     xlim = c(19.8, 21.6),
     ylim = c(3, 210),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
axis(1, at = seq(20, 21.5, 0.5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.7,
     line = 0)
axis(2, at = seq(0, 200, 50),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.7,
     line = 0)
title(xlab = "Retention Time (s)", 
      line = 1.3, 
      cex.lab = 0.8,
      outer = F)
title(ylab = "Intensity (mV)",      
      line = 1.7, 
      cex.lab = 0.9,
      outer = F)
# shade region for each BP
BP0 <- dat[dat$Time > 20 & dat$Time < 20.08, ]
polygon(c(BP0$Time, rev(BP0$Time)),
        c(BP0$pA, rep(0.4, length(BP0$pA))),
        col = rgb(col2rgb("blue")[1]/255,
                  col2rgb("blue")[2]/255,
                  col2rgb("blue")[3]/255,
                  alpha = 0.8),
        border = NA)

BP1 <- dat[dat$Time > 20.5 & dat$Time < 20.58, ]
polygon(c(BP1$Time, rev(BP1$Time)),
        c(BP1$pA, rep(0.4, length(BP1$pA))),
        col = rgb(col2rgb("cyan2")[1]/255,
                  col2rgb("cyan2")[2]/255,
                  col2rgb("cyan2")[3]/255,
                  alpha = 0.8),
        border = NA)

BP2 <- dat[dat$Time > 21.005 & dat$Time < 21.075, ]
polygon(c(BP2$Time, rev(BP2$Time)),
        c(BP2$pA, rep(0.4, length(BP2$pA))),
        col = rgb(col2rgb("deeppink")[1]/255,
                  col2rgb("deeppink")[2]/255,
                  col2rgb("deeppink")[3]/255,
                  alpha = 0.8),
        border = NA)

BP3 <- dat[dat$Time > 21.37 & dat$Time < 21.42, ]
polygon(c(BP3$Time, rev(BP3$Time)),
        c(BP3$pA, rep(0.4, length(BP3$pA))),
        col = rgb(col2rgb("goldenrod1")[1]/255,
                  col2rgb("goldenrod1")[2]/255,
                  col2rgb("goldenrod1")[3]/255,
                  alpha = 0.8),
        border = NA)

# add white space up to baseline
x_fill <- c(20, 21.5, 21.5, 20)
y_fill <- c(0, 0, 1.7, 1.7)
polygon(x_fill, 
        y_fill, 
        col = "white", 
        border = "white")
lines(subset$pA ~ subset$Time,
      col = "grey50",
      lwd = 1.5)
# label each peak
mtext("BP-0", 
      col = "blue",
      side = 1, 
      line = -5, 
      adj = 0.13,
      font = 2, 
      cex = 0.9)
mtext("BP-1", 
      col = "cyan2",
      side = 1, 
      line = -7.6, 
      adj = 0.42,
      font = 2, 
      cex = 0.9)
mtext("BP-2", 
      col = "deeppink",
      side = 1, 
      line = -7.4, 
      adj = 0.7,
      font = 2, 
      cex = 0.9)
mtext("BP-3", 
      col = "goldenrod1",
      side = 1, 
      line = -1.7, 
      adj = 0.89,
      font = 2, 
      cex = 0.9)
dev.off()