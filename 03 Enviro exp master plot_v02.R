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

##### set up for BP profiles ####
BP_summary <- dat %>%
  ungroup() %>% 
  group_by(ExpType2, Treatment) %>%
  summarize(
    BP0 = mean(BP0_RAb_norm, na.rm = TRUE),
    BP1 = mean(BP1_RAb_norm, na.rm = TRUE),
    BP2 = mean(BP2_RAb_norm, na.rm = TRUE),
    BP3 = mean(BP3_RAb_norm, na.rm = TRUE),
  )

# replace NaN with NA
BP_summary$BP0[is.nan(BP_summary$BP0)] <- NA
BP_summary$BP1[is.nan(BP_summary$BP1)] <- NA
BP_summary$BP2[is.nan(BP_summary$BP2)] <- NA
BP_summary$BP3[is.nan(BP_summary$BP3)] <- NA

# subset for indiv experiments 
Temp_BP <- subset(BP_summary, BP_summary$ExpType2 == "Temp")
pH_BP <- subset(BP_summary, BP_summary$ExpType2 == "pH")
RPM_BP <- subset(BP_summary, BP_summary$ExpType2 == "RPM")
DO_BP <- subset(BP_summary, BP_summary$ExpType2 == "DO")
Flux_BP <- subset(BP_summary, BP_summary$ExpType2 == "Flux")

# convert into matrices
# temp 
Temp.matrix <- Temp_BP %>%
  select(Treatment, BP0, BP1, BP2, BP3) %>%
  ungroup() %>%
  arrange(Treatment) %>%
  select(-ExpType2) %>%
  column_to_rownames(var = "Treatment") %>%
  t()

# pH
pH.matrix <- pH_BP %>%
  select(Treatment, BP0, BP1, BP2, BP3) %>%
  ungroup() %>%
  arrange(Treatment) %>%
  select(-ExpType2) %>%
  column_to_rownames(var = "Treatment") %>%
  t()

# RPM
RPM_BP <- RPM_BP %>%
  filter(!Treatment %in% c("RPM_0", "RPM_61", "RPM_75", "RPM_97", "RPM_200"))

custom_order_RPM <- c("RPM_50", 
                      "RPM_125",
                      "RPM_300")  
RPM.matrix <- RPM_BP %>%
  select(Treatment, BP0, BP1, BP2, BP3) %>%
  ungroup() %>%
  mutate(Treatment = factor(Treatment, levels = custom_order_RPM)) %>%
  select(-ExpType2) %>%
  column_to_rownames(var = "Treatment") %>%
  t()
RPM.matrix <- RPM.matrix[, custom_order_RPM]

# DO
DO.matrix <- DO_BP %>%
  filter(!Treatment %in% c("DO_0.22", "DO_1")) %>%
  select(Treatment, BP0, BP1, BP2, BP3) %>%
  ungroup() %>%
  arrange(Treatment) %>%
  select(-ExpType2) %>%
  column_to_rownames(var = "Treatment") %>%
  t()

# Flux
custom_order_Flux <- c("TD_7", 
                       "TD_21",
                       "TD_44")  
Flux.matrix <- Flux_BP %>%
  select(Treatment, BP0, BP1, BP2, BP3) %>%
  ungroup() %>%
  arrange(Treatment) %>%
  select(-ExpType2) %>%
  column_to_rownames(var = "Treatment") %>%
  t()
Flux.matrix <- Flux.matrix[, custom_order_Flux]

# set color palette 
BP_palette <- c("blue","cyan3", "deeppink2", "goldenrod1")


#### Doubling Time  #####
##### linear regs #####
lm_temp <- lm(DoublingTime_Mean ~ Temperature, data = Temp)
summary(lm_temp) # r2 = 0.94, p = 0.02

lm_pH <- lm(DoublingTime_Mean ~ pH, data = pH)
summary(lm_pH) # r2 = 0.43, p = 0.54

lm_pH_quad <- lm(DoublingTime_Mean ~ pH + I(pH^2), data = pH)
summary(lm_pH_quad) # no parameter estimates bc the model is overfit
# we have only 3 data points and 3 parameters to estimate, leaving no df left for residuals

lm_RPM <- lm(DoublingTime_Mean ~ RPM, data = RPM)
summary(lm_RPM)  # r2 = 0.51, p = 0.49
lm_DO <- lm(DoublingTime_Mean ~ log(DO), data = DO)
summary(lm_DO)  # r2 = 0.76, p = 0.13
lm_Flux <- lm(DoublingTime_Mean*1.001 ~ DoublingTime_Mean, data = Flux)
summary(lm_Flux) # r2 = 1, essentially perfect fit

png("02_SummaryFigs/03_Fig2_SaciResultsBigPanel.png",
    width = 200, height = 130, units = 'mm', res = 300)  
par(mfrow = c(4, 5),   # 4 rows and 5 columns of plots
    mar = c(1, 2.5, 1, 0), 
    oma = c(3, 1, 2, 3), 
    mgp = c(2, 0.4, 0)) 

##### Plot 1 Temp #### 
plot(Temp$Temp, Temp$DoublingTime_Mean,
     las = 1,
     cex = 1.5,     
     lwd = 1.5,
     type = "p",
     pch = Temp$pch,
     col = Temp$Col_exp,
     bg = "white",
     ylim = c(0, 10),
     xlim = c(63, 82),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_temp,
       col = Temp$Col_exp,
       lwd = 1.5)
axis(1, at = seq(60, 80, 5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(0, 10, 5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)

# add titles
mtext("A", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(main = "Temperature", 
      line = 0, 
      cex.main = 1.3,
      outer = T,
      adj = 0.08,
      font.main = 1)
title(ylab = expression("T"[D]~"(hours)"),
      line = 1.5, 
      cex.lab = 1,
      outer = F)


# add error bars for each experiment
treatments <- unique(Temp$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(Temp, Temp$Treatment == treatments[i])

  lines(temp2$Temp, temp2$DoublingTime_Mean,
        col = temp2$Col,
        lwd = 1.5)
  arrows(x0 = temp2$Temp,
         y0= temp2$DoublingTime_Mean - temp2$DoublingTime_SD,
         x1 = temp2$Temp,
         y1 = temp2$DoublingTime_Mean + temp2$DoublingTime_SD,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(Temp$Temp, Temp$DoublingTime_Mean,
     pch = Temp$pch_exp,
     col = Temp$Col_exp,
     bg = "white",
     cex = 1.5,
     lwd = 1.5)

##### Plot 2 pH #### 
plot(pH$pH, pH$DoublingTime_Mean,
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = pH$pch,
     col = pH$Col_exp,
     bg = "white",
     ylim = c(0, 10),
     xlim = c(1.5, 4.5),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
# abline(lm_pH,
#        col = pH$Col_exp,
#        lwd = 1.5)
axis(1, at = seq(2, 4, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(0, 10, 5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("B", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(main = "pH", 
      line = 0, 
      cex.main = 1.3,
      outer = T,
      adj = 0.32,
      font.main = 1)

# add error bars for each experiment
treatments <- unique(pH$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(pH, Treatment == treatments[i])
  
  arrows(x0 = temp2$pH,
         y0= temp2$DoublingTime_Mean - temp2$DoublingTime_SD,
         x1 = temp2$pH,
         y1 = temp2$DoublingTime_Mean + temp2$DoublingTime_SD,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(pH$pH, pH$DoublingTime_Mean,
       pch = pH$pch_exp,
       col = pH$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)


##### Plot 3 RPM #### 
plot(RPM$RPM, RPM$DoublingTime_Mean,
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = RPM$pch,
     col = RPM$Col_exp,
     bg = "white",
     ylim = c(0, 50),
     xlim = c(0, 320),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_RPM,
       col = RPM$Col_exp,
       lwd = 1.5)
axis(1, at = c(seq(0,300,100)),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(0, 50, 25),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("C", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(main = "Aeration Rate", 
      line = 0, 
      cex.main = 1.3,
      outer = T,
      adj = 0.53,
      font.main = 1)

# add error bars for each experiment
treatments <- unique(RPM$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(RPM, Treatment == treatments[i])
  
  arrows(x0 = temp2$RPM,
         y0= temp2$DoublingTime_Mean - temp2$DoublingTime_SD,
         x1 = temp2$RPM,
         y1 = temp2$DoublingTime_Mean + temp2$DoublingTime_SD,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(RPM$RPM, RPM$DoublingTime_Mean,
       pch = RPM$pch_exp,
       col = RPM$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)


##### Plot 4 Dissolved O2% sparge #### 
plot(DO$DO, DO$DoublingTime_Mean,
     log = "x",
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = DO$pch,
     col = DO$Col_exp,
     bg = "white",
     ylim = c(0, 50),
     xlim = c(0.1, 22),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_DO,
       col = DO$Col_exp,
       lwd = 1.5)

axis(1, at = c(0.2,  0.5, 2,  20),
     las = 1,
     labels = c("0.2",  "0.5",  "2", "20"),
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(0, 50, 25),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("D", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(main = expression("pO"[2]), 
      line = 0, 
      cex.main = 1.3,
      outer = T,
      adj = 0.73,
      font.main = 1)

# add error bars for each experiment
treatments <- unique(DO$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(DO, Treatment == treatments[i])
  
  arrows(x0 = temp2$DO,
         y0= temp2$DoublingTime_Mean - temp2$DoublingTime_SD,
         x1 = temp2$DO,
         y1 = temp2$DoublingTime_Mean + temp2$DoublingTime_SD,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(DO$DO, DO$DoublingTime_Mean,
       pch = DO$pch_exp,
       col = DO$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)

##### Plot 5 Flux #### 
plot(Flux$DoublingTime_Mean, Flux$DoublingTime_Mean,
     # log = "y",
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = Flux$pch,
     col = Flux$Col_exp,
     bg = "white",
     ylim = c(0, 50),
     xlim = c(0, 55),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_Flux,
       col = Flux$Col_exp,
       lwd = 1.5)
axis(1, at = c(seq(0,50,25)),
     las = 1,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(0, 50, 25),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("E", 
      side = 1, 
      line = -1.5, 
      adj = 0.9,
      font = 2, 
      cex = 0.9)
title(main = "e- donor flux", 
      line = 0, 
      cex.main = 1.3,
      outer = T,
      adj = 0.98,
      font.main = 1)

# add error bars for each experiment
treatments <- unique(Flux$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(Flux, Treatment == treatments[i])
  
  arrows(x0 = temp2$DoublingTime_Mean,
         y0= temp2$DoublingTime_Mean - temp2$DoublingTime_SD,
         x1 = temp2$DoublingTime_Mean,
         y1 = temp2$DoublingTime_Mean + temp2$DoublingTime_SD,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(Flux$DoublingTime_Mean, Flux$DoublingTime_Mean,
       pch = Flux$pch_exp,
       col = Flux$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)

#### BP Relative Abundance Plots ####
##### Plot 1 Temp #### 
barplot(Temp.matrix, 
        col = BP_palette,
        border="white",
        space=0.08,
        las = 1,
        yaxt = "n",
        xaxt = "n",
        cex.lab = 0.75,
        xlab="")
axis(1, 
     at = c(0.5, 1.6, 2.8, 4),
     las = 1,
     labels = c("65", "70", "75", "80"),
     tck = -0.035,
     cex.axis = 0.75)
axis(2, at = seq(0, 1, .5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 1, .25),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
mtext("F", 
      side = 1, 
      line = -6, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(ylab = "Relative Abundance", 
      line = 1.5, 
      cex.lab = 1,
      outer = F)

##### Plot 2 pH #### 
barplot(pH.matrix, 
        col = BP_palette ,
        border="white",
        space=0.08,
        las = 1,
        yaxt = "n",
        xaxt = "n",
        cex.lab = 0.75,
        xlab="")
axis(1, 
     at = c(0.5, 1.6, 2.8),
     las = 1,
     labels = c("2", "3", "4"),
     tck = -0.035,
     cex.axis = 0.75)
axis(2, at = seq(0, 1, .5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 1, .25),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
mtext("G", 
      side = 1, 
      line = -6, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)

##### Plot 3 RPM #### 
barplot(RPM.matrix, 
        col = BP_palette ,
        border="white",
        space=0.08,
        las = 1,
        yaxt = "n",
        xaxt = "n",
        cex.lab = 0.75,
        xlab="")
axis(1, 
     at = c(0.5, 1.6, 2.8, 4),
     las = 1,
     labels = c("50", "125", "200", "300"),
     tck = -0.035,
     cex.axis = 0.75)
axis(2, at = seq(0, 1, .5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 1, .25),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
mtext("H", 
      side = 1, 
      line = -6, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)

##### Plot 4 DO #### 
barplot(DO.matrix, 
        col = BP_palette ,
        border="white",
        space=0.08,
        las = 1,
        yaxt = "n",
        xaxt = "n",
        cex.lab = 0.75,
        xlab="")
axis(1, 
     at = c(0.5, 1.6, 2.8, 4),
     las = 1,
     labels = c("0.2", "0.5", "2", "20"),
     tck = -0.035,
     cex.axis = 0.75)

axis(2, at = seq(0, 1, .5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 1, .25),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
mtext("I", 
      side = 1, 
      line = -6, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)

##### Plot 5 Flux #### 
barplot(Flux.matrix, 
        col = BP_palette ,
        border="white",
        space=0.08,
        las = 1,
        yaxt = "n",
        xaxt = "n",
        cex.lab = 0.75,
        xlab="")
axis(1, 
     at = c(0.5, 1.6, 2.8),
     las = 1,
     labels = c("7", "21", "44"),
     tck = -0.035,
     cex.axis = 0.75)
axis(2, at = seq(0, 1, .5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 1, .25),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
mtext("J", 
      side = 1, 
      line = -6, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)

legend("topright",
       xpd = NA, # plots in outer margins
       cex = 0.8,
       inset = c(-0.35, 0),
       bty = "n",
       legend = c("BP-3", "BP-2", "BP-1", "BP-0"),
       col = rev(BP_palette),
       pt.cex = 1.5,
       pch = c(15, 15, 15, 15))

#### BP Ring Index  #####
##### linear regs #####
# lm_temp <- lm(RingIndex_BP ~ Temperature, data = Temp)
# summary(lm_temp) #r2 = 0.48, p = 0.3 
# lm_pH <- lm(RingIndex_BP ~ pH, data = pH)
# summary(lm_pH) # r2 = 0.83, p = 0.27
# lm_RPM <- lm(RingIndex_BP ~ RPM, data = RPM)
# summary(lm_RPM)  # r2 = 0.51, p = 0.49
# lm_DO <- lm(RingIndex_BP ~ log(DO), data = DO)
# summary(lm_DO)  # r2 = 0.30, p = 0.46
lm_Flux <- lm(RingIndex_BP ~ DoublingTime_Mean, data = Flux)
summary(lm_Flux) # r2 = 99, p = 0.04

##### Plot 1 Temp #### 
plot(Temp$Temp, Temp$RingIndex_BP,
     las = 1,
     cex = 1.5,     
     lwd = 1.5,
     type = "p",
     pch = Temp$pch,
     col = Temp$Col_exp,
     bg = "white",
     ylim = c(0.5, 2.5),
     xlim = c(63, 82),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_temp,
       col = Temp$Col_exp,
       lwd = 1.5)
axis(1, at = seq(60, 80, 5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(0.5, 2.5, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 10, 1),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("K", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(ylab = "Ring Index - BP",
      line = 1.6, 
      cex.lab = 1,
      outer = F)


# add error bars for each experiment
treatments <- unique(Temp$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(Temp, Temp$Treatment == treatments[i])
  
  lines(temp2$Temp, temp2$RingIndex_BP,
        col = temp2$Col,
        lwd = 1.5)
  arrows(x0 = temp2$Temp,
         y0= temp2$RingIndex_BP - temp2$RingIndex_BP_error,
         x1 = temp2$Temp,
         y1 = temp2$RingIndex_BP + temp2$RingIndex_BP_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(Temp$Temp, Temp$RingIndex_BP,
       pch = Temp$pch_exp,
       col = Temp$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)

##### Plot 2 pH #### 
plot(pH$pH, pH$RingIndex_BP,
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = pH$pch,
     col = pH$Col_exp,
     bg = "white",
     ylim = c(0.5, 2.5),
     xlim = c(1.5, 4.5),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_pH,
       col = pH$Col_exp,
       lwd = 1.5)
axis(1, at = seq(2, 4, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(0.5, 2.5, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 10, 1),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("L", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)

# add error bars for each experiment
treatments <- unique(pH$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(pH, Treatment == treatments[i])
  
  arrows(x0 = temp2$pH,
         y0= temp2$RingIndex_BP - temp2$RingIndex_BP_error,
         x1 = temp2$pH,
         y1 = temp2$RingIndex_BP + temp2$RingIndex_BP_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(pH$pH, pH$RingIndex_BP,
       pch = pH$pch_exp,
       col = pH$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)


##### Plot 3 RPM #### 
plot(RPM$RPM, RPM$RingIndex_BP,
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = RPM$pch,
     col = RPM$Col_exp,
     bg = "white",
     ylim = c(0.5, 2.5),
     xlim = c(0, 320),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_RPM,
       col = RPM$Col_exp,
       lwd = 1.5)
axis(1, at = c(seq(0,300,100)),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(0.5, 2.5, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 50, 5),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("M", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)

# add error bars for each experiment
treatments <- unique(RPM$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(RPM, Treatment == treatments[i])
  
  arrows(x0 = temp2$RPM,
         y0= temp2$RingIndex_BP - temp2$RingIndex_BP_error,
         x1 = temp2$RPM,
         y1 = temp2$RingIndex_BP + temp2$RingIndex_BP_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(RPM$RPM, RPM$RingIndex_BP,
       pch = RPM$pch_exp,
       col = RPM$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)


##### Plot 4 Dissolved O2% sparge #### 
plot(DO$DO, DO$RingIndex_BP,
     log = "x",
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = DO$pch,
     col = DO$Col_exp,
     bg = "white",
     ylim = c(0.5, 2.5),
     xlim = c(0.1, 22),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_DO,
       col = DO$Col_exp,
       lwd = 1.5)

axis(1, at = c(0.2,  0.5, 2,  20),
     las = 1,
     labels = c("0.2",  "0.5",  "2", "20"),
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(0.5, 2.5, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0.5, 2.5, 1),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("N", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)

# add error bars for each experiment
treatments <- unique(DO$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(DO, Treatment == treatments[i])
  
  arrows(x0 = temp2$DO,
         y0= temp2$RingIndex_BP - temp2$RingIndex_BP_error,
         x1 = temp2$DO,
         y1 = temp2$RingIndex_BP + temp2$RingIndex_BP_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(DO$DO, DO$RingIndex_BP,
       pch = DO$pch_exp,
       col = DO$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)


##### Plot 5 Flux #### 
plot(Flux$DoublingTime_Mean, Flux$RingIndex_BP,
     # log = "y",
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = Flux$pch,
     col = Flux$Col_exp,
     bg = "white",
     ylim = c(0.5, 2.5),
     xlim = c(0, 55),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_Flux,
       col = Flux$Col_exp,
       lwd = 1.5)
axis(1, at = c(seq(0,50,25)),
     las = 1,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(0.5, 2.5, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 75, 5),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("O", 
      side = 1, 
      line = -1.5, 
      adj = 0.9,
      font = 2, 
      cex = 0.9)

# add error bars for each experiment
treatments <- unique(Flux$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(Flux, Treatment == treatments[i])
  
  arrows(x0 = temp2$DoublingTime_Mean,
         y0= temp2$RingIndex_BP - temp2$RingIndex_BP_error,
         x1 = temp2$DoublingTime_Mean,
         y1 = temp2$RingIndex_BP + temp2$RingIndex_BP_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(Flux$DoublingTime_Mean, Flux$RingIndex_BP,
       pch = Flux$pch_exp,
       col = Flux$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)
# dev.off()  


#### Wt Mean Eps  #####
##### linear regs #####
# lm_temp <- lm(AllBP_EpsLW_wt_mean ~ Temperature, data = Temp)
# summary(lm_temp) #r2 = 0.33, p = 0.4
# lm_pH <- lm(AllBP_EpsLW_wt_mean ~ pH, data = pH)
# summary(lm_pH) # r2 = 0.11, p = 0.78
# lm_RPM <- lm(AllBP_EpsLW_wt_mean ~ RPM, data = RPM)
# summary(lm_RPM)  # r2 = 0.89, p = 0.21
# lm_DO <- lm(AllBP_EpsLW_wt_mean ~ log(DO), data = DO)
# summary(lm_DO)  # r2 = 0.22, p = 0.53
# lm_Flux <- lm(AllBP_EpsLW_wt_mean ~ DoublingTime_Mean, data = Flux)
# summary(lm_Flux) # r2 = 86, p = 0.25

# png("01_SummaryFigs/Fig31_enviro_DoublingTime.png",
#     width = 170, height = 40, units = 'mm', res = 300)
# par(mfrow=c(1, 5),
#     mar=c(2.5,2,1.5,0),
#     oma = c(1, 1, 0.5, 1),
#     mgp = c(2, 0.5, 0)) 

##### Plot 1 Temp #### 
eps_min = -250
eps_max = -150
plot(Temp$Temp, Temp$AllBP_EpsLW_wt_mean,
     las = 1,
     cex = 1.5,     
     lwd = 1.5,
     type = "p",
     pch = Temp$pch,
     col = Temp$Col_exp,
     bg = "white",
     ylim = c(eps_min, eps_max),
     xlim = c(63, 82),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
# abline(lm_temp,
#        col = Temp$Col_exp,
#        lwd = 1.5)
axis(1, at = seq(60, 80, 5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(eps_min, eps_max, 50),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("P", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(xlab = "T (°C)", 
      line = 0.9, 
      cex.lab = 1,
      adj = 0.11,
      outer = T)
title(ylab = expression(paste("Wt Mean", phantom(0)^2, epsilon[L/W], " (‰)")),       
      line = -0.8, 
      adj = 0.02,
      cex.lab = 1,
      outer = T)


# add error bars for each experiment
treatments <- unique(Temp$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(Temp, Temp$Treatment == treatments[i])
  
  lines(temp2$Temp, temp2$AllBP_EpsLW_wt_mean,
        col = temp2$Col,
        lwd = 1.5)
  arrows(x0 = temp2$Temp,
         y0= temp2$AllBP_EpsLW_wt_mean - temp2$AllBP_EpsLW_wt_error,
         x1 = temp2$Temp,
         y1 = temp2$AllBP_EpsLW_wt_mean + temp2$AllBP_EpsLW_wt_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(Temp$Temp, Temp$AllBP_EpsLW_wt_mean,
       pch = Temp$pch_exp,
       col = Temp$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)

##### Plot 2 pH #### 
plot(pH$pH, pH$AllBP_EpsLW_wt_mean,
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = pH$pch,
     col = pH$Col_exp,
     bg = "white",
     ylim = c(eps_min, eps_max),
     xlim = c(1.5, 4.5),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_pH,
       col = pH$Col_exp,
       lwd = 1.5)
axis(1, at = seq(2, 4, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(eps_min, eps_max, 50),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 10, 1),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("Q", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(xlab = "pH", 
      line = 0.9, 
      cex.lab = 1,
      adj = 0.32,
      outer = T)

# add error bars for each experiment
treatments <- unique(pH$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(pH, Treatment == treatments[i])
  
  arrows(x0 = temp2$pH,
         y0= temp2$AllBP_EpsLW_wt_mean - temp2$AllBP_EpsLW_wt_error,
         x1 = temp2$pH,
         y1 = temp2$AllBP_EpsLW_wt_mean + temp2$AllBP_EpsLW_wt_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(pH$pH, pH$AllBP_EpsLW_wt_mean,
       pch = pH$pch_exp,
       col = pH$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)


##### Plot 3 RPM #### 
plot(RPM$RPM, RPM$AllBP_EpsLW_wt_mean,
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = RPM$pch,
     col = RPM$Col_exp,
     bg = "white",
     ylim = c(eps_min, eps_max),
     xlim = c(0, 320),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_RPM,
       col = RPM$Col_exp,
       lwd = 1.5)
axis(1, at = c(seq(0,300,100)),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(eps_min, eps_max, 50),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 50, 5),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("R", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)

title(xlab = "Shaking (RPM)", 
      line = 0.9, 
      cex.lab = 1,
      adj = 0.52,
      outer = T)

# add error bars for each experiment
treatments <- unique(RPM$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(RPM, Treatment == treatments[i])
  
  arrows(x0 = temp2$RPM,
         y0= temp2$AllBP_EpsLW_wt_mean - temp2$AllBP_EpsLW_wt_error,
         x1 = temp2$RPM,
         y1 = temp2$AllBP_EpsLW_wt_mean + temp2$AllBP_EpsLW_wt_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(RPM$RPM, RPM$AllBP_EpsLW_wt_mean,
       pch = RPM$pch_exp,
       col = RPM$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)


##### Plot 4 Dissolved O2% sparge #### 
plot(DO$DO, DO$AllBP_EpsLW_wt_mean,
     log = "x",
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = DO$pch,
     col = DO$Col_exp,
     bg = "white",
     ylim = c(eps_min, eps_max),
     xlim = c(0.1, 22),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_DO,
       col = DO$Col_exp,
       lwd = 1.5)

axis(1, at = c(0.2,  0.5, 2,  20),
     las = 1,
     labels = c("0.2",  "0.5",  "2", "20"),
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(eps_min, eps_max, 50),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0.5, 2.5, 1),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("S", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(xlab = expression("log %O"[2]), 
      line = 0.9, 
      cex.lab = 1,
      adj = 0.74,
      outer = T)


# add error bars for each experiment
treatments <- unique(DO$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(DO, Treatment == treatments[i])
  
  arrows(x0 = temp2$DO,
         y0= temp2$AllBP_EpsLW_wt_mean - temp2$AllBP_EpsLW_wt_error,
         x1 = temp2$DO,
         y1 = temp2$AllBP_EpsLW_wt_mean + temp2$AllBP_EpsLW_wt_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(DO$DO, DO$AllBP_EpsLW_wt_mean,
       pch = DO$pch_exp,
       col = DO$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)


##### Plot 5 Flux #### 
plot(Flux$DoublingTime_Mean, Flux$AllBP_EpsLW_wt_mean,
     # log = "y",
     las = 1,
     cex = 1.5,        
     lwd = 1.5,
     type = "p",
     pch = Flux$pch,
     col = Flux$Col_exp,
     # col = "black",
     bg = "white",
     ylim = c(eps_min, eps_max),
     xlim = c(0, 55),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
abline(lm_Flux,
       col = Flux$Col_exp,
       lwd = 1.5)
axis(1, at = c(seq(0,50,25)),
     las = 1,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(eps_min, eps_max, 50),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 75, 5),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
# add titles
mtext("T", 
      side = 1, 
      line = -1.5, 
      adj = 0.9,
      font = 2, 
      cex = 0.9)
title(xlab = expression("T"[D]~"(hours)"), 
      line = 0.9, 
      cex.lab = 1,
      adj = 0.95,
      outer = T)



# add error bars for each experiment
treatments <- unique(Flux$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(Flux, Treatment == treatments[i])
  
  arrows(x0 = temp2$DoublingTime_Mean,
         y0= temp2$AllBP_EpsLW_wt_mean - temp2$AllBP_EpsLW_wt_error,
         x1 = temp2$DoublingTime_Mean,
         y1 = temp2$AllBP_EpsLW_wt_mean + temp2$AllBP_EpsLW_wt_error,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col_exp)
}
points(Flux$DoublingTime_Mean, Flux$AllBP_EpsLW_wt_mean,
       pch = Flux$pch_exp,
       col = Flux$Col_exp,
       bg = "white",
       cex = 1.5,
       lwd = 1.5)
dev.off()  
