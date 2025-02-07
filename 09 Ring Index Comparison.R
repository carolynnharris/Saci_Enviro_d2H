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
enviro <- readRDS("00_Experiment Dataframes/enviro_merge.rds")

#### data cleaning ####
# take summary across treatments 
RI_dat <- enviro %>%
  group_by(ExpType2, Treatment) %>%
  summarise(
    RingIndex_BP_mean = mean(RingIndex_BP, na.rm = TRUE),
    RingIndex_BP_sd = sd(RingIndex_BP, na.rm = TRUE),
    RingIndex_GDGT_mean = mean(RingIndex_GDGT, na.rm = TRUE),
    RingIndex_GDGT_sd = sd(RingIndex_GDGT, na.rm = TRUE),
    pch_exp = first(pch_exp),
    Col_exp = first(Col_exp)
  ) %>%
  ungroup()

#### BP RING INDEX vs GDGT RING INDEX PLOTS ####
minRI.GDGT = 1
maxRI.GDGT = 5
minRI.BP = 0.5
maxRI.BP = 2.5

##### linear regression #####
enviroLM <- lm(RI_dat$RingIndex_BP_mean ~ RI_dat$RingIndex_GDGT_mean)
summary_lm <- summary(enviroLM) # r2 = 0.52, p = 0.001

##### summary panel #####
png("02_SummaryFigs/FigS2_RIBPvsRIGDGT.png",
    width = 5, height = 4, units = 'in', res = 300)
par(mfrow=c(1,1),
    mar=c(3,2,1,0),
    oma = c(0, 2, 1, 2),
    mgp = c(3, 0.4, 0)) 
plot(RI_dat$RingIndex_GDGT_mean, RI_dat$RingIndex_BP_mean,
     las = 1,
     cex = 1.2,
     type = "p",
     pch = RI_dat$pch_exp,
     col = RI_dat$Col_exp,
     bg = RI_dat$Col_exp,
     ylim = c(minRI.BP, maxRI.BP),
     xlim = c(minRI.GDGT, maxRI.GDGT),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)

# add error bars
treatments <- unique(RI_dat$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(RI_dat, RI_dat$Treatment == treatments[i])
  # y error bars
  arrows(x0 = temp2$RingIndex_GDGT_mean,
         y0= temp2$RingIndex_BP_mean - temp2$RingIndex_BP_sd,
         x1 = temp2$RingIndex_GDGT_mean,
         y1 = temp2$RingIndex_BP_mean + temp2$RingIndex_BP_sd,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 2,
         col = temp2$Col_exp)
  # x error bars
  arrows(y0 = temp2$RingIndex_BP_mean,
         x0= temp2$RingIndex_GDGT_mean - temp2$RingIndex_GDGT_sd,
         y1 = temp2$RingIndex_BP_mean,
         x1 = temp2$RingIndex_GDGT_mean + temp2$RingIndex_GDGT_sd,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 2,
         col = temp2$Col_exp)
}
axis(1, at = seq(0, 5, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)

axis(2, at = seq(0, 3, 0.5),
     las = 1,
     labels = T,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)

# make symbology table
symbology_enviro <- RI_dat %>%
  mutate(ExpType3 = factor(ExpType2, 
                           levels = c("Temp",
                                      "pH",
                                      "RPM",
                                      "DO",
                                      "Flux"))) %>%
  group_by(ExpType2) %>%
  slice(1) %>%
  ungroup()

custom_order <- c("Temp", "pH", "RPM", "DO", "Flux")
symbology_enviro <- symbology_enviro %>%
  mutate(ExpType2 = factor(ExpType2, levels = custom_order))
symbology_enviro <- symbology_enviro %>%
  arrange(ExpType2)


# add legend
legend("topleft", 
       bty = "n",
       legend = c("Temp",
                  "pH",
                  "Aeration",
                  expression(pO[2]), 
                  "e- donor"),
       col = symbology_enviro$Col_exp,
       pt.bg = symbology_enviro$Col_exp,
       pch = symbology_enviro$pch_exp, 
       cex = 0.8)
# Add the linear regression line
abline(enviroLM, 
       col = "black",
       lwd = 1.5)

# Add 95% confidence interval
RI_dat <- RI_dat %>%
  arrange(RingIndex_GDGT_mean)
ci <- predict(enviroLM, RI_dat, 
              interval = "confidence", 
              level = 0.95)
polygon(c(RI_dat$RingIndex_GDGT_mean, rev(RI_dat$RingIndex_GDGT_mean)), 
        c(ci[, "lwr"], rev(ci[, "upr"])), 
        col = rgb(0.7, 
                  0.7, 
                  0.7, 
                  alpha = 0.3), 
        border = NA)
legend("topright", 
       bty = "n",
       cex = 0.8,
       legend = c("Linear", "95% CI"), 
       col = c("black", "grey90"), 
       lty = c(1, 2), 
       lwd = c(2,7))

# Add R-squared value and p-value
p_value <- summary_lm$coefficients[2, 4]
formatted_p_value <- format(p_value, scientific = FALSE)
text(x = 3, 
     y = 2.45, 
     labels = paste("R² =", round(summary_lm$r.squared, 2)), 
     pos = 4,
     cex = 0.8)
text(x = 3, 
     y = 2.34, 
     labels = paste("p =", round(as.numeric(formatted_p_value), 3)), 
     pos = 4,
     cex = 0.8)
title(xlab = "Ring Index - GDGT", 
      line = 1.3, 
      cex.lab = 0.9,
      outer = F)
title(ylab = "Ring Index - BP",
      line = 0,
      cex.lab = 1,
      outer = T)
dev.off()

