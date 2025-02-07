cat("\014") #clears console
rm(list=ls()) 
graphics.off()

#### Dirgangi and Pagani 2013 ####
# Grew Haloarcula marismortui under varying T and salinity conditions
# measured d2H of archaeal, calculated Eps_L/W using archaeol
# used yeast extract + peptone as substrates for growth

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
dat <- read_excel("00_DataInputs/06_Dirghangi and Pagani 2013/Dirghangi and Pagani 2013_H maris data.xlsx")

##### subset for experiment #####
Temp <- subset(dat, dat$Salinity == 23)
Salinity = subset(dat, dat$Temp == 45)

#### analysis ####
##### linear regressioms #####
# enviro vs Td
T_Td <- lm(DoublingTime ~ Temp, data = Temp)
summary(T_Td) # R2 = 0.80, p = 0.30
Sal_Td <- lm(DoublingTime ~ Salinity, data = Salinity)
summary(Sal_Td) # R2 = 0.88, p = 0.24

# enviro vs. epsilon
T_Eps <- lm(Eps_LW ~ Temp, data = Temp)
summary(T_Eps) # R2 = 0.27, p = 0.65
Sal_Eps <- lm(Eps_LW ~ Salinity, data = Salinity)
summary(Sal_Eps) # R2 = 0.33, p = 0.61

# Td vs. epsilon
Td_Eps <- lm(Eps_LW ~ DoublingTime, data = dat)
summary(Td_Eps) # R2 = 0.07, p = 0.67

#### pairwise variables ####
columns_to_use <- c("Salinity", "Temp", "DoublingTime", "Eps_LW") 
dat_pairs <- dat[, columns_to_use]
pairs(dat_pairs,
      las = 1,
      pch = 16,
      cex = 1.2,
      lower.panel = NULL)

# Custom function to add linear regression line for upper panel
add_lm_line <- function(x, y, ...) {
  points(x, y, ...)
  fit <- lm(y ~ x)
  r_squared <- summary(fit)$r.squared
  if (r_squared > 0.6) {
    abline(fit, col = "red", lwd = 2, lty = 1, ...)
  } else {
    abline(fit, col = "grey80", lwd = 2, lty = 2, ...)
  }
}

# Plot with linear regression line for upper panel
pairs(dat_pairs,
      las = 1,
      pch = 16,
      cex = 1.2,
      lower.panel = NULL,
      upper.panel = add_lm_line)


add_lower_panel <- function(x, y, ...) {
  # Calculate Pearson correlation coefficient
  r <- cor(x, y)
  
  # Determine the sign of the correlation coefficient
  if (r >= 0) {
    color <- rgb(1, 0, 0, r)
  } else {
    color <- rgb(0, 0, 1, abs(r))
  }
  
  # Plot background color in lower panel
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = color, border = NA)
  
  # Calculate and plot R-squared value in lower panel
  r_squared <- r^2
  text(x = mean(x), y = mean(y), labels = paste(round(r_squared, 2)),
       col = "black", font = 2, adj = 0.5, ...)
}

pairs(dat_pairs,
      las = 1,
      pch = 16,
      cex = 1.2,
      upper.panel = add_lm_line,
      lower.panel = add_lower_panel)

##### plots #####
##### T vs. TD, salinity vs. TD #####
# temp
plot(Temp$DoublingTime ~ Temp$Temp,
     las = 1,
     cex.axis = 0.9,
     col = "red",
     cex = 1.5,
     lwd = 2,
     pch = 21,
     xlim = c(25, 50),
     ylim = c(0, 12),
     ylab = "Doubling Time (hours)",
     xlab = "T °C")
abline(T_Td,
       col = "red",
       lty = 1,
       lwd = 1.5)
# salinity
plot(Salinity$DoublingTime ~ Salinity$Salinity,
     las = 1,
     cex.axis = 0.9,
     col = "maroon",
     cex = 1.5,
     lwd = 2,
     pch = 21,
     xlim = c(20, 32),
     ylim = c(0, 12),
     ylab = "",
     xlab = "% Salinity")
abline(Sal_Td,
       col = "maroon",
       lty = 1,
       lwd = 1.5)


##### T vs. Eps, salinity vs. Eps #####
# temp
plot(Temp$Eps_LW ~ Temp$Temp,
     las = 1,
     cex.axis = 0.9,
     col = "red",
     cex = 1.5,
     lwd = 2,
     pch = 21,
     xlim = c(25, 50),
     ylim = c(-200, -100),
     ylab = expression(paste("Archaeol", phantom(0)^2, epsilon[L/W], " (‰)")), 
     xlab = "T °C")
abline(T_Eps,
       col = "red",
       lty = 2,
       lwd = 1.5)
# salinity
plot(Salinity$Eps_LW ~ Salinity$Salinity,
     las = 1,
     cex.axis = 0.9,
     col = "maroon",
     cex = 1.5,
     lwd = 2,
     pch = 21,
     xlim = c(20, 32),
     ylim = c(-200, -100),
     ylab = "",
     xlab = "% Salinity")
abline(Sal_Eps,
       col = "maroon",
       lty = 2,
       lwd = 1.5)


##### Td vs. Eps #####
png("02_SummaryFigs/FigS5_DirghangiPagani2023_Td_vs_Eps.png",
    width = 5, height = 4, units = 'in', res = 300)
par(mfrow=c(1,1),
    mar=c(3,2,1,0),
    oma = c(0, 2, 1, 2),
    mgp = c(3, 0.6, 0)) 

# temp
plot(dat$Eps_LW ~ dat$DoublingTime,
     las = 1,
     col = dat$Col,
     cex = 1.5,
     cex.axis = 0.9,
     lwd = 2,
     pch = dat$pch,
     xlim = c(3, 13),
     ylim = c(-180, -100),
     ylab =  "",
     xlab = "")
title(xlab = "Doubling Time (hours)", 
      line = 1.7, 
      cex.lab = 1,
      outer = F)
title(ylab = expression(paste("Archaeol", phantom(0)^2, epsilon[L/W], " (‰)")), 
      line = 0.3, 
      cex.lab = 1,
      outer = T)
abline(Td_Eps,
       col = "black",
       lty = 1,
       lwd = 1.5)
# Add 95% confidence interval
dat2 <- dat %>%
  arrange(DoublingTime)
ci <- predict(Td_Eps, dat2, interval = "confidence", level = 0.95)

# Shade the region between the lower and upper bounds of the CI
polygon(c(dat2$DoublingTime, rev(dat2$DoublingTime)),
        c(ci[, "lwr"], rev(ci[, "upr"])),
        # col = rgb(0.7, 0.7, 0.7, alpha = 0.3),
        col = rgb(col2rgb("black")[1]/255,
                  col2rgb("black")[2]/255,
                  col2rgb("black")[3]/255,
                  alpha = 0.1),
        border = NA)
# Add R-squared value and p-value
summary_lm <- summary(Td_Eps)
variable_index <- grep("DoublingTime", rownames(summary_lm$coefficients))
p_value <- summary_lm$coefficients[2, 4]
formatted_p_value <- format(p_value, scientific = FALSE)
slope <- summary_lm$coefficients[variable_index, "Estimate"]
slope_se <- summary_lm$coefficients[variable_index, "Std. Error"]
text(x = 3,
     y = -106,
     labels = paste("R² =", round(summary_lm$r.squared, 2), "; p = ", round(as.numeric(formatted_p_value), 2),
                    "\nSlope:", round(slope, 2), "± ", round(slope_se, 2), "‰/ hour"),
     pos = 4,
     cex = 0.8)

legend("bottomleft",
       bty = "n",
       cex = 0.8,
       title = "T °C",
       legend = c("32", "40", "45"),
       col = c("black", "maroon", "red"),
       pch = c(16, 16, 16))
legend("bottomleft",
       inset = c(0.15, 0),
       bty = "n",
       cex = 0.8,
       title = "Salinity",
       legend = c("23%", "25%", "30%"),
       col = c("black", "black", "black"),
       pch = c(21, 23, 22))
legend("topright", 
       bty = "n",
       cex = 0.8,
       legend = c("Linear", "95% CI"), 
       col = c("black", "grey90"), 
       lty = c(1, 2), 
       lwd = c(2,7))
dev.off()
