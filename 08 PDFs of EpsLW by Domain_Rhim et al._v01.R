cat("\014") #clears console
rm(list=ls()) 
graphics.off()

#### load relevant packages ####
library("readxl")
library("tidyverse")
library("RColorBrewer")
library("colorspace")


#### data input ####
Fig6 <- read_xlsx("00_DataInputs/05_Rhim et al. 2024/Rhim et al., 2024_EpsilonCompilation_Fig6.xlsx",
                   sheet = "Fig6")
saci <- readRDS("00_Experiment Dataframes/enviro_merge.rds")


#### data cleaning ####
# subset by domain
euks <- subset(Fig6, Fig6$Domain == "Eukarya")
bac <- subset(Fig6, Fig6$Domain == "Bacteria")
arch <- subset(Fig6, Fig6$Domain == "Archaea")


##### rename Eps column #####
euks$Eps <- euks$epsilon
bac$Eps <- bac$epsilon
arch$Eps <- arch$epsilon

##### subset for only rows that contains Eps_L/W value #####
euks <- subset(euks, euks$Eps != "NA")
bac <- subset(bac, bac$Eps != "NA")
arch <- subset(arch, arch$Eps != "NA")
saci <- subset(saci, saci$EpsLW_wt_mean != "NA")

#### Compare variance between archaea and euks ####
##### combine data into one df #####
saci$Domain <- "Archaea"
saci$Eps <- saci$EpsLW_wt_mean

bac_subset <- bac %>% select(Domain, Eps)
euks_subset <- euks %>% select(Domain, Eps)
arch_subset <- arch %>% select(Domain, Eps)
saci_subset <- saci %>% select(Domain, Eps)
dat <- rbind(
  euks_subset,
  arch_subset,
  saci_subset)

dat_arch_bac <- rbind(
  bac_subset,
  arch_subset,
  saci_subset)

#####  test for equal variance #####
library(car)
var_arch <- var(arch$Eps)
var_euks <- var(euks$Eps)
var_bac <- var(bac$Eps)

variance_ratio_euks_arch <- var_euks / var_arch
variance_ratio_bac_arch <- var_bac / var_arch

leveneTest(Eps ~ Domain, data = dat)
# Levene's Test for Homogeneity of Variance , F(1,340) = 5.16, p = 0.024
leveneTest(Eps ~ Domain, data = dat_arch_bac)
# Levene's Test for Homogeneity of Variance , F(1,210) = 52, p <0.0001

bartlett.test(Eps ~ Domain, data = dat)
# Bartlett's K-squared = 16.374, df = 1, p-value = 5.201e-05
bartlett.test(Eps ~ Domain, data = dat_arch_bac)
# Bartlett's K-squared = 118.48, df = 1, p-value < 2.2e-16


boxplot(Eps ~ Domain, data = dat,
        main = "Variance Comparison",
        ylab = "Eps",
        xlab = "Domain",
        col = c("deeppink2", "chartreuse3"),
        border = "black")
stripchart(Eps ~ Domain, data = dat,
           vertical = TRUE, 
           method = "jitter",
           pch = 16, 
           cex = 0.5,
           col = "black", 
           add = TRUE)

#### Calculate density PDF ####
euks.Eps <- euks$Eps
euks.d_Eps <- density(euks.Eps,
                      bw = 30)
bac.Eps <- bac$Eps
bac.d_Eps <- density(bac.Eps,
                     bw = 30)
arch.Eps <- arch$Eps
arch.d_Eps <- density(arch.Eps,
                      bw = 30)

saci.Eps <- saci$EpsLW_wt_mean
saci.d_Eps <- density(saci.Eps,
                      bw = 20)
# all archaea
all.arch.Eps <- c(arch.Eps, saci.Eps)
all.arch.d_Eps <- density(all.arch.Eps,
                          bw = 20)

#### Plot density functions for Eps by domain ####
# scale so all range btwn 0 and 1
euks.d_Eps$y <- euks.d_Eps$y / (max(euks.d_Eps$y))
bac.d_Eps$y <- bac.d_Eps$y / (max(bac.d_Eps$y))
arch.d_Eps$y <- arch.d_Eps$y / (max(arch.d_Eps$y))
saci.d_Eps$y <- saci.d_Eps$y / (max(saci.d_Eps$y))
all.arch.d_Eps$y <- all.arch.d_Eps$y / (max(all.arch.d_Eps$y))

##### ALL DOMAINS- STACKED #####
# determine offsets
step = 1.1
euks.d_Eps$y <- euks.d_Eps$y + (step*2)
bac.d_Eps$y <- bac.d_Eps$y + step
arch.d_Eps$y <- arch.d_Eps$y 


png("02_SummaryFigs/08_Fig6_Eps_byDomain_stacked.png",
    width = 5, height = 3, units = 'in', res = 300)
par(mfrow=c(1,1),
    mar=c(3,1,1,0),
    oma = c(0, 0, 0, 0),
    mgp = c(3, 0.2, 0)) 
plot(0,0,
     type = "n",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     xlim = c(-500, 500),
     ylim = c(0, 3.2))

axis(1, at = seq(-500, 500, 250),
     las = 1,
     labels = T,
     tck = -0.02,
     cex.axis = 0.7,
     line = 0)
abline(v = 0,
       col = "black",
       lwd = 0.75)
lines(euks.d_Eps,
      col = "chartreuse3",
      lwd = 2)

polygon(euks.d_Eps,
        col = rgb(col2rgb("chartreuse3")[1]/255,
                  col2rgb("chartreuse3")[2]/255,
                  col2rgb("chartreuse3")[3]/255,
                  alpha = 0.3),
        border = NA)

lines(bac.d_Eps,
      col = "cyan2",
      lwd = 2)

polygon(bac.d_Eps,
        col = rgb(col2rgb("cyan2")[1]/255,
                  col2rgb("cyan2")[2]/255,
                  col2rgb("cyan2")[3]/255,
                  alpha = 0.3),
        border = NA)

lines(all.arch.d_Eps,
      col = "deeppink2",
      lwd = 2)

polygon(all.arch.d_Eps,
        col = rgb(col2rgb("deeppink2")[1]/255,
                  col2rgb("deeppink2")[2]/255,
                  col2rgb("deeppink2")[3]/255,
                  alpha = 0.3),
        border = NA)

# lines(saci.d_Eps,
#       col = "black",
#       lwd = 2)


# Add data points
rug(jitter(euks.Eps),
    col = "chartreuse3",
    lwd = 1,
    line = -7.3,
    ticksize = 0.02)
rug(jitter(bac.Eps),
    col = "cyan2",
    lwd = 1,
    line = -3.8,
    ticksize = 0.02)
rug(jitter(arch.Eps),
    col = "deeppink2",
    lwd = 1,
    line = -0.32,
    ticksize = 0.02)
rug(jitter(saci.Eps),
    col = "black",
    lwd = 1,
    line = -0.32,
    ticksize = 0.02)

title(xlab = expression(paste(" "^2, "ε"[L/W], " (‰)")),
      line = 1.5,
      cex.lab = 1,
      outer = F)
text(x = -400,
     y = 3,
     labels = "Eukarya",
     col = "chartreuse3",
     pos = 4,
     cex = 0.8)
text(x = -400,
     y = 2.8,
     labels = "n = 270",
     col = "chartreuse3",
     pos = 4,
     cex = 0.7)
text(x = -400,
     y = 1.9,
     labels = "Bacteria",
     col = "cyan2",
     pos = 4,
     cex = 0.8)
text(x = -400,
     y = 1.7,
     labels = "n = 140",
     col = "cyan2",
     pos = 4,
     cex = 0.7)
text(x = -460,
     y = 0.7,
     labels = "Archaea",
     col = "deeppink2",
     pos = 4,
     cex = 0.8)
text(x = -460,
     y = 0.5,
     labels = "n = 72",
     col = "deeppink2",
     pos = 4,
     cex = 0.7)

dev.off()


