cat("\014") #clears console
rm(list=ls()) 
graphics.off()

#### load relevant packages ####
library("readxl")
library("tidyverse")
library("reshape")
library("RColorBrewer")
library("colorspace")
library("Hmisc")
library("corrplot")


#### data input ####
enviro <- readRDS("00_Experiment Dataframes/enviro_merge.rds")
Temp <- readRDS("00_Experiment Dataframes/temp.rds")
pH <- readRDS("00_Experiment Dataframes/pH.rds")
RPM <- readRDS("00_Experiment Dataframes/RPM.rds")
DO <- readRDS("00_Experiment Dataframes/DO.rds")
Flux <- readRDS("00_Experiment Dataframes/flux.rds")



##### set color bar #####
col1 <- colorRampPalette(c("red", "white", "cyan3"))

#### Make correlation matrices ####
##### Temp #####
columns_to_use <- c("Temp", 
                    "DoublingTime", 
                    "SacrificeOD",
                    "RingIndex_GDGT",
                    "RingIndex_BP",
                    "RingDiff_all",
                    "EpsLW_wt_mean") 
dat_pairs_Temp <- Temp[, columns_to_use]

# correlation matrix
M_Temp<- round(cor(dat_pairs_Temp,
                   method = "pearson",
                   use = "complete.obs"), 2) # round correlations to 2 decimals
# calculate p-values
res_Temp <- cor.mtest(dat_pairs_Temp, 
                      conf.level = .95)
# rename columns and rows
colnames(M_Temp) <- c(":DegC",
                      ":T[D]", 
                      ":MaxOD",
                      ":RI[GDGT]",
                      ":RI[BP]",
                      "Δε/ring",
                      ":ε[L/W]")
rownames(M_Temp) <- colnames(M_Temp)

##### pH #####
columns_to_use <- c("pH", 
                    "DoublingTime", 
                    "SacrificeOD",
                    "RingIndex_GDGT",
                    "RingIndex_BP",
                    "RingDiff_all",
                    "EpsLW_wt_mean") 
dat_pairs_pH <- pH[, columns_to_use]

# correlation matrix
M_pH <- round(cor(dat_pairs_pH,
                  method = "pearson",
                  use = "complete.obs"), 2) # round correlations to 2 decimals
# calculate p-values
res_pH <- cor.mtest(dat_pairs_pH, 
                    conf.level = .95)
# rename columns and rows
colnames(M_pH) <- c(":pH",
                    ":T[D]", 
                    ":MaxOD",
                    ":RI[GDGT]",
                    ":RI[BP]",
                    "Δε/ring",
                    ":ε[L/W]")
rownames(M_pH) <- colnames(M_pH)

##### RPM  #####
columns_to_use <- c("RPM", 
                    "DoublingTime", 
                    "SacrificeOD",
                    "RingIndex_GDGT",
                    "RingIndex_BP",
                    "RingDiff_all",
                    "EpsLW_wt_mean") 
dat_pairs_RPM <- RPM[, columns_to_use]

# correlation matrix
M_RPM <- round(cor(dat_pairs_RPM,
                   method = "pearson",
                   use = "complete.obs"), 2) # round correlations to 2 decimals
# calculate p-values
res_RPM <- cor.mtest(dat_pairs_RPM, 
                     conf.level = .95)
# rename columns and rows
colnames(M_RPM) <- c(":RPM",
                     ":T[D]", 
                     ":MaxOD",
                     ":RI[GDGT]",
                     ":RI[BP]",
                     "Δε/ring",
                     ":ε[L/W]")
rownames(M_RPM) <- colnames(M_RPM)

##### DO  #####
columns_to_use <- c("DO", 
                    "DoublingTime", 
                    "SacrificeOD",
                    "RingIndex_GDGT",
                    "RingIndex_BP",
                    "RingDiff_all",
                    "EpsLW_wt_mean") 
dat_pairs_DO <- DO[, columns_to_use]

# correlation matrix
M_DO<- round(cor(dat_pairs_DO,
                 method = "pearson",
                 use = "complete.obs"), 2) # round correlations to 2 decimals
# calculate p-values
res_DO <- cor.mtest(dat_pairs_DO, 
                    conf.level = .95)
# rename columns and rows
colnames(M_DO) <- c(":O[2]",
                    ":T[D]", 
                    ":MaxOD",
                    ":RI[GDGT]",
                    ":RI[BP]",
                    "Δε/ring",
                    ":ε[L/W]")
rownames(M_DO) <- colnames(M_DO)


##### e- donor flux #####
columns_to_use <- c("DoublingTime", 
                    "SacrificeOD",
                    "RingIndex_GDGT",
                    "RingIndex_BP",
                    "RingDiff_all",
                    "EpsLW_wt_mean") 

dat_pairs_Flux <- Flux[, columns_to_use]

# correlation matrix
M_Flux<- round(cor(dat_pairs_Flux,
                   method = "pearson",
                   use = "complete.obs"), 2) # round correlations to 2 decimals
# calculate p-values
res_Flux <- cor.mtest(dat_pairs_Flux, 
                      conf.level = .95)
# rename columns and rows
colnames(M_Flux) <- c(":T[D]", 
                      ":MaxOD",
                      ":RI[GDGT]",
                      ":RI[BP]",
                      "Δε/ring",
                      ":ε[L/W]")
rownames(M_Flux) <- colnames(M_Flux)





#### PLOT HERE ####
png("02_SummaryFigs/07_Fig4_SaciCorrelograms.png",
    width = 300, height = 100, units = 'mm', res = 300)
par(mfrow = c(1, 5),   # 4 rows and 5 columns of plots
    mar = c(0, 0, 0, 0), 
    oma = c(0, 0, 0, 0), 
    mgp = c(2, 0.5, 0)) 

##### Temp #####
 
corrplot(M_Temp, 
         col = col1(100),
         method = "circle", 
         type = "upper",
         tl.col = "black",
         cl.ratio = 0.2, # width of color bar
         cl.align = "l",
         tl.srt = 45,
         p.mat = res_Temp$p, 
         diag = F,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 0.9, 
         pch.col = "black",
         bg = "white")
mtext("A", 
      side = 3, 
      line = -7, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
mtext("Temperature", 
      side = 3, 
      line = -7, 
      adj = 0.3,
      font = 1, 
      cex = 0.9)

##### pH #####
corrplot(M_pH, 
         col = col1(100),
         method = "circle", 
         type = "upper",
         tl.col = "black",
         cl.ratio = 0.2, # width of color bar
         cl.align = "l",
         tl.srt = 45,
         p.mat = res_pH$p, 
         diag = F,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 0.9, 
         pch.col = "black",
         bg = "white")
mtext("B", 
      side = 3, 
      line = -7, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
mtext("pH", 
      side = 3, 
      line = -7, 
      adj = 0.25,
      font = 1, 
      cex = 0.9)

##### RPM #####
corrplot(M_RPM, 
         col = col1(100),
         method = "circle", 
         type = "upper",
         tl.col = "black",
         cl.ratio = 0.2, # width of color bar
         cl.align = "l",
         tl.srt = 45,
         p.mat = res_RPM$p, 
         diag = F,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 0.9, 
         pch.col = "black",
         bg = "white")
mtext("C", 
      side = 3, 
      line = -7, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
mtext("Aeration Rate", 
      side = 3, 
      line = -7, 
      adj = 0.3,
      font = 1, 
      cex = 0.9)

##### DO #####
corrplot(M_DO, 
         col = col1(100),
         method = "circle", 
         type = "upper",
         tl.col = "black",
         cl.ratio = 0.2, # width of color bar
         cl.align = "l",
         tl.srt = 45,
         p.mat = res_DO$p, 
         diag = F,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 0.9, 
         pch.col = "black",
         bg = "white")
mtext("D", 
      side = 3, 
      line = -7, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
mtext(expression(pO[2]), 
      side = 3, 
      line = -7, 
      adj = 0.25,
      font = 1, 
      cex = 0.9)

##### FLUX #####
corrplot(M_Flux, 
         col = col1(100),
         method = "circle", 
         type = "upper",
         tl.col = "black",
         cl.ratio = 0.2, # width of color bar
         cl.align = "l",
         tl.srt = 45,
         p.mat = res_Flux$p, 
         diag = F,
         insig = "label_sig",
         sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 0.9, 
         pch.col = "black",
         bg = "white")
mtext("E", 
      side = 3, 
      line = -7, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
mtext("e- donor flux", 
      side = 3, 
      line = -7, 
      adj = 0.35,
      font = 1, 
      cex = 0.9)
dev.off()



