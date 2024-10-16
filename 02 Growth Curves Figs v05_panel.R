cat("\014") #clears console
rm(list=ls()) 
graphics.off()

#### load relevant packages ####
library("readxl")
library("tidyverse")
library("reshape")
library("RColorBrewer")
library("colorspace")


##### DATA INPUTS ####
# enviro experiments
Temp <- read.csv("00_DataInputs/01_Growth_Curves/Cobban2020_T_growthCurve.csv",
                  stringsAsFactors = F)
pH <- read.csv("00_DataInputs/01_Growth_Curves/Cobban2020_pH_growthCurve.csv",
                  stringsAsFactors = F)
RPM <- read.csv("00_DataInputs/01_Growth_Curves/Cobban2020_RPM_growthCurve.csv",
                  stringsAsFactors = F)
DO <- read.csv("00_DataInputs/01_Growth_Curves/Cobban2020_O2_growthCurve.csv",
                  stringsAsFactors = F)
Flux1 <- read_xlsx("00_DataInputs/01_Growth_Curves/Zhou2020_Chemostat_ODdata_10hr18hr30hr.xlsx")
Flux2 <- read_xlsx("00_DataInputs/01_Growth_Curves/Zhou2020_Chemostat_ODdata_70hr.xlsx")


#### initial data cleaning ####
# new dataframes for each chemostat treatment
# Subset into Flux_10hr
Flux_18hr <- Flux1 %>%
  filter(Time_Hours >= 743 & Time_Hours <= 893) %>%
  mutate(Time_Hours_New = Time_Hours - min(Time_Hours))

# Subset into Flux_18hr
Flux_10hr <- Flux1 %>%
  filter(Time_Hours >= 899 & Time_Hours <= 1083) %>%
  mutate(Time_Hours_New = Time_Hours - min(Time_Hours))

# Subset into Flux_32hr
Flux_32hr <- Flux1 %>%
  filter(Time_Hours >= 1083 & Time_Hours <= 1129) %>%
  mutate(Time_Hours_New = Time_Hours - min(Time_Hours))

# Subset into Flux_54hr
Flux_54hr <- Flux2 %>%
  filter(Time_Hours >= 1002 & Time_Hours <= 1171) %>%
  mutate(Time_Hours_New = Time_Hours - min(Time_Hours))



#### assign unique symbology ####
pH <- pH %>%
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

Temp <- Temp %>%
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

RPM <- RPM %>%
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
DO <- DO %>%
  mutate(Col = case_when(
    Treatment == "RPM_0.2_LL" ~ "darkslategray1",
    Treatment == "RPM_0.5_LL" ~ "cyan",
    Treatment == "RPM_2_LL" ~ "cyan3",
    Treatment == "RPM_20_LL" ~ "cyan4",
    TRUE ~ "black"  # Default color 
  ),
  pch = case_when(
    Treatment == "RPM_0.5_LL" ~ 15,
    Treatment == "RPM_2_LL" ~ 18,
    Treatment == "RPM_20_LL" ~ 16,
    TRUE ~ 16  # Default pch 
  )
  )



#### GROWTH CURVE PLOTS ####
#### ENVIRO EXPERIMENTS  - linear - 1 x 5 ####  
enviro <- rbind(Temp, pH, RPM, DO)
maxtime <- max(enviro$ElapsedTime)
maxOD <- max(enviro$ODmean) + 0.1

png("02_SummaryFigs/02_FigS1_GrowthCurves_Enviro_Linear_1x5.png",
    width = 170, height = 40, units = 'mm', res = 300)
par(mfrow=c(1, 5),
    mar=c(2.5,2,1.5,0),
    oma = c(1, 1, 0.5, 1),
    mgp = c(2, 0.3, 0)) 

##### Plot 1 Temp #### 
maxtime <- max(Temp$ElapsedTime)
maxOD <- max(Temp$ODmean) + 0.1
plot(Temp$ElapsedTime, Temp$OD2mean,
     las = 1,
     cex = 1,
     type = "p",
     pch = Temp$pch,
     col = Temp$Col,
     bg = "white",
     ylim = c(0, maxOD),
     xlim = c(0, maxtime),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
axis(1, at = seq(0, maxtime, 24),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)

axis(2, at = seq(0, 2, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 2, 1),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
mtext("A", 
      side = 1, 
      line = -6, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(main = "Temperature", 
      line = 0.5, 
      cex.main = 1,
      outer = F,
      adj = 0.5,
      font.main = 1)
title(xlab = "Time (hours)",   
      line = -0.7,    
      cex.lab = 1.1,   
      outer = T)
title(ylab = expression("OD"[600]), 
      line = -0.4, 
      cex.lab = 1.1,
      outer = T)


# add lines for each experiment
treatments <- unique(Temp$Treatment)
for (i in 1:length(treatments)){
  temp2 <- Temp[Temp$Treatment == treatments[i], ]
  temp2 <- temp2[with(temp2, order(ElapsedTime)), ] # sort by elapsed time
  temp2 <- temp2[!is.na(temp2$OD2mean),] # remove NAs

  arrows(x0 = temp2$ElapsedTime,
         y0= temp2$ODmean - temp2$OD2se,
         x1 = temp2$ElapsedTime,
         y1 = temp2$ODmean + temp2$OD2se,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col)
}


# symbology for legend
symbology <- as.data.frame(cbind(Temp$Treatment, Temp$Col, Temp$pch))
symbology <- distinct(symbology)
colnames(symbology) <- c("Treatment", "Col", "pch")
symbology$Col <- as.character(symbology$Col)
symbology$pch <- as.numeric(as.character(symbology$pch))

legend("bottomright",
       xpd = NA, # plots in outer margin area
       cex = 0.6,
       bty = "n",
       legend = c("65°C", "70°C", "75°C", "80°C"),
       text.col = "black",
       col = symbology$Col,
       pt.cex = 1.2,
       pch = symbology$pch)

##### Plot 2 pH #### 
maxtime <- max(pH$ElapsedTime)
maxOD <- max(pH$ODmean) + 0.1
plot(pH$ElapsedTime, pH$OD2mean,
     las = 1,
     cex = 1,
     type = "p",
     pch = pH$pch,
     col = pH$Col,
     bg = "white",
     ylim = c(0, maxOD),
     xlim = c(0, maxtime),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
axis(1, at = seq(0, maxtime, 24),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)

axis(2, at = seq(0, 2, 1),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 2, 1),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
mtext("B", 
      side = 1, 
      line = -2, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(main = "pH", 
      line = 0.5, 
      cex.main = 1,
      outer = F,
      adj = 0.5,
      font.main = 1)

# add lines for each experiment
treatments <- unique(pH$Treatment)
for (i in 1:length(treatments)){
  temp2 <- pH[pH$Treatment == treatments[i], ]
  temp2 <- temp2[with(temp2, order(ElapsedTime)), ] # sort by elapsed time
  temp2 <- temp2[!is.na(temp2$OD2mean),] # rempve NAs

  arrows(x0 = temp2$ElapsedTime,
         y0= temp2$ODmean - temp2$OD2se,
         x1 = temp2$ElapsedTime,
         y1 = temp2$ODmean + temp2$OD2se,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col)
}

# symbology for legend
symbology <- as.data.frame(cbind(pH$Treatment, pH$Col, pH$pch))
symbology <- distinct(symbology)
colnames(symbology) <- c("Treatment", "Col", "pch")
symbology$Col <- as.character(symbology$Col)
symbology$pch <- as.numeric(as.character(symbology$pch))

legend("topleft",
       xpd = NA, # plots in outer margin area
       cex = 0.6,
       bty = "n",
       legend = c("pH 2", "pH 3", "pH 4"),
       text.col = "black",
       col = symbology$Col,
       pt.cex = 1.2,
       pch = symbology$pch)



##### Plot 3 RPM #### 
excluded_treatments <- c("RPM_0", "RPM_61", "RPM_75", "RPM_97")
RPM_subset <- RPM[!(RPM$Treatment %in% excluded_treatments), ]

maxtime <- max(RPM_subset$ElapsedTime)
maxOD <- max(RPM_subset$ODmean) + 0.1
plot(RPM_subset$ElapsedTime, RPM_subset$OD2mean,
     las = 1,
     cex = 1,
     type = "p",
     pch = RPM_subset$pch,
     col = "blueviolet",
     bg = "white",
     ylim = c(0, 1),
     xlim = c(0, maxtime),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
axis(1, at = seq(0, (maxtime+24), 24),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)

axis(2, at = seq(0, 2, 0.5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 2, 0.5),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
mtext("C", 
      side = 1, 
      line = -6, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(main = "Aeration Rate", 
      line = 0.5, 
      cex.main = 1,
      outer = F,
      adj = 0.5,
      font.main = 1)

# add lines for each experiment
treatments <- unique(RPM_subset$Treatment)
for (i in 1:length(treatments)){
  temp2 <- RPM_subset[RPM_subset$Treatment == treatments[i], ]
  temp2 <- temp2[with(temp2, order(ElapsedTime)), ] # sort by elapsed time
  temp2 <- temp2[!is.na(temp2$OD2mean),] # rempve NAs
  
  arrows(x0 = temp2$ElapsedTime,
         y0= temp2$ODmean - temp2$OD2se,
         x1 = temp2$ElapsedTime,
         y1 = temp2$ODmean + temp2$OD2se,
         code = 1, # no cap of bars
         angle = 90,
         length = 0,
         lwd = 1.5,
         col = temp2$Col)
  # add points for each experiment (so points are on top of lines)
  points(temp2$ElapsedTime, temp2$OD2mean,
         col = temp2$Col,
         bg = "white",
         pch = temp2$pch,
         cex = 1)
}


# symbology for legend
symbology <- as.data.frame(cbind(RPM_subset$Treatment, 
                                 RPM_subset$Col, 
                                 RPM_subset$pch))
symbology <- distinct(symbology)
colnames(symbology) <- c("Treatment", "Col", "pch")
symbology$Col <- as.character(symbology$Col)
symbology$pch <- as.numeric(as.character(symbology$pch))

legend("topright",
       xpd = NA, # plots in outer margin area
       cex = 0.6,
       bty = "n",
       legend = c("RPM 50", "RPM 125", "RPM 300"),
       text.col = "black",
       col = symbology$Col,
       pt.cex = 1.2,
       pch = symbology$pch)



##### Plot 4 DO Sparge Gas #### 
maxtime <- max(DO$ElapsedTime)
maxOD <- max(DO$ODmean) + 0.1
DO <- subset(DO, DO$Treatment != "RPM_1_LL")
plot(DO$ElapsedTime, DO$OD2mean,
     las = 1,
     cex = 1,
     type = "p",
     pch = DO$pch,
     col = DO$Col,
     bg = "white",
     ylim = c(0, 0.5),
     xlim = c(0, maxtime),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)
axis(1, at = seq(0, (maxtime+24), 24),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)

axis(2, at = seq(0, 1, 0.25),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 1, 0.25),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
mtext("D", 
      side = 1, 
      line = -6, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(main = expression("pO"[2]), 
      line = 0.8, 
      cex.main = 1,
      outer = F,
      adj = 0.5,
      font.main = 1)

# add lines for each experiment
treatments <- unique(DO$Treatment)
for (i in 1:length(treatments)){
  temp2 <- subset(DO, DO$Treatment == treatments[i])
  temp2 <- temp2[with(temp2, order(ElapsedTime)), ] # sort by elapsed time
  temp2 <- temp2[!is.na(temp2$OD2mean),] # rempve NAs

  arrows(x0 = temp2$ElapsedTime, 
         y0= temp2$ODmean - temp2$OD2se, 
         x1 = temp2$ElapsedTime, 
         y1 = temp2$ODmean + temp2$OD2se, 
         code = 1, # no cap of bars
         angle = 90, 
         length = 0,
         lwd = 1.5, 
         col = temp2$Col)
}

# symbology for legend
symbology <- as.data.frame(cbind(DO$Treatment, DO$Col, DO$pch))
symbology <- distinct(symbology)
colnames(symbology) <- c("Treatment", "Col", "pch")
symbology$Col <- as.character(symbology$Col)
symbology$pch <- as.numeric(as.character(symbology$pch))

legend("topright",
       cex = 0.6,
       bty = "n",
       legend = c("0.2%" , "0.5%", "2%", "20%"),
       text.col = "black",
       col = symbology$Col,
       pt.cex = 1.2,
       pch = symbology$pch)

##### Plot 5 Flux - chemostat subsets - USE THIS #### 
plot(Flux_10hr$Time_Hours_New, Flux_10hr$Bioreactor_mean,
     # log = "y",
     las = 1,
     cex = 1,
     type = "p",
     pch = 16,
     col = "deeppink1",
     bg = "white",
     ylim = c(0, 1.2),
     xlim = c(0, 170),
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n",
     cex.axis = 0.75)

points(Flux_32hr$Time_Hours_New, Flux_32hr$Bioreactor_mean,
       cex = 1,
       pch = 17,
       # col = "blueviolet"
       col = "deeppink3")
points(Flux_54hr$Time_Hours_New, Flux_54hr$Bioreactor_mean,
       cex = 1.1,
       pch = 18,
       # col = "blue"
       col = "deeppink4")

axis(1, at = seq(0, 200, 24),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
axis(2, at = seq(0, 1, 0.5),
     las = 1,
     labels = T,
     tck = -0.035,
     cex.axis = 0.75,
     line = 0)
# add minor Y ticks
axis(2, at = seq(0, 2, 0.5),
     las = 1,
     labels = F,
     tck = -0.02,
     cex.axis = 0.75,
     line = 0)
mtext("E", 
      side = 1, 
      line = -1.5, 
      adj = 0.1,
      font = 2, 
      cex = 0.9)
title(main = "e- donor flux", 
      line = 0.5, 
      cex.main = 1,
      outer = F,
      adj = 0.5,
      font.main = 1)
legend("bottomright",
       cex = 0.6,
       bty = "n",
       legend = c("7 hr",  "21 hr" , "44 hr"),
       col = c("deeppink", "deeppink3", "deeppink4"),
       pt.cex = 1.2,
       pch = c(16, 17, 18))
dev.off()
  
  