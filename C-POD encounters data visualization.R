#### PLOTTING AND VISUALIZING PONTOPORIA C-POD DATA

#Loading packages:

packages <- c("ggplot2","ggdist", "tidyquant", "tidyverse", "dplyr", "tidyr", 
              "ggfortify", "gamlss", "reshape2", "knitr", "vegan", "esquisse",
              "permute", "ggplot2", "ggpubr", "cluster", "caret","ggThemeAssist")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

setwd("C:/Users/jmner/OneDrive/Documents/Mestrado UFSC/SOLAMAC 2020")

# Data acquisition (only Travelling clicks)
cencqual<- read.csv("c_enc_quali.csv", header=TRUE)

# Click trains quality          
ggplot(cencqual,aes (x = factor (FMA), y = PercentModQ, fill = factor (FMA))) + 
  theme_bw() +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.1, .width = 0, point_colour = NA)+ 
  geom_boxplot(outlier.shape = NA, width = .12, outlier.color= NA, alpha = 0.5) +
  scale_fill_tq() +
  scale_y_continuous(limits = quantile(cencqual$PercentModQ, c(0.015, 1))) +
  theme_tq() +
  labs (title="Quality of click trains in encounters",
        subtitle = "Per FMA",
        x = "FMA", y = "Percentages of Moderate-Quality Click Trains (%)",
        fill="FMA") + 
  coord_flip()
ggsave("encounters - PercentModQ x fma.png")

# 87.5th centile of click kHz distribution         
ggplot(cencqual,aes (x = factor (FMA), y =  kHz87.5ile.T, fill = factor (FMA))) + 
  theme_bw() +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.1, .width = 0, point_colour = NA)+ 
  geom_boxplot(outlier.shape = NA, width = .12, outlier.color= NA, alpha = 0.5) +
  scale_fill_tq() +
  scale_y_continuous(limits = quantile(cencqual$ kHz87.5ile.T, c(0.015, 0.95))) +
  theme_tq() +
  labs (title="Click kHz distribution",
        subtitle = "Per FMA",
        x = "FMA", y = "87.5th centile of click frequency distribution (kHz)",
        fill="FMA") + 
  coord_flip()
ggsave("encounters -  kHz875ileTx fma.png")

# Percentage of clicks with an end frequency higher than the average frequency (%)        
ggplot(cencqual,aes (x = factor (FMA), y =  X.up.T, fill = factor (FMA))) + 
  theme_bw() +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.1, .width = 0, point_colour = NA)+ 
  geom_boxplot(outlier.shape = NA, width = .12, outlier.color= NA, alpha = 0.5) +
  scale_fill_tq() +
  scale_y_continuous(limits = quantile(cencqual$X.up.T, c(0.015, 0.95))) +
  theme_tq() +
  labs (title="End frequencies higher than the average click train frequency",
        subtitle = "Per FMA",
        x = "FMA", y = "Percentage of clicks with an end frequency higher than the average frequency (%)",
        fill="FMA") + 
  coord_flip()
ggsave("encounters -  percentUpT x fma.png")

# Clicks per second        
ggplot(cencqual,aes (x = factor (FMA), y =  Clks.s_75ile, fill = factor (FMA))) + 
  theme_bw() +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.1, .width = 0, point_colour = NA)+ 
  geom_boxplot(outlier.shape = NA, width = .12, outlier.color= NA, alpha = 0.5) +
  scale_fill_tq() +
  scale_y_continuous(limits = quantile(cencqual$ Clks.s_75ile, c(0.015, 0.95))) +
  theme_tq() +
  labs (title="Number of clicks in encounters",
        subtitle = "Per FMA",
        x = "FMA", y = "Clicks per second",
        fill="FMA") + 
  coord_flip()
ggsave("encounters -  Clks_s_75ile x fma.png")

# Encounter duration (min.)        
ggplot(cencqual,aes (x = factor (FMA), y =  Minutes, fill = factor (FMA))) + 
  theme_bw() +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.1, .width = 0, point_colour = NA)+ 
  geom_boxplot(outlier.shape = NA, width = .12, outlier.color= NA, alpha = 0.5) +
  scale_fill_tq() +
  scale_y_continuous(limits = quantile(cencqual$ Minutes, c(0, 0.95))) +
  theme_tq() +
  labs (title="Encounter duration ",
        subtitle = "Per FMA",
        x = "FMA", y = "Encounter duration (min.)",
        fill="FMA") + 
  coord_flip()
ggsave("encounters -  duration minutes x fma.png")


