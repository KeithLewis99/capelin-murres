## load libraries ----
library(dplyr)

## read data ----
cap_biom <- read.csv('data-raw/acoustic_estimates_2017.csv',header = T, as.is = T)

## function normalize ----
normalize <- function(x){
  z <- (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  z
}

## normalize capelin biomas ----
cap_biom$nbiomass <- normalize(cap_biom$biomass)

## ranges of capelin biomass observed during the two regimes ----
ranreg2 <- cap_biom %>%
  filter(complete.cases(.)) %>%
  filter(year > 1990) %>%
  summarise(min = min(na.omit(nbiomass)), max = max(na.omit(nbiomass)))

ranreg1 <- cap_biom %>%
  filter(complete.cases(.)) %>%
  filter(year < 1991) %>%
  summarise(min = min(na.omit(nbiomass)), max = max(na.omit(nbiomass)))

## a third of maximum capelin biomass observed  ----
# (acoustic - not sure if there are other historical estimates)
athird <- max(na.omit(cap_biom$nbiomass))/3


## plot ---- 
png('analysis/output/AThirdForTheBirds.png', width = 900, height = 900, bg = 'transparent', pointsize = 15)
# set up the plot
with(cap_biom, plot(nbiomass, nbiomass, pch = '', yaxt = 'n', ylab = '', xlab = 'Normalized prey abundance', xlim = c(-2, 3), xaxp = c(-2,3,5)))
# plot the rectangle for prior to 1991
rect(ranreg1$min, min(na.omit(cap_biom$nbiomass)), ranreg1$max, max(na.omit(cap_biom$nbiomass)),col = 'grey90', border = 'grey90')
text(2, 1.3, 'Observed capelin biomass\n prior to 1991', cex = 0.9)
# plot the rectangle for after 1990
rect(ranreg2$min, min(na.omit(cap_biom$nbiomass)), ranreg2$max, max(na.omit(cap_biom$nbiomass)),col = 'grey60', border = 'grey60')
text(-0.23, 1, 'Observed capelin biomass during this study', srt = 90, cex = 0.9)
# plot the rectangle not observed
rect(-2, min(na.omit(cap_biom$nbiomass)), ranreg1$min, max(na.omit(cap_biom$nbiomass)),col = 'grey50', border = 'grey50')
text(-1.2, 1.3, 'Not observed', cex = 0.9)
# plot a third for the birds
abline(v = athird, col = 'red')
text(athird - 0.1, 1, 'A third of maximum capelin biomass observed', srt = 90, cex = 0.9, col = 'red')

dev.off()
