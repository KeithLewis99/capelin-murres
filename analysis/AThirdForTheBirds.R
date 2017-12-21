## load libraries ----
library(dplyr)
library(psych)

## read data ----
cap_biom <- read.csv('data-raw/acoustic_estimates_2017.csv',header = T, as.is = T)

## function normalize ----
normalize <- function(x){
  z <- (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  z
}
## function funct respose type II
funcresponse <- function (x, a, h, delta) {(a*(x^delta))/(1 + a*h*(x^delta))}

## normalize capelin biomas ----
cap_biom$nbiomass <- normalize(cap_biom$biomass)
cap_biom <- cap_biom[order(cap_biom$nbiomass),]

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
athird <- 1/3 * max(cap_biom$nbiomass, na.rm = T)


## plot ---- 
#
type2 <- curve(funcresponse(x, 2.8,.93, 1), 0, 5)
type2$x <- type2$x - 2
type3 <- curve(funcresponse(x, 1, 1, 2), 0, 5)
type3$x <- type3$x - 2

#png('analysis/output/AThirdForTheBirds.png', width = 900, height = 900, bg = 'transparent', pointsize = 15)
# set up the plot
with(cap_biom, plot(nbiomass, tanh(nbiomass), pch = '', yaxt = 'n', ylab = '', xlab = 'Normalized prey abundance', ylim = c(0,1), xlim = c(-2, 3), xaxp = c(-2,3,5)))
# plot the rectangle for prior to 1991
rect(ranreg1$min, 0, ranreg1$max, 1,col = 'grey90', border = 'grey90')
text(2, .5, 'Observed capelin biomass\n prior to 1991', cex = 0.9)
# plot the rectangle for after 1990
rect(ranreg2$min, 0, ranreg2$max, 1,col = 'grey60', border = 'grey60')
text(-0.23, .4, 'Observed capelin biomass during this study', srt = 90, cex = 0.9)
# plot the rectangle not observed
rect(-2, 0, ranreg1$min, 1,col = 'grey50', border = 'grey50')
text(-1.2, .5, 'Not observed', cex = 0.9)
# plot a third for the birds
abline(v = athird, col = 'red')
text(athird - 0.1, .4, 'A third of maximum capelin biomass observed', srt = 90, cex = 0.9, col = 'red')

# add a logistic curve
lines(type2$x, type2$y, lwd = 2)
curve(logistic(x,a = 3), -2, 3, add = T, lwd = 2)
#lines(type3$x, type3$y, lwd = 2)

#lines(cap_biom$nbiomass, funcresponse(cap_biom$nbiomass, 1, 1))

#dev.off()
