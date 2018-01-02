## load libraries ----
# library(ggplot2)
library(lubridate)
library(cowplot)
library(dplyr)

library(TMB)

TMB::compile("inst/fit.cpp")
dyn.load("inst/fit")
source("R/fit.R")

############################
### COMU CONDITION DATA#########
############################

## read data ----
cc <- read.csv("data-raw/COMU_condition.csv", header = T, as.is = T)

## manipulate data ----
cc <- subset(cc, colony == "Funk")
cc <- subset(cc, species == "COMU")
cc$dates <- dmy(cc$date)
cc$condition <- cc$bird_weight / cc$winglength
cc[which(cc$stage == "Adult"), "stage"] <- "adult"


## Look at adult weights only ----
adw <- filter(cc, stage == "adult" & year > 2000)
## No adult data prior to 2000, nothing in 2001-2004: Is this correct?
## There  are only 3 adult weights in 2000 - they all seem too high: 1125 and 1175 g. Did not use them
avw <- adw %>%
  filter(complete.cases(bird_weight)) %>%
  group_by(year, stage) %>%
  summarize(avg = mean(bird_weight))

dif <- max(avw[, 3]) - min(avw[, 3])
av <- sum(avw[, 3]) / nrow(avw)
dif / av
adw %>%
  group_by(year) %>%
  summarise(mean(bird_weight, na.rm = T))

## trick to force ggplot to include all years in plot
plotad <- adw %>%
  select(year, bird_weight, stage)
y <- 2005:2017
plotad <- rbind(plotad, data.frame(year = y[which(!y %in% sort(as.integer(unique(plotad$year))))], bird_weight = NA, stage = "adult"))
## plot
g <- ggplot(plotad, aes(as.factor(year), bird_weight))
g <- g + geom_violin()
g <- g + stat_summary(aes(as.factor(year)), fun.y = mean, geom = "point", fill = "black", shape = 21, size = 1.5)
g <- g + xlab("Year") + ylab("Body weight (g)")
g <- g + theme_cowplot(font_size = 10) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
print(g)
save_plot("analysis/output/AdultWeights.png", g, base_aspect_ratio = 1.4, base_width = 6, bg = "transparent") # make room for figure legend)

## converted to kg to help model converge
adult_model <- fit_model(
  year = adw$year[!is.na(adw$bird_weight)],
  response = adw$bird_weight[!is.na(adw$bird_weight)] / 1000
)
plot_model(adult_model, ylab = "Body weight (g)", scale = 1000)
ggsave("analysis/output/adult_mass_trend.png", height = 5, width = 7)
get_beta(adult_model) * 1000


## Chick and fledgling condition ----
flc <- filter(cc, stage != "adult" & year > 1990) %>%
  select(year, condition, stage)
y <- 1990:2017
flc <- rbind(flc, data.frame(year = y[which(!y %in% sort(as.integer(unique(flc$year))))], condition = NA, stage = "chick"))
# get rid of 2 outliers
flc[which(flc$condition > 9), "condition"] <- NA
summary(lm(condition ~ as.integer(year) + as.factor(stage), data = flc))

g <- ggplot(flc, aes(as.factor(year), condition))
g <- g + geom_violin()
g <- g + stat_summary(aes(as.factor(year)), fun.y = mean, geom = "point", fill = "black", shape = 21, size = 1.5)
g <- g + xlab("Year") + ylab("Condition (g/cm)")
g <- g + facet_grid(stage ~ ., scales = "free")
g <- g + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8))
g <- g + ylim(2,7)
print(g)
save_plot("analysis/output/ChickFledglingCondition.png", g, base_aspect_ratio = 1.4, base_width = 6, bg = "transparent") # make room for figure legend)

chick_cond <- na.omit(flc[flc$stage == "chick", ])
chick_cond %>%
  group_by(year) %>%
  summarize(N = n())
chick_cond <- chick_cond[chick_cond$year != 1998, ] # drop 98 because of low sample size
chick_model <- fit_model(year = chick_cond$year, response = chick_cond$condition)
chick_model$sd_rep # not happy. too short of a time series??
plot_model(chick_model, ylab = "Body weight (g)", scale = 1)

fledg_cond <- na.omit(flc[flc$stage == "fledgling", ])
fledg_cond %>%
  group_by(year) %>%
  summarize(N = n())
fledg_cond <- fledg_cond[fledg_cond$year != 1997, ] # drop 97 because of low sample size
fledg_model <- fit_model(year = fledg_cond$year, response = fledg_cond$condition)
fledg_model$sd_rep
plot_model(fledg_model, ylab = "Condition (g/cm)", scale = 1)
get_beta(fledg_model)
ggsave("analysis/output/fledgling_condition_trend.png", height = 5, width = 7)

fc_cond <- na.omit(flc)
fc_cond %>%
  group_by(year) %>%
  summarize(N = n())
fc_model <- fit_model(year = fc_cond$year, response = fc_cond$condition)
fc_model$sd_rep
plot_model(fc_model, ylab = "Condition (g/cm)", scale = 1)
get_beta(fc_model)
ggsave("analysis/output/combined_chick_condition_trend.png", height = 5, width = 7)


## Chick and fledgling weight ----
flc <- filter(cc, stage != "adult" & year > 1990) %>%
  select(year, condition, stage, bird_weight)
y <- 1990:2017
flc <- rbind(flc, data.frame(year = y[which(!y %in% sort(as.integer(unique(flc$year))))], condition = NA, stage = "chick", bird_weight = NA))
flc[which(flc$condition > 9), "condition"] <- NA
flc[which(flc$bird_weight > 900), "bird_weight"] <- NA

w <- ggplot(flc, aes(as.factor(year), bird_weight))
w <- w + geom_violin()
w <- w + stat_summary(aes(as.factor(year)), fun.y = mean, geom = "point", fill = "black", shape = 21, size = 1.5)
w <- w + xlab("Year") + ylab("Weight (g)")
w <- w + facet_grid(stage ~ ., scales = "free")
w <- w + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
w <- w + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8))
save_plot("analysis/output/ChickFledglingWeight.png", w, base_aspect_ratio = 1.4, base_width = 6, bg = "transparent") # make room for figure legend)

## Chick and fledgling wing length ----
wl <- filter(cc, stage != "adult" & year > 1990) %>%
  select(year, stage, winglength)
y <- 1990:2017
wl <- rbind(wl, data.frame(year = y[which(!y %in% sort(as.integer(unique(wl$year))))], stage = "chick", winglength = NA))

ww <- ggplot(wl, aes(as.factor(year), winglength))
ww <- ww + geom_violin()
ww <- ww + stat_summary(aes(as.factor(year)), fun.y = mean, geom = "point", fill = "black", shape = 21, size = 1.5)
ww <- ww + xlab("Year") + ylab("Wing length (cm)")
ww <- ww + facet_grid(stage ~ ., scales = "free")
# w <- w + theme_cowplot(font_size = 10) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
ww <- ww + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ww <- ww + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8))
print(ww)
save_plot("analysis/output/ChickFledglingWlength.png", ww, base_aspect_ratio = 1.4, base_width = 6, bg = "transparent") # make room for figure legend)