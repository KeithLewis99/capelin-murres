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
cc <- read.csv("data-raw/COMU_condition_consolidated.csv", header = T, as.is = T)

## manipulate data ----
cc <- subset(cc, colony == "Funk")
cc <- subset(cc, species == "COMU")
cc$dates <- dmy(cc$date)
cc$bird_weight <- as.numeric(cc$bird_weight)
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
plot_model(adult_model, ylab = "Mass (g)", scale = 1000)
ggsave("analysis/output/adult_mass_trend.png", height = 5, width = 7)
get_beta(adult_model) * 1000


## Chick and fledgling condition ----
flc <- filter(cc, stage != "adult" & year >= 1980) %>%
  select(year, condition, stage, bird_weight)
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


dodge <- position_dodge(width = 1)
g <- ggplot(flc, aes(as.factor(year), condition, fill = stage)) +
  geom_violin(position = dodge, draw_quantiles = 0.5) +
  #geom_boxplot(width = 0.1, outlier.colour = "grey", outlier.size = 0, position = dodge) +
  theme_bw() + xlab("Year") + ylab("Condition (g/cm)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8))
save_plot("analysis/output/ChickFledglingCondition_dodge.png", g, 
          base_aspect_ratio = 1.4, base_width = 8, bg = "transparent",
          dpi = 600)



chick_cond <- na.omit(flc[flc$stage == "chick", ])
chick_cond %>%
  group_by(year) %>%
  summarize(N = n())
chick_cond <- chick_cond[chick_cond$year != 1998, ] # drop 98 because of low sample size
chick_model <- fit_model(year = chick_cond$year, response = chick_cond$condition)
chick_model$sd_rep # not happy. too short of a time series??
plot_model(chick_model, ylab = "Condition (g/cm)", scale = 1)

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

fledg_model <- fit_model(year = fledg_cond$year, response = fledg_cond$bird_weight / 100)
fledg_model$sd_rep
plot_model(fledg_model, ylab = "Mass (g)", scale = 100)
get_beta(fledg_model) * 100
ggsave("analysis/output/fledgling_mass_trend.png", height = 5, width = 7) # key figure


fc_cond <- na.omit(flc)
fc_cond %>%
  group_by(year) %>%
  summarize(N = n())
fc_model <- fit_model(year = fc_cond$year, response = fc_cond$condition)
fc_model$sd_rep
plot_model(fc_model, ylab = "Condition (g/cm)", scale = 1)
get_beta(fc_model)
#ggsave("analysis/output/combined_chick_condition_trend.png", height = 5, width = 7)
## not comfortable with this analysis because the chick data appear to be driving the trend
## ...and the gradual nature of the trend does not make biological sense.
## our hypothesis is that chick condition would drop under reduced capelin abundance
## abundance reduced in the early 90's, yet the pattern in condition does not reflect this on/off change



## Chick and fledgling weight ----
flc <- filter(cc, stage != "adult") %>%
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
print(w)
save_plot("analysis/output/ChickFledglingWeight.png", w, base_aspect_ratio = 1.4, base_width = 6, bg = "transparent") # make room for figure legend)

## Chick and fledgling wing length ----
wl <- filter(cc, stage != "adult") %>%
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


## Bootstrap ----------

## Bootstrap for adult and fledgling mass during 2014 and 2016
quantile(sample(x = na.omit(adw[which(adw$year == 2014 & adw$stage == 'adult'), 'bird_weight']), replace = T, size = 100000), probs = c(0.025, 0.975))
quantile(sample(x = na.omit(adw[which(adw$year == 2016 & adw$stage == 'adult'), 'bird_weight']), replace = T, size = 100000), probs = c(0.025, 0.975))
quantile(sample(x = na.omit(adw[which(adw$year == 2014 & adw$stage == 'fledgling'), 'bird_weight']), replace = T, size = 100000), probs = c(0.025, 0.975))
quantile(sample(x = na.omit(adw[which(adw$year == 2016 & adw$stage == 'fledgling'), 'bird_weight']), replace = T, size = 100000), probs = c(0.025, 0.975))


library(data.table)

## bootstrap adult and fledgling mass by year
weights <- data.table(cc[!is.na(cc$bird_weight), c("year", "stage", "bird_weight")])
weights <- weights[!(weights$stage == "fledgling" & weights$bird_weight > 500)] # drop outlier

boot <- vector("list", 1000)
for (i in seq_along(boot)) {
  boot[[i]] <- weights[, list(rep = i, bird_weight = bird_weight[sample.int(.N, replace = TRUE)]), 
                       by = c("year", "stage")]
}
boot <- rbindlist(boot)

boot_means <- boot[, list(mean = mean(bird_weight)), by = c("year", "stage", "rep")]
boot_ci <- boot_means[, list(median = quantile(mean, 0.5), 
                             lower = quantile(mean, 0.025),
                             upper = quantile(mean, 0.975)), by = c("year", "stage")]

weights %>% 
  filter(stage == "fledgling") %>% 
  ggplot(aes(x = year, y = bird_weight)) + geom_point()

boot_means %>% 
  filter(stage == "fledgling") %>% 
  ggplot(aes(x = year, y = mean, group = year)) +
  geom_violin(draw_quantiles = 0.5) + theme_bw()

boot_ci %>% 
  filter(stage == "fledgling") %>% 
  ggplot(aes(x = year, y = median, group = year)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) + 
  geom_point() + theme_bw() +
  xlab("Year") + ylab("Mass (g)") +
  scale_x_continuous(breaks = seq(1980, 2017, 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8))
ggsave("analysis/output/fledgling_mass_boot_means.png", height = 5, width = 7)

boot_ci %>% 
  filter(stage == "adult" & year > 2004) %>% 
  ggplot(aes(x = year, y = median, group = year)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) + 
  geom_point() + theme_bw() +
  xlab("Year") + ylab("Mass (g)") +
  scale_x_continuous(breaks = seq(1980, 2017, 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8))
ggsave("analysis/output/adult_mass_boot_means.png", height = 5, width = 7)




## bootstrap fledgling condition by year
cond <- data.table(cc[!is.na(cc$condition) & cc$stage != "adult", 
                         c("year", "stage", "condition")])
cond <- cond[!(cond$condition > 8)] # drop outliers

boot <- vector("list", 1000)
for (i in seq_along(boot)) {
  boot[[i]] <- cond[, list(rep = i, condition = condition[sample.int(.N, replace = TRUE)]), 
                       by = c("year", "stage")]
}
boot <- rbindlist(boot)

boot_means <- boot[, list(mean = mean(condition)), by = c("year", "stage", "rep")]
boot_ci <- boot_means[, list(median = quantile(mean, 0.5), 
                             lower = quantile(mean, 0.025),
                             upper = quantile(mean, 0.975)), by = c("year", "stage")]

dodge <- position_dodge(width = 1)
boot_ci %>% 
  ggplot(aes(x = year, y = median, colour = stage)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = dodge) + 
  geom_point(position = dodge) + theme_bw() +
  xlab("Year") + ylab("Condition (g/cm)") +
  scale_x_continuous(breaks = seq(1980, 2017, 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8))
ggsave("analysis/output/combined_condition_boot_means.png", height = 5, width = 8)


