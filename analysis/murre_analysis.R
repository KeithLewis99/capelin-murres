## load libraries ----
# library(ggplot2)
library(lubridate)
library(cowplot)
library(dplyr)

library(TMB)
library(tmbstan)

TMB::compile("inst/fit.cpp")
dyn.load("inst/fit")
source("R/fit.R")

############################
### COMU CONDITION DATA#########
############################

## read data ----
cc <- read.csv("data-raw/COMU_condition_june2018.csv", header = T, as.is = T)
#cc2 <- read.csv("data-raw/COMU_condition_consolidated.csv", header = T, as.is = T)

cc <- subset(cc, rec != "")
cc$year <- as.numeric(cc$year)  

## manipulate data ----
cc <- subset(cc, colony == "Funk")
cc <- subset(cc, species == "COMU")
cc$dates <- dmy(cc$date)
cc$bird_weight <- as.numeric(cc$bird_weight)
cc$condition <- cc$bird_weight / cc$winglength
cc[which(cc$stage == "Adult"), "stage"] <- "adult"

figa1 <- cc %>% 
  filter(stage != "adult") %>% 
  ggplot(aes(y = bird_weight, 
             x = winglength, 
             fill = stage)) +
  geom_point(shape = 21, color = 'black', size = 2) + 
  ylim(0, 400) + 
 # xlim(0, 100) +
  ylab("Mass (g)") + 
  xlab("Wing length (mm)") + 
  theme_cowplot() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8),
  #       axis.text.y = element_text(hjust = 1, vjust = 0, size = 8)) +
  scale_fill_manual(name = 'Stage', values = c('white', 'grey40'))


cowplot::ggsave("analysis/output/figa1.png")



d <- cc[cc$year == 1998, c("stage", "bird_weight", "winglength")]
nrow(d)
nrow(unique(d))

## TODO:
## - Figure out what's going on with the 1998 data
## - Filter out the small chicks outside linear portion of growth
## - Fix spelling of Fulton's K - DONE

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
#save_plot("analysis/output/AdultWeights.png", g, base_aspect_ratio = 1.4, base_width = 6, bg = "transparent") # make room for figure legend)


## scaled to help model converge
s <- 150 ## ADB, June 26, 2018. Changed this scaling from 126 to 150 to help convergence
weights <- adw[!is.na(adw$bird_weight), ]
adult_model <- fit_model(
  year = weights$year,
  response = weights$bird_weight / s
)
adult_model$sd_rep # convergance issues
plot_model(adult_model, ylab = "Mass (g)", scale = s)
ggsave("analysis/output/adult_mass_trend.png", height = 5, width = 7)
get_beta(adult_model) * s

fit <- tmbstan(adult_model$obj, chains = 3, iter = 10000)
# traceplot(fit, pars = names(adult_model$obj$par), inc_warmup = FALSE)
# pairs(fit)
# fit
get_beta(adult_model) * s
summary(fit)$summary["beta", c("mean", "2.5%", "97.5%")] * s
## running into a convergence issues here. I think it is because of the low
## beta value. Fiddled with scale. Still not happy with the gradiant, but param
## estimates are similar to those obtained from tmbstan

## alternate result (linear regression with inverse sd as the weights)
x <- sort(unique(weights$year))
y <- tapply(weights$bird_weight, weights$year, mean)
wt <- 1 / tapply(weights$bird_weight, weights$year, sd)
mod <- lm(y ~ x, weights = wt)
summary(mod)
confint(mod)


## Chick and fledgling condition ----

flc <- filter(cc, stage != "adult" & year >= 1990 & winglength > 30) %>%
  select(year, condition, stage, bird_weight, winglength)
## filtered out chicks with winglength <= 30 mm because they have yet to enter
## the linear portion of the growth curve. The exclusion hleps make fledgling and
## chick condition values comprable

y <- 1990:2017
flc <- rbind(flc, data.frame(year = y[which(!y %in% sort(as.integer(unique(flc$year))))], condition = NA, stage = "chick", bird_weight = NA, winglength = NA))
# get rid of 2 outliers
#flc[which(flc$condition > 9), "condition"] <- NA
summary(lm(condition ~ as.integer(year) + as.factor(stage), data = flc))

g <- ggplot(flc, aes(as.factor(year), condition))
g <- g + geom_violin()
g <- g + stat_summary(aes(as.factor(year)), fun.y = mean, geom = "point", fill = "black", shape = 21, size = 1.5)
g <- g + xlab("Year") + ylab("Condition (g/mm)")
#g <- g + facet_grid(stage ~ ., scales = "free")
g <- g + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8))
g <- g + ylim(2,7)
print(g)
save_plot("analysis/output/OffspringCondition.png", g, base_aspect_ratio = 1.4, base_width = 6, bg = "transparent") # make room for figure legend)


dodge <- position_dodge(width = 1)
g <- ggplot(flc, aes(as.factor(year), condition, fill = stage)) +
  geom_violin(position = dodge, draw_quantiles = 0.5) +
  #geom_boxplot(width = 0.1, outlier.colour = "grey", outlier.size = 0, position = dodge) +
  theme_bw() + xlab("Year") + ylab("Condition (g/mm)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8))
save_plot("analysis/output/OffspringCondition_dodge.png", g, 
          base_aspect_ratio = 1.4, base_width = 8, bg = "transparent",
          dpi = 600)


chick_cond <- na.omit(flc[flc$stage == "chick", ])
chick_cond %>%
  group_by(year) %>%
  summarize(N = n())


## ADB: June 2018: this is no longer valid with the data sert that Bill sent
## n98 = 23  -- this changes the condition trend greatly
#chick_cond <- chick_cond[chick_cond$year != 1998, ] # drop 98 because of low sample size
chick_model <- fit_model(year = chick_cond$year, response = chick_cond$condition)
chick_model$sd_rep
plot_model(chick_model, ylab = "Condition (g/mm)", scale = 1)

fledg_cond <- na.omit(flc[flc$stage == "fledgling", ])
fledg_cond %>%
  group_by(year) %>%
  summarize(N = n())
fledg_cond <- fledg_cond[fledg_cond$year != 1997, ] # drop 97 because of low sample size
fledg_model <- fit_model(year = fledg_cond$year, response = fledg_cond$condition)
fledg_model$sd_rep
plot_model(fledg_model, ylab = "Condition (g/mm)", scale = 1)
get_beta(fledg_model)
ggsave("analysis/output/fledgling_condition_trend.png", height = 5, width = 7)

fledg_model <- fit_model(year = fledg_cond$year, response = fledg_cond$bird_weight / 100)
fledg_model$sd_rep
plot_model(fledg_model, ylab = "Mass (g)", scale = 100)
get_beta(fledg_model) * 100
ggsave("analysis/output/fledgling_mass_trend.png", height = 5, width = 7) # key figure

fledg_model <- fit_model(year = fledg_cond$year, response = fledg_cond$winglength / 10)
fledg_model$sd_rep
plot_model(fledg_model, ylab = "Wing length (mm)", scale = 10)
get_beta(fledg_model) * 10
ggsave("analysis/output/fledgling_winglength_trend.png", height = 5, width = 7) # key figure


fc_cond <- na.omit(flc)
fc_cond %>%
  group_by(year) %>%
  summarize(N = n())
fc_model <- fit_model(year = fc_cond$year, response = fc_cond$condition)
fc_model$sd_rep
plot_model(fc_model, ylab = "Condition (g/mm)", scale = 1)
get_beta(fc_model)
ggsave("analysis/output/combined_chick_condition_trend.png", height = 5, width = 7)


## alternate result (linear regression with inverse sd as the weights)
x <- sort(unique(fc_cond$year))
y <- tapply(fc_cond$condition, fc_cond$year, mean)
wt <- 1 / tapply(fc_cond$condition, fc_cond$year, sd)
mod <- lm(y ~ x, weights = wt)
summary(mod)
confint(mod)
get_beta(fc_model)


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
ww <- ww + xlab("Year") + ylab("Wing length (mm)")
ww <- ww + facet_grid(stage ~ ., scales = "free")
# w <- w + theme_cowplot(font_size = 10) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))
ww <- ww + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ww <- ww + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8))
print(ww)
save_plot("analysis/output/ChickFledglingWlength.png", ww, base_aspect_ratio = 1.4, base_width = 6, bg = "transparent") # make room for figure legend)


## Bootstrap ----------
library(data.table)

## bootstrap adult and fledgling mass by year
weights <- data.table(cc[!is.na(cc$bird_weight), c("year", "stage", "bird_weight")])
weights <- weights[!(weights$stage == "fledgling" & weights$bird_weight > 500)] # drop outlier

boot <- vector("list", 10)
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
cond <- cond[cond$year > 1990, ]

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
  xlab("Year") + ylab("Condition (g/mm)") +
  scale_x_continuous(breaks = seq(1980, 2017, 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 8))
ggsave("analysis/output/combined_condition_boot_means.png", height = 5, width = 8)


