## ADB - June 27, 2018
## Script to produce regression plots for murre - capelin ms
## 1 plot, 3 panels: A. capelin mass, B. murre offspring condition, C. murre adult mass


# libraries ---------------------------------------------------------------
library(data.table)
library(cowplot)
library(knitr)
library(dplyr)
library(lubridate)

## read data ----
cc <- read.csv("data-raw/COMU_condition_june2018.csv", header = T, as.is = T)

## manipulate data ----
cc <- subset(cc, rec != "")
cc$year <- as.numeric(cc$year)  
cc <- subset(cc, colony == "Funk")
cc <- subset(cc, species == "COMU")
cc$dates <- dmy(cc$date)
cc$bird_weight <- as.numeric(cc$bird_weight)
cc$condition <- cc$bird_weight / cc$winglength
cc[which(cc$stage == "Adult"), "stage"] <- "adult"


## define years used for the x-axis in plots
yrs <- 1990:2017

## Chick and fledgling condition ----
flc <- data.table(filter(cc, stage != "adult" & year >= 1990 & winglength > 30) %>%
  select(year, condition))
flc <- na.omit(flc)

## filtered out chicks with winglength <= 30 mm because they have yet to enter
## the linear portion of the growth curve. The exclusion hleps make fledgling and
## chick condition values comprable

## confidence interval
boot <- vector("list", 10000)
for (i in seq_along(boot)) {
  boot[[i]] <- flc[, list(rep = i, condition = condition[sample.int(.N, replace = TRUE)]), by = c("year")]
}
boot <- rbindlist(boot)
boot_means <- boot[, list(mean = mean(condition)), by = c("year", "rep")]
boot_ci <- boot_means[, list(lower = quantile(mean, 0.025),
                             upper = quantile(mean, 0.975)), by = c("year")]


flc_sum <- cbind(flc[, list(mean = mean(condition)), by = c("year")], boot_ci[,2:3])
flc_sum <- flc_sum[order(year),] 

## alternate result (linear regression with inverse var as the weights)
x <- sort(unique(flc$year))
y <- tapply(flc$condition, flc$year, mean)
wt <- 1 / tapply(flc$condition, flc$year, var)
mod <- lm(y ~ x, weights = wt)
summary(mod)
confint(mod)
predicted <- predict(mod, x = flc$year, se.fit = TRUE, interval = "confidence", level = 0.95)

flc_analysis <- cbind(flc_sum, predicted$fit)
flc_mod <- mod

ylab <- "Offspring condition (g/mm)"
offspring_condition <- ggplot(data = flc_analysis) +
  geom_ribbon(aes(x = year, ymin = lwr, ymax = upr), fill = "lightgrey") +
  geom_line(aes(x = year, y = fit), colour = "grey60") +
  geom_point(aes(x = year, y = mean)) +
  geom_errorbar(aes(x = year, ymin = lower, ymax = upper), width = 0) +
  scale_x_continuous(breaks = yrs, expand = c(0.01, 0), limits = c(yrs[1], yrs[28])) +
  xlab("Year") + ylab(ylab) +
  cowplot::theme_cowplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


## Adult mass ----
adw <- data.table(filter(cc, stage == "adult" & year > 2000 ) %>%
                    select(year, bird_weight))
adw <- na.omit(adw)


## confidence interval
boot <- vector("list", 10000)
for (i in seq_along(boot)) {
  boot[[i]] <- adw[, list(rep = i, bird_weight = bird_weight[sample.int(.N, replace = TRUE)]), by = c("year")]
}
boot <- rbindlist(boot)
boot_means <- boot[, list(mean = mean(bird_weight)), by = c("year", "rep")]
boot_ci <- boot_means[, list(lower = quantile(mean, 0.025),
                             upper = quantile(mean, 0.975)), by = c("year")]


adw_sum <- cbind(adw[, list(mean = mean(bird_weight)), by = c("year")], boot_ci[,2:3])
adw_sum <- adw_sum[order(year),] 

## alternate result (linear regression with inverse var as the weights)
x <- sort(unique(adw$year))
y <- tapply(adw$bird_weight, adw$year, mean)
wt <- 1 / tapply(adw$bird_weight, adw$year, var)
mod <- lm(y ~ x, weights = wt)
summary(mod)
confint(mod)
predicted <- predict(mod, x = adw$year, se.fit = TRUE, interval = "confidence", level = 0.95)

adw_analysis <- cbind(adw_sum, predicted$fit)
adw_mod <- mod

ylab <- "Adult mass (g)"
adult_mass_p <- ggplot(data = adw_analysis) +
#  geom_ribbon(aes(x = year, ymin = lwr, ymax = upr), fill = "lightgrey") +
#  geom_line(aes(x = year, y = fit), colour = "grey60") +
  geom_point(aes(x = year, y = mean)) +
  geom_errorbar(aes(x = year, ymin = lower, ymax = upper), width = 0) +
  scale_x_continuous(breaks = yrs, expand = c(0.01, 0), limits = c(yrs[1], yrs[28])) +
  xlab("Year") + ylab(ylab) +
  cowplot::theme_cowplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# capelin mass ------------------------------------------------------------
prey <- read.csv("data-raw/prey_deliveries.csv", na.strings = c("", "NA", "N/A"),
                 stringsAsFactors = FALSE)
whole_capelin <- prey[prey$prey == "capelin" & grepl("^fresh|^eyeless", prey$digestion),
                      c("year", "mass")]
low_n <- whole_capelin %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  filter(n <= 10) %>% 
  select(year)

prey <- data.table(whole_capelin[!whole_capelin$year %in% low_n$year, ]) # exclude years with less than 10 observations
prey <- na.omit(prey)

## confidence interval
boot <- vector("list", 10000)
for (i in seq_along(boot)) {
  boot[[i]] <- prey[, list(rep = i, mass = mass[sample.int(.N, replace = TRUE)]), by = c("year")]
}
boot <- rbindlist(boot)
boot_means <- boot[, list(mean = mean(mass)), by = c("year", "rep")]
boot_ci <- boot_means[, list(lower = quantile(mean, 0.025),
                             upper = quantile(mean, 0.975)), by = c("year")]


prey_sum <- cbind(prey[, list(mean = mean(mass)), by = c("year")], boot_ci[,2:3])
prey_sum <- prey_sum[order(year),] 

## alternate result (linear regression with inverse var as the weights)
x <- sort(unique(prey$year))
y <- tapply(prey$mass, prey$year, mean)
wt <- 1 / tapply(prey$mass, prey$year, var)
mod <- lm(y ~ x, weights = wt)
summary(mod)
confint(mod)
predicted <- predict(mod, x = prey$year, se.fit = TRUE, interval = "confidence", level = 0.95)

prey_analysis <- cbind(prey_sum, predicted$fit)
prey_mod <- mod

ylab <- "Capelin mass (g)"
fish_mass_p  <- ggplot(data = prey_analysis) +
  geom_ribbon(aes(x = year, ymin = lwr, ymax = upr), fill = "lightgrey") +
  geom_line(aes(x = year, y = fit), colour = "grey60") +
  geom_point(aes(x = year, y = mean)) +
  geom_errorbar(aes(x = year, ymin = lower, ymax = upper), width = 0) +
  scale_x_continuous(breaks = yrs, expand = c(0.01, 0), limits = c(yrs[1], yrs[28])) +
  xlab("Year") + ylab(ylab) +
  cowplot::theme_cowplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


remove_x <- theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
cowplot::plot_grid(fish_mass_p + remove_x, offspring_condition + remove_x, adult_mass_p, 
                   ncol = 1, labels = "AUTO", 
                   rel_heights = c(0.8, 0.8, 1),
                   align = "v")

cowplot::ggsave("analysis/output/murre-capelin-figure.png", height = 10, width = 7)


prey_pars <- data.frame(cbind(slope = coef(prey_mod)[[2]], lower = confint(prey_mod)[2,1], upper = confint(prey_mod)[2,2], model = 'capelin_mass'))
adw_pars <- data.frame(cbind(slope = coef(adw_mod)[[2]], lower = confint(adw_mod)[2,1], upper = confint(adw_mod)[2,2], model = 'adult_weight'))
flc_pars <- data.frame(cbind(slope = coef(flc_mod)[[2]], lower = confint(flc_mod)[2,1], upper = confint(flc_mod)[2,2], model = 'offspring_condition'))

pars <- rbind(prey_pars, flc_pars, adw_pars)           
kable(pars, booktabs = T)
