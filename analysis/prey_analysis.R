
library(ggplot2)
library(dplyr)
library(TMB)
library(glmmTMB)

TMB::compile("inst/fit.cpp")  
dyn.load("inst/fit")
source("R/fit.R")

## data import and quick exploration -------------------------------------------

prey <- read.csv("data-raw/prey_deliveries.csv", na.strings = c("", "NA", "N/A"),
                 stringsAsFactors = FALSE)
prey$tail_length <- as.numeric(prey$tail_length)
prey$prey_maturity <- tolower(prey$prey_maturity)
head(prey)
table(prey$prey)


with(prey, plot(year, fork_length))
with(prey, plot(year, tail_length))
with(prey, plot(year, mass))

ggplot(prey) + geom_violin(aes(x = year, y = tail_length, group = year))
ggplot(prey, aes(x = as.factor(year), y = mass)) + 
    geom_violin(fill = "steelblue") + 
    geom_jitter(alpha = 0.2, width = 0.2, height = 0.2) +
    theme_minimal()


## percent gravid --------------------------------------------------------------

female_capelin <- prey[prey$prey == "capelin" & prey$prey_sex == "female", ]
female_capelin$prey_maturity[female_capelin$prey_maturity == "immature"] <- NA
female_capelin$prey_maturity[female_capelin$prey_maturity == "unknown"] <- NA
female_capelin <- female_capelin[!is.na(female_capelin$prey_maturity), ]
female_capelin$prey_maturity <- factor(female_capelin$prey_maturity, levels = c("spent", "gravid"))

prop <- female_capelin %>% 
    group_by(year, prey_maturity) %>% 
    summarise(n = n()) %>%
    mutate(prop = n / sum(n)) %>% 
    mutate(lab = scales::percent(round(prop, 2))) %>% 
    mutate(lab = ifelse(prey_maturity == "spent", "", lab))

ggplot(prop, aes(x = as.factor(year), y = prop, fill = prey_maturity)) + 
    geom_bar(stat = "identity") +
    geom_text(label = prop$lab, nudge_y = -0.02, size = 3) +
    scale_fill_manual(values = c("grey90", "khaki1"), name = "Maturity") +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
    xlab("Year") + ylab("Percent") +
    cowplot::theme_cowplot() + cowplot::panel_border(colour = "black") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("analysis/output/percent_gravid_spent.png", height = 5, width = 9)


## prey composition ------------------------------------------------------------

prey_comp <- prey
main_prey <- c("capelin", "sandlance")
prey_comp$prey[!prey_comp$prey %in% main_prey] <- "other"
prey_comp$prey_sex[is.na(prey_comp$prey_sex)] <- "unknown"
prey_comp$prey_sex[!prey_comp$prey_sex %in% c("male", "female")] <- "unknown"
prey_comp$prey <- ifelse(prey_comp$prey == "capelin" & prey_comp$prey_sex != "unknown", 
                         paste(prey_comp$prey_sex, prey_comp$prey),
                         prey_comp$prey)
main_prey <- c("other", "sandlance", 
               "capelin", "male capelin", "female capelin")

prop <- prey_comp %>% 
    group_by(year, prey) %>% 
    summarise(n = n()) %>%
    mutate(prop = n / sum(n))
prop$prey <- factor(prop$prey, levels = main_prey)

ggplot(prop, aes(x = as.factor(year), y = prop, fill = prey)) + 
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Pastel1", direction = -1, name = "Prey") +
    #scale_fill_viridis_d(direction = -1, alpha = 0.5) +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
    xlab("Year") + ylab("Percent") +
    cowplot::theme_cowplot() + cowplot::panel_border(colour = "black") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("analysis/output/prey_comp.png", height = 5, width = 9)



## condition -------------------------------------------------------------------


prey$length <- ifelse(is.na(prey$fork_length), 
                      prey$tail_length - (prey$tail_length * 0.067),
                      prey$fork_length) / 10
prey$condition <- 1000 * (prey$mass / prey$length ^ 3)

whole_capelin <- prey[prey$prey == "capelin" & grepl("^fresh|^eyeless", prey$digestion),
                   c("year", "month", "day", "mass", "length", "condition")]
whole_capelin <- na.omit(whole_capelin)
whole_capelin <- whole_capelin[whole_capelin$condition < 10, ] # condition values > 10 assumed to be a measurement error
whole_capelin <- whole_capelin[whole_capelin$length > 9, ] # lengths less than 9 are assumed to be an error

low_n <- whole_capelin %>% 
    group_by(year) %>% 
    summarise(n = n()) %>% 
    filter(n <= 10) %>% 
    select(year)

whole_capelin <- whole_capelin[!whole_capelin$year %in% low_n$year, ] # exclude years with less than 10 observations

ggplot(whole_capelin, aes(x = year, y = condition, group = year)) + 
    geom_violin(fill = "grey") + 
    #geom_jitter(alpha = 0.2, width = 0.2, height = 0.2) +
    theme_minimal()

ggplot(whole_capelin, aes(x = year, y = mass, group = year)) + 
    geom_violin(fill = "grey") + 
    #geom_jitter(alpha = 0.2, width = 0.2, height = 0.2) +
    theme_minimal()

ggplot(whole_capelin, aes(x = year, y = length, group = year)) + 
    geom_violin(fill = "grey") + 
    #geom_jitter(alpha = 0.2, width = 0.2, height = 0.2) +
    theme_minimal()



fit <- glmmTMB(condition ~ year + (1|year), data = whole_capelin)
whole_capelin$fits <- predict(fit)
whole_capelin$resids <- whole_capelin$fits - whole_capelin$condition
plot(resids ~ fits, data = whole_capelin)

## predictions of annual condition
preds <- data.frame(year = sort(unique(whole_capelin$year)))
preds <- cbind(preds, predict(fit, newdata = preds, se.fit = TRUE))
preds$lwr <- preds$fit - 1.96 * preds$se.fit
preds$upr <- preds$fit + 1.96 * preds$se.fit

## simple prediction of trend given intercept and slope
## note that I extend the line a little by adding a year on both ends
preds2 <- data.frame(year = (min(preds$year) - 0.5):(max(preds$year) + 0.5))
intercept <- fit$fit$par[1]
slope <- fit$fit$par[2]
preds2$linear_fit <- intercept + slope * preds2$year

ggplot() + 
    geom_violin(aes(x = year, y = condition, group = year), 
                col = "grey", fill = "grey", data = whole_capelin) +
    geom_errorbar(aes(x = year, ymin = lwr, ymax = upr), width = 0, data = preds) + 
    geom_point(aes(x = year, y = fit), data = preds) + 
    geom_line(aes(x = year, y = linear_fit), data = preds2) +
    scale_x_continuous(breaks = min(preds$year):max(preds$year), expand = c(0.01, 0)) +
    xlab("Year") + ylab("Condition (Fulton's K)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# ggsave("analysis/output/condition_trend.png", height = 5, width = 8)


## Not sure I like this model structure...hard to relate to random intercepts in 
## this context...


## custom TMB analysis of condition --------------------------------------------

condition_model <- fit_model(year = whole_capelin$year, response = whole_capelin$condition)
weight_model <- fit_model(year = whole_capelin$year, response = whole_capelin$mass)
length_model <- fit_model(year = whole_capelin$year, response = whole_capelin$length)

p1 <- plot_model(condition_model, ylab = "Condition (Fulton's K)")
p2 <- plot_model(weight_model, ylab = "Mass (g)")
p3 <- plot_model(length_model, ylab = "Length (cm)")
remove_x <- theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
cowplot::plot_grid(p1 + remove_x, p2 + remove_x, p3, 
                   ncol = 1, labels = "AUTO", 
                   rel_heights = c(0.8, 0.8, 1),
                   align = "v")
cowplot::ggsave("analysis/output/condition_trend.png", height = 10, width = 7)

get_beta(condition_model)
get_beta(weight_model)
get_beta(length_model)


# ## compare estimates with alternate approach
# summary(lm(condition ~ year, data = whole_capelin))
# 
# raw_means <- whole_capelin %>% 
#     group_by(year) %>% 
#     summarise(condition = mean(condition))
# 
# lin_mod <- lm(condition ~ year, data = raw_means)
# summary(lin_mod)
# summary(condition_model$sd_rep)["beta", ]
# 
# summary(lm(length ~ year, data = whole_capelin))
# 
# raw_means <- whole_capelin %>% 
#     group_by(year) %>% 
#     summarise(length = mean(length))
# 
# lin_mod <- lm(length ~ year, data = raw_means)
# summary(lin_mod)
# summary(length_model$sd_rep)["beta", ]





