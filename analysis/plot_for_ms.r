## ADB
## June 25, 2018
## Create figure for manuscript
## A: fish mass, B: offspring condition, C: adult mass (remove line for adult mass plot)

## run prey analysis.r until line 177
## run murre_analysis.r until line 184


plot_model_no_fit <- function(model, ylab = NULL, scale = 1) {
  
  sd_rep <- model$sd_rep
  tmb_data <- model$tmb_data
  vals <- split(sd_rep$value, names(sd_rep$value)) 
  vals <- data.frame(vals)
  sds <- split(sd_rep$sd, names(sd_rep$value))
  sds <- data.frame(sds)
  names(sds) <- paste0(names(sds), "_sd")
  fits <- data.frame(year = tmb_data$year, vals, sds)
  fits$fits_lwr <- fits$fits - 1.96 * fits$fits_sd
  fits$fits_upr <- fits$fits + 1.96 * fits$fits_sd
  fits$mu_lwr <- fits$mu - 1.96 * fits$mu_sd
  fits$mu_upr <- fits$mu + 1.96 * fits$mu_sd
  nms <- c("fits", "fits_lwr", "fits_upr", "mu", "mu_lwr", "mu_upr")
  fits[, nms] <- fits[, nms] * scale
  
  ggplot() +
#    geom_ribbon(aes(x = year, ymin = fits_lwr, ymax = fits_upr), data = fits, fill = "lightgrey") +
#    geom_line(aes(x = year, y = fits), data = fits, colour = "grey60") +
    geom_errorbar(aes(x = year, ymin = mu_lwr, ymax = mu_upr), data = fits, width = 0) +
    geom_point(aes(x = year, y = mu), data = fits) +
    scale_x_continuous(breaks = min(tmb_data$years):max(tmb_data$years), expand = c(0.01, 0)) +
    xlab("Year") + ylab(ylab) +
    cowplot::theme_cowplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
}

## scale used to help the adult model converge
s <- 150
adult_mass_p <- plot_model_no_fit(adult_model, ylab = "Adult mass (g)", scale = s)

fish_mass_p <- plot_model(weight_model, ylab = "Capelin mass (g)")
offspring_condition <- plot_model(fc_model, ylab = "Offspring condition (g/mm)", scale = 1)




cowplot::plot_grid(fish_mass_p + remove_x, offspring_condition + remove_x, adult_mass_p, 
                   ncol = 1, labels = "AUTO", 
                   rel_heights = c(0.8, 0.8, 1),
                   align = "v")
cowplot::ggsave("analysis/output/fig_for_ms.png", height = 10, width = 7)
