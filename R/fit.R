

#' Fit linear regression on data with repeated measures within years
#'
#' @param year        Year vector
#' @param response    Response vector
#'
#' @return
#' @export
#'

fit_model <- function(year = NULL, response = NULL) {

  tmb_data <- list(
    y = response,
    g = as.numeric(factor(year)) - 1,
    Ny = length(response),
    Ng = nlevels(factor(year))
  )
  tmb_data$x <- sort(unique(tmb_data$g))
  tmb_data$years <- sort(unique(year))
  
  start_par <- list(
    alpha = 0,                   
    beta = 0,
    log_sigma = 0,
    mu = rep(0, tmb_data$Ng),
    log_sigma_mu = rep(0, tmb_data$Ng)
  )
  
  obj <- MakeADFun(tmb_data, start_par, DLL = "fit")
  opt <- nlminb(obj$par, obj$fn, obj$gr)
  sd_rep <- sdreport(obj)
  
  list(tmb_data = tmb_data, obj = obj, opt = opt, sd_rep = sd_rep)
  
}


#' Plot output from fit_model
#'
#' @param model Object produced by \code{\link{fit_model}}
#' @param ylab  Y-label for plot
#'
#' @return
#' @export
#'

plot_model <- function(model, ylab = NULL) {
  
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
  
  ggplot() +
    geom_ribbon(aes(x = year, ymin = fits_lwr, ymax = fits_upr), data = fits, fill = "lightgrey") +
    geom_line(aes(x = year, y = fits), data = fits) +
    geom_errorbar(aes(x = year, ymin = mu_lwr, ymax = mu_upr), data = fits, width = 0) +
    geom_point(aes(x = year, y = mu), data = fits) +
    scale_x_continuous(breaks = min(preds$year):max(preds$year), expand = c(0.01, 0)) +
    xlab("Year") + ylab(ylab) +
    cowplot::theme_cowplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
}

#' Helper function for extracting beta estimates from fit_model
#'
#' @param model Object produced by \code{\link{fit_model}}
#'
#' @return
#' @export
#'

get_beta <- function(model) {
  
  beta_se <- summary(model$sd_rep)["beta", ]
  beta <- beta_se["Estimate"]
  lwr <- beta_se["Estimate"] - 1.96 * beta_se["Std. Error"]
  upr <- beta_se["Estimate"] + 1.96 * beta_se["Std. Error"]
  c(Estimate = unname(beta), Lower = unname(lwr), Upper = unname(upr))
  
}

