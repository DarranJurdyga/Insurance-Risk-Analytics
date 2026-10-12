#file 7: Extreme Value Theory

#1. load packages and data
library(dplyr)
library(ggplot2)
library(evir)
library(fitdistrplus)
select <- dplyr::select
setwd("C:/Insurance-Risk-Analytics")

severity_data <- read.csv(
  "data/processed/severity_data.csv",
  stringsAsFactors = FALSE
)

str(severity_data)

summary(severity_data$claim_severity)

nrow(severity_data)

sum(is.na(severity_data$claim_severity))

sum(severity_data$claim_severity <= 0, na.rm = TRUE)



#2. candidate thresholds

threshold_candidates <- c(
  5000,
  7500,
  10000,
  12500,
  15000,
  17500,
  20000,
  22500,
  25000,
  27500,
  30000
)

threshold_candidates


#3. GPD fits across candidate thresholds

gpd_results <- lapply(
  threshold_candidates,
  function(u) {
    
    fit <- gpd(
      severity_data$claim_severity,
      threshold = u,
      method = "ml"
    )
    
    data.frame(
      threshold = u,
      exceedances = fit$n.exceed,
      xi = fit$par.ests["xi"],
      beta = fit$par.ests["beta"],
      xi_se = fit$par.ses["xi"],
      beta_se = fit$par.ses["beta"],
      convergence = fit$converged,
      neg_loglik = fit$nllh.final
    )
  }
)

gpd_results <- bind_rows(gpd_results)

gpd_results


#4. 95% confidence intervals for XI

gpd_results <- gpd_results |>
  mutate(
    xi_lower = xi - 1.96 * xi_se,
    xi_upper = xi + 1.96 * xi_se
  )

gpd_results


#5.Plot GPD shape for parameter stability
ggplot(
  gpd_results,
  aes(
    x = threshold,
    y = xi
  )
) +
  geom_errorbar(
    aes(
      ymin = xi_lower,
      ymax = xi_upper
    ),
    width = 400
  ) +
  geom_point(
    size = 3
  ) +
  geom_line() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "GPD Shape Parameter Stability",
    x = "Threshold (€)",
    y = "Estimated GPD Shape (xi)"
  ) +
  theme_minimal()



#6. plot number of exceedances

ggplot(
  gpd_results,
  aes(
    x = threshold,
    y = exceedances
  )
) +
  geom_point(
    size = 3
  ) +
  geom_line() +
  labs(
    title = "Number of GPD Exceedances by Threshold",
    x = "Threshold (€)",
    y = "Number of exceedances"
  ) +
  theme_minimal()


#7. checking convergence

table(gpd_results$convergence)


#8. select promising threshold for further diagnostics

selected_thresholds <- c(
  7500,
  10000,
  12500,
  15000,
  20000
)

gpd_fits <- lapply(
  selected_thresholds,
  function(u) {
    
    gpd(
      severity_data$claim_severity,
      threshold = u,
      method = "ml"
    )
  }
)

names(gpd_fits) <- paste0(
  "u_",
  selected_thresholds
)

gpd_fits


#9. GPD goodness-of-fit function

gpd_gof <- function(fit, threshold) {
  
  # Observed values above the threshold
  y <- fit$data - threshold
  
  # Estimated GPD parameters
  xi <- fit$par.ests["xi"]
  beta <- fit$par.ests["beta"]
  
  n <- length(y)
  
  
  # Fitted GPD CDF
  if (abs(xi) < 1e-8) {
    
    fitted_prob <- 1 - exp(-y / beta)
    
  } else {
    
    fitted_prob <- 1 -
      (1 + xi * y / beta)^(-1 / xi)
  }
  
  
  # Avoid probabilities of exactly 0 or 1
  fitted_prob <- pmin(
    pmax(fitted_prob, 1e-10),
    1 - 1e-10
  )
  
  
  # Sort fitted probabilities
  u <- sort(fitted_prob)
  
  
  # Kolmogorov-Smirnov statistic
  ks <- max(
    max((1:n) / n - u),
    max(u - ((1:n - 1) / n))
  )
  
  
  # Cramer-von Mises statistic
  cvm <- 1 / (12 * n) +
    sum(
      (
        u -
          (2 * (1:n) - 1) / (2 * n)
      )^2
    )
  
  
  # Anderson-Darling statistic
  ad <- -n -
    (1 / n) *
    sum(
      (2 * (1:n) - 1) *
        (
          log(u) +
            log(1 - rev(u))
        )
    )
  
  
  # GPD log-likelihood
  log_likelihood <- if (abs(xi) < 1e-8) {
    
    sum(
      -log(beta) -
        y / beta
    )
    
  } else {
    
    sum(
      -log(beta) -
        (1 / xi + 1) *
        log(1 + xi * y / beta)
    )
  }
  
  
  data.frame(
    threshold = threshold,
    exceedances = n,
    xi = xi,
    beta = beta,
    KS = ks,
    CvM = cvm,
    AD = ad,
    avg_neg_loglik = -log_likelihood / n
  )
}


#10. goodness of fit comparison

gof_results <- bind_rows(
  gpd_gof(
    gpd_fits$u_7500,
    7500
  ),
  
  gpd_gof(
    gpd_fits$u_10000,
    10000
  ),
  
  gpd_gof(
    gpd_fits$u_12500,
    12500
  ),
  
  gpd_gof(
    gpd_fits$u_15000,
    15000
  ),
  
  gpd_gof(
    gpd_fits$u_20000,
    20000
  )
)

gof_results


#11. final EVT threshold

evt_threshold <- 10000


#12. fit final GPD model

evt_gpd <- gpd(
  severity_data$claim_severity,
  threshold = evt_threshold,
  method = "ml"
)

evt_gpd


#13. extract final GPD parameters

evt_xi <- evt_gpd$par.ests["xi"]

evt_beta <- evt_gpd$par.ests["beta"]

evt_xi

evt_beta


#14. probability of exceeding the EVT threshold

tail_probability <- 1 -
  evt_gpd$p.less.thresh

tail_probability



#15. EVT quantile function

evt_quantile <- function(
    p,
    u,
    xi,
    beta,
    tail_probability
) {
  
  u +
    (beta / xi) *
    (
      (
        tail_probability /
          (1 - p)
      )^xi -
        1
    )
}



# 16. EVT EXTREME QUANTILES
#
# The GPD only applies above the threshold.
# Since the threshold is €10,000, only probabilities above
# P(X <= €10,000) can be modelled using the GPD.

evt_probabilities <- c(
  0.995,
  0.999,
  0.9995,
  0.9999
)

evt_quantiles <- sapply(
  evt_probabilities,
  function(p) {
    
    evt_quantile(
      p = p,
      u = evt_threshold,
      xi = evt_xi,
      beta = evt_beta,
      tail_probability = tail_probability
    )
  }
)

evt_quantiles


#17. fit lognormal model for comparison

severity_scaled <-
  severity_data$claim_severity / 1000

severity_lognorm <- fitdist(
  severity_scaled,
  "lnorm"
)

severity_lognorm


#18. lognormal quantiles
# Multiply by 1000 to convert back from € thousands to euros.

probabilities <- c(
  0.99,
  0.995,
  0.999,
  0.9995,
  0.9999
)

lognormal_quantiles <- 1000 * qlnorm(
  probabilities,
  meanlog = severity_lognorm$estimate["meanlog"],
  sdlog = severity_lognorm$estimate["sdlog"]
)

lognormal_quantiles


#19. empirical + lognormal + EVT comparison

empirical_quantiles <- quantile(
  severity_data$claim_severity,
  probabilities
)

# EVT cannot be used at 99% because 99% is below the
# €10,000 EVT threshold. Therefore return NA there.

evt_quantiles_comparison <- c(
  NA,
  sapply(
    probabilities[-1],
    function(p) {
      
      evt_quantile(
        p = p,
        u = evt_threshold,
        xi = evt_xi,
        beta = evt_beta,
        tail_probability = tail_probability
      )
    }
  )
)

comparison <- data.frame(
  percentile = probabilities,
  empirical = as.numeric(
    empirical_quantiles
  ),
  lognormal = lognormal_quantiles,
  EVT = evt_quantiles_comparison
)

comparison


#21. save final EVT model

dir.create(
  "models",
  showWarnings = FALSE
)

saveRDS(
  evt_gpd,
  "models/evt_gpd_10000.rds"
)


#22. save EVT settings 

evt_settings <- list(
  threshold = evt_threshold,
  xi = evt_xi,
  beta = evt_beta,
  tail_probability = tail_probability
)

saveRDS(
  evt_settings,
  "models/evt_settings.rds"
)


#23. save results tables

dir.create(
  "outputs",
  showWarnings = FALSE
)

write.csv(
  gpd_results,
  "outputs/gpd_threshold_results.csv",
  row.names = FALSE
)

write.csv(
  gof_results,
  "outputs/gpd_goodness_of_fit.csv",
  row.names = FALSE
)

write.csv(
  comparison,
  "outputs/severity_quantile_comparison.csv",
  row.names = FALSE
)


#24. final summary

cat(
  "\nFinal EVT threshold: €",
  evt_threshold,
  "\n",
  sep = ""
)

cat(
  "GPD shape parameter (xi): ",
  round(evt_xi, 4),
  "\n",
  sep = ""
)

cat(
  "GPD scale parameter (beta): €",
  round(evt_beta, 2),
  "\n",
  sep = ""
)

cat(
  "Number of exceedances: ",
  evt_gpd$n.exceed,
  "\n",
  sep = ""
)

cat(
  "Probability of exceeding threshold: ",
  round(tail_probability, 6),
  "\n",
  sep = ""
)


