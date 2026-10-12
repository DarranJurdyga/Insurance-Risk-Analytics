# SEVERITY MODELLING

library(dplyr)
library(ggplot2)
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

severity_summary <- severity_data |>
  summarise(
    n = n(),
    mean = mean(claim_severity),
    median = median(claim_severity),
    sd = sd(claim_severity),
    min = min(claim_severity),
    max = max(claim_severity),
    
    q90 = quantile(
      claim_severity,
      0.90
    ),
    
    q95 = quantile(
      claim_severity,
      0.95
    ),
    
    q99 = quantile(
      claim_severity,
      0.99
    ),
    
    q995 = quantile(
      claim_severity,
      0.995
    ),
    
    q999 = quantile(
      claim_severity,
      0.999
    )
  )

severity_summary

ggplot(
  severity_data,
  aes(x = claim_severity)
) +
  geom_histogram(
    bins = 50
  ) +
  labs(
    title = "Claim Severity Distribution",
    x = "Claim severity (€)",
    y = "Number of claims"
  ) +
  theme_minimal()

ggplot(
  severity_data,
  aes(x = claim_severity)
) +
  geom_histogram(
    bins = 50
  ) +
  scale_x_log10() +
  labs(
    title = "Claim Severity Distribution",
    subtitle = "Logarithmic x-axis",
    x = "Claim severity (€) — log scale",
    y = "Number of claims"
  ) +
  theme_minimal()

################################################################################

severity_scaled <- severity_data$claim_severity / 1000

gamma_start_shape <- mean(severity_scaled)^2 /
  var(severity_scaled)

gamma_start_rate <- mean(severity_scaled) /
  var(severity_scaled)

gamma_start_shape
gamma_start_rate

gamma_fit_scaled <- fitdist(
  severity_scaled,
  "gamma",
  start = list(
    shape = gamma_start_shape,
    rate = gamma_start_rate
  )
)

summary(gamma_fit_scaled)

gamma_shape <- gamma_fit_scaled$estimate["shape"]

gamma_rate <- gamma_fit_scaled$estimate["rate"] / 1000

gamma_mean <- gamma_shape / gamma_rate

gamma_variance <- gamma_shape / gamma_rate^2

gamma_sd <- sqrt(gamma_variance)

gamma_shape
gamma_rate
gamma_mean
gamma_sd

################################################################################

plot.legend <- TRUE

denscomp(
  list(gamma_fit_scaled),
  legendtext = "Gamma"
)
denscomp(
  gamma_fit_scaled,
  main = "Gamma Density Fit"
)
cdfcomp(
  gamma_fit_scaled,
  main = "Gamma CDF Fit"
)
qqcomp(
  gamma_fit_scaled,
  main = "Gamma QQ Plot"
)

gamma_tail_comparison <- data.frame(
  probability = c(
    0.90,
    0.95,
    0.99,
    0.995,
    0.999
  ),
  
  observed = quantile(
    severity_data$claim_severity,
    probs = c(
      0.90,
      0.95,
      0.99,
      0.995,
      0.999
    )
  ),
  
  gamma = qgamma(
    c(
      0.90,
      0.95,
      0.99,
      0.995,
      0.999
    ),
    shape = gamma_shape,
    rate = gamma_rate
  )
)

gamma_tail_comparison

#################################################################################

lognormal_fit <- fitdist(
  severity_scaled,
  "lnorm"
)

summary(lognormal_fit)

lognormal_meanlog <- lognormal_fit$estimate["meanlog"]

lognormal_sdlog <- lognormal_fit$estimate["sdlog"]

lognormal_meanlog
lognormal_sdlog

lognormal_mean_scaled <- exp(
  lognormal_meanlog +
    lognormal_sdlog^2 / 2
)

lognormal_variance_scaled <- (
  exp(lognormal_sdlog^2) - 1
) *
  exp(
    2 * lognormal_meanlog +
      lognormal_sdlog^2
  )

lognormal_mean <- lognormal_mean_scaled * 1000

lognormal_sd <- sqrt(
  lognormal_variance_scaled
) * 1000

lognormal_mean
lognormal_sd

lognormal_tail_comparison <- data.frame(
  probability = c(
    0.90,
    0.95,
    0.99,
    0.995,
    0.999
  ),
  
  observed = quantile(
    severity_data$claim_severity,
    probs = c(
      0.90,
      0.95,
      0.99,
      0.995,
      0.999
    )
  ),
  
  gamma = qgamma(
    c(
      0.90,
      0.95,
      0.99,
      0.995,
      0.999
    ),
    shape = gamma_shape,
    rate = gamma_rate
  ),
  
  lognormal = qlnorm(
    c(
      0.90,
      0.95,
      0.99,
      0.995,
      0.999
    ),
    meanlog = lognormal_meanlog + log(1000),
    sdlog = lognormal_sdlog
  )
)

lognormal_tail_comparison

################################################################################

denscomp(
  lognormal_fit,
  main = "Lognormal Density Fit"
)

cdfcomp(
  lognormal_fit,
  main = "Lognormal CDF Fit"
)

qqcomp(
  lognormal_fit,
  main = "Lognormal QQ Plot"
)

distribution_comparison <- data.frame(
  probability = c(
    0.90,
    0.95,
    0.99,
    0.995,
    0.999
  ),
  
  observed = quantile(
    severity_data$claim_severity,
    probs = c(
      0.90,
      0.95,
      0.99,
      0.995,
      0.999
    )
  ),
  
  gamma = qgamma(
    c(
      0.90,
      0.95,
      0.99,
      0.995,
      0.999
    ),
    shape = gamma_shape,
    rate = gamma_rate
  ),
  
  lognormal = qlnorm(
    c(
      0.90,
      0.95,
      0.99,
      0.995,
      0.999
    ),
    meanlog = lognormal_meanlog + log(1000),
    sdlog = lognormal_sdlog
  )
)

distribution_comparison

################################################################################

tail_thresholds <- c(
  5000,
  7500,
  10000,
  15000,
  20000,
  25000,
  30000,
  50000,
  100000
)

tail_summary <- data.frame(
  threshold = tail_thresholds
) |>
  mutate(
    exceedances = sapply(
      threshold,
      function(u) {
        sum(
          severity_data$claim_severity > u
        )
      }
    ),
    
    proportion = exceedances /
      nrow(severity_data)
  )

tail_summary

tail_data <- severity_data |>
  arrange(claim_severity) |>
  mutate(
    rank = row_number(),
    empirical_survival =
      (n() - rank + 1) / n()
  ) |>
  filter(
    claim_severity >= 2000
  )

ggplot(
  tail_data,
  aes(
    x = claim_severity,
    y = empirical_survival
  )
) +
  geom_point(
    size = 1
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Empirical Claim Severity Survival Function",
    subtitle = "Extreme-tail behaviour",
    x = "Claim severity (€) — log scale",
    y = "P(X > x) — log scale"
  ) +
  theme_minimal()

mean_excess <- data.frame(
  threshold = seq(
    1000,
    30000,
    by = 250
  )
) |>
  mutate(
    exceedances = sapply(
      threshold,
      function(u) {
        sum(
          severity_data$claim_severity > u
        )
      }
    ),
    
    mean_excess = sapply(
      threshold,
      function(u) {
        excesses <-
          severity_data$claim_severity[
            severity_data$claim_severity > u
          ] - u
        
        if (length(excesses) > 0) {
          mean(excesses)
        } else {
          NA_real_
        }
      }
    )
  ) |>
  filter(
    exceedances >= 50
  )

ggplot(
  mean_excess,
  aes(
    x = threshold,
    y = mean_excess
  )
) +
  geom_line() +
  geom_point(
    size = 1
  ) +
  labs(
    title = "Mean Excess Plot",
    x = "Threshold (€)",
    y = "Mean excess (€)"
  ) +
  theme_minimal()

################################################################################


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

threshold_check <- data.frame(
  threshold = threshold_candidates
) |>
  mutate(
    exceedances = sapply(
      threshold,
      function(u) {
        sum(
          severity_data$claim_severity > u
        )
      }
    ),
    
    mean_excess = sapply(
      threshold,
      function(u) {
        excesses <-
          severity_data$claim_severity[
            severity_data$claim_severity > u
          ] - u
        
        mean(excesses)
      }
    )
  )

threshold_check


severity_model_comparison <- data.frame(
  distribution = c(
    "Gamma",
    "Lognormal"
  ),
  
  AIC = c(
    gamma_fit_scaled$aic,
    lognormal_fit$aic
  ),
  
  BIC = c(
    gamma_fit_scaled$bic,
    lognormal_fit$bic
  )
)

severity_model_comparison


extreme_probabilities <- c(
  0.99,
  0.995,
  0.999,
  0.9995,
  0.9999
)

extreme_comparison <- data.frame(
  percentile = extreme_probabilities,
  
  empirical = as.numeric(
    quantile(
      severity_data$claim_severity,
      extreme_probabilities
    )
  ),
  
  gamma = qgamma(
    extreme_probabilities,
    shape = gamma_shape,
    rate = gamma_rate
  ),
  
  lognormal = 1000 * qlnorm(
    extreme_probabilities,
    meanlog = lognormal_meanlog,
    sdlog = lognormal_sdlog
  )
)

extreme_comparison

#file 5 conclusion

# Claim severity is strongly right-skewed, with a substantial upper tail.
#
# The Gamma distribution provides a reasonable description of the central
# severity distribution but substantially underestimates the most extreme
# observations.
#
# The Lognormal distribution provides a substantially better overall fit,
# supported by lower AIC and BIC values and improved agreement with the
# empirical upper quantiles.
#
# However, the Lognormal still underestimates the most extreme observed
# severities, particularly at very high quantiles.
#
# The empirical survival function and increasing mean excess function indicate
# that the upper tail warrants further investigation.
#
# Therefore, Extreme Value Theory is investigated in File 7 using a
# Generalized Pareto Distribution for observations exceeding a high threshold.