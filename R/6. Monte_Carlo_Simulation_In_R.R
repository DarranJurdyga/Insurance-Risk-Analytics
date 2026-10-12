#file 6 collective risk with monte carlo simulation


#1. load packages 

library(dplyr)
library(ggplot2)
select <- dplyr::select
setwd("C:/Insurance-Risk-Analytics")


#2. load data
frequency_data <- read.csv(
  "data/processed/frequency_data.csv",
  stringsAsFactors = FALSE
)

severity_data <- read.csv(
  "data/processed/severity_data.csv",
  stringsAsFactors = FALSE
)

str(frequency_data)
str(severity_data)


#3. load final models
frequency_nb_final <- readRDS(
  "models/frequency_nb_final.rds"
)

evt_gpd <- readRDS(
  "models/evt_gpd_10000.rds"
)

evt_settings <- readRDS(
  "models/evt_settings.rds"
)

frequency_nb_final
evt_gpd
evt_settings


#4. define 2024 portfolio
portfolio_2024 <- frequency_data |>
  filter(year == 2024)

nrow(portfolio_2024)

summary(portfolio_2024$total_exposure)

nrow(portfolio_2024)

names(portfolio_2024)



#5. prepare 2024 portfoilo for frequency model
portfolio_2024 <- portfolio_2024 |>
  
  # Create the same age bands used in the final frequency model
  mutate(
    age_band = cut(
      driver_age,
      breaks = c(
        -Inf,
        24,
        34,
        44,
        54,
        64,
        Inf
      ),
      labels = c(
        "<25",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65+"
      )
    )
  ) |>
  
  # Use the same reference level as the fitted model
  mutate(
    age_band = relevel(
      age_band,
      ref = "25-34"
    )
  )



#6. handle missing predictor values
# Indicator for originally missing vehicle value
portfolio_2024 <- portfolio_2024 |>
  mutate(
    vehicle_value_missing = is.na(vehicle_value)
  )


# Calculate year + policy type medians using the modelling dataset
vehicle_value_medians <- frequency_data |>
  group_by(
    year,
    policy_type
  ) |>
  summarise(
    vehicle_value_median = median(
      vehicle_value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Impute missing vehicle values
portfolio_2024 <- portfolio_2024 |>
  left_join(
    vehicle_value_medians,
    by = c(
      "year",
      "policy_type"
    )
  ) |>
  mutate(
    vehicle_value = if_else(
      is.na(vehicle_value),
      vehicle_value_median,
      vehicle_value
    )
  ) |>
  dplyr::select(
    -vehicle_value_median
  )


# Replace missing fuel type with "Unknown"
portfolio_2024 <- portfolio_2024 |>
  mutate(
    fuel_type = if_else(
      is.na(fuel_type),
      "Unknown",
      fuel_type
    )
  )



#7. group rare vehicle brands
brand_counts <- frequency_data |>
  count(
    vehicle_brand
  )


portfolio_2024 <- portfolio_2024 |>
  left_join(
    brand_counts,
    by = "vehicle_brand"
  ) |>
  mutate(
    vehicle_brand_group = if_else(
      n >= 1000,
      vehicle_brand,
      "Other"
    )
  ) |>
  dplyr::select(
    -n
  )



#8. check the model variables
summary(portfolio_2024$age_band)

table(
  portfolio_2024$fuel_type,
  useNA = "ifany"
)

sum(
  is.na(portfolio_2024$vehicle_value)
)

sum(
  portfolio_2024$vehicle_value_missing
)

table(
  portfolio_2024$vehicle_brand_group
)

summary(portfolio_2024$age_band)
sum(is.na(portfolio_2024$vehicle_value))
sum(portfolio_2024$vehicle_value_missing)
table(portfolio_2024$vehicle_brand_group)

predict(
  frequency_nb_final,
  newdata = portfolio_2024,
  type = "response"
)






#9. predict expected claim frequency
portfolio_2024 <- portfolio_2024 |>
  mutate(
    expected_claims = predict(
      frequency_nb_final,
      newdata = portfolio_2024,
      type = "response"
    )
  )

summary(portfolio_2024$expected_claims)


#10. portfolio frequency check
total_expected_claims <- sum(
  portfolio_2024$expected_claims
)

total_observed_claims <- sum(
  portfolio_2024$total_claims
)

total_exposure <- sum(
  portfolio_2024$total_exposure
)

expected_frequency <- total_expected_claims /
  total_exposure

observed_frequency <- total_observed_claims /
  total_exposure

total_expected_claims
total_observed_claims
total_exposure
expected_frequency
observed_frequency





#11. simulate policy-level claim counts
set.seed(123)

portfolio_2024$simulated_claims <- rnbinom(
  n = nrow(portfolio_2024),
  mu = portfolio_2024$expected_claims,
  size = frequency_nb_final$theta
)


#12. check simulated claim counts
summary(portfolio_2024$simulated_claims)

simulated_total_claims <- sum(
  portfolio_2024$simulated_claims
)

simulated_total_claims






#13. define split severity simulator

# Lognormal parameters from File 7
# The model was fitted to severity measured in € thousands.
lognormal_meanlog <- -0.5888836
lognormal_sdlog <- 1.1221676

# EVT parameters from saved settings
evt_threshold <- evt_settings$threshold
evt_xi <- evt_settings$xi
evt_beta <- evt_settings$beta
evt_tail_probability <- evt_settings$tail_probability


simulate_severity <- function(n) {
  
  is_tail <- runif(n) < evt_tail_probability
  
  severity <- numeric(n)
  
  
  # BODY: Lognormal conditional on severity <= €10,000
  
  
  n_body <- sum(!is_tail)
  
  if (n_body > 0) {
    
    # Lognormal was fitted in € thousands
    threshold_scaled <- evt_threshold / 1000
    
    upper_probability <- plnorm(
      threshold_scaled,
      meanlog = lognormal_meanlog,
      sdlog = lognormal_sdlog
    )
    
    u <- runif(
      n_body,
      min = 0,
      max = upper_probability
    )
    
    severity[!is_tail] <- 1000 * qlnorm(
      u,
      meanlog = lognormal_meanlog,
      sdlog = lognormal_sdlog
    )
  }
  
  
  # TAIL: GPD excess above €10,000
  
  
  n_tail <- sum(is_tail)
  
  if (n_tail > 0) {
    
    u <- runif(n_tail)
    
    excess <- if (abs(evt_xi) > 1e-8) {
      evt_beta / evt_xi *
        ((1 - u)^(-evt_xi) - 1)
    } else {
      -evt_beta * log(1 - u)
    }
    
    severity[is_tail] <- evt_threshold + excess
  }
  
  severity
}



#14. test severity simulator
set.seed(123)

test_severities <- simulate_severity(100000)

summary(test_severities)

quantile(
  test_severities,
  probs = c(0.90, 0.95, 0.99, 0.995, 0.999)
)

mean(test_severities)
max(test_severities)




mean(test_severities > evt_threshold)
sum(test_severities > evt_threshold)






#15. combine claim counts and severities

# Total number of claims in this simulated 2024 portfolio
simulated_total_claims <- sum(
  portfolio_2024$simulated_claims
)

# Generate one severity for every simulated claim
simulated_severities <- simulate_severity(
  simulated_total_claims
)

# Aggregate annual loss
simulated_aggregate_loss <- sum(
  simulated_severities
)

#16. check simulated aggregate loss
simulated_total_claims

simulated_aggregate_loss

mean(simulated_severities)

max(simulated_severities)



#17. check contribution of EVT tail
tail_claims <- simulated_severities > evt_threshold

number_tail_claims <- sum(tail_claims)

tail_loss <- sum(
  simulated_severities[tail_claims]
)

body_loss <- sum(
  simulated_severities[!tail_claims]
)

tail_loss_percentage <- tail_loss / simulated_aggregate_loss

number_tail_claims
tail_loss
body_loss
tail_loss_percentage



#18. derive portfolio-level frequency distribution
# Portfolio expected number of claims
portfolio_mean_claims <- sum(
  portfolio_2024$expected_claims
)

# Portfolio variance implied by the policy-level Negative Binomial model
portfolio_variance_claims <- sum(
  portfolio_2024$expected_claims +
    (portfolio_2024$expected_claims^2 /
       frequency_nb_final$theta)
)

# Moment-matched Negative Binomial size parameter
portfolio_theta <- (
  portfolio_mean_claims^2
) / (
  portfolio_variance_claims -
    portfolio_mean_claims
)

portfolio_mean_claims
portfolio_variance_claims
portfolio_theta


#19. calculate truncated lognormal body moments
threshold_scaled <- evt_threshold / 1000

body_probability <- plnorm(
  threshold_scaled,
  meanlog = lognormal_meanlog,
  sdlog = lognormal_sdlog
)

# First raw moment of Lognormal conditional on X <= threshold
body_mean_scaled <- (
  exp(
    lognormal_meanlog +
      0.5 * lognormal_sdlog^2
  ) *
    pnorm(
      (
        log(threshold_scaled) -
          lognormal_meanlog -
          lognormal_sdlog^2
      ) / lognormal_sdlog
    )
) / body_probability

# Second raw moment
body_second_moment_scaled <- (
  exp(
    2 * lognormal_meanlog +
      2 * lognormal_sdlog^2
  ) *
    pnorm(
      (
        log(threshold_scaled) -
          lognormal_meanlog -
          2 * lognormal_sdlog^2
      ) / lognormal_sdlog
    )
) / body_probability

body_variance_scaled <- (
  body_second_moment_scaled -
    body_mean_scaled^2
)

# Convert from € thousands back to euros
body_mean <- body_mean_scaled * 1000

body_variance <- body_variance_scaled * 1000^2

body_sd <- sqrt(body_variance)

body_probability
body_mean
body_sd


#20. monte carlo aggragate loss simulation
set.seed(123)

n_simulations <- 10000

# Simulate total annual claim counts
simulated_claim_counts <- rnbinom(
  n = n_simulations,
  mu = portfolio_mean_claims,
  size = portfolio_theta
)

# Split each year's claims into body and EVT tail
simulated_tail_counts <- rbinom(
  n = n_simulations,
  size = simulated_claim_counts,
  prob = evt_tail_probability
)

simulated_body_counts <- (
  simulated_claim_counts -
    simulated_tail_counts
)


#body loss
simulated_body_losses <- rnorm(
  n = n_simulations,
  mean = simulated_body_counts * body_mean,
  sd = sqrt(simulated_body_counts * body_variance)
)

# Aggregate body loss cannot be negative
simulated_body_losses <- pmax(
  simulated_body_losses,
  0
)

#evt tail loss
simulated_tail_losses <- numeric(
  n_simulations
)

for (i in seq_len(n_simulations)) {
  
  n_tail <- simulated_tail_counts[i]
  
  if (n_tail > 0) {
    
    u <- runif(n_tail)
    
    excess <- if (abs(evt_xi) > 1e-8) {
      
      evt_beta / evt_xi *
        ((1 - u)^(-evt_xi) - 1)
      
    } else {
      
      -evt_beta * log(1 - u)
    }
    
    simulated_tail_losses[i] <- (
      evt_threshold * n_tail +
        sum(excess)
    )
  }
}


#total aggregate loss
simulated_aggregate_losses <- (
  simulated_body_losses +
    simulated_tail_losses
)

summary(simulated_aggregate_losses)



#monte carlo loss quantiles
quantile(
  simulated_aggregate_losses,
  probs = c(
    0.90,
    0.95,
    0.975,
    0.99,
    0.995,
    0.999
  )
)

#22. extreme simulation check
sort(
  simulated_aggregate_losses,
  decreasing = TRUE
)[1:10]



#23. value at risk and TVAR
confidence_levels <- c(
  0.99,
  0.995,
  0.999
)

VaR <- quantile(
  simulated_aggregate_losses,
  probs = confidence_levels
)

TVaR <- sapply(
  confidence_levels,
  function(p) {
    
    var_p <- quantile(
      simulated_aggregate_losses,
      probs = p
    )
    
    mean(
      simulated_aggregate_losses[
        simulated_aggregate_losses >= var_p
      ]
    )
  }
)

risk_measures <- data.frame(
  confidence_level = confidence_levels,
  VaR = as.numeric(VaR),
  TVaR = as.numeric(TVaR)
)

risk_measures



#24. monte carlo convergence check
simulation_sizes <- c(
  1000,
  2500,
  5000,
  10000,
  25000
)

convergence_results <- data.frame()

set.seed(456)

for (n_sim in simulation_sizes) {
  
  
  # CLAIM COUNTS
  
  
  claim_counts <- rnbinom(
    n = n_sim,
    mu = portfolio_mean_claims,
    size = portfolio_theta
  )
  
  
  # SPLIT INTO BODY AND TAIL
  
  
  tail_counts <- rbinom(
    n = n_sim,
    size = claim_counts,
    prob = evt_tail_probability
  )
  
  body_counts <- claim_counts - tail_counts
  
  
  # BODY LOSSES
  
  
  body_losses <- rnorm(
    n = n_sim,
    mean = body_counts * body_mean,
    sd = sqrt(body_counts * body_variance)
  )
  
  body_losses <- pmax(
    body_losses,
    0
  )
  
  
  # TAIL LOSSES
  
  
  tail_losses <- numeric(n_sim)
  
  for (i in seq_len(n_sim)) {
    
    n_tail <- tail_counts[i]
    
    if (n_tail > 0) {
      
      u <- runif(n_tail)
      
      excess <- evt_beta / evt_xi *
        ((1 - u)^(-evt_xi) - 1)
      
      tail_losses[i] <- (
        evt_threshold * n_tail +
          sum(excess)
      )
    }
  }
  
  aggregate_losses <- body_losses + tail_losses
  
  
  # RISK MEASURES
  
  
  vaR_99 <- quantile(
    aggregate_losses,
    0.99
  )
  
  vaR_995 <- quantile(
    aggregate_losses,
    0.995
  )
  
  vaR_999 <- quantile(
    aggregate_losses,
    0.999
  )
  
  tvaR_99 <- mean(
    aggregate_losses[
      aggregate_losses >= vaR_99
    ]
  )
  
  tvaR_995 <- mean(
    aggregate_losses[
      aggregate_losses >= vaR_995
    ]
  )
  
  tvaR_999 <- mean(
    aggregate_losses[
      aggregate_losses >= vaR_999
    ]
  )
  
  convergence_results <- rbind(
    convergence_results,
    data.frame(
      simulations = n_sim,
      VaR_99 = as.numeric(vaR_99),
      VaR_995 = as.numeric(vaR_995),
      VaR_999 = as.numeric(vaR_999),
      TVaR_99 = as.numeric(tvaR_99),
      TVaR_995 = as.numeric(tvaR_995),
      TVaR_999 = as.numeric(tvaR_999)
    )
  )
}

convergence_results



#The Monte Carlo simulation gives reasonably stable estimates for the 99% and 99.5% portfolio VaR
#as the number of simulations increases, whereas the 99.9% VaR and TVaR
#remain highly sensitive to simulation size because of the heavy-tailed EVT severity model.






#25.lognormal only severity moments

lognormal_mean <- exp(
  lognormal_meanlog +
    0.5 * lognormal_sdlog^2
) * 1000

lognormal_variance <- (
  exp(lognormal_sdlog^2) - 1
) *
  exp(
    2 * lognormal_meanlog +
      lognormal_sdlog^2
  ) *
  1000^2

lognormal_sd <- sqrt(
  lognormal_variance
)

lognormal_mean
lognormal_sd



#26. lognormal only aggregate loss simulation
set.seed(789)

n_comparison_simulations <- 25000

comparison_claim_counts <- rnbinom(
  n = n_comparison_simulations,
  mu = portfolio_mean_claims,
  size = portfolio_theta
)

#lognormal only aggregate loss

lognormal_aggregate_losses <- rnorm(
  n = n_comparison_simulations,
  mean = comparison_claim_counts * lognormal_mean,
  sd = sqrt(
    comparison_claim_counts * lognormal_variance
  )
)

# Aggregate loss cannot be negative
lognormal_aggregate_losses <- pmax(
  lognormal_aggregate_losses,
  0
)

summary(lognormal_aggregate_losses)


#27. lognormal + EVT aggregate loss simulation
set.seed(789)

evt_tail_counts_comparison <- rbinom(
  n = n_comparison_simulations,
  size = comparison_claim_counts,
  prob = evt_tail_probability
)

evt_body_counts_comparison <- (
  comparison_claim_counts -
    evt_tail_counts_comparison
)

#body

evt_body_losses_comparison <- rnorm(
  n = n_comparison_simulations,
  mean = evt_body_counts_comparison * body_mean,
  sd = sqrt(
    evt_body_counts_comparison * body_variance
  )
)

evt_body_losses_comparison <- pmax(
  evt_body_losses_comparison,
  0
)

#evt tail

evt_tail_losses_comparison <- numeric(
  n_comparison_simulations
)

for (i in seq_len(n_comparison_simulations)) {
  
  n_tail <- evt_tail_counts_comparison[i]
  
  if (n_tail > 0) {
    
    u <- runif(n_tail)
    
    excess <- evt_beta / evt_xi *
      ((1 - u)^(-evt_xi) - 1)
    
    evt_tail_losses_comparison[i] <- (
      evt_threshold * n_tail +
        sum(excess)
    )
  }
}

#total

evt_aggregate_losses_comparison <- (
  evt_body_losses_comparison +
    evt_tail_losses_comparison
)

summary(evt_aggregate_losses_comparison)





#28. lognormal vs lognormal + EVT

comparison_levels <- c(
  0.99,
  0.995,
  0.999
)

calculate_risk_measures <- function(losses) {
  
  var_values <- quantile(
    losses,
    probs = comparison_levels
  )
  
  tvar_values <- sapply(
    comparison_levels,
    function(p) {
      
      var_p <- quantile(
        losses,
        probs = p
      )
      
      mean(
        losses[
          losses >= var_p
        ]
      )
    }
  )
  
  data.frame(
    confidence_level = comparison_levels,
    VaR = as.numeric(var_values),
    TVaR = as.numeric(tvar_values)
  )
}

lognormal_risk <- calculate_risk_measures(
  lognormal_aggregate_losses
)

evt_risk <- calculate_risk_measures(
  evt_aggregate_losses_comparison
)

risk_comparison <- data.frame(
  confidence_level = comparison_levels,
  Lognormal_VaR = lognormal_risk$VaR,
  EVT_VaR = evt_risk$VaR,
  Lognormal_TVaR = lognormal_risk$TVaR,
  EVT_TVaR = evt_risk$TVaR
)

risk_comparison



#29. compare aggreagate loss distrubtions 
comparison_plot_data <- data.frame(
  loss = c(
    lognormal_aggregate_losses,
    evt_aggregate_losses_comparison
  ),
  model = rep(
    c(
      "Lognormal",
      "Lognormal + EVT"
    ),
    each = n_comparison_simulations
  )
)

ggplot(
  comparison_plot_data,
  aes(
    x = loss,
    colour = model
  )
) +
  geom_density() +
  coord_cartesian(
    xlim = c(
      0,
      quantile(
        comparison_plot_data$loss,
        0.995
      )
    )
  ) +
  labs(
    title = "Aggregate Loss Distribution: Lognormal vs Lognormal + EVT",
    x = "Aggregate annual loss (€)",
    y = "Density",
    colour = "Severity model"
  ) +
  theme_minimal()



#30. saving results from monte carlo
dir.create(
  "outputs",
  showWarnings = FALSE
)

write.csv(
  risk_measures,
  "outputs/monte_carlo_risk_measures.csv",
  row.names = FALSE
)

write.csv(
  convergence_results,
  "outputs/monte_carlo_convergence.csv",
  row.names = FALSE
)

write.csv(
  risk_comparison,
  "outputs/lognormal_vs_evt_risk_comparison.csv",
  row.names = FALSE
)

saveRDS(
  simulated_aggregate_losses,
  "models/aggregate_losses_evt.rds"
)

saveRDS(
  lognormal_aggregate_losses,
  "models/aggregate_losses_lognormal.rds"
)



#31. final monte carlo summary

cat(
  "\nPortfolio expected claims: ",
  round(portfolio_mean_claims, 2),
  "\n",
  sep = ""
)

cat(
  "Monte Carlo simulations: ",
  n_simulations,
  "\n",
  sep = ""
)

cat(
  "EVT threshold: €",
  evt_threshold,
  "\n",
  sep = ""
)

cat(
  "EVT tail probability: ",
  round(evt_tail_probability, 6),
  "\n",
  sep = ""
)

cat(
  "\nRisk measures:\n"
)

print(risk_measures)

cat(
  "\nLognormal vs Lognormal + EVT:\n"
)

print(risk_comparison)


#32. final risk measure with using 25000 simulations

final_confidence_levels <- c(
  0.99,
  0.995,
  0.999
)

final_VaR <- quantile(
  evt_aggregate_losses_comparison,
  probs = final_confidence_levels
)

final_TVaR <- sapply(
  final_confidence_levels,
  function(p) {
    
    var_p <- quantile(
      evt_aggregate_losses_comparison,
      probs = p
    )
    
    mean(
      evt_aggregate_losses_comparison[
        evt_aggregate_losses_comparison >= var_p
      ]
    )
  }
)

final_risk_measures <- data.frame(
  confidence_level = final_confidence_levels,
  VaR = as.numeric(final_VaR),
  TVaR = as.numeric(final_TVaR)
)

final_risk_measures


#Saving final monte carlo results
write.csv(
  final_risk_measures,
  "outputs/monte_carlo_risk_measures.csv",
  row.names = FALSE
)

write.csv(
  convergence_results,
  "outputs/monte_carlo_convergence.csv",
  row.names = FALSE
)

write.csv(
  risk_comparison,
  "outputs/lognormal_vs_evt_risk_comparison.csv",
  row.names = FALSE
)

saveRDS(
  evt_aggregate_losses_comparison,
  "models/aggregate_losses_evt_25000.rds"
)

saveRDS(
  lognormal_aggregate_losses,
  "models/aggregate_losses_lognormal_25000.rds"
)



#34. final monte carlo summary
cat(
  "\n========================================\n",
  "FINAL MONTE CARLO SUMMARY\n",
  "========================================\n",
  sep = ""
)

cat(
  "Portfolio expected claims: ",
  round(portfolio_mean_claims, 2),
  "\n",
  sep = ""
)

cat(
  "Monte Carlo simulations: ",
  n_comparison_simulations,
  "\n",
  sep = ""
)

cat(
  "EVT threshold: €",
  format(evt_threshold, big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "EVT tail probability: ",
  round(evt_tail_probability, 6),
  "\n",
  sep = ""
)

cat(
  "\nFinal VaR / TVaR:\n"
)

print(final_risk_measures)

cat(
  "\nLognormal vs Lognormal + EVT:\n"
)

print(risk_comparison)

cat(
  "\nMonte Carlo convergence:\n"
)

print(convergence_results)