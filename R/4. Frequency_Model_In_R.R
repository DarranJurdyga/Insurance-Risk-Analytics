# MOTOR INSURANCE RISK ANALYTICS
# FILE 4: CLAIM FREQUENCY MODELLING


library(dplyr)
library(ggplot2)
library(MASS)
select <- dplyr::select
setwd("C:/Insurance-Risk-Analytics")



#1. load data
frequency_data <- read.csv(
  "data/processed/frequency_data.csv",
  stringsAsFactors = FALSE
)

str(frequency_data)



#2. baseline poisoon model
frequency_model <- glm(
  total_claims ~ driver_age +
    vehicle_value +
    power_to_weight_ratio +
    policy_type +
    fuel_type +
    vehicle_brand +
    year +
    offset(log(total_exposure)),
  family = poisson(link = "log"),
  data = frequency_data
)

summary(frequency_model)


#3. poisson model diagnostics
# Deviance dispersion
deviance(frequency_model) /
  df.residual(frequency_model)

# Pearson dispersion
sum(
  residuals(frequency_model, type = "pearson")^2
) / df.residual(frequency_model)

# Model information
AIC(frequency_model)
summary(frequency_model)$deviance
summary(frequency_model)$aic

# Observed claim-count distribution
table(frequency_data$total_claims)


#4. observed compared to poisson expected claim counts
observed_zero_rate <- mean(frequency_model$y == 0)

expected_zero_rate <- mean(
  dpois(
    0,
    lambda = fitted(frequency_model)
  )
)

observed_zero_rate
expected_zero_rate


observed_counts <- table(frequency_model$y)

expected_counts <- sapply(
  0:10,
  function(k) {
    sum(
      dpois(
        k,
        lambda = fitted(frequency_model)
      )
    )
  }
)

comparison <- data.frame(
  claims = 0:10,
  observed = as.numeric(
    observed_counts[as.character(0:10)]
  ),
  expected = expected_counts
)

comparison


#5. poisson count distribution plots
lambda_hat <- fitted(frequency_model)

count_comparison <- data.frame(
  claims = 0:10,
  
  observed = sapply(
    0:10,
    function(k) {
      sum(frequency_model$y == k)
    }
  ),
  
  expected = sapply(
    0:10,
    function(k) {
      sum(
        dpois(
          k,
          lambda = lambda_hat
        )
      )
    }
  )
)


# Linear scale

ggplot(count_comparison, aes(x = claims)) +
  geom_col(
    aes(y = observed),
    width = 0.6
  ) +
  geom_point(
    aes(y = expected),
    size = 3
  ) +
  geom_line(
    aes(y = expected),
    linewidth = 1
  ) +
  labs(
    title = "Observed vs Poisson Expected Claim Counts",
    x = "Number of claims",
    y = "Number of policy-years"
  ) +
  theme_minimal()


# Logarithmic scale

ggplot(count_comparison, aes(x = claims)) +
  geom_col(
    aes(y = observed),
    width = 0.6
  ) +
  geom_point(
    aes(y = expected),
    size = 3
  ) +
  geom_line(
    aes(y = expected),
    linewidth = 1
  ) +
  scale_y_log10() +
  labs(
    title = "Observed vs Poisson Expected Claim Counts",
    subtitle = "Logarithmic y-axis",
    x = "Number of claims",
    y = "Number of policy-years (log scale)"
  ) +
  theme_minimal()



#6. inital negative binomial model
frequency_nb <- glm.nb(
  total_claims ~ driver_age +
    vehicle_value +
    power_to_weight_ratio +
    policy_type +
    fuel_type +
    vehicle_brand +
    year +
    offset(log(total_exposure)),
  data = frequency_data
)

summary(frequency_nb)


#7. negative binomial compared to poisson
lambda_nb <- fitted(frequency_nb)

expected_counts_nb <- sapply(
  0:10,
  function(k) {
    sum(
      dnbinom(
        k,
        mu = lambda_nb,
        size = frequency_nb$theta
      )
    )
  }
)

comparison_models <- data.frame(
  claims = 0:10,
  
  observed = sapply(
    0:10,
    function(k) {
      sum(frequency_nb$y == k)
    }
  ),
  
  poisson = sapply(
    0:10,
    function(k) {
      sum(
        dpois(
          k,
          lambda = fitted(frequency_model)
        )
      )
    }
  ),
  
  negative_binomial = expected_counts_nb
)

comparison_models


ggplot(comparison_models, aes(x = claims)) +
  geom_col(
    aes(y = observed),
    width = 0.6
  ) +
  geom_point(
    aes(y = poisson),
    size = 2.5
  ) +
  geom_line(
    aes(y = poisson),
    linewidth = 0.8
  ) +
  geom_point(
    aes(y = negative_binomial),
    size = 2.5
  ) +
  geom_line(
    aes(y = negative_binomial),
    linewidth = 0.8
  ) +
  scale_y_log10() +
  labs(
    title = "Observed vs Poisson and Negative Binomial",
    subtitle = "Logarithmic y-axis",
    x = "Number of claims",
    y = "Number of policy-years (log scale)"
  ) +
  theme_minimal()


# Pearson dispersion for Negative Binomial

sum(
  residuals(frequency_nb, type = "pearson")^2
) / df.residual(frequency_nb)


#8. inital age-band diagnostic
nb_used <- model.frame(frequency_nb)

nb_used$fitted_claims <- fitted(frequency_nb)

nb_used$exposure <- exp(
  model.offset(nb_used)
)

frequency_age_bands <- nb_used |>
  mutate(
    age_band = cut(
      driver_age,
      breaks = c(-Inf, 24, 34, 44, 54, 64, Inf),
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
  group_by(age_band) |>
  summarise(
    observed_frequency =
      sum(total_claims) / sum(exposure),
    
    fitted_frequency =
      sum(fitted_claims) / sum(exposure),
    
    n = n(),
    
    .groups = "drop"
  )

frequency_age_bands


ggplot(
  frequency_age_bands,
  aes(x = age_band)
) +
  geom_point(
    aes(y = observed_frequency),
    size = 3
  ) +
  geom_line(
    aes(
      y = fitted_frequency,
      group = 1
    ),
    linewidth = 1
  ) +
  geom_point(
    aes(y = fitted_frequency),
    size = 3
  ) +
  labs(
    title = "Observed vs Negative Binomial Claim Frequency by Driver Age",
    x = "Driver age",
    y = "Claim frequency"
  ) +
  theme_minimal()


#9. create modelling data with age bands
frequency_model_data <- frequency_data |>
  mutate(
    # Keep track of which vehicle values were originally missing
    vehicle_value_missing = is.na(vehicle_value),
    
    # Treat missing fuel type as its own category
    fuel_type = if_else(
      is.na(fuel_type),
      "Unknown",
      fuel_type
    ),
    
    # Create age bands
    age_band = factor(
      cut(
        driver_age,
        breaks = c(-Inf, 24, 34, 44, 54, 64, Inf),
        labels = c(
          "<25",
          "25-34",
          "35-44",
          "45-54",
          "55-64",
          "65+"
        )
      ),
      levels = c(
        "<25",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65+"
      )
    ),
    
    # Use 25-34 as regression reference category
    age_band = relevel(
      age_band,
      ref = "25-34"
    )
  )


table(frequency_model_data$age_band)



#10. negative binomial with age bands
frequency_nb_ageband <- glm.nb(
  total_claims ~ age_band +
    vehicle_value +
    power_to_weight_ratio +
    policy_type +
    fuel_type +
    vehicle_brand +
    year +
    offset(log(total_exposure)),
  data = frequency_model_data
)

summary(frequency_nb_ageband)

AIC(
  frequency_nb,
  frequency_nb_ageband
)

sum(
  residuals(
    frequency_nb_ageband,
    type = "pearson"
  )^2
) / df.residual(frequency_nb_ageband)


#11. likelihood-ratio test for age bands
anova(
  frequency_nb,
  frequency_nb_ageband,
  test = "Chisq"
)


ageband_used <- model.frame(
  frequency_nb_ageband
)

ageband_used$fitted_claims <-
  fitted(frequency_nb_ageband)

ageband_used$exposure <-
  exp(model.offset(ageband_used))


ageband_diagnostic <- ageband_used |>
  group_by(age_band) |>
  summarise(
    observed_frequency =
      sum(total_claims) / sum(exposure),
    
    fitted_frequency =
      sum(fitted_claims) / sum(exposure),
    
    n = n(),
    
    .groups = "drop"
  )

ageband_diagnostic


ggplot(
  ageband_diagnostic,
  aes(x = age_band)
) +
  geom_point(
    aes(y = observed_frequency),
    size = 3
  ) +
  geom_point(
    aes(y = fitted_frequency),
    size = 3
  ) +
  geom_line(
    aes(
      y = fitted_frequency,
      group = 1
    ),
    linewidth = 1
  ) +
  scale_x_discrete(
    limits = c(
      "<25",
      "25-34",
      "35-44",
      "45-54",
      "55-64",
      "65+"
    )
  ) +
  labs(
    title = "Observed vs Negative Binomial Frequency by Driver Age Band",
    x = "Driver age band",
    y = "Claim frequency"
  ) +
  theme_minimal()



#12. investigate missing data
missing_check <- frequency_model_data |>
  mutate(
    missing_pattern = case_when(
      is.na(vehicle_value) &
        is.na(fuel_type) ~
        "Both missing",
      
      is.na(vehicle_value) &
        !is.na(fuel_type) ~
        "Vehicle value missing",
      
      !is.na(vehicle_value) &
        is.na(fuel_type) ~
        "Fuel type missing",
      
      TRUE ~ "Complete"
    )
  ) |>
  group_by(missing_pattern) |>
  summarise(
    n = n(),
    total_exposure = sum(total_exposure),
    total_claims = sum(total_claims),
    claim_frequency =
      sum(total_claims) /
      sum(total_exposure),
    mean_vehicle_value =
      mean(
        vehicle_value,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

missing_check


missing_by_year <- frequency_model_data |>
  mutate(
    missing_pattern = case_when(
      is.na(vehicle_value) &
        is.na(fuel_type) ~
        "Both missing",
      
      is.na(vehicle_value) &
        !is.na(fuel_type) ~
        "Vehicle value missing",
      
      !is.na(vehicle_value) &
        is.na(fuel_type) ~
        "Fuel type missing",
      
      TRUE ~ "Complete"
    )
  ) |>
  count(
    year,
    missing_pattern
  )

missing_by_year


missing_by_policy <- frequency_model_data |>
  mutate(
    missing_pattern = case_when(
      is.na(vehicle_value) &
        is.na(fuel_type) ~
        "Both missing",
      
      is.na(vehicle_value) &
        !is.na(fuel_type) ~
        "Vehicle value missing",
      
      !is.na(vehicle_value) &
        is.na(fuel_type) ~
        "Fuel type missing",
      
      TRUE ~ "Complete"
    )
  ) |>
  group_by(
    missing_pattern,
    policy_type
  ) |>
  summarise(
    n = n(),
    claims = sum(total_claims),
    exposure = sum(total_exposure),
    frequency = claims / exposure,
    .groups = "drop"
  )

missing_by_policy



#13. vehicle-value median imputation
vehicle_value_medians <- frequency_model_data |>
  filter(!is.na(vehicle_value)) |>
  group_by(
    year,
    policy_type
  ) |>
  summarise(
    median_vehicle_value =
      median(vehicle_value),
    n = n(),
    .groups = "drop"
  )

vehicle_value_medians


frequency_model_data <- frequency_model_data |>
  mutate(
    vehicle_value_missing =
      is.na(vehicle_value),
    
    fuel_type = if_else(
      is.na(fuel_type),
      "Unknown",
      fuel_type
    )
  ) |>
  left_join(
    dplyr::select(
      vehicle_value_medians,
      year,
      policy_type,
      median_vehicle_value
    ),
    by = c(
      "year",
      "policy_type"
    )
  ) |>
  mutate(
    vehicle_value = if_else(
      is.na(vehicle_value),
      median_vehicle_value,
      vehicle_value
    )
  ) |>
  dplyr::select(
    -median_vehicle_value
  )


# Check missing values

sum(
  is.na(
    frequency_model_data$vehicle_value
  )
)

sum(
  is.na(
    frequency_model_data$fuel_type
  )
)

sum(
  frequency_model_data$vehicle_value_missing
)



#14. final negative binomial model
frequency_nb_final <- glm.nb(
  total_claims ~ age_band +
    vehicle_value +
    vehicle_value_missing +
    power_to_weight_ratio +
    policy_type +
    fuel_type +
    vehicle_brand +
    year +
    offset(log(total_exposure)),
  data = frequency_model_data
)

summary(frequency_nb_final)

AIC(frequency_nb_final)

sum(
  residuals(
    frequency_nb_final,
    type = "pearson"
  )^2
) / df.residual(
  frequency_nb_final
)



#15. final model count distribution
lambda_final <- fitted(
  frequency_nb_final
)

comparison_final <- data.frame(
  claims = 0:10,
  
  observed = sapply(
    0:10,
    function(k) {
      sum(
        frequency_nb_final$y == k
      )
    }
  ),
  
  expected = sapply(
    0:10,
    function(k) {
      sum(
        dnbinom(
          k,
          mu = lambda_final,
          size = frequency_nb_final$theta
        )
      )
    }
  )
)

comparison_final


ggplot(
  comparison_final,
  aes(x = claims)
) +
  geom_col(
    aes(y = observed),
    width = 0.6
  ) +
  geom_point(
    aes(y = expected),
    size = 3
  ) +
  geom_line(
    aes(y = expected),
    linewidth = 1
  ) +
  scale_y_log10() +
  labs(
    title = "Observed vs Final Negative Binomial Expected Claim Counts",
    subtitle = "Logarithmic y-axis",
    x = "Number of claims",
    y = "Number of policy-years (log scale)"
  ) +
  theme_minimal()



#16. final age-band diagnostic
ageband_final_used <-
  model.frame(
    frequency_nb_final
  )

ageband_final_used$fitted_claims <-
  fitted(frequency_nb_final)

ageband_final_used$exposure <-
  exp(
    model.offset(
      ageband_final_used
    )
  )


ageband_final_diagnostic <-
  ageband_final_used |>
  group_by(age_band) |>
  summarise(
    observed_frequency =
      sum(total_claims) /
      sum(exposure),
    
    fitted_frequency =
      sum(fitted_claims) /
      sum(exposure),
    
    n = n(),
    
    .groups = "drop"
  )

ageband_final_diagnostic


ggplot(
  ageband_final_diagnostic,
  aes(x = age_band)
) +
  geom_point(
    aes(y = observed_frequency),
    size = 3
  ) +
  geom_point(
    aes(y = fitted_frequency),
    size = 3
  ) +
  geom_line(
    aes(
      y = fitted_frequency,
      group = 1
    ),
    linewidth = 1
  ) +
  scale_x_discrete(
    limits = c(
      "<25",
      "25-34",
      "35-44",
      "45-54",
      "55-64",
      "65+"
    )
  ) +
  labs(
    title = "Observed vs Final Negative Binomial Claim Frequency",
    subtitle = "By driver age band",
    x = "Driver age band",
    y = "Claim frequency"
  ) +
  theme_minimal()



#17. group rare vehicle brands
brand_counts <- frequency_model_data |>
  count(vehicle_brand)


frequency_model_data <- frequency_model_data |>
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
  dplyr::select(-n)


table(
  frequency_model_data$vehicle_brand_group
)


#18. final model with grouped bands
frequency_nb_brandgrouped <- glm.nb(
  total_claims ~ age_band +
    vehicle_value +
    vehicle_value_missing +
    power_to_weight_ratio +
    policy_type +
    fuel_type +
    vehicle_brand_group +
    year +
    offset(log(total_exposure)),
  data = frequency_model_data
)


AIC(
  frequency_nb_final,
  frequency_nb_brandgrouped
)


sum(
  residuals(
    frequency_nb_brandgrouped,
    type = "pearson"
  )^2
) / df.residual(
  frequency_nb_brandgrouped
)


#19. likelihood-ratio test for brand grouping
anova(
  frequency_nb_brandgrouped,
  frequency_nb_final,
  test = "Chisq"
)


AIC(
  frequency_nb_brandgrouped
)

sum(
  residuals(
    frequency_nb_brandgrouped,
    type = "pearson"
  )^2
) / df.residual(
  frequency_nb_brandgrouped
)



#20. final grouped-brand count distribution
lambda_grouped <-
  fitted(
    frequency_nb_brandgrouped
  )


comparison_grouped <- data.frame(
  claims = 0:10,
  
  observed = sapply(
    0:10,
    function(k) {
      sum(
        frequency_nb_brandgrouped$y == k
      )
    }
  ),
  
  expected = sapply(
    0:10,
    function(k) {
      sum(
        dnbinom(
          k,
          mu = lambda_grouped,
          size = frequency_nb_brandgrouped$theta
        )
      )
    }
  )
)


comparison_grouped


ggplot(
  comparison_grouped,
  aes(x = claims)
) +
  geom_col(
    aes(y = observed),
    width = 0.6
  ) +
  geom_point(
    aes(y = expected),
    size = 3
  ) +
  geom_line(
    aes(y = expected),
    linewidth = 1
  ) +
  scale_y_log10() +
  labs(
    title = "Observed vs Final Negative Binomial Claim Counts",
    subtitle = "Rare vehicle brands grouped; logarithmic y-axis",
    x = "Number of claims",
    y = "Number of policy-years (log scale)"
  ) +
  theme_minimal()



#21. final grouped-brand age diagnostic
ageband_grouped_used <-
  model.frame(
    frequency_nb_brandgrouped
  )

ageband_grouped_used$fitted_claims <-
  fitted(
    frequency_nb_brandgrouped
  )

ageband_grouped_used$exposure <-
  exp(
    model.offset(
      ageband_grouped_used
    )
  )


ageband_grouped_diagnostic <-
  ageband_grouped_used |>
  group_by(age_band) |>
  summarise(
    observed_frequency =
      sum(total_claims) /
      sum(exposure),
    
    fitted_frequency =
      sum(fitted_claims) /
      sum(exposure),
    
    n = n(),
    
    .groups = "drop"
  )


ageband_grouped_diagnostic


ggplot(
  ageband_grouped_diagnostic,
  aes(x = age_band)
) +
  geom_point(
    aes(y = observed_frequency),
    size = 3
  ) +
  geom_point(
    aes(y = fitted_frequency),
    size = 3
  ) +
  geom_line(
    aes(
      y = fitted_frequency,
      group = 1
    ),
    linewidth = 1
  ) +
  scale_x_discrete(
    limits = c(
      "<25",
      "25-34",
      "35-44",
      "45-54",
      "55-64",
      "65+"
    )
  ) +
  labs(
    title = "Observed vs Final Negative Binomial Frequency",
    subtitle = "By driver age band",
    x = "Driver age band",
    y = "Claim frequency"
  ) +
  theme_minimal()



#22. vehicle-value functional-form diagnostic
vehicle_value_breaks <- quantile(
  ageband_grouped_used$vehicle_value,
  probs = c(
    0,
    0.10,
    0.20,
    0.40,
    0.60,
    0.80,
    0.90,
    0.95,
    0.99,
    1
  ),
  na.rm = TRUE
)


ageband_grouped_used <- ageband_grouped_used |>
  mutate(
    vehicle_value_band = cut(
      vehicle_value,
      breaks = vehicle_value_breaks,
      include.lowest = TRUE,
      labels = c(
        "0-10%",
        "10-20%",
        "20-40%",
        "40-60%",
        "60-80%",
        "80-90%",
        "90-95%",
        "95-99%",
        "99-100%"
      )
    )
  )


vehicle_value_diagnostic <-
  ageband_grouped_used |>
  group_by(vehicle_value_band) |>
  summarise(
    observed_frequency =
      sum(total_claims) /
      sum(exposure),
    
    fitted_frequency =
      sum(fitted_claims) /
      sum(exposure),
    
    n = n(),
    
    .groups = "drop"
  )


vehicle_value_diagnostic


ggplot(
  vehicle_value_diagnostic,
  aes(x = vehicle_value_band)
) +
  geom_point(
    aes(y = observed_frequency),
    size = 3
  ) +
  geom_point(
    aes(y = fitted_frequency),
    size = 3
  ) +
  geom_line(
    aes(
      y = fitted_frequency,
      group = 1
    ),
    linewidth = 1
  ) +
  labs(
    title = "Observed vs Final Negative Binomial Frequency",
    subtitle = "By vehicle value percentile",
    x = "Vehicle value percentile",
    y = "Claim frequency"
  ) +
  theme_minimal()



#23. power-to-weight functional-form diagnostic
power_weight_breaks <- quantile(
  ageband_grouped_used$power_to_weight_ratio,
  probs = c(
    0,
    0.10,
    0.20,
    0.40,
    0.60,
    0.80,
    0.90,
    0.95,
    0.99,
    1
  ),
  na.rm = TRUE
)


ageband_grouped_used <- ageband_grouped_used |>
  mutate(
    power_weight_band = cut(
      power_to_weight_ratio,
      breaks = power_weight_breaks,
      include.lowest = TRUE,
      labels = c(
        "0-10%",
        "10-20%",
        "20-40%",
        "40-60%",
        "60-80%",
        "80-90%",
        "90-95%",
        "95-99%",
        "99-100%"
      )
    )
  )


power_weight_diagnostic <-
  ageband_grouped_used |>
  group_by(power_weight_band) |>
  summarise(
    observed_frequency =
      sum(total_claims) /
      sum(exposure),
    
    fitted_frequency =
      sum(fitted_claims) /
      sum(exposure),
    
    n = n(),
    
    .groups = "drop"
  )


power_weight_diagnostic


ggplot(
  power_weight_diagnostic,
  aes(x = power_weight_band)
) +
  geom_point(
    aes(y = observed_frequency),
    size = 3
  ) +
  geom_point(
    aes(y = fitted_frequency),
    size = 3
  ) +
  geom_line(
    aes(
      y = fitted_frequency,
      group = 1
    ),
    linewidth = 1
  ) +
  labs(
    title = "Observed vs Final Negative Binomial Frequency",
    subtitle = "By power-to-weight ratio percentile",
    x = "Power-to-weight percentile",
    y = "Claim frequency"
  ) +
  theme_minimal()



#24. out of sample validation to train on 2022-2023, do test on 2024

# Preprocessing for validation is based only on the training period.
# This prevents information from 2024 leaking into the validation process.


# Training data


frequency_train_raw <- frequency_data |>
  filter(year < 2024)


# Create age bands

frequency_train <- frequency_train_raw |>
  mutate(
    vehicle_value_missing =
      is.na(vehicle_value),
    
    fuel_type = if_else(
      is.na(fuel_type),
      "Unknown",
      fuel_type
    ),
    
    age_band = factor(
      cut(
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
      ),
      levels = c(
        "<25",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65+"
      )
    ),
    
    age_band = relevel(
      age_band,
      ref = "25-34"
    )
  )


# Training-only vehicle-value medians.
# Use policy type only so the rule can be applied to unseen 2024 data.

training_vehicle_value_medians <-
  frequency_train |>
  filter(!is.na(vehicle_value)) |>
  group_by(policy_type) |>
  summarise(
    median_vehicle_value =
      median(vehicle_value),
    .groups = "drop"
  )


frequency_train <- frequency_train |>
  left_join(
    training_vehicle_value_medians,
    by = "policy_type"
  ) |>
  mutate(
    vehicle_value = if_else(
      is.na(vehicle_value),
      median_vehicle_value,
      vehicle_value
    )
  ) |>
  dplyr::select(
    -median_vehicle_value
  )


# Training-only vehicle-brand counts

training_brand_counts <-
  frequency_train |>
  count(vehicle_brand)


frequency_train <- frequency_train |>
  left_join(
    training_brand_counts,
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



# Fit validation model


frequency_nb_validation <- glm.nb(
  total_claims ~ age_band +
    vehicle_value +
    vehicle_value_missing +
    power_to_weight_ratio +
    policy_type +
    fuel_type +
    vehicle_brand_group +
    year +
    offset(log(total_exposure)),
  data = frequency_train
)


summary(frequency_nb_validation)



# Prepare 2024 test data


frequency_test <- frequency_data |>
  filter(year == 2024) |>
  mutate(
    vehicle_value_missing =
      is.na(vehicle_value),
    
    fuel_type = if_else(
      is.na(fuel_type),
      "Unknown",
      fuel_type
    ),
    
    age_band = factor(
      cut(
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
      ),
      levels = c(
        "<25",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65+"
      )
    )
  )


# Keep exactly the same reference level

frequency_test$age_band <-
  relevel(
    frequency_test$age_band,
    ref = "25-34"
  )


# Apply training-only vehicle-value medians

frequency_test <- frequency_test |>
  left_join(
    training_vehicle_value_medians,
    by = "policy_type"
  ) |>
  mutate(
    vehicle_value = if_else(
      is.na(vehicle_value),
      median_vehicle_value,
      vehicle_value
    )
  ) |>
  dplyr::select(
    -median_vehicle_value
  )


# Apply training-only vehicle-brand grouping

frequency_test <- frequency_test |>
  left_join(
    training_brand_counts,
    by = "vehicle_brand"
  ) |>
  mutate(
    vehicle_brand_group = if_else(
      coalesce(n, 0L) >= 1000,
      vehicle_brand,
      "Other"
    )
  ) |>
  dplyr::select(
    -n
  )


# Check that prediction variables are complete

sum(
  is.na(
    frequency_test$vehicle_value
  )
)

sum(
  is.na(
    frequency_test$fuel_type
  )
)

sum(
  is.na(
    frequency_test$vehicle_brand_group
  )
)



# Predict 2024


frequency_test <- frequency_test |>
  mutate(
    predicted_claims = predict(
      frequency_nb_validation,
      newdata = frequency_test,
      type = "response"
    )
  )



# Compare actual vs predicted


validation_result <- frequency_test |>
  summarise(
    actual_claims =
      sum(total_claims),
    
    predicted_claims =
      sum(predicted_claims),
    
    exposure =
      sum(total_exposure),
    
    actual_frequency =
      actual_claims / exposure,
    
    predicted_frequency =
      predicted_claims / exposure
  ) |>
  mutate(
    percentage_error =
      (
        predicted_frequency -
          actual_frequency
      ) /
      actual_frequency *
      100
  )


validation_result



#25. saving final frequency model
dir.create(
  "models",
  showWarnings = FALSE
)


saveRDS(
  frequency_nb_brandgrouped,
  "models/frequency_nb_final.rds"
)


file.exists(
  "models/frequency_nb_final.rds"
)



# 26.file 4 conclusion
#
# A Poisson GLM was initially fitted for claim frequency. The Pearson
# dispersion statistic indicated overdispersion, so a Negative Binomial
# model was introduced.
#
# The Negative Binomial model substantially reduced the remaining
# dispersion and reproduced the main claim-count distribution much more
# closely than the Poisson model.
#
# Driver age was initially modelled linearly. Diagnostic analysis showed
# that this was too restrictive, particularly for drivers under 25.
# Replacing the continuous age effect with age bands significantly improved
# model fit.
#
# Vehicle value and power-to-weight ratio were retained as continuous
# predictors because their observed frequency patterns were reproduced
# well by the fitted model.
#
# Missing fuel types were treated as an "Unknown" category.
# Missing vehicle values were imputed using year/policy-type medians in
# the final full-data model, with a separate missingness indicator retained.
#
# Rare vehicle brands were grouped into an "Other" category to reduce
# instability and unnecessary model complexity.
#
# The final Negative Binomial frequency model was validated against the
# unseen 2024 policy-year using a model fitted only on 2022-2023 data.
#
# The final full-data frequency model is saved to:
# models/frequency_nb_final.rds
#
# The selected frequency model is intended for use in the subsequent
# severity modelling and aggregate-loss simulation stages.
###############################################################################