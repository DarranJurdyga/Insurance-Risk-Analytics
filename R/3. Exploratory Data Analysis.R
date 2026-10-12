# MOTOR INSURANCE RISK ANALYTICS
# EXPLORATORY DATA ANALYSIS
#
# EDA: Examined missingness, claim frequency/severity distributions,
# risk-factor relationships, year effects, tail behaviour, and
# predictor dependence before modelling.


#0. Packages
library(dplyr)
library(ggplot2)
select <- dplyr::select

#1. loading data
setwd("C:/Insurance-Risk-Analytics")

# Load processed datasets
insurance_clean <- read.csv(
  "data/processed/insurance_clean.csv",
  stringsAsFactors = FALSE
)

frequency_data <- read.csv(
  "data/processed/frequency_data.csv",
  stringsAsFactors = FALSE
)

severity_data <- read.csv(
  "data/processed/severity_data.csv",
  stringsAsFactors = FALSE
)



# 2. Basic dataset checks
dim(insurance_clean)
dim(frequency_data)
dim(severity_data)

str(insurance_clean)

colSums(is.na(insurance_clean))


# Missing-value summary
missing_summary <- data.frame(
  variable = names(insurance_clean),
  missing = colSums(is.na(insurance_clean))
) |>
  filter(missing > 0) |>
  arrange(desc(missing))

missing_summary


# Missing vehicle value by year
insurance_clean |>
  filter(is.na(vehicle_value)) |>
  count(year)


# Missing fuel type by year
insurance_clean |>
  filter(is.na(fuel_type)) |>
  count(year)


# Check total number of missing vehicle values
sum(is.na(insurance_clean$vehicle_value))

insurance_clean |>
  filter(is.na(vehicle_value)) |>
  count(year)



#3. outcomes
summary(
  insurance_clean[, c(
    "total_exposure",
    "total_claims",
    "total_incurred",
    "total_premium"
  )]
)

summary(frequency_data$claim_frequency)

summary(frequency_data$loss_ratio)

summary(severity_data$claim_severity)


# Proportion of policy-years with zero claims
mean(insurance_clean$total_claims == 0)

# Claim-count distribution
table(insurance_clean$total_claims)


# Severity checks
min(severity_data$claim_severity)

sum(severity_data$claim_severity <= 0)

severity_data |>
  filter(claim_severity < 1e-8) |>
  dplyr::select(
    insured_id,
    year,
    total_claims,
    total_incurred,
    claim_severity
  )

nrow(severity_data)


# 4. Distributions of insurance outcomes
# 4.1 Distribution of claim counts

ggplot(
  insurance_clean,
  aes(x = total_claims)
) +
  geom_histogram(
    binwidth = 1
  ) +
  labs(
    title = "Distribution of Claim Counts",
    x = "Number of claims",
    y = "Number of policy-years"
  )#


# 4.2 Distribution of total incurred losses

ggplot(
  insurance_clean,
  aes(x = total_incurred)
) +
  geom_histogram(
    bins = 50
  ) +
  labs(
    title = "Distribution of Total Incurred Loss",
    x = "Total incurred loss (€)",
    y = "Number of policy-years"
  )


# 4.3 Distribution of positive claim severity

ggplot(
  severity_data,
  aes(x = claim_severity)
) +
  geom_histogram(
    bins = 50
  ) +
  labs(
    title = "Distribution of Positive Claim Severity",
    x = "Average claim severity (€)",
    y = "Number of policy-years"
  )


# 4.4 Positive claim severity on a log scale

ggplot(
  severity_data,
  aes(x = claim_severity)
) +
  geom_histogram(
    bins = 50
  ) +
  scale_x_log10() +
  labs(
    title = "Distribution of Positive Claim Severity (Log Scale)",
    x = "Claim severity (€), log scale",
    y = "Number of policy-years"
  )


# 4.5 Total incurred loss on a log1p scale

ggplot(
  insurance_clean,
  aes(x = log1p(total_incurred))
) +
  geom_histogram(
    bins = 50
  ) +
  labs(
    title = "Distribution of Total Incurred Losses (log1p scale)",
    x = "log(1 + total incurred loss)",
    y = "Number of policy-years"
  )


# 4.6 Positive total incurred losses

sum(insurance_clean$total_incurred == 0)

positive_incurred <- insurance_clean |>
  filter(total_incurred > 1e-8)


ggplot(
  positive_incurred,
  aes(x = total_incurred)
) +
  geom_histogram(
    bins = 50
  ) +
  labs(
    title = "Distribution of Positive Incurred Losses",
    x = "Total incurred loss (€)",
    y = "Number of policy-years"
  )


# 4.7 Positive total incurred losses on a log scale

ggplot(
  positive_incurred,
  aes(x = total_incurred)
) +
  geom_histogram(
    bins = 50
  ) +
  scale_x_log10() +
  labs(
    title = "Distribution of Positive Incurred Losses (Log Scale)",
    x = "Total incurred loss (€), log scale",
    y = "Number of policy-years"
  )



#5. distribution of key numerical risk factors
# Quantiles of driver age

quantile(
  insurance_clean$driver_age,
  probs = c(
    0,
    0.25,
    0.5,
    0.75,
    0.9,
    0.95,
    0.99,
    1
  ),
  na.rm = TRUE
)


# Quantiles of vehicle age

quantile(
  insurance_clean$vehicle_age,
  probs = c(
    0,
    0.25,
    0.5,
    0.75,
    0.9,
    0.95,
    0.99,
    1
  ),
  na.rm = TRUE
)


# Quantiles of age at driving licence

quantile(
  insurance_clean$age_driving_licence,
  probs = c(
    0,
    0.25,
    0.5,
    0.75,
    0.9,
    0.95,
    0.99,
    1
  ),
  na.rm = TRUE
)


# Quantiles of vehicle value

quantile(
  insurance_clean$vehicle_value,
  probs = c(
    0,
    0.25,
    0.5,
    0.75,
    0.9,
    0.95,
    0.99,
    1
  ),
  na.rm = TRUE
)


# Quantiles of power-to-weight ratio

quantile(
  insurance_clean$power_to_weight_ratio,
  probs = c(
    0,
    0.25,
    0.5,
    0.75,
    0.9,
    0.95,
    0.99,
    1
  ),
  na.rm = TRUE
)


# 5.1 Vehicle value distribution

ggplot(
  insurance_clean,
  aes(x = vehicle_value)
) +
  geom_histogram(
    bins = 50
  ) +
  labs(
    title = "Distribution of Vehicle Value",
    x = "Vehicle value (€)",
    y = "Number of policy-years"
  )


# 5.2 Vehicle value on log scale

ggplot(
  insurance_clean,
  aes(x = vehicle_value)
) +
  geom_histogram(
    bins = 50
  ) +
  scale_x_log10() +
  labs(
    title = "Distribution of Vehicle Value (Log Scale)",
    x = "Vehicle value (€), log scale",
    y = "Number of policy-years"
  )


# 5.3 Driver age

ggplot(
  insurance_clean,
  aes(x = driver_age)
) +
  geom_histogram(
    binwidth = 1,
    boundary = 0
  ) +
  labs(
    title = "Distribution of Driver Age",
    x = "Driver age",
    y = "Number of policy-years"
  )


# 5.4 Vehicle age

ggplot(
  insurance_clean,
  aes(x = vehicle_age)
) +
  geom_histogram(
    binwidth = 1,
    boundary = 0
  ) +
  labs(
    title = "Distribution of Vehicle Age",
    x = "Vehicle age",
    y = "Number of policy-years"
  )


# 5.5 Driving experience
# Already created in 01_import_and_clean.R

ggplot(
  insurance_clean,
  aes(x = driving_experience)
) +
  geom_histogram(
    binwidth = 1,
    boundary = 0
  ) +
  labs(
    title = "Distribution of Driving Experience",
    x = "Years of driving experience",
    y = "Number of policy-years"
  )


summary(insurance_clean$driving_experience)

sum(
  insurance_clean$driving_experience < 0,
  na.rm = TRUE
)



#6. claim frequency (by driver age)
driver_age_frequency <- insurance_clean |>
  filter(!is.na(driver_age)) |>
  mutate(
    age_group = case_when(
      driver_age < 25 ~ "<25",
      driver_age < 35 ~ "25-34",
      driver_age < 45 ~ "35-44",
      driver_age < 55 ~ "45-54",
      driver_age < 65 ~ "55-64",
      TRUE ~ "65+"
    )
  ) |>
  group_by(age_group) |>
  summarise(
    exposure = sum(total_exposure),
    claims = sum(total_claims),
    claim_frequency = claims / exposure
  ) |>
  mutate(
    age_group = factor(
      age_group,
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


driver_age_frequency


ggplot(
  driver_age_frequency,
  aes(
    x = age_group,
    y = claim_frequency,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Claim Frequency by Driver Age",
    x = "Driver age",
    y = "Claims per unit of exposure"
  )


#7. claim frequency (by vehicle age)

vehicle_age_frequency <- insurance_clean |>
  filter(!is.na(vehicle_age)) |>
  mutate(
    vehicle_age_group = case_when(
      vehicle_age < 5 ~ "0-4",
      vehicle_age < 10 ~ "5-9",
      vehicle_age < 15 ~ "10-14",
      vehicle_age < 20 ~ "15-19",
      vehicle_age < 30 ~ "20-29",
      TRUE ~ "30+"
    )
  ) |>
  group_by(vehicle_age_group) |>
  summarise(
    exposure = sum(total_exposure),
    claims = sum(total_claims),
    claim_frequency = claims / exposure
  ) |>
  mutate(
    vehicle_age_group = factor(
      vehicle_age_group,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-29",
        "30+"
      )
    )
  )


vehicle_age_frequency


ggplot(
  vehicle_age_frequency,
  aes(
    x = vehicle_age_group,
    y = claim_frequency,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Claim Frequency by Vehicle Age",
    x = "Vehicle age",
    y = "Claims per unit of exposure"
  )


#8. claim frequency by driving experience
experience_frequency <- insurance_clean |>
  filter(!is.na(driving_experience)) |>
  mutate(
    experience_group = case_when(
      driving_experience < 5 ~ "0-4",
      driving_experience < 10 ~ "5-9",
      driving_experience < 20 ~ "10-19",
      driving_experience < 30 ~ "20-29",
      driving_experience < 40 ~ "30-39",
      TRUE ~ "40+"
    )
  ) |>
  group_by(experience_group) |>
  summarise(
    exposure = sum(total_exposure),
    claims = sum(total_claims),
    claim_frequency = claims / exposure
  ) |>
  mutate(
    experience_group = factor(
      experience_group,
      levels = c(
        "0-4",
        "5-9",
        "10-19",
        "20-29",
        "30-39",
        "40+"
      )
    )
  )


experience_frequency


ggplot(
  experience_frequency,
  aes(
    x = experience_group,
    y = claim_frequency,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Claim Frequency by Driving Experience",
    x = "Driving experience",
    y = "Claims per unit of exposure"
  )


#9. vehicle value and power-to-weight quantiles
quantile(
  insurance_clean$vehicle_value,
  probs = c(
    0,
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


quantile(
  insurance_clean$power_to_weight_ratio,
  probs = c(
    0,
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



#10. claim frequency (by vehicle value)
vehicle_value_frequency <- insurance_clean |>
  filter(!is.na(vehicle_value)) |>
  mutate(
    vehicle_value_group = cut(
      vehicle_value,
      breaks = quantile(
        vehicle_value,
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
      ),
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
      ),
      include.lowest = TRUE
    )
  ) |>
  group_by(vehicle_value_group) |>
  summarise(
    exposure = sum(total_exposure),
    claims = sum(total_claims),
    claim_frequency = claims / exposure
  )


vehicle_value_frequency


ggplot(
  vehicle_value_frequency,
  aes(
    x = vehicle_value_group,
    y = claim_frequency,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Claim Frequency by Vehicle Value",
    x = "Vehicle value percentile group",
    y = "Claims per unit of exposure"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )



#11. claim frequency (by power-to-weight ratio)
power_weight_frequency <- insurance_clean |>
  filter(!is.na(power_to_weight_ratio)) |>
  mutate(
    power_to_weight_group = cut(
      power_to_weight_ratio,
      breaks = quantile(
        power_to_weight_ratio,
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
      ),
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
      ),
      include.lowest = TRUE
    )
  ) |>
  group_by(power_to_weight_group) |>
  summarise(
    exposure = sum(total_exposure),
    claims = sum(total_claims),
    claim_frequency = claims / exposure
  )


power_weight_frequency


ggplot(
  power_weight_frequency,
  aes(
    x = power_to_weight_group,
    y = claim_frequency,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Claim Frequency by Power-to-Weight Ratio",
    x = "Power-to-weight percentile group",
    y = "Claims per unit of exposure"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )



#12. driver age to positive claim severity
driver_age_severity <- severity_data |>
  filter(!is.na(driver_age)) |>
  mutate(
    age_group = case_when(
      driver_age < 25 ~ "<25",
      driver_age < 35 ~ "25-34",
      driver_age < 45 ~ "35-44",
      driver_age < 55 ~ "45-54",
      driver_age < 65 ~ "55-64",
      TRUE ~ "65+"
    )
  ) |>
  group_by(age_group) |>
  summarise(
    observations = n(),
    mean_severity = mean(claim_severity),
    median_severity = median(claim_severity)
  ) |>
  mutate(
    age_group = factor(
      age_group,
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


driver_age_severity


ggplot(
  driver_age_severity,
  aes(
    x = age_group,
    y = median_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Median Claim Severity by Driver Age",
    x = "Driver age",
    y = "Median claim severity (€)"
  )


ggplot(
  driver_age_severity,
  aes(
    x = age_group,
    y = mean_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Claim Severity by Driver Age",
    x = "Driver age",
    y = "Mean claim severity (€)"
  )


#13. driving experience to positive claim severity
driving_experience_severity <- severity_data |>
  filter(!is.na(driving_experience)) |>
  mutate(
    experience_group = case_when(
      driving_experience < 5 ~ "0-4",
      driving_experience < 10 ~ "5-9",
      driving_experience < 20 ~ "10-19",
      driving_experience < 30 ~ "20-29",
      driving_experience < 40 ~ "30-39",
      TRUE ~ "40+"
    )
  ) |>
  group_by(experience_group) |>
  summarise(
    observations = n(),
    mean_severity = mean(claim_severity),
    median_severity = median(claim_severity)
  ) |>
  mutate(
    experience_group = factor(
      experience_group,
      levels = c(
        "0-4",
        "5-9",
        "10-19",
        "20-29",
        "30-39",
        "40+"
      )
    )
  )


driving_experience_severity


ggplot(
  driving_experience_severity,
  aes(
    x = experience_group,
    y = median_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Median Claim Severity by Driving Experience",
    x = "Driving experience",
    y = "Median claim severity (€)"
  )


ggplot(
  driving_experience_severity,
  aes(
    x = experience_group,
    y = mean_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Claim Severity by Driving Experience",
    x = "Driving experience",
    y = "Mean claim severity (€)"
  )



#14. vehicle age to positive claim severity
vehicle_age_severity <- severity_data |>
  filter(!is.na(vehicle_age)) |>
  mutate(
    vehicle_age_group = case_when(
      vehicle_age < 5 ~ "0-4",
      vehicle_age < 10 ~ "5-9",
      vehicle_age < 15 ~ "10-14",
      vehicle_age < 20 ~ "15-19",
      vehicle_age < 30 ~ "20-29",
      TRUE ~ "30+"
    )
  ) |>
  group_by(vehicle_age_group) |>
  summarise(
    observations = n(),
    mean_severity = mean(claim_severity),
    median_severity = median(claim_severity)
  ) |>
  mutate(
    vehicle_age_group = factor(
      vehicle_age_group,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-29",
        "30+"
      )
    )
  )


vehicle_age_severity


ggplot(
  vehicle_age_severity,
  aes(
    x = vehicle_age_group,
    y = median_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Median Claim Severity by Vehicle Age",
    x = "Vehicle age",
    y = "Median claim severity (€)"
  )


ggplot(
  vehicle_age_severity,
  aes(
    x = vehicle_age_group,
    y = mean_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Claim Severity by Vehicle Age",
    x = "Vehicle age",
    y = "Mean claim severity (€)"
  )


#15. vehicle value to positive claim severity
vehicle_value_severity <- severity_data |>
  filter(!is.na(vehicle_value)) |>
  mutate(
    vehicle_value_group = cut(
      vehicle_value,
      breaks = quantile(
        vehicle_value,
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
      ),
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
      ),
      include.lowest = TRUE
    )
  ) |>
  group_by(vehicle_value_group) |>
  summarise(
    observations = n(),
    mean_severity = mean(claim_severity),
    median_severity = median(claim_severity)
  )


vehicle_value_severity


ggplot(
  vehicle_value_severity,
  aes(
    x = vehicle_value_group,
    y = median_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Median Claim Severity by Vehicle Value",
    x = "Vehicle value percentile group",
    y = "Median claim severity (€)"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


ggplot(
  vehicle_value_severity,
  aes(
    x = vehicle_value_group,
    y = mean_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Claim Severity by Vehicle Value",
    x = "Vehicle value percentile group",
    y = "Mean claim severity (€)"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )



#16. power to weight to positive claim severity
power_weight_severity <- severity_data |>
  filter(!is.na(power_to_weight_ratio)) |>
  mutate(
    power_to_weight_group = cut(
      power_to_weight_ratio,
      breaks = quantile(
        power_to_weight_ratio,
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
      ),
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
      ),
      include.lowest = TRUE
    )
  ) |>
  group_by(power_to_weight_group) |>
  summarise(
    observations = n(),
    mean_severity = mean(claim_severity),
    median_severity = median(claim_severity)
  )


power_weight_severity


ggplot(
  power_weight_severity,
  aes(
    x = power_to_weight_group,
    y = median_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Median Claim Severity by Power-to-Weight Ratio",
    x = "Power-to-weight percentile group",
    y = "Median claim severity (€)"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


ggplot(
  power_weight_severity,
  aes(
    x = power_to_weight_group,
    y = mean_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Claim Severity by Power-to-Weight Ratio",
    x = "Power-to-weight percentile group",
    y = "Mean claim severity (€)"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )



#17. policy type to positive claim severity
policy_type_severity <- severity_data |>
  group_by(policy_type) |>
  summarise(
    observations = n(),
    mean_severity = mean(claim_severity),
    median_severity = median(claim_severity)
  ) |>
  arrange(desc(median_severity))


policy_type_severity


# Median severity by policy type
ggplot(
  policy_type_severity,
  aes(
    x = reorder(policy_type, median_severity),
    y = median_severity
  )
) +
  geom_point(size = 3) +
  labs(
    title = "Median Claim Severity by Policy Type",
    x = "Policy type",
    y = "Median claim severity (€)"
  )


# Mean severity by policy type
ggplot(
  policy_type_severity,
  aes(
    x = reorder(policy_type, mean_severity),
    y = mean_severity
  )
) +
  geom_point(size = 3) +
  labs(
    title = "Mean Claim Severity by Policy Type",
    x = "Policy type",
    y = "Mean claim severity (€)"
  )



#18. vehicle brand to positive claim severity
vehicle_brand_severity <- severity_data |>
  group_by(vehicle_brand) |>
  summarise(
    observations = n(),
    mean_severity = mean(claim_severity),
    median_severity = median(claim_severity)
  ) |>
  filter(observations >= 1000) |>
  arrange(desc(median_severity))


vehicle_brand_severity


# Median severity by vehicle brand
ggplot(
  vehicle_brand_severity,
  aes(
    x = reorder(vehicle_brand, median_severity),
    y = median_severity
  )
) +
  geom_point() +
  coord_flip() +
  labs(
    title = "Median Claim Severity by Vehicle Brand",
    x = "Vehicle brand",
    y = "Median claim severity (€)"
  )


# Mean severity by vehicle brand
ggplot(
  vehicle_brand_severity,
  aes(
    x = reorder(vehicle_brand, mean_severity),
    y = mean_severity
  )
) +
  geom_point() +
  coord_flip() +
  labs(
    title = "Mean Claim Severity by Vehicle Brand",
    x = "Vehicle brand",
    y = "Mean claim severity (€)"
  )



#19. Display Severity Summary
driver_age_severity

driving_experience_severity

vehicle_age_severity

vehicle_value_severity

power_weight_severity

policy_type_severity

vehicle_brand_severity



#20. year effects
# 20.1 Claim frequency by year

year_frequency <- insurance_clean |>
  group_by(year) |>
  summarise(
    exposure = sum(total_exposure),
    claims = sum(total_claims),
    claim_frequency = claims / exposure
  )


year_frequency


ggplot(
  year_frequency,
  aes(
    x = year,
    y = claim_frequency,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Claim Frequency by Year",
    x = "Year",
    y = "Claims per unit of exposure"
  )


# 20.2 Positive claim severity by year

year_severity <- severity_data |>
  group_by(year) |>
  summarise(
    observations = n(),
    mean_severity = mean(claim_severity),
    median_severity = median(claim_severity)
  )


year_severity


# Median severity by year
ggplot(
  year_severity,
  aes(
    x = year,
    y = median_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Median Claim Severity by Year",
    x = "Year",
    y = "Median claim severity (€)"
  )


# Mean severity by year
ggplot(
  year_severity,
  aes(
    x = year,
    y = mean_severity,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Claim Severity by Year",
    x = "Year",
    y = "Mean claim severity (€)"
  )


# 20.3 Loss ratio by year

year_loss_ratio <- insurance_clean |>
  group_by(year) |>
  summarise(
    premium = sum(total_premium),
    incurred = sum(total_incurred),
    loss_ratio = incurred / premium
  )


year_loss_ratio


ggplot(
  year_loss_ratio,
  aes(
    x = year,
    y = loss_ratio,
    group = 1
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Loss Ratio by Year",
    x = "Year",
    y = "Loss ratio"
  )



#21. severity tail quantiles
severity_tail <- quantile(
  severity_data$claim_severity,
  probs = c(
    0.90,
    0.95,
    0.99,
    0.995,
    0.999
  ),
  na.rm = TRUE
)


severity_tail


# Number of observations above each threshold

severity_tail_counts <- data.frame(
  threshold = c(
    "90%",
    "95%",
    "99%",
    "99.5%",
    "99.9%"
  ),
  observations_above = c(
    sum(
      severity_data$claim_severity >
        quantile(
          severity_data$claim_severity,
          0.90,
          na.rm = TRUE
        )
    ),
    sum(
      severity_data$claim_severity >
        quantile(
          severity_data$claim_severity,
          0.95,
          na.rm = TRUE
        )
    ),
    sum(
      severity_data$claim_severity >
        quantile(
          severity_data$claim_severity,
          0.99,
          na.rm = TRUE
        )
    ),
    sum(
      severity_data$claim_severity >
        quantile(
          severity_data$claim_severity,
          0.995,
          na.rm = TRUE
        )
    ),
    sum(
      severity_data$claim_severity >
        quantile(
          severity_data$claim_severity,
          0.999,
          na.rm = TRUE
        )
    )
  )
)


severity_tail_counts



#22. prediction of dependence
# Driver age vs driving experience

cor(
  insurance_clean$driver_age,
  insurance_clean$driving_experience,
  use = "complete.obs"
)


# Vehicle age vs vehicle value

cor(
  insurance_clean$vehicle_age,
  insurance_clean$vehicle_value,
  use = "complete.obs"
)


# Correlation matrix of key numerical predictors

predictor_dependence <- insurance_clean |>
  select(
    driver_age,
    age_driving_licence,
    driving_experience,
    vehicle_age,
    vehicle_value,
    power_to_weight_ratio
  )


cor(
  predictor_dependence,
  use = "pairwise.complete.obs"
)


# 22.1 Driver age vs driving experience

ggplot(
  insurance_clean,
  aes(
    x = driver_age,
    y = driving_experience
  )
) +
  geom_point(
    alpha = 0.1
  ) +
  labs(
    title = "Driver Age vs Driving Experience",
    x = "Driver age",
    y = "Driving experience"
  )


# 22.2 Vehicle age vs vehicle value

ggplot(
  insurance_clean,
  aes(
    x = vehicle_age,
    y = vehicle_value
  )
) +
  geom_point(
    alpha = 0.1
  ) +
  labs(
    title = "Vehicle Age vs Vehicle Value",
    x = "Vehicle age",
    y = "Vehicle value (€)"
  )



#vehicle age compared to driving experince
# Difference between the two variables

summary(
  insurance_clean$vehicle_age -
    insurance_clean$driving_experience
)


# Number of observations where they differ

sum(
  insurance_clean$vehicle_age !=
    insurance_clean$driving_experience,
  na.rm = TRUE
)


# Proportion where they are equal

mean(
  insurance_clean$vehicle_age ==
    insurance_clean$driving_experience,
  na.rm = TRUE
)


# Distribution of their difference

table(
  insurance_clean$vehicle_age -
    insurance_clean$driving_experience
)


# EDA conclusion:
#
# Examined data quality, outcome distributions, numerical risk-factor
# distributions, claim frequency and positive claim severity relationships,
# year effects, severity tail behaviour, and predictor dependence before
# selecting and fitting statistical models.