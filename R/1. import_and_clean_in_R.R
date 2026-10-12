# MOTOR INSURANCE RISK ANALYTICS , IMPORT , VALIDATION AND CLEANING



# 0.Packages


library(dplyr)
library(janitor)

# 1.Import

# Location of the original Mendeley dataset
raw_file <- paste0(
  "C:/Insurance-Risk-Analytics/data/raw/",
  "A detailed dataset of motor insurance policies wit/",
  "Dataset of motor insurance portfolio.csv"
)

# Import the raw CSV
insurance <- read.csv(
  raw_file,
  sep = ";",
  dec = ".",
  stringsAsFactors = FALSE
)

# Standardise column names
insurance <- insurance |>
  clean_names()


# 2. Initial Exploration

# Dimensions
dim(insurance)

# Variable names
names(insurance)

# First few observations
head(insurance)

# Structure of the dataset
str(insurance)

# Summary of important numerical variables
summary(insurance$driver_age)
summary(insurance$vehicle_age)
summary(insurance$age_driving_licence)
summary(insurance$vehicle_value)
summary(insurance$total_premium)
summary(insurance$total_claims)
summary(insurance$total_incurred)
summary(insurance$total_exposure)

# Categorical variables
unique(insurance$policy_type)
unique(insurance$policy_status)
unique(insurance$business_type)
unique(insurance$payment_frequency)
unique(insurance$bonus_score)
unique(insurance$fuel_type)
unique(insurance$vehicle_brand)
unique(insurance$municipality_type)
unique(insurance$circulation_area)

# Missing values by variable
colSums(is.na(insurance))

# 3. Data Validation

# 3.1 Claim count reconciliation

claim_columns <- c(
  "liability_claims",
  "property_claims",
  "theft_claims",
  "fire_claims",
  "glass_claims",
  "legal_protection_claims",
  "occupants_claims"
)

claim_sum <- rowSums(
  insurance[, claim_columns],
  na.rm = TRUE
)

claim_mismatches <- sum(
  claim_sum != insurance$total_claims
)

claim_mismatches

# 3.2 Incurred loss reconciliation

incurred_columns <- c(
  "liability_incurred",
  "property_incurred",
  "theft_incurred",
  "fire_incurred",
  "glass_incurred",
  "legal_protection_incurred",
  "occupants_incurred"
)

incurred_sum <- rowSums(
  insurance[, incurred_columns],
  na.rm = TRUE
)

incurred_mismatches <- sum(
  abs(incurred_sum - insurance$total_incurred) > 0.01
)

incurred_mismatches

# 3.3 Premium reconciliation

premium_columns <- c(
  "liability_premium",
  "property_damage_premium",
  "theft_premium",
  "fire_premium",
  "glass_premium",
  "legal_protection_premium",
  "occupants_premium"
)

premium_sum <- rowSums(
  insurance[, premium_columns],
  na.rm = TRUE
)

premium_mismatches <- sum(
  abs(premium_sum - insurance$total_premium) > 0.01
)

premium_mismatches

# 3.4 Exposure reconciliation


exposure_mismatches <- sum(
  abs(
    insurance$total_exposure -
      insurance$liability_exposure
  ) > 0.01
)

exposure_mismatches

# 3.5 Negative value checks

numeric_columns <- c(
  "driver_age",
  "vehicle_age",
  "age_driving_licence",
  "vehicle_value",
  "seats",
  "power_to_weight_ratio",
  "total_premium",
  "liability_premium",
  "property_damage_premium",
  "theft_premium",
  "fire_premium",
  "glass_premium",
  "legal_protection_premium",
  "occupants_premium",
  "total_claims",
  "total_incurred",
  "total_exposure",
  "liability_exposure"
)

negative_counts <- sapply(
  insurance[numeric_columns],
  function(x) sum(x < 0, na.rm = TRUE)
)

negative_counts

# 3.6 Logical age checks

# Driving-licence age should not exceed driver's age
impossible_licence_ages <- sum(
  insurance$age_driving_licence > insurance$driver_age,
  na.rm = TRUE
)

impossible_licence_ages


# Vehicle age should not be negative
negative_vehicle_age <- sum(
  insurance$vehicle_age < 0,
  na.rm = TRUE
)

negative_vehicle_age

# 3.7 Zero-exposure observations

zero_exposure <- insurance |>
  filter(total_exposure == 0) |>
  count(
    policy_status,
    total_premium,
    total_claims,
    total_incurred
  )

zero_exposure


# 3.8 Claims with zero incurred amount


claims_zero_incurred <- insurance |>
  filter(
    total_claims > 0,
    total_incurred == 0
  ) |>
  summarise(
    observations = n()
  )

claims_zero_incurred



# 4. CLEANING / DERIVED DATASETS


# 4.1 Master cleaned dataset

# Keep the original observations and add derived variables.

insurance_clean <- insurance |>
  mutate(
    driving_experience =
      driver_age - age_driving_licence
  )

# 4.2 Frequency modelling dataset

# Exposure must be greater than zero for exposure-based
# claim-frequency modelling.

frequency_data <- insurance_clean |>
  filter(total_exposure > 0)


# 4.3 Severity modelling dataset

# For positive-loss severity modelling, retain observations
# with both claims and positive incurred losses.

severity_data <- insurance_clean |>
  filter(
    total_claims > 0,
    total_incurred > 1e-8
  ) |>
  mutate(
    claim_severity = total_incurred / total_claims
  )


# 4.4 Basic derived variables

# Claim frequency per unit of exposure
frequency_data <- frequency_data |>
  mutate(
    claim_frequency =
      total_claims / total_exposure
  )

# Loss ratio
frequency_data <- frequency_data |>
  mutate(
    loss_ratio =
      total_incurred / total_premium
  )

# Average incurred loss per reported claim
severity_data <- severity_data |>
  mutate(
    claim_severity =
      total_incurred / total_claims
  )


# 5.Export
setwd("C:/Insurance-Risk-Analytics")
getwd()
# Create processed-data folder if it does not exist
if (!dir.exists("data/processed")) {
  dir.create("data/processed")
}

# Save cleaned master dataset
write.csv(
  insurance_clean,
  "data/processed/insurance_clean.csv",
  row.names = FALSE
)

# Save frequency dataset
write.csv(
  frequency_data,
  "data/processed/frequency_data.csv",
  row.names = FALSE
)

# Save severity dataset
write.csv(
  severity_data,
  "data/processed/severity_data.csv",
  row.names = FALSE
)
list.files("data/processed")

# 6. Final Checks

dim(insurance_clean)
dim(frequency_data)
dim(severity_data)

summary(frequency_data$claim_frequency)
summary(frequency_data$loss_ratio)
summary(severity_data$claim_severity)

sum(is.infinite(frequency_data$loss_ratio))
sum(is.na(frequency_data$loss_ratio))

