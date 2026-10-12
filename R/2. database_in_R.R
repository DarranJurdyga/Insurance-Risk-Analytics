library(DBI)
library(RPostgres)
library(askpass)

setwd("C:/Insurance-Risk-Analytics")

# Load cleaned dataset
insurance_clean <- read.csv(
  "data/processed/insurance_clean.csv",
  stringsAsFactors = FALSE
)

# Connect to PostgreSQL
password <- askpass("PostgreSQL password:")

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "insurance_analytics",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = password
)

# Upload dataset to PostgreSQL
dbWriteTable(
  con,
  "motor_insurance",
  insurance_clean,
  overwrite = TRUE,
  row.names = FALSE
)

# Check number of rows
dbGetQuery(
  con,
  "SELECT COUNT(*) AS number_of_rows
   FROM motor_insurance;"
)

# First portfolio analysis
dbGetQuery(
  con,
  "
  SELECT
      year,
      COUNT(*) AS policies,
      SUM(total_claims) AS claims,
      SUM(total_premium) AS premium,
      SUM(total_incurred) AS incurred
  FROM motor_insurance
  GROUP BY year
  ORDER BY year;
  "
)

dbGetQuery(
  con,
  "
  SELECT
      policy_type,
      COUNT(*) AS policies,
      SUM(total_claims) AS claims,
      SUM(total_incurred) AS incurred,
      SUM(total_premium) AS premium
  FROM motor_insurance
  GROUP BY policy_type
  ORDER BY policies DESC;
  "
)

dbGetQuery(
  con,
  "
  SELECT
      policy_type,
      COUNT(*) AS policies,
      SUM(total_exposure) AS exposure,
      SUM(total_claims) AS claims,
      SUM(total_incurred) AS incurred,
      SUM(total_premium) AS premium,
      SUM(total_claims) / SUM(total_exposure) AS claim_frequency,
      SUM(total_incurred) / SUM(total_exposure) AS pure_premium,
      SUM(total_incurred) / SUM(total_premium) AS loss_ratio
  FROM motor_insurance
  GROUP BY policy_type
  ORDER BY loss_ratio DESC;
  "
)

dbGetQuery(
  con,
  "
  SELECT
      policy_type,
      COUNT(*) AS policies,
      SUM(total_claims) AS claims,
      SUM(total_incurred) AS incurred,
      SUM(total_premium) AS premium,
      SUM(total_exposure) AS exposure,
      SUM(total_claims) / SUM(total_exposure) AS claim_frequency,
      SUM(total_incurred) / NULLIF(SUM(total_claims), 0) AS average_claim_severity,
      SUM(total_incurred) / SUM(total_exposure) AS pure_premium,
      SUM(total_incurred) / SUM(total_premium) AS loss_ratio
  FROM motor_insurance
  GROUP BY policy_type
  ORDER BY pure_premium DESC;
  "
)

dbGetQuery(
  con,
  "
  SELECT
      CASE
          WHEN driver_age < 25 THEN '<25'
          WHEN driver_age < 35 THEN '25-34'
          WHEN driver_age < 45 THEN '35-44'
          WHEN driver_age < 55 THEN '45-54'
          WHEN driver_age < 65 THEN '55-64'
          ELSE '65+'
      END AS age_group,
      COUNT(*) AS policies,
      SUM(total_exposure) AS exposure,
      SUM(total_claims) AS claims,
      SUM(total_incurred) AS incurred,
      SUM(total_premium) AS premium,
      SUM(total_claims) / SUM(total_exposure) AS claim_frequency,
      SUM(total_incurred) / NULLIF(SUM(total_claims), 0) AS average_claim_severity,
      SUM(total_incurred) / SUM(total_exposure) AS pure_premium,
      SUM(total_incurred) / NULLIF(SUM(total_premium), 0) AS loss_ratio
  FROM motor_insurance
  GROUP BY age_group
  ORDER BY age_group;
  "
)

dbGetQuery(
  con,
  "
  SELECT
      MIN(vehicle_age) AS minimum_age,
      MAX(vehicle_age) AS maximum_age,
      AVG(vehicle_age) AS average_age
  FROM motor_insurance;
  "
)

dbGetQuery(
  con,
  "
  SELECT
      vehicle_age,
      COUNT(*) AS policies
  FROM motor_insurance
  GROUP BY vehicle_age
  ORDER BY vehicle_age;
  "
)

dbGetQuery(
  con,
  "
  SELECT *
  FROM (
      SELECT
          CASE
              WHEN vehicle_age IS NULL THEN 'Unknown'
              WHEN vehicle_age < 5 THEN '0-4'
              WHEN vehicle_age < 10 THEN '5-9'
              WHEN vehicle_age < 15 THEN '10-14'
              WHEN vehicle_age < 20 THEN '15-19'
              WHEN vehicle_age < 30 THEN '20-29'
              ELSE '30+'
          END AS vehicle_age_group,
          COUNT(*) AS policies,
          SUM(total_exposure) AS exposure,
          SUM(total_claims) AS claims,
          SUM(total_incurred) AS incurred,
          SUM(total_premium) AS premium,
          SUM(total_claims) / SUM(total_exposure) AS claim_frequency,
          SUM(total_incurred) / NULLIF(SUM(total_claims), 0) AS average_claim_severity,
          SUM(total_incurred) / SUM(total_exposure) AS pure_premium,
          SUM(total_incurred) / NULLIF(SUM(total_premium), 0) AS loss_ratio
      FROM motor_insurance
      GROUP BY vehicle_age_group
  ) AS age_analysis
  ORDER BY
      CASE
          WHEN vehicle_age_group = '0-4' THEN 1
          WHEN vehicle_age_group = '5-9' THEN 2
          WHEN vehicle_age_group = '10-14' THEN 3
          WHEN vehicle_age_group = '15-19' THEN 4
          WHEN vehicle_age_group = '20-29' THEN 5
          WHEN vehicle_age_group = '30+' THEN 6
          ELSE 7
      END;
  "
)


########################################################

dbGetQuery(
  con,
  "
  SELECT
      MIN(age_driving_licence) AS minimum_age_driving_licence,
      MAX(age_driving_licence) AS maximum_age_driving_licence,
      AVG(age_driving_licence) AS average_age_driving_licence,
      
      MIN(vehicle_value) AS minimum_vehicle_value,
      MAX(vehicle_value) AS maximum_vehicle_value,
      AVG(vehicle_value) AS average_vehicle_value,
      
      MIN(power_to_weight_ratio) AS minimum_power_to_weight_ratio,
      MAX(power_to_weight_ratio) AS maximum_power_to_weight_ratio,
      AVG(power_to_weight_ratio) AS average_power_to_weight_ratio
      
  FROM motor_insurance;
  "
)
  
  
dbGetQuery(
  con,
  "
  SELECT
      COUNT(DISTINCT vehicle_brand) AS number_of_brands
  FROM motor_insurance;
  "
)

dbGetQuery(
  con,
  "
  SELECT
      vehicle_brand,
      COUNT(*) AS policies
  FROM motor_insurance
  GROUP BY vehicle_brand
  ORDER BY policies DESC
  LIMIT 20;
  "
)

################


dbGetQuery(
  con,
  "
  SELECT
      age_driving_licence,
      COUNT(*) AS policies
  FROM motor_insurance
  GROUP BY age_driving_licence
  ORDER BY age_driving_licence
  LIMIT 15;
  "
)
dbGetQuery(
  con,
  "
  SELECT
      age_driving_licence,
      COUNT(*) AS policies
  FROM motor_insurance
  GROUP BY age_driving_licence
  ORDER BY age_driving_licence DESC
  LIMIT 15;
  "
)



dbGetQuery(
  con,
  "
  SELECT *
  FROM (
      SELECT
          CASE
              WHEN vehicle_value IS NULL THEN 'Unknown'
              WHEN vehicle_value < 10000 THEN '<10k'
              WHEN vehicle_value < 20000 THEN '10k-19,999'
              WHEN vehicle_value < 30000 THEN '20k-29,999'
              WHEN vehicle_value < 50000 THEN '30k-49,999'
              WHEN vehicle_value < 100000 THEN '50k-99,999'
              ELSE '100k+'
          END AS vehicle_value_group,
          COUNT(*) AS policies,
          SUM(total_exposure) AS exposure,
          SUM(total_claims) AS claims,
          SUM(total_incurred) AS incurred,
          SUM(total_premium) AS premium,
          SUM(total_claims) / SUM(total_exposure) AS claim_frequency,
          SUM(total_incurred) / NULLIF(SUM(total_claims), 0) AS average_claim_severity,
          SUM(total_incurred) / SUM(total_exposure) AS pure_premium,
          SUM(total_incurred) / NULLIF(SUM(total_premium), 0) AS loss_ratio
      FROM motor_insurance
      GROUP BY vehicle_value_group
  ) AS value_analysis
  ORDER BY
      CASE
          WHEN vehicle_value_group = '<10k' THEN 1
          WHEN vehicle_value_group = '10k-19,999' THEN 2
          WHEN vehicle_value_group = '20k-29,999' THEN 3
          WHEN vehicle_value_group = '30k-49,999' THEN 4
          WHEN vehicle_value_group = '50k-99,999' THEN 5
          WHEN vehicle_value_group = '100k+' THEN 6
          ELSE 7
      END;
  "
)

dbGetQuery(
  con,
  "
  SELECT
      power_to_weight_ratio,
      COUNT(*) AS policies
  FROM motor_insurance
  GROUP BY power_to_weight_ratio
  ORDER BY power_to_weight_ratio
  LIMIT 20;
  "
)

dbGetQuery(
  con,
  "
  SELECT
      vehicle_brand,
      COUNT(*) AS policies,
      SUM(total_exposure) AS exposure,
      SUM(total_claims) AS claims,
      SUM(total_incurred) AS incurred,
      SUM(total_premium) AS premium,
      SUM(total_claims) / SUM(total_exposure) AS claim_frequency,
      SUM(total_incurred) / NULLIF(SUM(total_claims), 0) AS average_claim_severity,
      SUM(total_incurred) / SUM(total_exposure) AS pure_premium,
      SUM(total_incurred) / NULLIF(SUM(total_premium), 0) AS loss_ratio
  FROM motor_insurance
  GROUP BY vehicle_brand
  HAVING COUNT(*) >= 1000
  ORDER BY policies DESC;
  "
)


dbGetQuery(
  con,
  "
  SELECT
      MIN(driver_age - age_driving_licence) AS minimum_experience,
      MAX(driver_age - age_driving_licence) AS maximum_experience,
      AVG(driver_age - age_driving_licence) AS average_experience
  FROM motor_insurance
  WHERE driver_age IS NOT NULL
    AND age_driving_licence IS NOT NULL;
  "
)

dbGetQuery(
  con,
  "
  SELECT *
  FROM (
      SELECT
          CASE
              WHEN power_to_weight_ratio IS NULL THEN 'Unknown'
              WHEN power_to_weight_ratio < 10 THEN '<10'
              WHEN power_to_weight_ratio < 15 THEN '10-14.99'
              WHEN power_to_weight_ratio < 20 THEN '15-19.99'
              WHEN power_to_weight_ratio < 30 THEN '20-29.99'
              WHEN power_to_weight_ratio < 40 THEN '30-39.99'
              ELSE '40+'
          END AS power_to_weight_group,
          COUNT(*) AS policies,
          SUM(total_exposure) AS exposure,
          SUM(total_claims) AS claims,
          SUM(total_incurred) AS incurred,
          SUM(total_premium) AS premium,
          SUM(total_claims) / SUM(total_exposure) AS claim_frequency,
          SUM(total_incurred) / NULLIF(SUM(total_claims), 0) AS average_claim_severity,
          SUM(total_incurred) / SUM(total_exposure) AS pure_premium,
          SUM(total_incurred) / NULLIF(SUM(total_premium), 0) AS loss_ratio
      FROM motor_insurance
      GROUP BY power_to_weight_group
  ) AS power_analysis
  ORDER BY
      CASE
          WHEN power_to_weight_group = '<10' THEN 1
          WHEN power_to_weight_group = '10-14.99' THEN 2
          WHEN power_to_weight_group = '15-19.99' THEN 3
          WHEN power_to_weight_group = '20-29.99' THEN 4
          WHEN power_to_weight_group = '30-39.99' THEN 5
          WHEN power_to_weight_group = '40+' THEN 6
          ELSE 7
      END;
  "
)