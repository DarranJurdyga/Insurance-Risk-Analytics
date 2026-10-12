-- MOTOR INSURANCE RISK ANALYTICS

-- 1. Data Validation
-- 1.1 Portfolio size and observation period
SELECT
    COUNT(*) AS policy_year_records,
    COUNT(DISTINCT insured_id) AS unique_insured_ids,
    MIN(year) AS first_year,
    MAX(year) AS last_year
FROM motor_insurance;


-- 1.2 Missing values in selected risk variables
SELECT
    COUNT(*) AS total_records,
    COUNT(*) FILTER (WHERE vehicle_age IS NULL) AS missing_vehicle_age,
    COUNT(*) FILTER (WHERE age_driving_licence IS NULL) AS missing_licence_age,
    COUNT(*) FILTER (WHERE vehicle_value IS NULL) AS missing_vehicle_value,
    COUNT(*) FILTER (WHERE driving_experience IS NULL) AS missing_driving_experience
FROM motor_insurance;


-- 1.3 Zero-exposure records and associated claims
SELECT
    COUNT(*) FILTER (WHERE total_exposure = 0) AS zero_exposure_records,
    COUNT(*) FILTER (
        WHERE total_exposure = 0 AND total_claims > 0
    ) AS zero_exposure_claim_records,
    COALESCE(SUM(total_incurred) FILTER (
        WHERE total_exposure = 0
    ), 0) AS incurred_on_zero_exposure
FROM motor_insurance;


-- 1.4 Claims recorded with zero incurred loss
SELECT
    COUNT(*) AS claims_with_zero_incurred_records
FROM motor_insurance
WHERE total_claims > 0
  AND total_incurred = 0;


-- 2. Annual Portfolio Analysis
-- 2.1 Annual portfolio totals
SELECT
    year,
    COUNT(*) AS policy_year_records,
    ROUND(SUM(total_exposure)::numeric, 2) AS exposure,
    SUM(total_claims) AS total_claims,
    ROUND(SUM(total_premium)::numeric, 2) AS premium_eur,
    ROUND(SUM(total_incurred)::numeric, 2) AS incurred_eur
FROM motor_insurance
GROUP BY year
ORDER BY year;


-- 2.2 Annual insurance risk measures
SELECT
    year,
    SUM(total_claims) AS total_claims,

    ROUND(
        (SUM(total_claims) /
        NULLIF(SUM(total_exposure), 0))::numeric, 4
    ) AS claim_frequency,

    ROUND(
        (SUM(total_incurred) /
        NULLIF(SUM(total_claims), 0))::numeric, 2
    ) AS average_claim_severity_eur,

    ROUND(
        (SUM(total_incurred) /
        NULLIF(SUM(total_exposure), 0))::numeric, 2
    ) AS pure_premium_eur,

    ROUND(
        (100.0 * SUM(total_incurred) /
        NULLIF(SUM(total_premium), 0))::numeric, 2
    ) AS loss_ratio_percent

FROM motor_insurance
WHERE total_exposure > 0
GROUP BY year
ORDER BY year;


-- 3. Policy Type Analysis

-- 3.1 Portfolio totals by policy type
SELECT
    policy_type,
    COUNT(*) AS policy_year_records,
    SUM(total_claims) AS total_claims,
    ROUND(SUM(total_exposure)::numeric, 2) AS exposure,
    ROUND(SUM(total_premium)::numeric, 2) AS premium_eur,
    ROUND(SUM(total_incurred)::numeric, 2) AS incurred_eur
FROM motor_insurance
GROUP BY policy_type
ORDER BY policy_year_records DESC;


-- 3.2 Insurance risk measures by policy type
SELECT
    policy_type,
    SUM(total_claims) AS total_claims,

    ROUND(
        (SUM(total_claims) /
        NULLIF(SUM(total_exposure), 0))::numeric, 4
    ) AS claim_frequency,

    ROUND(
        (SUM(total_incurred) /
        NULLIF(SUM(total_claims), 0))::numeric, 2
    ) AS average_claim_severity_eur,

    ROUND(
        (SUM(total_incurred) /
        NULLIF(SUM(total_exposure), 0))::numeric, 2
    ) AS pure_premium_eur,

    ROUND(
        (100.0 * SUM(total_incurred) /
        NULLIF(SUM(total_premium), 0))::numeric, 2
    ) AS loss_ratio_percent

FROM motor_insurance
WHERE total_exposure > 0
GROUP BY policy_type
ORDER BY pure_premium_eur DESC;



-- 4. Driver Age Analysis

-- 4.1 Portfolio size by driver age group
SELECT
    CASE
        WHEN driver_age < 25 THEN 'Under 25'
        WHEN driver_age < 35 THEN '25-34'
        WHEN driver_age < 45 THEN '35-44'
        WHEN driver_age < 55 THEN '45-54'
        WHEN driver_age < 65 THEN '55-64'
        ELSE '65+'
    END AS age_group,

    COUNT(*) AS policy_year_records,
    SUM(total_claims) AS total_claims,
    ROUND(SUM(total_exposure)::numeric, 2) AS exposure

FROM motor_insurance
GROUP BY age_group
ORDER BY MIN(driver_age);


-- 4.2 Insurance risk measures by driver age group
WITH age_groups AS (
    SELECT
        CASE
            WHEN driver_age < 25 THEN 'Under 25'
            WHEN driver_age < 35 THEN '25-34'
            WHEN driver_age < 45 THEN '35-44'
            WHEN driver_age < 55 THEN '45-54'
            WHEN driver_age < 65 THEN '55-64'
            ELSE '65+'
        END AS age_group,
        CASE
            WHEN driver_age < 25 THEN 1
            WHEN driver_age < 35 THEN 2
            WHEN driver_age < 45 THEN 3
            WHEN driver_age < 55 THEN 4
            WHEN driver_age < 65 THEN 5
            ELSE 6
        END AS sort_order,
        total_claims,
        total_exposure,
        total_incurred,
        total_premium
    FROM motor_insurance
    WHERE total_exposure > 0
)

SELECT
    age_group,
    COUNT(*) AS policy_year_records,

    ROUND(
        (SUM(total_claims) /
        NULLIF(SUM(total_exposure), 0))::numeric, 4
    ) AS claim_frequency,

    ROUND(
        (SUM(total_incurred) /
        NULLIF(SUM(total_claims), 0))::numeric, 2
    ) AS average_claim_severity_eur,

    ROUND(
        (SUM(total_incurred) /
        NULLIF(SUM(total_exposure), 0))::numeric, 2
    ) AS pure_premium_eur,

    ROUND(
        (100.0 * SUM(total_incurred) /
        NULLIF(SUM(total_premium), 0))::numeric, 2
    ) AS loss_ratio_percent

FROM age_groups
GROUP BY age_group, sort_order
ORDER BY sort_order;



-- 5. Vehicle Risk Analysis

-- 5.1 Claim frequency by vehicle age
SELECT
    CASE
        WHEN vehicle_age < 5 THEN '0-4'
        WHEN vehicle_age < 10 THEN '5-9'
        WHEN vehicle_age < 15 THEN '10-14'
        WHEN vehicle_age < 20 THEN '15-19'
        WHEN vehicle_age >= 20 THEN '20+'
        ELSE 'Unknown'
    END AS vehicle_age_group,

    COUNT(*) AS policy_year_records,
    ROUND(SUM(total_exposure)::numeric, 2) AS exposure,
    SUM(total_claims) AS total_claims,

    ROUND(
        (SUM(total_claims) /
        NULLIF(SUM(total_exposure), 0))::numeric, 4
    ) AS claim_frequency,

    ROUND(
        (SUM(total_incurred) /
        NULLIF(SUM(total_exposure), 0))::numeric, 2
    ) AS pure_premium_eur

FROM motor_insurance
WHERE total_exposure > 0
GROUP BY vehicle_age_group
ORDER BY MIN(vehicle_age) NULLS LAST;


-- 5.2 Claim frequency by vehicle value
SELECT
    CASE
        WHEN vehicle_value < 10000 THEN 'Under 10k'
        WHEN vehicle_value < 20000 THEN '10k-20k'
        WHEN vehicle_value < 30000 THEN '20k-30k'
        WHEN vehicle_value < 50000 THEN '30k-50k'
        WHEN vehicle_value >= 50000 THEN '50k+'
        ELSE 'Unknown'
    END AS vehicle_value_group,

    COUNT(*) AS policy_year_records,
    SUM(total_claims) AS total_claims,

    ROUND(
        (SUM(total_claims) /
        NULLIF(SUM(total_exposure), 0))::numeric, 4
    ) AS claim_frequency,

    ROUND(
        (100.0 * SUM(total_incurred) /
        NULLIF(SUM(total_premium), 0))::numeric, 2
    ) AS loss_ratio_percent

FROM motor_insurance
WHERE total_exposure > 0
GROUP BY vehicle_value_group
ORDER BY MIN(vehicle_value) NULLS LAST;



-- 6. In-depth SQL Analysis

-- 6.1 Year-on-year changes in claims, premiums and incurred losses
WITH annual_totals AS (
    SELECT
        year,
        SUM(total_claims) AS claims,
        SUM(total_premium) AS premium,
        SUM(total_incurred) AS incurred
    FROM motor_insurance
    GROUP BY year
),

annual_comparison AS (
    SELECT
        year,
        claims,
        premium,
        incurred,

        LAG(claims) OVER (ORDER BY year) AS previous_claims,
        LAG(premium) OVER (ORDER BY year) AS previous_premium,
        LAG(incurred) OVER (ORDER BY year) AS previous_incurred

    FROM annual_totals
)

SELECT
    year,
    claims,

    ROUND(
        (100.0 * (claims - previous_claims) /
        NULLIF(previous_claims, 0))::numeric, 2
    ) AS claims_growth_percent,

    ROUND(
        (100.0 * (premium - previous_premium) /
        NULLIF(previous_premium, 0))::numeric, 2
    ) AS premium_growth_percent,

    ROUND(
        (100.0 * (incurred - previous_incurred) /
        NULLIF(previous_incurred, 0))::numeric, 2
    ) AS incurred_growth_percent

FROM annual_comparison
ORDER BY year;


-- 6.2 Rank policy types by pure premium
WITH policy_risk AS (
    SELECT
        policy_type,
        SUM(total_claims) AS claims,
        SUM(total_exposure) AS exposure,
        SUM(total_incurred) AS incurred
    FROM motor_insurance
    WHERE total_exposure > 0
    GROUP BY policy_type
)

SELECT
    policy_type,

    ROUND(
        (claims / NULLIF(exposure, 0))::numeric, 4
    ) AS claim_frequency,

    ROUND(
        (incurred / NULLIF(exposure, 0))::numeric, 2
    ) AS pure_premium_eur,

    RANK() OVER (
        ORDER BY incurred / NULLIF(exposure, 0) DESC
    ) AS risk_rank

FROM policy_risk
ORDER BY risk_rank;



-- 7. Final Checks

-- 7.1 Confirm portfolio totals
SELECT
    COUNT(*) AS total_records,
    SUM(total_claims) AS total_claims,
    ROUND(SUM(total_premium)::numeric, 2) AS total_premium_eur,
    ROUND(SUM(total_incurred)::numeric, 2) AS total_incurred_eur
FROM motor_insurance;


-- 7.2 Confirm records and exposure used for frequency analysis
SELECT
    COUNT(*) AS frequency_records,
    ROUND(SUM(total_exposure)::numeric, 2) AS total_exposure,
    SUM(total_claims) AS total_claims
FROM motor_insurance
WHERE total_exposure > 0;

