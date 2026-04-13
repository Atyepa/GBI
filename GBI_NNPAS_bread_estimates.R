# =============================================================================
# GBI Aggregate Bread Intake Estimates — NNPAS 2011-12
# =============================================================================
# Produces population-level bread intake estimates from the ABS 2011-12
# National Nutrition and Physical Activity Survey (NNPAS) Basic CURF,
# formatted for the Global Bread Intake (GBI) Study aggregate data request.
#
# Output: Long-format data frame matching the GBI Data_Template structure,
#         with fully stratified and age×sex-only estimates for:
#         - "Bread alone" and "Bread all sources"
#         - Total bread, wholegrain bread, refined bread
#         - Non-energy-adjusted and energy-adjusted (residual method)
#
# Key assumptions documented in-line and in output notes column.
# =============================================================================
library(tidyverse)
library(readxl)
library(survey)
library(srvyr)

# --- Paths ----------------------------------------------------------------
curf_dir <- "CURF"

# =============================================================================
# SECTION 1: Load and prepare reference data
# =============================================================================

# --- 1a. ADG Database (food composition / disaggregation) -------------------
adg <- read_excel(file.path(curf_dir, "ADG_Database.xlsx"))

# The ADG database contains two sets of rows per food: "g/100g" (gram weights)
# and "Serves/100g" (dietary guideline serves). We need only the gram weights.
adg <- adg %>% filter(Unit == "g/100g")

cat("ADG filtered to g/100g:", nrow(adg), "rows,",
    n_distinct(adg$FOODCODC), "unique FOODCODCs\n")

# Rename the numeric ADG columns for clarity
# Column 1011 = wholegrain bread g/100g, column 1021 = refined bread g/100g
adg <- adg %>%
  rename(
    adg_wg_bread = `1011`,   # wholegrain bread (g per 100g of food)
    adg_ref_bread = `1021`   # refined/low-fibre bread (g per 100g of food)
  )

# --- 1b. Build bread-alone classification lookup ----------------------------
# "Bread alone" = FOODCODC in range 12201001 to 12307004
# These are standalone bread products (not sandwiches, pizza, wraps etc.)

bread_alone_codes <- adg %>%
  filter(FOODCODC >= 12201001, FOODCODC <= 12307004) %>%
  select(FOODCODC, Description, adg_wg_bread, adg_ref_bread)

# Classify each bread code as wholegrain or refined:
#   - ADG column 1011 > 0 → wholegrain
#   - ADG column 1021 > 0 → refined
#   - Neither (42 items: English muffins, sweet buns, garlic bread, etc.)
#     → classify by keyword, defaulting to refined
wg_keywords <- c("wholemeal", "wholegrain", "whole grain", "mixed grain",
                  "multigrain", "multi-grain", "rye")

bread_alone_codes <- bread_alone_codes %>%
  mutate(
    desc_lower = str_to_lower(Description),
    bread_class = case_when(
      adg_wg_bread > 0  ~ "wholegrain",
      adg_ref_bread > 0 ~ "refined",
      # For items with neither ADG flag, use keyword matching
      str_detect(desc_lower, str_c(wg_keywords, collapse = "|")) ~ "wholegrain",
      TRUE ~ "refined"
    )
  ) %>%
  select(FOODCODC, Description, bread_class)

cat("Bread alone classification:\n")
count(bread_alone_codes, bread_class) %>% print()

# --- 1c. ADG lookup for "bread all sources" ---------------------------------
# For every food, the ADG gives g of wholegrain bread and g of refined bread
# per 100g of the food as consumed. This handles mixed-dish disaggregation.
adg_bread_content <- adg %>%
  filter(adg_wg_bread > 0 | adg_ref_bread > 0) %>%
  select(FOODCODC, adg_wg_bread, adg_ref_bread)

cat("\nFoods with any bread content:", nrow(adg_bread_content), "\n")

# =============================================================================
# SECTION 2: Load CURF data files
# =============================================================================

# --- 2a. Person file --------------------------------------------------------
persons <- read_csv(file.path(curf_dir, "npa11bp.csv"),
                    show_col_types = FALSE)

# Keep the variables we need
persons <- persons %>%
  select(
    ABSHID, ABSPID,
    AGE, SEX,
    NPAFINWT,                          # Day 1 person weight
    NPAD2WGT,                          # Day 2 weight (0 if no Day 2)
    NUMRECAL,                          # Number of recall days (1 or 2)
    ARIABC,                            # Remoteness (1=Major cities, 2=Inner regional, 3=Other)
    HYSCHCBC,                          # Highest year of school completed
    LVHNSQBC,                          # Level of highest non-school qualification
    ENERGYT1, ENERGYT2,                # Total energy intake kJ (Day 1, Day 2)
    starts_with("WPM01")              # 60 jackknife replicate weights
  )

cat("\nPersons loaded:", nrow(persons), "\n")

# --- 2b. Food file (Day 1 only for point estimates) -------------------------
foods <- read_csv(file.path(curf_dir, "npa11bf.csv"),
                  show_col_types = FALSE) %>%
  select(ABSPID, DAYNUM, FOODCODC, GRAMWGT)

cat("Food records loaded:", nrow(foods), "\n")
cat("Day 1 records:", sum(foods$DAYNUM == 1), "\n")
cat("Day 2 records:", sum(foods$DAYNUM == 2), "\n")

# =============================================================================
# SECTION 3: Create stratification variables on person file
# =============================================================================

# --- 3a. Age groups ---------------------------------------------------------
age_breaks <- c(2, 6, 11, 15, 20, 25, 35, 45, 55, 65, 75, 85, Inf)
age_labels <- 1:12  # GBI codes: 1 = 2-5y, 2 = 6-10y, ..., 12 = 85-100y

persons <- persons %>%
  mutate(
    age_grp = cut(AGE,
                  breaks = age_breaks,
                  labels = age_labels,
                  right = FALSE,
                  include.lowest = TRUE) %>% as.integer()
  )

# Verify
cat("\nAge group distribution:\n")
persons %>% count(age_grp) %>% print()

# --- 3b. Education level ----------------------------------------------------
# GBI categories:
#   1 = Primary (≤6 years formal education / ISCED 0-2)
#   2 = Secondary (>6 to ≤12 years / ISCED 3-4)
#   3 = Tertiary (>12 years / ISCED 5-8)
#
# Mapping from NNPAS:
#   Tertiary: LVHNSQBC in 1-4 (postgrad, bachelor, diploma, cert III/IV)
#   Secondary: HYSCHCBC in 1-3 (Year 10-12) AND no tertiary qual
#   Primary: HYSCHCBC in 4-5 (Year 9 or below) AND no tertiary qual
#
# For children <15: HYSCHCBC=0, LVHNSQBC=0
#   → Use education of the selected adult in the same household

# First, classify adults (age >= 15)
persons <- persons %>%
  mutate(
    edu_level_own = case_when(
      AGE < 15 ~ NA_integer_,
      LVHNSQBC %in% 1:4 ~ 3L,                          # Tertiary
      HYSCHCBC %in% 1:3 ~ 2L,                           # Secondary
      HYSCHCBC %in% 4:5 ~ 1L,                           # Primary
      # Edge cases: LVHNSQBC=8 (not determined), HYSCHCBC=0 for 15+ (shouldn't happen)
      TRUE ~ 2L                                          # Default to Secondary
    )
  )

# For children: assign education from an adult in the same household.
# The NNPAS CURF includes all persons aged 2+ per household, so some
# households have multiple adults. We take the highest education level
# among adults in the household to assign to children.
adult_edu <- persons %>%
  filter(AGE >= 15, !is.na(edu_level_own)) %>%
  group_by(ABSHID) %>%
  summarise(hh_adult_edu = max(edu_level_own), .groups = "drop")

persons <- persons %>%
  left_join(adult_edu, by = "ABSHID") %>%
  mutate(
    edu_level = if_else(AGE < 15, hh_adult_edu, edu_level_own)
  )

# Check for any remaining NAs (children in households with no adult record)
n_edu_na <- sum(is.na(persons$edu_level))
if (n_edu_na > 0) {
  cat("\nWARNING:", n_edu_na, "persons with missing education level — assigning to Secondary\n")
  persons <- persons %>%
    mutate(edu_level = replace_na(edu_level, 2L))
}

cat("\nEducation level distribution:\n")
persons %>% count(edu_level) %>% print()

# --- 3c. Area of residence --------------------------------------------------
# Urban (1) = Major cities (ARIABC=1) + Inner regional (ARIABC=2)
# Rural (2) = Other (ARIABC=3)
persons <- persons %>%
  mutate(
    residence = if_else(ARIABC %in% c(1, 2), 1L, 2L)
  )

cat("\nResidence distribution:\n")
persons %>% count(residence) %>% print()

# --- 3d. Energy in kcal -----------------------------------------------------
persons <- persons %>%
  mutate(
    energy_kcal_d1 = ENERGYT1 / 4.184,
    energy_kcal_d2 = if_else(NUMRECAL == 2, ENERGYT2 / 4.184, NA_real_)
  )

# =============================================================================
# SECTION 4: Calculate person-level bread intake (g/day)
# =============================================================================

# --- 4a. "Bread alone" — Day 1 ----------------------------------------------
# Filter food file to Day 1, join to bread-alone classification
bread_alone_d1 <- foods %>%
  filter(DAYNUM == 1) %>%
  inner_join(bread_alone_codes, by = "FOODCODC") %>%
  group_by(ABSPID) %>%
  summarise(
    ba_total_g  = sum(GRAMWGT, na.rm = TRUE),
    ba_wg_g     = sum(GRAMWGT[bread_class == "wholegrain"], na.rm = TRUE),
    ba_ref_g    = sum(GRAMWGT[bread_class == "refined"], na.rm = TRUE),
    .groups = "drop"
  )

# --- 4b. "Bread alone" — Day 2 (for SD correction only) --------------------
bread_alone_d2 <- foods %>%
  filter(DAYNUM == 2) %>%
  inner_join(bread_alone_codes, by = "FOODCODC") %>%
  group_by(ABSPID) %>%
  summarise(
    ba_total_g_d2 = sum(GRAMWGT, na.rm = TRUE),
    ba_wg_g_d2    = sum(GRAMWGT[bread_class == "wholegrain"], na.rm = TRUE),
    ba_ref_g_d2   = sum(GRAMWGT[bread_class == "refined"], na.rm = TRUE),
    .groups = "drop"
  )

# --- 4c. "Bread all sources" — Day 1 ----------------------------------------
# Join all food records to ADG bread content, calculate bread grams
bread_allsrc_d1 <- foods %>%
  filter(DAYNUM == 1) %>%
  inner_join(adg_bread_content, by = "FOODCODC") %>%
  mutate(
    wg_bread_g  = GRAMWGT * adg_wg_bread / 100,
    ref_bread_g = GRAMWGT * adg_ref_bread / 100
  ) %>%
  group_by(ABSPID) %>%
  summarise(
    bas_total_g = sum(wg_bread_g + ref_bread_g, na.rm = TRUE),
    bas_wg_g    = sum(wg_bread_g, na.rm = TRUE),
    bas_ref_g   = sum(ref_bread_g, na.rm = TRUE),
    .groups = "drop"
  )

# --- 4d. "Bread all sources" — Day 2 (for SD correction only) ---------------
bread_allsrc_d2 <- foods %>%
  filter(DAYNUM == 2) %>%
  inner_join(adg_bread_content, by = "FOODCODC") %>%
  mutate(
    wg_bread_g  = GRAMWGT * adg_wg_bread / 100,
    ref_bread_g = GRAMWGT * adg_ref_bread / 100
  ) %>%
  group_by(ABSPID) %>%
  summarise(
    bas_total_g_d2 = sum(wg_bread_g + ref_bread_g, na.rm = TRUE),
    bas_wg_g_d2    = sum(wg_bread_g, na.rm = TRUE),
    bas_ref_g_d2   = sum(ref_bread_g, na.rm = TRUE),
    .groups = "drop"
  )

# --- 4e. Merge bread intakes onto person file --------------------------------
# Non-consumers get 0 (left join, then replace NA with 0)
persons <- persons %>%
  left_join(bread_alone_d1, by = "ABSPID") %>%
  left_join(bread_alone_d2, by = "ABSPID") %>%
  left_join(bread_allsrc_d1, by = "ABSPID") %>%
  left_join(bread_allsrc_d2, by = "ABSPID") %>%
  mutate(across(c(ba_total_g, ba_wg_g, ba_ref_g,
                  ba_total_g_d2, ba_wg_g_d2, ba_ref_g_d2,
                  bas_total_g, bas_wg_g, bas_ref_g,
                  bas_total_g_d2, bas_wg_g_d2, bas_ref_g_d2),
                ~ replace_na(.x, 0)))

cat("\n--- Person-level bread intake summary (Day 1) ---\n")
persons %>%
  summarise(across(c(ba_total_g, ba_wg_g, ba_ref_g,
                     bas_total_g, bas_wg_g, bas_ref_g),
                   list(mean = mean, sd = sd, pct_zero = ~ mean(.x == 0) * 100),
                   .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("var", "stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  print(n = Inf)

# =============================================================================
# SECTION 5: SD correction — partition within/between-person variance
# =============================================================================
# Use ANOVA on persons with 2 recall days to estimate the ratio of
# between-person variance to total variance. Apply this ratio to
# deflate the Day 1 SD in each stratum.
#
# For each bread variable:
#   Total variance = between-person var + within-person var
#   Corrected SD = sqrt(between-person var) = sqrt(total var - within-person var)
#
# We estimate the variance ratio from the pooled 2-day subsample, then
# apply it within each stratum.

# Persons with 2 recall days
two_day <- persons %>%
  filter(NUMRECAL == 2)

cat("\nPersons with 2 recall days:", nrow(two_day), "\n")

# Function to estimate between-person variance ratio from 2-day data
# Returns ratio = var_between / var_total (pooled across all persons)
calc_bp_var_ratio <- function(day1, day2) {
  # Stack into long format for one-way ANOVA
  n <- length(day1)
  if (n < 2) return(1)  # fallback if too few

  person_mean <- (day1 + day2) / 2
  grand_mean  <- mean(person_mean)

  # Between-person sum of squares
  ss_between <- 2 * sum((person_mean - grand_mean)^2)
  # Within-person sum of squares
  ss_within  <- sum((day1 - person_mean)^2 + (day2 - person_mean)^2)
  # Mean squares
  ms_between <- ss_between / (n - 1)
  ms_within  <- ss_within / n    # df_within = n (each person contributes 1 df)

  # Variance components
  var_within  <- ms_within
  var_between <- max((ms_between - ms_within) / 2, 0)  # ensure non-negative

  var_total <- var_between + var_within
  if (var_total == 0) return(1)

  return(var_between / var_total)
}

# Calculate variance ratios for each bread variable
bread_vars <- c("ba_total_g", "ba_wg_g", "ba_ref_g",
                "bas_total_g", "bas_wg_g", "bas_ref_g")
bread_vars_d2 <- paste0(bread_vars, "_d2")

var_ratios <- tibble(
  var_name = bread_vars,
  bp_var_ratio = map2_dbl(
    bread_vars, bread_vars_d2,
    ~ calc_bp_var_ratio(two_day[[.x]], two_day[[.y]])
  )
)

cat("\nBetween-person variance ratios (pooled):\n")
print(var_ratios)

# =============================================================================
# SECTION 6: Set up survey design
# =============================================================================
# The NNPAS Basic CURF provides 60 jackknife replicate weights (WPM0101-WPM0160)
# for variance estimation.

rep_wt_cols <- paste0("WPM01", sprintf("%02d", 1:60))

svy_design <- persons %>%
  as_survey_rep(
    weights = NPAFINWT,
    repweights = all_of(rep_wt_cols),
    type = "JKn",
    scale = 59/60,       # standard jackknife scaling for 60 replicates
    rscales = rep(1, 60)
  )

cat("\nSurvey design created with", length(rep_wt_cols), "replicate weights\n")

# =============================================================================
# SECTION 7: Compute aggregate estimates for each stratum
# =============================================================================

# Function to produce GBI estimates for one bread definition × bread subtype
# across all strata defined by grouping variables.
#
# Args:
#   svy        - srvyr survey design object (built from persons data)
#   person_df  - the underlying persons data frame (for weighted SD calc)
#   intake_var - column name of the bread intake variable
#   grp_vars   - character vector of grouping variable names (can be empty)
#   var_ratio  - between-person variance ratio for SD correction
#
# Returns a tibble with: grouping vars, n, mean_g, sd_g, se_g,
#                         pct_non_consumers, mean_kcal

compute_estimates <- function(svy, person_df, intake_var, grp_vars, var_ratio) {

  # Handle the ungrouped case (population total) explicitly
  if (length(grp_vars) == 0) {
    mean_est <- svy %>%
      summarise(
        n          = unweighted(n()),
        mean_g     = survey_mean(.data[[intake_var]], na.rm = TRUE, vartype = "se"),
        mean_kcal  = survey_mean(energy_kcal_d1, na.rm = TRUE, vartype = "se"),
        pct_non_consumers = survey_mean(.data[[intake_var]] == 0, na.rm = TRUE,
                                         vartype = "se"),
        .groups = "drop"
      )
  } else {
    mean_est <- svy %>%
      group_by(across(all_of(grp_vars))) %>%
      summarise(
        n          = unweighted(n()),
        mean_g     = survey_mean(.data[[intake_var]], na.rm = TRUE, vartype = "se"),
        mean_kcal  = survey_mean(energy_kcal_d1, na.rm = TRUE, vartype = "se"),
        pct_non_consumers = survey_mean(.data[[intake_var]] == 0, na.rm = TRUE,
                                         vartype = "se"),
        .groups = "drop"
      )
  }

  mean_est <- mean_est %>%
    rename(se_g = mean_g_se) %>%
    select(-mean_kcal_se, -pct_non_consumers_se)

  # Weighted SD (Day 1), corrected for within-person variance
  # Weighted variance: sum(w * (x - xbar)^2) / (sum(w) - 1)
  calc_weighted_sd <- function(df) {
    x <- df[[intake_var]]
    w <- df[["NPAFINWT"]]
    wm <- weighted.mean(x, w, na.rm = TRUE)
    sd_raw <- sqrt(sum(w * (x - wm)^2, na.rm = TRUE) / (sum(w, na.rm = TRUE) - 1))
    sd_raw * sqrt(var_ratio)
  }

  if (length(grp_vars) == 0) {
    sd_est <- tibble(sd_g = calc_weighted_sd(person_df))
  } else {
    sd_est <- person_df %>%
      group_by(across(all_of(grp_vars))) %>%
      group_modify(~ tibble(sd_g = calc_weighted_sd(.x))) %>%
      ungroup()
  }

  # Merge
  if (length(grp_vars) == 0) {
    result <- bind_cols(mean_est, sd_est)
  } else {
    result <- mean_est %>%
      left_join(sd_est, by = grp_vars)
  }

  result <- result %>%
    mutate(
      pct_non_consumers = pct_non_consumers * 100,
      mean_kcal = round(mean_kcal, 1)
    ) %>%
    select(any_of(grp_vars), n, mean_g, sd_g, se_g, pct_non_consumers, mean_kcal)

  return(result)
}

# --- Define all strata combinations -----------------------------------------

# Map from internal variable names to GBI bread_subtype codes
bread_subtype_map <- list(
  ba_total_g  = list(bread_def = 1L, bread_subtype = 0L, var_name = "ba_total_g"),
  ba_wg_g     = list(bread_def = 1L, bread_subtype = 1L, var_name = "ba_wg_g"),
  ba_ref_g    = list(bread_def = 1L, bread_subtype = 2L, var_name = "ba_ref_g"),
  bas_total_g = list(bread_def = 2L, bread_subtype = 0L, var_name = "bas_total_g"),
  bas_wg_g    = list(bread_def = 2L, bread_subtype = 1L, var_name = "bas_wg_g"),
  bas_ref_g   = list(bread_def = 2L, bread_subtype = 2L, var_name = "bas_ref_g")
)

# --- 7a. Fully stratified estimates (age × sex × education × residence) -----
cat("\n--- Computing fully stratified estimates ---\n")

# Rename SEX to sex in persons and survey design so output columns are consistent
persons <- persons %>% rename(sex = SEX)
svy_design <- persons %>%
  as_survey_rep(
    weights = NPAFINWT,
    repweights = all_of(rep_wt_cols),
    type = "JKn",
    scale = 59/60,
    rscales = rep(1, 60)
  )

grp_full <- c("sex", "residence", "edu_level", "age_grp")

results_full <- map_dfr(bread_subtype_map, function(info) {
  cat("  ", info$var_name, "\n")
  vr <- var_ratios %>% filter(var_name == info$var_name) %>% pull(bp_var_ratio)

  compute_estimates(svy_design, persons, info$var_name, grp_full, vr) %>%
    mutate(
      bread_def     = info$bread_def,
      bread_subtype = info$bread_subtype,
      energy_adj    = 1L
    )
})

# --- 7b. Age × Sex only estimates (including sex-combined) ------------------
cat("\n--- Computing age × sex estimates ---\n")

# Male and Female separately, by age group
results_agesex <- map_dfr(bread_subtype_map, function(info) {
  cat("  ", info$var_name, "\n")
  vr <- var_ratios %>% filter(var_name == info$var_name) %>% pull(bp_var_ratio)

  compute_estimates(svy_design, persons, info$var_name, c("sex", "age_grp"), vr) %>%
    mutate(
      bread_def     = info$bread_def,
      bread_subtype = info$bread_subtype,
      energy_adj    = 1L,
      residence     = NA_integer_,
      edu_level     = NA_integer_
    )
})

# Sex-combined (sex = 0) by age group
results_sexcomb <- map_dfr(bread_subtype_map, function(info) {
  vr <- var_ratios %>% filter(var_name == info$var_name) %>% pull(bp_var_ratio)

  compute_estimates(svy_design, persons, info$var_name, "age_grp", vr) %>%
    mutate(
      bread_def     = info$bread_def,
      bread_subtype = info$bread_subtype,
      energy_adj    = 1L,
      sex           = 0L,
      residence     = NA_integer_,
      edu_level     = NA_integer_
    )
})

# Total (all ages) for age×sex outputs (age_grp = 0)
results_total_agesex <- map_dfr(bread_subtype_map, function(info) {
  vr <- var_ratios %>% filter(var_name == info$var_name) %>% pull(bp_var_ratio)

  # Male, Female for Total (all ages)
  by_sex <- compute_estimates(svy_design, persons, info$var_name, "sex", vr) %>%
    mutate(age_grp = 0L)

  # Sex-combined for Total (all ages)
  overall <- compute_estimates(svy_design, persons, info$var_name, character(0), vr) %>%
    mutate(age_grp = 0L, sex = 0L)

  bind_rows(by_sex, overall) %>%
    mutate(
      bread_def     = info$bread_def,
      bread_subtype = info$bread_subtype,
      energy_adj    = 1L,
      residence     = NA_integer_,
      edu_level     = NA_integer_
    )
})

results_agesex_all <- bind_rows(results_agesex, results_sexcomb,
                                results_total_agesex)

# =============================================================================
# SECTION 8: Energy adjustment (residual method)
# =============================================================================
# For each bread variable, within the full sample:
#   1. Regress bread intake (or log(bread+0.00001)) on total energy (kcal)
#   2. Get residuals
#   3. Predict bread at 2000 kcal, add to residuals
#   4. Exponentiate if log-transformed
#   5. Report mean/SD of energy-adjusted values per stratum

cat("\n--- Computing energy-adjusted estimates ---\n")

energy_adjust <- function(bread_g, energy_kcal) {
  # Check if log transformation is needed (Shapiro-Wilk on subsample)
  use_log <- FALSE
  test_sample <- bread_g[bread_g > 0]
  if (length(test_sample) > 10) {
    sw <- shapiro.test(sample(test_sample, min(5000, length(test_sample))))
    if (sw$p.value < 0.05) use_log <- TRUE
  }

  if (use_log) {
    y <- log(bread_g + 0.00001)
  } else {
    y <- bread_g
  }

  fit <- lm(y ~ energy_kcal)
  residuals <- residuals(fit)
  predicted_2000 <- predict(fit, newdata = data.frame(energy_kcal = 2000))
  ea_values <- residuals + predicted_2000

  if (use_log) {
    ea_values <- exp(ea_values)
  }

  return(ea_values)
}

# Apply energy adjustment to each bread variable
for (var_name in bread_vars) {
  ea_col <- paste0(var_name, "_ea")
  persons[[ea_col]] <- energy_adjust(persons[[var_name]], persons$energy_kcal_d1)
  cat("  Energy-adjusted:", var_name, "→", ea_col, "\n")
}

# Rebuild survey design with energy-adjusted variables added
svy_design <- persons %>%
  as_survey_rep(
    weights = NPAFINWT,
    repweights = all_of(rep_wt_cols),
    type = "JKn",
    scale = 59/60,
    rscales = rep(1, 60)
  )

# EA variable mapping
ea_subtype_map <- list(
  ba_total_g_ea  = list(bread_def = 1L, bread_subtype = 0L, var_name = "ba_total_g_ea",
                        orig_var = "ba_total_g"),
  ba_wg_g_ea     = list(bread_def = 1L, bread_subtype = 1L, var_name = "ba_wg_g_ea",
                         orig_var = "ba_wg_g"),
  ba_ref_g_ea    = list(bread_def = 1L, bread_subtype = 2L, var_name = "ba_ref_g_ea",
                          orig_var = "ba_ref_g"),
  bas_total_g_ea = list(bread_def = 2L, bread_subtype = 0L, var_name = "bas_total_g_ea",
                         orig_var = "bas_total_g"),
  bas_wg_g_ea    = list(bread_def = 2L, bread_subtype = 1L, var_name = "bas_wg_g_ea",
                          orig_var = "bas_wg_g"),
  bas_ref_g_ea   = list(bread_def = 2L, bread_subtype = 2L, var_name = "bas_ref_g_ea",
                           orig_var = "bas_ref_g")
)

# Fully stratified EA estimates
results_full_ea <- map_dfr(ea_subtype_map, function(info) {
  cat("  EA fully stratified:", info$var_name, "\n")
  vr <- var_ratios %>% filter(var_name == info$orig_var) %>% pull(bp_var_ratio)

  compute_estimates(svy_design, persons, info$var_name, grp_full, vr) %>%
    mutate(
      bread_def     = info$bread_def,
      bread_subtype = info$bread_subtype,
      energy_adj    = 2L
    )
})

# Age × Sex EA estimates (Male, Female, Sex-combined, plus Total all ages)
results_agesex_ea <- map_dfr(ea_subtype_map, function(info) {
  vr <- var_ratios %>% filter(var_name == info$orig_var) %>% pull(bp_var_ratio)

  bind_rows(
    # By sex and age_grp
    compute_estimates(svy_design, persons, info$var_name, c("sex", "age_grp"), vr),
    # Sex-combined by age_grp
    compute_estimates(svy_design, persons, info$var_name, "age_grp", vr) %>%
      mutate(sex = 0L),
    # Total (all ages) by sex
    compute_estimates(svy_design, persons, info$var_name, "sex", vr) %>%
      mutate(age_grp = 0L),
    # Total (all ages), sex-combined
    compute_estimates(svy_design, persons, info$var_name, character(0), vr) %>%
      mutate(age_grp = 0L, sex = 0L)
  ) %>%
    mutate(
      bread_def     = info$bread_def,
      bread_subtype = info$bread_subtype,
      energy_adj    = 2L,
      residence     = NA_integer_,
      edu_level     = NA_integer_
    )
})

# =============================================================================
# SECTION 9: Assemble final output
# =============================================================================

cat("\n--- Assembling final output ---\n")

final_output <- bind_rows(
  results_full,         # Block 1 & 2: Non-EA, fully stratified
  results_agesex_all,   # Block 5 & 7: Non-EA, age×sex only
  results_full_ea,      # Block 3 & 4: EA, fully stratified
  results_agesex_ea     # Block 6 & 8: EA, age×sex only
) %>%
  mutate(
    survey_name = "National Nutrition and Physical Activity Survey",
    year_start  = 2011L,
    year_end    = 2012L,
    notes       = case_when(
      bread_def == 1 & energy_adj == 1 ~
        paste("Bread alone includes all AUSNUT 2011-13 codes 12201001-12307004",
              "(standalone bread products including bagels, rolls, flatbreads,",
              "English muffins, sweet buns, garlic bread).",
              "Urban = Major cities + Inner regional (ARIABC 1-2);",
              "Rural = Other (ARIABC 3).",
              "Education for children <15 assigned from the highest-educated",
              "adult in the household."),
      TRUE ~ NA_character_
    )
  ) %>%
  # Round numeric columns
  mutate(
    mean_g            = round(mean_g, 1),
    sd_g              = round(sd_g, 1),
    se_g              = round(se_g, 2),
    pct_non_consumers = round(pct_non_consumers, 1)
  ) %>%
  # Select and order columns to match Data_Template
  select(
    survey_name, year_start, year_end,
    bread_def, bread_subtype, energy_adj,
    sex, residence, edu_level, age_grp,
    n, mean_g, sd_g, se_g, pct_non_consumers, mean_kcal,
    notes
  ) %>%
  arrange(
    energy_adj, bread_def, bread_subtype,
    desc(!is.na(residence)),  # fully stratified first
    sex, residence, edu_level, age_grp
  )

# =============================================================================
# SECTION 10: QC checks
# =============================================================================

cat("\n=== FINAL OUTPUT SUMMARY ===\n")
cat("Total rows:", nrow(final_output), "\n\n")

final_output %>%
  count(bread_def, energy_adj, is_fully_strat = !is.na(residence)) %>%
  print()

# --- QC: Total bread ≈ WG + Refined within each stratum --------------------
# Only applies to non-energy-adjusted estimates (energy_adj==1).
# Energy adjustment runs separate regressions, so additivity doesn't hold.
cat("\n--- QC: Total bread = WG + Refined (non-energy-adjusted only) ---\n")

qc <- final_output %>%
  filter(energy_adj == 1) %>%
  select(bread_def, energy_adj, sex, residence, edu_level, age_grp,
         bread_subtype, mean_g) %>%
  pivot_wider(names_from = bread_subtype, values_from = mean_g,
              names_prefix = "bt_") %>%
  mutate(diff = bt_0 - (bt_1 + bt_2))

cat("Max absolute difference:", max(abs(qc$diff), na.rm = TRUE), "g/day\n")
cat("(Should be ≤0.2 — rounding artefact only)\n")

# --- QC: Sample size -------------------------------------------------------
cat("\nMax n in any stratum:", max(final_output$n, na.rm = TRUE), "\n")
cat("Total persons in sample:", nrow(persons), "\n")
if (max(final_output$n, na.rm = TRUE) > nrow(persons)) {
  warning("n exceeds sample size — possible duplication issue!")
}

# =============================================================================
# SECTION 11: Export CSV
# =============================================================================

write_csv(final_output, "GBI_DataTemplate_NNPAS_2011-12.csv")
cat("\nCSV exported to: GBI_DataTemplate_NNPAS_2011-12.csv\n")

# =============================================================================
# SECTION 12: Fill GBI Excel template
# =============================================================================
# Reads the original GBI_Aggregate_Data_Form.xlsx template and produces a
# filled copy with Survey_Info_Template, Codebook, and Data_Template populated.
# Requires: install.packages("openxlsx")

library(openxlsx)

template_path <- "GBI_Aggregate_Data_Form.xlsx"
output_xlsx   <- "GBI_AggregateDataForm_NNPAS_2011-12.xlsx"

cat("\n--- Filling GBI Excel template ---\n")

wb <- loadWorkbook(template_path)

# --- 12a. Survey_Info_Template ------------------------------------------------
# The template has field labels in column B and entry cells in column C.
# Row layout (from template inspection):
#   Row 3: Survey name
#   Row 4: Country
#   Row 5: Survey year(s)
#   Row 6: Geographic coverage
#   Row 7: Target population
#   Row 8: Sample size
#   Row 9: Dietary assessment method
#   Row 10: Number of recall/record days
#   Row 11: Sampling design
#   Row 12: Survey weights available?
#   Row 13: Replicate weights available?
#   Row 14: Data source / reference
#   Row 15: Food composition database used
#   Row 16: Bread classification method
#   Row 17: Mixed dish disaggregation method
#   Row 18: Energy adjustment method
#   Row 19: Additional notes

survey_info <- list(
  c(3, "National Nutrition and Physical Activity Survey (NNPAS)"),
  c(4, "Australia"),
  c(5, "2011-2012"),
  c(6, "National (all states and territories, urban and rural)"),
  c(7, "Persons aged 2 years and over, usual residents of private dwellings"),
  c(8, "12,153 persons (7,735 with 2-day recall)"),
  c(9, "24-hour dietary recall (interviewer-administered, AMPM method)"),
  c(10, "2 days for most respondents (Day 1 face-to-face, Day 2 telephone); Day 1 used for point estimates"),
  c(11, "Multi-stage area probability sample of private dwellings; stratified by state/territory and capital city/rest of state"),
  c(12, "Yes — NPAFINWT (Day 1 person weight)"),
  c(13, "Yes — 60 jackknife replicate weights (WPM0101–WPM0160)"),
  c(14, "ABS Cat. No. 4324.0.55.002, Microdata: Australian Health Survey, Nutrition and Physical Activity, 2011-12, Basic CURF"),
  c(15, "AUSNUT 2011-13 (ABS/FSANZ); ADG Database used for mixed dish disaggregation"),
  c(16, paste("Bread alone: all AUSNUT 2011-13 food codes 12201001–12307004",
              "(standalone bread products including regular breads, rolls, flatbreads,",
              "English muffins, sweet buns, garlic bread).",
              "Wholegrain/refined classification based on ADG Database columns 1011/1021,",
              "with keyword matching for items not classified by ADG.")),
  c(17, paste("ADG Database provides g/100g of wholegrain bread (col 1011) and",
              "refined bread (col 1021) for all foods.",
              "Bread content of mixed dishes calculated as GRAMWGT × (ADG value / 100).")),
  c(18, paste("Residual method: regress bread intake (log-transformed if non-normal)",
              "on total energy intake (kcal/day); add predicted value at 2000 kcal",
              "to residuals; exponentiate if log-transformed.",
              "Energy adjustment performed on full sample, not per-stratum.")),
  c(19, paste("Urban = Major cities + Inner regional (ARIABC 1-2); Rural = Other remote areas (ARIABC 3).",
              "Education for children <15 assigned from the highest-educated adult in the household.",
              "SD corrected for within-person variation using ANOVA on 2-day subsample",
              "(between-person variance ratio applied to Day 1 weighted SD).",
              "SE estimated via jackknife replication (60 replicate weights)."))
)

# Write to column C (col 3), starting at the rows indicated
ws_info <- "Survey_Info_Template"
for (item in survey_info) {
  row_num <- as.integer(item[[1]])
  value   <- item[[2]]
  writeData(wb, sheet = ws_info, x = value, startCol = 3, startRow = row_num,
            colNames = FALSE)
}

cat("  Survey_Info_Template filled\n")

# --- 12b. Codebook -----------------------------------------------------------
# Column A (col 1) contains "Availability in your dataset (Yes/No)"
# Rows 3-19 correspond to each variable (survey_name through notes)
# All variables are available in our data except we document specifics

codebook_availability <- c(
  "Yes",  # survey_name (row 3)
  "Yes",  # year_start (row 4)
  "Yes",  # year_end (row 5)
  "Yes",  # bread_def (row 6)
  "Yes",  # bread_subtype (row 7)
  "Yes",  # energy_adj (row 8)
  "Yes",  # sex (row 9)
  "Yes",  # residence (row 10)
  "Yes",  # edu_level (row 11)
  "Yes",  # age_grp (row 12)
  "Yes",  # n (row 13)
  "Yes",  # mean_g (row 14)
  "Yes",  # sd_g (row 15)
  "Yes",  # se_g (row 16)
  "Yes",  # pct_non_consumers (row 17)
  "Yes",  # mean_kcal (row 18)
  "Yes"   # notes (row 19)
)

# Variable-specific comments (column J, col 10)
codebook_comments <- c(
  NA,  # survey_name
  NA,  # year_start
  NA,  # year_end
  "Both bread alone (1) and bread all sources (2) provided.",  # bread_def
  "Wholegrain/refined classification based on ADG Database columns 1011/1021, with keyword matching for unclassified items.",  # bread_subtype
  "Both non-energy-adjusted (1) and energy-adjusted (2) estimates provided. Energy adjustment uses the residual method standardised to 2000 kcal.",  # energy_adj
  NA,  # sex
  "Urban = ARIABC 1 (Major cities) + ARIABC 2 (Inner regional). Rural = ARIABC 3 (Other/remote). This differs from a strict urban/rural split.",  # residence
  paste("Mapped from NNPAS: Tertiary = LVHNSQBC 1-4 (postgrad, bachelor, diploma, cert III/IV);",
        "Secondary = Year 10-12 (HYSCHCBC 1-3) without tertiary qual;",
        "Primary = Year 9 or below (HYSCHCBC 4-5) without tertiary qual.",
        "Children <15: highest-educated adult in household."),  # edu_level
  "NNPAS age range is 2-85+. Mapped to GBI bands. Age 85 coded as 85+ (GBI band 12: 85-100).",  # age_grp
  NA,  # n
  "Weighted mean using NPAFINWT (Day 1 person weight). Day 1 recall only.",  # mean_g
  "Weighted SD, corrected for within-person variation using ANOVA on 2-day subsample. Between-person variance ratio applied to deflate Day 1 SD.",  # sd_g
  "Design-based SE from jackknife replication using 60 replicate weights (WPM0101-WPM0160).",  # se_g
  "Weighted proportion of persons with zero intake on Day 1.",  # pct_non_consumers
  "Weighted mean total energy intake (Day 1), converted from kJ to kcal (÷ 4.184).",  # mean_kcal
  NA   # notes
)

for (i in seq_along(codebook_availability)) {
  row_num <- i + 2  # data starts at row 3
  writeData(wb, sheet = "Codebook", x = codebook_availability[i],
            startCol = 1, startRow = row_num, colNames = FALSE)
  if (!is.na(codebook_comments[i])) {
    writeData(wb, sheet = "Codebook", x = codebook_comments[i],
              startCol = 10, startRow = row_num, colNames = FALSE)
  }
}

cat("  Codebook filled\n")

# --- 12c. Data_Template -------------------------------------------------------
# Clear example data rows (rows 4-35) and write our estimates
# Header row is row 3 with variable names
# We write starting at row 4

# Remove example data (rows 4 onwards)
# openxlsx doesn't have a clean "delete rows" so we overwrite
data_for_template <- final_output %>%
  select(survey_name, year_start, year_end,
         bread_def, bread_subtype, energy_adj,
         sex, residence, edu_level, age_grp,
         n, mean_g, sd_g, se_g, pct_non_consumers, mean_kcal,
         notes)

writeData(wb, sheet = "Data_Template", x = data_for_template,
          startCol = 1, startRow = 4, colNames = FALSE)

cat("  Data_Template filled with", nrow(data_for_template), "rows\n")

# --- Save filled workbook ----------------------------------------------------
saveWorkbook(wb, output_xlsx, overwrite = TRUE)
cat("\nFilled Excel template saved to:", output_xlsx, "\n")

cat("\n=== ALL DONE ===\n")

