# =============================================================================
# GBI Aggregate Bread Intake Estimates -- NNPAS 2023
# =============================================================================
# Produces population-level bread intake estimates from the ABS 2023
# National Nutrition and Physical Activity Survey (NNPAS), formatted for
# the Global Bread Intake (GBI) Study aggregate data request.
#
# This is the 2023 companion to GBI_bread_estimates_NNPAS_2011_11.R.
# Differences from the 2011-12 script:
#   - Uses sas7bdat unit-record files (haven::read_sas) instead of CSV CURF.
#   - "Bread all sources" is computed directly from food-record-level WGBRGM
#     and RFBRGM (already in grams), so no ADG disaggregation is required.
#   - Standalone-bread classification combines (a) description keywords,
#     (b) AUSNUT 2023 WGBRGM/RFBRGM majority, (c) FIBRE g/100 g cut-off.
#   - Education uses the single combined HIGHLVLD field.
#   - Sex uses SEXBIRTH.
#   - Remoteness from ARIA21SL: urban = 1-2, rural = 3-5.
#   - 2-day responder flag derived from COUNTFD2 > 0.
#   - Day-1 energy uses ENERGYF1 (food-only); kcal = ENERGYF1 / 4.184.
#
# Output: Long-format data frame matching the GBI Data_Template structure,
#         with fully stratified and age x sex-only estimates for:
#         - "Bread alone" and "Bread all sources"
#         - Total bread, wholegrain bread, refined bread
#         - Non-energy-adjusted and energy-adjusted (residual method)
# =============================================================================

library(tidyverse)
library(readxl)
library(haven)
library(survey)
library(srvyr)
library(openxlsx)

# --- Paths ----------------------------------------------------------------
# Update this to the directory holding the NNPAS 2023 sas7bdat files.
nnpas_dir   <- "NNPAS_2023"
ausnut_path <- file.path(nnpas_dir, "AUSNUT_2023.xlsx")
hhd_path    <- file.path(nnpas_dir, "nnpas23hhd.sas7bdat")
sps_path    <- file.path(nnpas_dir, "nnpas23sps.sas7bdat")
food_path   <- file.path(nnpas_dir, "nnpas23food.sas7bdat")

template_path <- "GBI_Aggregate_Data_Form_unprotected.xlsx"
output_csv    <- "GBI_DataTemplate_NNPAS_2023.csv"
output_xlsx   <- "GBI_AggregateDataForm_NNPAS_2023.xlsx"

# =============================================================================
# SECTION 1: Build standalone-bread classification from AUSNUT 2023
# =============================================================================
# "Bread alone" = AUSNUT 2023 food codes 12201001 to 12306003.
# Each code is classified as wholegrain or refined using a layered rule:
#   Rule A: description keywords (wholemeal, wholegrain, multigrain, mixed
#           grain, rye, spelt, pumpernickel, "added fibre", "high fibre",
#           chapatti, injera, roti) -> wholegrain.
#   Rule B: AUSNUT WGBRGM vs RFBRGM majority (whichever is larger).
#   Rule C: FIBRE > 5 g/100 g -> wholegrain, else refined.
#   Default: refined (the Australian default for bread is white flour).

ausnut <- read_excel(ausnut_path, sheet = 1)

bread_alone_codes <- ausnut %>%
  filter(FOODCODL >= 12201001, FOODCODL <= 12306003) %>%
  select(FOODCODL, Food_name, FIBRE, WGBRGM, RFBRGM)

wg_keywords <- c("wholemeal", "wholegrain", "whole grain", "mixed grain",
                 "multigrain", "multi-grain", "\\brye\\b", "spelt",
                 "pumpernickel", "added fibre", "high fibre",
                 "chapatti", "injera", "roti")

bread_alone_codes <- bread_alone_codes %>%
  mutate(
    desc_lower = str_to_lower(Food_name),
    desc_wg    = str_detect(desc_lower, str_c(wg_keywords, collapse = "|")),
    bread_class = case_when(
      desc_wg                                    ~ "wholegrain",  # Rule A
      WGBRGM > RFBRGM                            ~ "wholegrain",  # Rule B
      RFBRGM > WGBRGM                            ~ "refined",     # Rule B
      FIBRE > 5                                  ~ "wholegrain",  # Rule C
      TRUE                                       ~ "refined"      # default
    )
  ) %>%
  select(FOODCODL, Food_name, FIBRE, WGBRGM, RFBRGM, bread_class)

cat("Standalone bread codes (12201001-12306003):", nrow(bread_alone_codes), "\n")
cat("Classification breakdown:\n")
count(bread_alone_codes, bread_class) %>% print()

# Save the classification table for QC
write_csv(bread_alone_codes, "bread_alone_classification_NNPAS_2023.csv")

# =============================================================================
# SECTION 2: Load NNPAS 2023 unit-record files
# =============================================================================

# --- 2a. Household file -----------------------------------------------------
hhd <- read_sas(hhd_path) %>%
  select(ABSHIDD, ARIA21SL)

cat("\nHouseholds loaded:", nrow(hhd), "\n")

# --- 2b. Selected person file -----------------------------------------------
# We need: IDs, weights and replicate weights, age, sex, education,
# Day-1 energy (food), Day-2 energy (food), and Day-2 food count
# (to flag 2-day responders).
sps <- read_sas(sps_path) %>%
  rename(ABSPID = ABSPIDD)   # DIL labels it ABSPID; sas file column is ABSPIDD

# Identify replicate weight columns. The DIL labels them "WPM01" with
# 60 replicates -- they appear in the sas file as WPM0101..WPM0160.
rep_wt_cols <- grep("^WPM01[0-9]{2}$", names(sps), value = TRUE)
if (length(rep_wt_cols) != 60) {
  warning("Expected 60 replicate weight columns matching ^WPM01[0-9]{2}$, ",
          "found ", length(rep_wt_cols), ". Check column naming.")
}

persons <- sps %>%
  select(
    ABSHIDD, ABSPID,
    AGE99, SEXBIRTH,
    HIGHLVLD,
    NPAFINWT,
    all_of(rep_wt_cols),
    ENERGYF1, ENERGYF2,
    COUNTFD2
  ) %>%
  left_join(hhd, by = "ABSHIDD") %>%
  # Survey-wide person key: ABSPID is only unique within a household,
  # so concatenate with ABSHIDD before any group_by/join across files.
  mutate(person_uid = paste(ABSHIDD, ABSPID, sep = "_"))

cat("Persons loaded:", nrow(persons), "\n")
cat("Distinct person_uid:", dplyr::n_distinct(persons$person_uid), "\n")

# --- 2c. Food file ----------------------------------------------------------
foods <- read_sas(food_path) %>%
  rename(ABSPID = ABSPIDD) %>%   # DIL labels it ABSPID; sas file column is ABSPIDD
  select(ABSHIDD, ABSPID, DAYNUM, FOODCODL, GRAMWGT, WGBRGM, RFBRGM) %>%
  mutate(person_uid = paste(ABSHIDD, ABSPID, sep = "_"))

cat("Food records loaded:", nrow(foods), "\n")
cat("  Day 1 records:", sum(foods$DAYNUM == 1), "\n")
cat("  Day 2 records:", sum(foods$DAYNUM == 2), "\n")

# =============================================================================
# SECTION 3: Stratification variables on the person file
# =============================================================================

# --- 3a. Age groups (GBI bands 1-12) ----------------------------------------
age_breaks <- c(2, 6, 11, 15, 20, 25, 35, 45, 55, 65, 75, 85, Inf)
age_labels <- 1:12  # 1 = 2-5y, 2 = 6-10y, ..., 12 = 85+

persons <- persons %>%
  mutate(
    age_grp = cut(AGE99,
                  breaks = age_breaks,
                  labels = age_labels,
                  right = FALSE,
                  include.lowest = TRUE) %>% as.integer()
  )

cat("\nAge group distribution:\n")
persons %>% count(age_grp) %>% print()

# --- 3b. Sex (use SEXBIRTH for biological sex) ------------------------------
# Convention: 1 = Male, 2 = Female (matches GBI Data_Template).
persons <- persons %>%
  mutate(sex = as.integer(SEXBIRTH))

# --- 3c. Education (HIGHLVLD -> GBI Primary/Secondary/Tertiary) -------------
# Tertiary (3): Postgraduate, Bachelor, Diploma/Adv Diploma, Cert III/IV.
# Secondary (2): Year 10-12, Cert I/II.
# Primary (1): Year 9 and below, no educational attainment.
# 999 = Not applicable (children); 011 = Level not determined.
# HIGHLVLD is character ("100","611",...). Convert with care.

tertiary_codes  <- c("100","110","111","114","120",
                     "200","211","212","213","221","222",
                     "310",
                     "411","413","421",
                     "510","511","514")
secondary_codes <- c("520","521","524",
                     "611","610","613","621")
primary_codes   <- c("000","600","622","623")

persons <- persons %>%
  mutate(
    HIGHLVLD_chr = sprintf("%s", HIGHLVLD),
    edu_level_own = case_when(
      AGE99 < 15                                  ~ NA_integer_,
      HIGHLVLD_chr %in% tertiary_codes            ~ 3L,
      HIGHLVLD_chr %in% secondary_codes           ~ 2L,
      HIGHLVLD_chr %in% primary_codes             ~ 1L,
      HIGHLVLD_chr %in% c("500","011")            ~ 2L,  # cert nfd / not det -> Secondary
      HIGHLVLD_chr %in% c("999")                  ~ NA_integer_,
      TRUE                                        ~ 2L   # safety default
    )
  )

# Children <15: assign highest adult education in the same household
adult_edu <- persons %>%
  filter(AGE99 >= 15, !is.na(edu_level_own)) %>%
  group_by(ABSHIDD) %>%
  summarise(hh_adult_edu = max(edu_level_own), .groups = "drop")

persons <- persons %>%
  left_join(adult_edu, by = "ABSHIDD") %>%
  mutate(
    edu_level = if_else(AGE99 < 15, hh_adult_edu, edu_level_own)
  )

n_edu_na <- sum(is.na(persons$edu_level))
if (n_edu_na > 0) {
  cat("\nWARNING:", n_edu_na, "persons with missing education -- defaulting to Secondary\n")
  persons <- persons %>% mutate(edu_level = replace_na(edu_level, 2L))
}

cat("\nEducation level distribution:\n")
persons %>% count(edu_level) %>% print()

# --- 3d. Residence (ARIA21SL -> Urban/Rural) --------------------------------
# Urban (1) = ARIA21SL 1 (Major cities) + 2 (Inner regional)
# Rural (2) = ARIA21SL 3 (Outer regional) + 4 (Remote) + 5 (Very remote)
persons <- persons %>%
  mutate(
    ARIA21SL_n = as.integer(ARIA21SL),
    residence = case_when(
      ARIA21SL_n %in% c(1, 2)        ~ 1L,
      ARIA21SL_n %in% c(3, 4, 5)     ~ 2L,
      TRUE                           ~ NA_integer_
    )
  )

cat("\nResidence distribution:\n")
persons %>% count(residence) %>% print()

# --- 3e. Energy in kcal and 2-day flag --------------------------------------
persons <- persons %>%
  mutate(
    energy_kcal_d1 = ENERGYF1 / 4.184,
    energy_kcal_d2 = if_else(!is.na(ENERGYF2) & COUNTFD2 > 0,
                             ENERGYF2 / 4.184, NA_real_),
    has_day2 = !is.na(COUNTFD2) & COUNTFD2 > 0
  )

cat("\n2-day responders:", sum(persons$has_day2), "of", nrow(persons), "\n")

# =============================================================================
# SECTION 4: Person-level bread intake (g/day)
# =============================================================================

# --- 4a. Bread alone (Day 1) ------------------------------------------------
# Filter food records to standalone bread codes, classify, sum GRAMWGT.
bread_alone_d1 <- foods %>%
  filter(DAYNUM == 1) %>%
  inner_join(bread_alone_codes %>% select(FOODCODL, bread_class),
             by = "FOODCODL") %>%
  group_by(person_uid) %>%
  summarise(
    ba_total_g = sum(GRAMWGT, na.rm = TRUE),
    ba_wg_g    = sum(GRAMWGT[bread_class == "wholegrain"], na.rm = TRUE),
    ba_ref_g   = sum(GRAMWGT[bread_class == "refined"],    na.rm = TRUE),
    .groups    = "drop"
  )

# --- 4b. Bread alone (Day 2) for SD correction ------------------------------
bread_alone_d2 <- foods %>%
  filter(DAYNUM == 2) %>%
  inner_join(bread_alone_codes %>% select(FOODCODL, bread_class),
             by = "FOODCODL") %>%
  group_by(person_uid) %>%
  summarise(
    ba_total_g_d2 = sum(GRAMWGT, na.rm = TRUE),
    ba_wg_g_d2    = sum(GRAMWGT[bread_class == "wholegrain"], na.rm = TRUE),
    ba_ref_g_d2   = sum(GRAMWGT[bread_class == "refined"],    na.rm = TRUE),
    .groups       = "drop"
  )

# --- 4c. Bread all sources (Day 1) ------------------------------------------
# WGBRGM and RFBRGM in the food file are already grams of bread per food
# record (AUSNUT g/100g x GRAMWGT/100). Sum directly.
bread_allsrc_d1 <- foods %>%
  filter(DAYNUM == 1) %>%
  group_by(person_uid) %>%
  summarise(
    bas_wg_g    = sum(WGBRGM,  na.rm = TRUE),
    bas_ref_g   = sum(RFBRGM,  na.rm = TRUE),
    bas_total_g = bas_wg_g + bas_ref_g,
    .groups     = "drop"
  )

# --- 4d. Bread all sources (Day 2) for SD correction ------------------------
bread_allsrc_d2 <- foods %>%
  filter(DAYNUM == 2) %>%
  group_by(person_uid) %>%
  summarise(
    bas_wg_g_d2    = sum(WGBRGM, na.rm = TRUE),
    bas_ref_g_d2   = sum(RFBRGM, na.rm = TRUE),
    bas_total_g_d2 = bas_wg_g_d2 + bas_ref_g_d2,
    .groups        = "drop"
  )

# --- 4e. Merge onto persons; non-consumers get 0 ----------------------------
persons <- persons %>%
  left_join(bread_alone_d1,    by = "person_uid") %>%
  left_join(bread_alone_d2,    by = "person_uid") %>%
  left_join(bread_allsrc_d1,   by = "person_uid") %>%
  left_join(bread_allsrc_d2,   by = "person_uid") %>%
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
# SECTION 5: SD correction -- partition within/between-person variance
# =============================================================================
# Same logic as 2011-12: ANOVA on 2-day responders to estimate the ratio of
# between-person variance to total variance, then deflate the Day-1 weighted
# SD in each stratum. Two-day responders are flagged by COUNTFD2 > 0.

two_day <- persons %>% filter(has_day2)
cat("\nPersons with 2 recall days:", nrow(two_day), "\n")

calc_bp_var_ratio <- function(day1, day2) {
  n <- length(day1)
  if (n < 2) return(1)
  person_mean <- (day1 + day2) / 2
  grand_mean  <- mean(person_mean)
  ss_between  <- 2 * sum((person_mean - grand_mean)^2)
  ss_within   <- sum((day1 - person_mean)^2 + (day2 - person_mean)^2)
  ms_between  <- ss_between / (n - 1)
  ms_within   <- ss_within  / n
  var_within  <- ms_within
  var_between <- max((ms_between - ms_within) / 2, 0)
  var_total   <- var_between + var_within
  if (var_total == 0) return(1)
  var_between / var_total
}

bread_vars    <- c("ba_total_g", "ba_wg_g", "ba_ref_g",
                   "bas_total_g", "bas_wg_g", "bas_ref_g")
bread_vars_d2 <- paste0(bread_vars, "_d2")

var_ratios <- tibble(
  var_name     = bread_vars,
  bp_var_ratio = map2_dbl(bread_vars, bread_vars_d2,
                          ~ calc_bp_var_ratio(two_day[[.x]], two_day[[.y]]))
)

cat("\nBetween-person variance ratios (pooled):\n")
print(var_ratios)

# =============================================================================
# SECTION 6: Survey design
# =============================================================================
# NPAFINWT is the selected-person Day-1 weight; WPM0101..WPM0160 are the
# 60 jackknife replicate weights.

svy_design <- persons %>%
  as_survey_rep(
    weights    = NPAFINWT,
    repweights = all_of(rep_wt_cols),
    type       = "JKn",
    scale      = 59 / 60,
    rscales    = rep(1, 60)
  )

cat("\nSurvey design created with", length(rep_wt_cols), "replicate weights\n")

# =============================================================================
# SECTION 7: Aggregate estimates per stratum
# =============================================================================

compute_estimates <- function(svy, person_df, intake_var, grp_vars, var_ratio) {

  if (length(grp_vars) == 0) {
    mean_est <- svy %>%
      summarise(
        n                 = unweighted(n()),
        mean_g            = survey_mean(.data[[intake_var]], na.rm = TRUE,
                                        vartype = "se"),
        mean_kcal         = survey_mean(energy_kcal_d1, na.rm = TRUE,
                                        vartype = "se"),
        pct_non_consumers = survey_mean(.data[[intake_var]] == 0,
                                        na.rm = TRUE, vartype = "se"),
        .groups = "drop"
      )
  } else {
    mean_est <- svy %>%
      group_by(across(all_of(grp_vars))) %>%
      summarise(
        n                 = unweighted(n()),
        mean_g            = survey_mean(.data[[intake_var]], na.rm = TRUE,
                                        vartype = "se"),
        mean_kcal         = survey_mean(energy_kcal_d1, na.rm = TRUE,
                                        vartype = "se"),
        pct_non_consumers = survey_mean(.data[[intake_var]] == 0,
                                        na.rm = TRUE, vartype = "se"),
        .groups = "drop"
      )
  }

  mean_est <- mean_est %>%
    rename(se_g = mean_g_se) %>%
    select(-mean_kcal_se, -pct_non_consumers_se)

  calc_weighted_sd <- function(df) {
    x <- df[[intake_var]]
    w <- df[["NPAFINWT"]]
    wm <- weighted.mean(x, w, na.rm = TRUE)
    sd_raw <- sqrt(sum(w * (x - wm)^2, na.rm = TRUE) /
                     (sum(w, na.rm = TRUE) - 1))
    sd_raw * sqrt(var_ratio)
  }

  if (length(grp_vars) == 0) {
    sd_est <- tibble(sd_g = calc_weighted_sd(person_df))
    result <- bind_cols(mean_est, sd_est)
  } else {
    sd_est <- person_df %>%
      group_by(across(all_of(grp_vars))) %>%
      group_modify(~ tibble(sd_g = calc_weighted_sd(.x))) %>%
      ungroup()
    result <- mean_est %>% left_join(sd_est, by = grp_vars)
  }

  result %>%
    mutate(
      pct_non_consumers = pct_non_consumers * 100,
      mean_kcal         = round(mean_kcal, 1)
    ) %>%
    select(any_of(grp_vars), n, mean_g, sd_g, se_g,
           pct_non_consumers, mean_kcal)
}

bread_subtype_map <- list(
  ba_total_g  = list(bread_def = 1L, bread_subtype = 0L, var_name = "ba_total_g"),
  ba_wg_g     = list(bread_def = 1L, bread_subtype = 1L, var_name = "ba_wg_g"),
  ba_ref_g    = list(bread_def = 1L, bread_subtype = 2L, var_name = "ba_ref_g"),
  bas_total_g = list(bread_def = 2L, bread_subtype = 0L, var_name = "bas_total_g"),
  bas_wg_g    = list(bread_def = 2L, bread_subtype = 1L, var_name = "bas_wg_g"),
  bas_ref_g   = list(bread_def = 2L, bread_subtype = 2L, var_name = "bas_ref_g")
)

# --- 7a. Fully stratified estimates -----------------------------------------
cat("\n--- Computing fully stratified estimates ---\n")

grp_full <- c("sex", "residence", "edu_level", "age_grp")

results_full <- map_dfr(bread_subtype_map, function(info) {
  cat("  ", info$var_name, "\n")
  vr <- var_ratios %>% filter(var_name == info$var_name) %>% pull(bp_var_ratio)
  compute_estimates(svy_design, persons, info$var_name, grp_full, vr) %>%
    mutate(bread_def = info$bread_def, bread_subtype = info$bread_subtype,
           energy_adj = 1L)
})

# --- 7b. Age x sex (with sex-combined and total-all-ages rows) --------------
cat("\n--- Computing age x sex estimates ---\n")

results_agesex <- map_dfr(bread_subtype_map, function(info) {
  cat("  ", info$var_name, "\n")
  vr <- var_ratios %>% filter(var_name == info$var_name) %>% pull(bp_var_ratio)
  compute_estimates(svy_design, persons, info$var_name,
                    c("sex", "age_grp"), vr) %>%
    mutate(bread_def = info$bread_def, bread_subtype = info$bread_subtype,
           energy_adj = 1L,
           residence = NA_integer_, edu_level = NA_integer_)
})

results_sexcomb <- map_dfr(bread_subtype_map, function(info) {
  vr <- var_ratios %>% filter(var_name == info$var_name) %>% pull(bp_var_ratio)
  compute_estimates(svy_design, persons, info$var_name, "age_grp", vr) %>%
    mutate(bread_def = info$bread_def, bread_subtype = info$bread_subtype,
           energy_adj = 1L,
           sex = 0L, residence = NA_integer_, edu_level = NA_integer_)
})

results_total_agesex <- map_dfr(bread_subtype_map, function(info) {
  vr <- var_ratios %>% filter(var_name == info$var_name) %>% pull(bp_var_ratio)
  by_sex  <- compute_estimates(svy_design, persons, info$var_name, "sex", vr) %>%
    mutate(age_grp = 0L)
  overall <- compute_estimates(svy_design, persons, info$var_name,
                               character(0), vr) %>%
    mutate(age_grp = 0L, sex = 0L)
  bind_rows(by_sex, overall) %>%
    mutate(bread_def = info$bread_def, bread_subtype = info$bread_subtype,
           energy_adj = 1L,
           residence = NA_integer_, edu_level = NA_integer_)
})

results_agesex_all <- bind_rows(results_agesex, results_sexcomb,
                                results_total_agesex)

# =============================================================================
# SECTION 8: Energy adjustment (residual method)
# =============================================================================
cat("\n--- Computing energy-adjusted estimates ---\n")

energy_adjust <- function(bread_g, energy_kcal) {
  use_log <- FALSE
  test_sample <- bread_g[bread_g > 0]
  if (length(test_sample) > 10) {
    sw <- shapiro.test(sample(test_sample, min(5000, length(test_sample))))
    if (sw$p.value < 0.05) use_log <- TRUE
  }
  y <- if (use_log) log(bread_g + 0.00001) else bread_g
  fit <- lm(y ~ energy_kcal)
  ea_values <- residuals(fit) +
    predict(fit, newdata = data.frame(energy_kcal = 2000))
  if (use_log) ea_values <- exp(ea_values)
  ea_values
}

for (var_name in bread_vars) {
  ea_col <- paste0(var_name, "_ea")
  persons[[ea_col]] <- energy_adjust(persons[[var_name]],
                                     persons$energy_kcal_d1)
  cat("  Energy-adjusted:", var_name, "->", ea_col, "\n")
}

# Rebuild design with EA columns
svy_design <- persons %>%
  as_survey_rep(
    weights    = NPAFINWT,
    repweights = all_of(rep_wt_cols),
    type       = "JKn",
    scale      = 59 / 60,
    rscales    = rep(1, 60)
  )

ea_subtype_map <- list(
  ba_total_g_ea  = list(bread_def = 1L, bread_subtype = 0L,
                        var_name = "ba_total_g_ea",  orig_var = "ba_total_g"),
  ba_wg_g_ea     = list(bread_def = 1L, bread_subtype = 1L,
                        var_name = "ba_wg_g_ea",     orig_var = "ba_wg_g"),
  ba_ref_g_ea    = list(bread_def = 1L, bread_subtype = 2L,
                        var_name = "ba_ref_g_ea",    orig_var = "ba_ref_g"),
  bas_total_g_ea = list(bread_def = 2L, bread_subtype = 0L,
                        var_name = "bas_total_g_ea", orig_var = "bas_total_g"),
  bas_wg_g_ea    = list(bread_def = 2L, bread_subtype = 1L,
                        var_name = "bas_wg_g_ea",    orig_var = "bas_wg_g"),
  bas_ref_g_ea   = list(bread_def = 2L, bread_subtype = 2L,
                        var_name = "bas_ref_g_ea",   orig_var = "bas_ref_g")
)

results_full_ea <- map_dfr(ea_subtype_map, function(info) {
  cat("  EA fully stratified:", info$var_name, "\n")
  vr <- var_ratios %>% filter(var_name == info$orig_var) %>% pull(bp_var_ratio)
  compute_estimates(svy_design, persons, info$var_name, grp_full, vr) %>%
    mutate(bread_def = info$bread_def, bread_subtype = info$bread_subtype,
           energy_adj = 2L)
})

results_agesex_ea <- map_dfr(ea_subtype_map, function(info) {
  vr <- var_ratios %>% filter(var_name == info$orig_var) %>% pull(bp_var_ratio)
  bind_rows(
    compute_estimates(svy_design, persons, info$var_name,
                      c("sex", "age_grp"), vr),
    compute_estimates(svy_design, persons, info$var_name, "age_grp", vr) %>%
      mutate(sex = 0L),
    compute_estimates(svy_design, persons, info$var_name, "sex", vr) %>%
      mutate(age_grp = 0L),
    compute_estimates(svy_design, persons, info$var_name,
                      character(0), vr) %>%
      mutate(age_grp = 0L, sex = 0L)
  ) %>%
    mutate(bread_def = info$bread_def, bread_subtype = info$bread_subtype,
           energy_adj = 2L,
           residence = NA_integer_, edu_level = NA_integer_)
})

# =============================================================================
# SECTION 9: Final output assembly
# =============================================================================
cat("\n--- Assembling final output ---\n")

ba_notes <- paste(
  "Bread alone includes AUSNUT 2023 food codes 12201001-12306003",
  "(standalone bread products including bagels, rolls, flatbreads,",
  "English muffins, sweet buns, garlic bread, bread+cheese mixes).",
  "Bread all sources sums the food-record-level WGBRGM and RFBRGM,",
  "which the ABS already disaggregates per food (no ADG step needed).",
  "Wholegrain/refined classification of standalone breads layered:",
  "(a) description keyword match -> wholegrain;",
  "(b) AUSNUT 2023 WGBRGM/RFBRGM majority;",
  "(c) FIBRE > 5 g/100 g -> wholegrain;",
  "default refined.",
  "Urban = ARIA21SL 1-2 (Major cities + Inner regional);",
  "Rural = ARIA21SL 3-5 (Outer regional + Remote + Very remote).",
  "Education from HIGHLVLD (level of highest educational attainment).",
  "Education for children <15 assigned from the highest-educated",
  "adult in the household."
)

final_output <- bind_rows(
  results_full,
  results_agesex_all,
  results_full_ea,
  results_agesex_ea
) %>%
  mutate(
    survey_name = "National Nutrition and Physical Activity Survey",
    year_start  = 2023L,
    year_end    = 2024L,
    notes       = if_else(bread_def == 1 & energy_adj == 1, ba_notes,
                          NA_character_)
  ) %>%
  mutate(
    mean_g            = round(mean_g, 1),
    sd_g              = round(sd_g, 1),
    se_g              = round(se_g, 2),
    pct_non_consumers = round(pct_non_consumers, 1)
  ) %>%
  select(
    survey_name, year_start, year_end,
    bread_def, bread_subtype, energy_adj,
    sex, residence, edu_level, age_grp,
    n, mean_g, sd_g, se_g, pct_non_consumers, mean_kcal,
    notes
  ) %>%
  arrange(
    energy_adj, bread_def, bread_subtype,
    desc(!is.na(residence)),
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

cat("\n--- QC: Total bread = WG + Refined (non-energy-adjusted only) ---\n")
qc <- final_output %>%
  filter(energy_adj == 1) %>%
  select(bread_def, energy_adj, sex, residence, edu_level, age_grp,
         bread_subtype, mean_g) %>%
  pivot_wider(names_from = bread_subtype, values_from = mean_g,
              names_prefix = "bt_") %>%
  mutate(diff = bt_0 - (bt_1 + bt_2))
cat("Max absolute difference:", max(abs(qc$diff), na.rm = TRUE), "g/day\n")
cat("(Should be <= 0.2 -- rounding artefact only)\n")

cat("\nMax n in any stratum:", max(final_output$n, na.rm = TRUE), "\n")
cat("Total persons in sample:", nrow(persons), "\n")
if (max(final_output$n, na.rm = TRUE) > nrow(persons)) {
  warning("n exceeds sample size -- possible duplication issue!")
}

# =============================================================================
# SECTION 11: Export CSV
# =============================================================================
write_csv(final_output, output_csv)
cat("\nCSV exported to:", output_csv, "\n")

# =============================================================================
# SECTION 12: Fill GBI Excel template
# =============================================================================
cat("\n--- Filling GBI Excel template ---\n")

wb <- loadWorkbook(template_path)

# --- 12a. Survey_Info_Template ----------------------------------------------
n_total       <- nrow(persons)
n_two_day     <- sum(persons$has_day2)
sample_text   <- sprintf("%s persons (%s with 2-day recall)",
                         format(n_total, big.mark = ","),
                         format(n_two_day, big.mark = ","))

survey_info <- list(
  c(3,  "National Nutrition and Physical Activity Survey (NNPAS)"),
  c(4,  "Australia"),
  c(5,  "Started January 2023, finished March 2024"),
  c(6,  "National (all states and territories, urban and rural)"),
  c(7,  "Persons aged 2 years and over, usual residents of private dwellings"),
  c(8,  sample_text),
  c(9,  "24-hour dietary recall (Intake24-based, web/CATI administered)"),
  c(10, paste("2 days for most respondents (Day 1 face-to-face/web,",
              "Day 2 follow-up); Day 1 used for point estimates")),
  c(11, paste("Multi-stage area probability sample of private dwellings;",
              "stratified by state/territory and capital city/rest of state")),
  c(12, "Yes -- NPAFINWT (selected-person Day-1 weight)"),
  c(13, "Yes -- 60 jackknife replicate weights (WPM0101-WPM0160)"),
  c(14, paste("ABS, Microdata: National Nutrition and Physical Activity Survey,",
              "Australia, 2023 (DataLab/SAS unit-record files)")),
  c(15, "AUSNUT 2023 (ABS/FSANZ)"),
  c(16, paste("Bread alone: AUSNUT 2023 food codes 12201001-12306003",
              "(standalone bread products including regular breads, rolls,",
              "flatbreads, English muffins, sweet buns, garlic bread,",
              "bread+cheese mixes).",
              "Wholegrain/refined classification layered: (a) description",
              "keyword match (wholemeal/wholegrain/multigrain/mixed grain/",
              "rye/spelt/pumpernickel/added fibre/high fibre/chapatti/",
              "injera/roti) -> wholegrain; (b) AUSNUT WGBRGM vs RFBRGM",
              "majority; (c) FIBRE > 5 g/100 g -> wholegrain;",
              "default refined.")),
  c(17, paste("Bread all sources: food-record-level WGBRGM (wholegrain or",
              "high fibre breads, g) and RFBRGM (refined or lower fibre",
              "grain breads, g) summed directly across all food records.",
              "These ABS-derived fields already capture the bread component",
              "of mixed dishes (sandwiches, burgers, pizza, etc.), so no",
              "separate disaggregation step is required.")),
  c(18, paste("Residual method: regress bread intake (log-transformed if",
              "non-normal by Shapiro-Wilk) on total energy intake (kcal/day);",
              "add predicted value at 2000 kcal to residuals; exponentiate",
              "if log-transformed. Energy adjustment performed on full sample,",
              "not per-stratum.")),
  c(19, paste("Urban = ARIA21SL 1-2 (Major cities + Inner regional);",
              "Rural = ARIA21SL 3-5 (Outer regional + Remote + Very remote).",
              "Education from HIGHLVLD (level of highest educational",
              "attainment); children <15 assigned the highest education",
              "level of an adult in the same household.",
              "Sex = SEXBIRTH (sex at birth).",
              "Day-1 energy = ENERGYF1 (food only, kJ -> kcal /4.184).",
              "SD corrected for within-person variation using ANOVA on",
              "2-day subsample (between-person variance ratio applied to",
              "Day-1 weighted SD).",
              "SE estimated via jackknife replication (60 replicate weights)."))
)

ws_info <- "Survey_Info_Template"
for (item in survey_info) {
  writeData(wb, sheet = ws_info, x = item[[2]],
            startCol = 3, startRow = as.integer(item[[1]]),
            colNames = FALSE)
}
cat("  Survey_Info_Template filled\n")

# --- 12b. Codebook -----------------------------------------------------------
codebook_availability <- rep("Yes", 17)

codebook_comments <- c(
  NA, NA, NA,
  "Both bread alone (1) and bread all sources (2) provided.",
  paste("Wholegrain/refined classification layered: description keywords ->",
        "AUSNUT 2023 WGBRGM/RFBRGM majority -> FIBRE > 5 g/100 g;",
        "default refined."),
  paste("Both non-energy-adjusted (1) and energy-adjusted (2) estimates",
        "provided. Energy adjustment uses the residual method standardised",
        "to 2000 kcal."),
  "Sex = SEXBIRTH (sex at birth) on the NNPAS 2023 selected-person file.",
  paste("Urban = ARIA21SL 1 (Major cities) + 2 (Inner regional);",
        "Rural = ARIA21SL 3 (Outer regional) + 4 (Remote) + 5 (Very remote)."),
  paste("Mapped from HIGHLVLD: Tertiary = postgraduate, bachelor, advanced",
        "diploma/diploma, certificate III/IV; Secondary = year 10-12,",
        "certificate I/II; Primary = year 9 or below, no educational",
        "attainment. Children <15: highest-educated adult in household."),
  "NNPAS age range is 2 years and over. Mapped to GBI bands; band 12 = 85+.",
  NA,
  "Weighted mean using NPAFINWT. Day 1 recall only.",
  paste("Weighted SD, corrected for within-person variation using ANOVA on",
        "2-day subsample. Between-person variance ratio applied to deflate",
        "the Day-1 weighted SD."),
  paste("Design-based SE from jackknife replication using 60 replicate",
        "weights (WPM0101-WPM0160)."),
  "Weighted proportion of persons with zero intake on Day 1.",
  paste("Weighted mean total energy intake (food, Day 1), converted from",
        "kJ to kcal (/4.184). ENERGYF1."),
  NA
)

for (i in seq_along(codebook_availability)) {
  writeData(wb, sheet = "Codebook",
            x = codebook_availability[i],
            startCol = 1, startRow = i + 2, colNames = FALSE)
  if (!is.na(codebook_comments[i])) {
    writeData(wb, sheet = "Codebook",
              x = codebook_comments[i],
              startCol = 10, startRow = i + 2, colNames = FALSE)
  }
}
cat("  Codebook filled\n")

# --- 12c. Data_Template ------------------------------------------------------
data_for_template <- final_output %>%
  select(survey_name, year_start, year_end,
         bread_def, bread_subtype, energy_adj,
         sex, residence, edu_level, age_grp,
         n, mean_g, sd_g, se_g, pct_non_consumers, mean_kcal,
         notes)

writeData(wb, sheet = "Data_Template", x = data_for_template,
          startCol = 1, startRow = 4, colNames = FALSE)
cat("  Data_Template filled with", nrow(data_for_template), "rows\n")

saveWorkbook(wb, output_xlsx, overwrite = TRUE)
cat("\nFilled Excel template saved to:", output_xlsx, "\n")

cat("\n=== ALL DONE ===\n")
