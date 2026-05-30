# =============================================================================
# GBI Aggregate Bread Intake Estimates -- NNPAS 2023  (LOCAL / DUMMY COPY)
# =============================================================================
# Local-testing version of GBI_bread_estimates_NNPAS_2023.R.
# Identical analytical logic to the production script. Two differences only:
#   - Loads pre-saved dummy microdata frames via read_rds() (the real
#     sas7bdat unit-record files are not available outside the DataLab).
#   - Paths point to the local dummy-data directory and the GBI folder
#     for outputs.
# Keep changes here mirrored against the production script.
#
# Updated 2026-05 to follow the GBI methodological clarification note
# ("GBI_Methodological_Note_Energy_Adjustment.pdf", 29 April 2026). See
# the production script header for the full change list.
# =============================================================================

library(tidyverse)
library(readxl)
library(haven)
library(survey)
library(srvyr)
library(openxlsx)

# --- Paths ----------------------------------------------------------------
nnpas_dir   <- "C:\\Users\\atyeo\\OneDrive\\R data\\NNPAS2023\\Dummy microdata"
ausnut_path <- file.path(nnpas_dir, "AUSNUT_2023.xlsx")
hhd_path    <- file.path(nnpas_dir, "nnpas23hhd.rds")
sps_path    <- file.path(nnpas_dir, "nnpas23sps.rds")
food_path   <- file.path(nnpas_dir, "nnpas23food.rds")

GBI_path      <- "C:\\Users\\atyeo\\OneDrive\\R data\\GBI"
template_path <- file.path(GBI_path, "GBI_Aggregate_Data_Form_unprotected.xlsx")
output_csv    <- file.path(GBI_path, "GBI_DataTemplate_NNPAS_2023.csv")
output_xlsx   <- file.path(GBI_path, "GBI_AggregateDataForm_NNPAS_2023.xlsx")

# --- Disclosure-control "rule of n" --------------------------------------
# Minimum cell size below which a stratum is primary-suppressed.
# 3 is a common general rule; ABS DataLab egress requires 10. Change here
# (e.g., MIN_CELL_N <- 10) when preparing data for DataLab clearance.
MIN_CELL_N <- 3

# =============================================================================
# SECTION 1: AUSNUT 2023 food-code lists and bread-alone classification
# =============================================================================

bread_alone_codes_2023 <- c(
  12201001:12305002, 12305004:12305007, 12306001:12306004,
  13201001:13201002, 13201018:13201019,
  13202001, 13204003
)

all_bread_codes_2023 <- c(
  bread_alone_codes_2023,
  13503001:13504001, 13504004:13507004,
  13507008:13507012, 13507016:13507019
)

ausnut <- read_excel(ausnut_path, sheet = 1)

bread_alone_codes <- ausnut %>%
  filter(FOODCODL %in% bread_alone_codes_2023) %>%
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
      desc_wg                                    ~ "wholegrain",
      WGBRGM > RFBRGM                            ~ "wholegrain",
      RFBRGM > WGBRGM                            ~ "refined",
      FIBRE > 5                                  ~ "wholegrain",
      TRUE                                       ~ "refined"
    )
  ) %>%
  select(FOODCODL, Food_name, FIBRE, WGBRGM, RFBRGM, bread_class)

cat("Standalone bread codes in AUSNUT 2023 list:", nrow(bread_alone_codes), "\n")
cat("Classification breakdown:\n")
count(bread_alone_codes, bread_class) %>% print()

write_csv(bread_alone_codes,
          file.path(GBI_path, "bread_alone_classification_NNPAS_2023.csv"))

# =============================================================================
# SECTION 2: Load NNPAS 2023 dummy unit-record files
# =============================================================================

hhd <- read_rds(hhd_path) %>%
  select(ABSHIDD, ARIA21SL)

cat("\nHouseholds loaded:", nrow(hhd), "\n")

sps <- read_rds(sps_path) %>%
  rename(ABSPID = ABSPIDD)

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
    # ABS NNPAS 2023 already provides BMR (Schofield-style, computed on
    # the imputed measured weight/height) and EIBMR1/EIBMR2 (energy-
    # intake-to-BMR ratio per recall day). We use these directly --
    # no need to recompute BMR ourselves.
    any_of(c("BMR", "EIBMR1", "EIBMR2")),
    NPAFINWT,
    all_of(rep_wt_cols),
    ENERGYF1, ENERGYF2,
    COUNTFD2
  ) %>%
  left_join(hhd, by = "ABSHIDD") %>%
  mutate(person_uid = paste(ABSHIDD, ABSPID, sep = "_"))

if (!"BMR"    %in% names(persons)) persons$BMR    <- NA_real_
if (!"EIBMR1" %in% names(persons)) persons$EIBMR1 <- NA_real_
if (!"EIBMR2" %in% names(persons)) persons$EIBMR2 <- NA_real_

cat("Persons loaded:", nrow(persons), "\n")

foods <- read_rds(food_path) %>%
  rename(ABSPID = ABSPIDD) %>%
  select(ABSHIDD, ABSPID, DAYNUM, FOODCODL, GRAMWGT,
         WGBRGM, WGSVGM, WGMFGM,
         RFBRGM, RFSVGM, RFMFGM) %>%
  mutate(person_uid = paste(ABSHIDD, ABSPID, sep = "_"))

cat("Food records loaded:", nrow(foods), "\n")
cat("  Day 1 records:", sum(foods$DAYNUM == 1), "\n")
cat("  Day 2 records:", sum(foods$DAYNUM == 2), "\n")

# =============================================================================
# SECTION 3: Stratification and person-level prep
# =============================================================================

n_under2 <- sum(persons$AGE99 < 2, na.rm = TRUE)
if (n_under2 > 0) {
  cat("\nExcluding", n_under2, "participants aged <2 years (GBI eligibility).\n")
  persons <- persons %>% filter(AGE99 >= 2)
}

age_breaks <- c(2, 6, 11, 15, 20, 25, 35, 45, 55, 65, 75, 85, Inf)
age_labels <- 1:12

persons <- persons %>%
  mutate(
    age_grp = cut(AGE99, breaks = age_breaks, labels = age_labels,
                  right = FALSE, include.lowest = TRUE) %>% as.integer()
  )

cat("\nAge group distribution:\n")
persons %>% count(age_grp) %>% print()

persons <- persons %>%
  mutate(sex = as.integer(SEXBIRTH))

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
      HIGHLVLD_chr %in% c("500","011")            ~ 2L,
      HIGHLVLD_chr %in% c("999")                  ~ NA_integer_,
      TRUE                                        ~ 2L
    )
  )

adult_edu <- persons %>%
  filter(AGE99 >= 15, !is.na(edu_level_own)) %>%
  group_by(ABSHIDD) %>%
  summarise(hh_adult_edu = max(edu_level_own), .groups = "drop")

persons <- persons %>%
  left_join(adult_edu, by = "ABSHIDD") %>%
  mutate(edu_level = if_else(AGE99 < 15, hh_adult_edu, edu_level_own))

n_edu_na <- sum(is.na(persons$edu_level))
if (n_edu_na > 0) {
  cat("\nWARNING:", n_edu_na, "persons with missing education -- defaulting to Secondary\n")
  persons <- persons %>% mutate(edu_level = replace_na(edu_level, 2L))
}

cat("\nEducation level distribution:\n")
persons %>% count(edu_level) %>% print()

persons <- persons %>%
  mutate(
    ARIA21SL_n = as.integer(ARIA21SL),
    residence = case_when(
      ARIA21SL_n %in% c(0, 1)        ~ 1L,
      ARIA21SL_n %in% c(2, 3, 4)     ~ 2L,
      TRUE                           ~ NA_integer_
    )
  )

cat("\nResidence distribution:\n")
persons %>% count(residence) %>% print()

# =============================================================================
# SECTION 4: Person-level bread intake (mean across all valid recall days)
# =============================================================================

person_recall_days <- foods %>%
  distinct(person_uid, DAYNUM) %>%
  group_by(person_uid) %>%
  summarise(
    has_d1 = any(DAYNUM == 1),
    has_d2 = any(DAYNUM == 2),
    n_days = n_distinct(DAYNUM),
    .groups = "drop"
  )

persons <- persons %>%
  left_join(person_recall_days, by = "person_uid") %>%
  mutate(
    has_d1 = coalesce(has_d1, FALSE),
    has_d2 = coalesce(has_d2, FALSE),
    n_days = coalesce(n_days, 0L)
  )

cat("\nRecall-day coverage:\n")
persons %>% count(has_d1, has_d2) %>% print()

recall_days_long <- foods %>% distinct(person_uid, DAYNUM)

bread_alone_pd <- recall_days_long %>%
  left_join(
    foods %>%
      filter(FOODCODL %in% bread_alone_codes_2023) %>%
      inner_join(bread_alone_codes %>% select(FOODCODL, bread_class),
                 by = "FOODCODL") %>%
      group_by(person_uid, DAYNUM) %>%
      summarise(
        ba_total_g_d = sum(GRAMWGT, na.rm = TRUE),
        ba_wg_g_d    = sum(GRAMWGT[bread_class == "wholegrain"], na.rm = TRUE),
        ba_ref_g_d   = sum(GRAMWGT[bread_class == "refined"],    na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("person_uid", "DAYNUM")
  ) %>%
  mutate(across(c(ba_total_g_d, ba_wg_g_d, ba_ref_g_d),
                ~ replace_na(.x, 0)))

bread_allsrc_pd <- recall_days_long %>%
  left_join(
    foods %>%
      filter(FOODCODL %in% all_bread_codes_2023) %>%
      mutate(
        wg_g  = coalesce(WGBRGM, 0) + coalesce(WGSVGM, 0) + coalesce(WGMFGM, 0),
        ref_g = coalesce(RFBRGM, 0) + coalesce(RFSVGM, 0) + coalesce(RFMFGM, 0)
      ) %>%
      group_by(person_uid, DAYNUM) %>%
      summarise(
        bas_wg_g_d    = sum(wg_g,  na.rm = TRUE),
        bas_ref_g_d   = sum(ref_g, na.rm = TRUE),
        bas_total_g_d = bas_wg_g_d + bas_ref_g_d,
        .groups = "drop"
      ),
    by = c("person_uid", "DAYNUM")
  ) %>%
  mutate(across(c(bas_total_g_d, bas_wg_g_d, bas_ref_g_d),
                ~ replace_na(.x, 0)))

bread_alone_person <- bread_alone_pd %>%
  group_by(person_uid) %>%
  summarise(
    ba_total_g = mean(ba_total_g_d),
    ba_wg_g    = mean(ba_wg_g_d),
    ba_ref_g   = mean(ba_ref_g_d),
    .groups    = "drop"
  )

bread_allsrc_person <- bread_allsrc_pd %>%
  group_by(person_uid) %>%
  summarise(
    bas_total_g = mean(bas_total_g_d),
    bas_wg_g    = mean(bas_wg_g_d),
    bas_ref_g   = mean(bas_ref_g_d),
    .groups     = "drop"
  )

persons <- persons %>%
  mutate(
    energy_kcal_d1 = if_else(has_d1 & !is.na(ENERGYF1),
                             ENERGYF1 / 4.184, NA_real_),
    energy_kcal_d2 = if_else(has_d2 & !is.na(ENERGYF2),
                             ENERGYF2 / 4.184, NA_real_),
    mean_energy_kcal = case_when(
      has_d1 & has_d2 & !is.na(energy_kcal_d1) & !is.na(energy_kcal_d2) ~
        (energy_kcal_d1 + energy_kcal_d2) / 2,
      has_d1 & !is.na(energy_kcal_d1) ~ energy_kcal_d1,
      has_d2 & !is.na(energy_kcal_d2) ~ energy_kcal_d2,
      TRUE                            ~ NA_real_
    )
  )

persons <- persons %>%
  left_join(bread_alone_person,  by = "person_uid") %>%
  left_join(bread_allsrc_person, by = "person_uid")

n_no_recall <- sum(persons$n_days == 0L)
if (n_no_recall > 0) {
  cat("\nExcluding", n_no_recall, "persons with no recall days at all.\n")
  persons <- persons %>% filter(n_days > 0L)
}

# =============================================================================
# SECTION 5: Energy-plausibility flag using ABS-derived EIBMR1/EIBMR2
# =============================================================================
# NNPAS 2023 provides BMR (Schofield-style, on imputed measured weight/height)
# and EIBMR1/EIBMR2 (energy-intake-to-BMR ratio per recall day) directly on
# the SPS file. We use these instead of recomputing BMR ourselves:
#   - mean_eibmr = mean of EIBMR across the same valid recall days used for
#     bread and energy (1 day or 2).
#   - Implausible if mean_eibmr < 0.9 or > 2.4 (Goldberg cutoffs).
#   - Children <10y exempt (always treated as plausible).
#   - Pregnant women: ABS does not impute their measured weight/height, so
#     BMR (and therefore EIBMR1/EIBMR2) is NA -- they fall through the
#     "can't screen -> retain" branch automatically.
#   - Anyone else with NA EI:BMR (e.g. missing energy) is retained as
#     plausible. They may still be excluded from EA estimates for missing
#     energy via the energy_adjust_residual eligibility check, but we never
#     mark them as implausible.

persons <- persons %>%
  mutate(
    # ABS reserved codes -- treat as missing. BMR uses 99998 = N/A (e.g.,
    # pregnant women whose h/w wasn't imputed). EIBMR1/EIBMR2 use 97/98
    # as "Not applicable / Not stated"; real EI:BMR ratios are always
    # well under 10.
    EIBMR1_clean = if_else(as.numeric(EIBMR1) >= 90, NA_real_, as.numeric(EIBMR1)),
    EIBMR2_clean = if_else(as.numeric(EIBMR2) >= 90, NA_real_, as.numeric(EIBMR2)),
    eibmr_d1 = if_else(has_d1, EIBMR1_clean, NA_real_),
    eibmr_d2 = if_else(has_d2, EIBMR2_clean, NA_real_),
    mean_eibmr = case_when(
      has_d1 & has_d2 & !is.na(eibmr_d1) & !is.na(eibmr_d2) ~
        (eibmr_d1 + eibmr_d2) / 2,
      has_d1 & !is.na(eibmr_d1) ~ eibmr_d1,
      has_d2 & !is.na(eibmr_d2) ~ eibmr_d2,
      TRUE                      ~ NA_real_
    ),
    energy_implausible = case_when(
      AGE99 < 10                          ~ FALSE,   # exempt young children
      is.na(mean_eibmr)                   ~ FALSE,   # can't screen -> retain
      mean_eibmr < 0.9 | mean_eibmr > 2.4 ~ TRUE,
      TRUE                                ~ FALSE
    )
  )

n_impl       <- sum(persons$energy_implausible, na.rm = TRUE)
n_eibmr_miss <- sum(is.na(persons$mean_eibmr))
n_screened   <- sum(!is.na(persons$mean_eibmr) & persons$AGE99 >= 10)
cat("\nEnergy plausibility (ABS EIBMR + Goldberg <0.9 or >2.4):\n")
cat("  Mean EI:BMR not available: ", n_eibmr_miss,
    "(retained as plausible -- includes pregnant women whose BMR is NA)\n")
cat("  Flagged implausible:       ", n_impl,
    sprintf("(%.1f%% of %d screened)\n", 100 * n_impl / max(1, n_screened),
            n_screened))
cat("  Exempt (AGE99 < 10):       ", sum(persons$AGE99 < 10), "\n")

# =============================================================================
# SECTION 6: Energy adjustment (residual method, log-log, pooled per outcome)
# =============================================================================
bread_vars <- c("ba_total_g", "ba_wg_g", "ba_ref_g",
                "bas_total_g", "bas_wg_g", "bas_ref_g")
ea_ref_kcal <- 2000

energy_adjust_residual <- function(bread_g, energy_kcal, implausible) {
  ea <- rep(NA_real_, length(bread_g))
  # Assign 0 to non-consumers FIRST, so they stay in the EA stratum
  # estimates even when too few consumers exist to fit a residual model.
  non_consumer <- !is.na(bread_g) & bread_g == 0
  ea[non_consumer] <- 0
  consumer_eligible <- !is.na(bread_g) & bread_g > 0 &
                       !is.na(energy_kcal) & energy_kcal > 0 &
                       !is.na(implausible) & !implausible
  if (sum(consumer_eligible) < 10) {
    warning("Too few consumers (", sum(consumer_eligible),
            ") for energy adjustment; non-consumers still recorded as 0.")
    return(ea)
  }
  log_b <- log(bread_g[consumer_eligible])
  log_e <- log(energy_kcal[consumer_eligible])
  fit   <- lm(log_b ~ log_e)
  beta  <- unname(coef(fit)[2])
  log_adj <- log_b - beta * (log_e - log(ea_ref_kcal))
  ea[consumer_eligible] <- exp(log_adj)
  ea
}

for (vn in bread_vars) {
  ea_col <- paste0(vn, "_ea")
  persons[[ea_col]] <- energy_adjust_residual(
    persons[[vn]], persons$mean_energy_kcal, persons$energy_implausible
  )
  cat(sprintf("  EA %-12s n_modelled=%d  n_zero=%d  n_na=%d\n",
              vn,
              sum(!is.na(persons[[ea_col]]) & persons[[ea_col]] > 0),
              sum(!is.na(persons[[ea_col]]) & persons[[ea_col]] == 0),
              sum(is.na(persons[[ea_col]]))))
}

# =============================================================================
# SECTION 7: Survey design (jackknife replicates)
# =============================================================================
svy_design <- persons %>%
  as_survey_rep(
    weights    = NPAFINWT,
    repweights = all_of(rep_wt_cols),
    type       = "JKn",
    scale      = 59/60,
    rscales    = rep(1, 60)
  )

cat("\nSurvey design created with", length(rep_wt_cols), "replicate weights\n")

# =============================================================================
# SECTION 8: Stratum-level estimates
# =============================================================================
calc_weighted_sd <- function(x, w) {
  x <- as.numeric(x); w <- as.numeric(w)
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (sum(keep) < 2) return(NA_real_)
  x <- x[keep]; w <- w[keep]
  wm <- sum(w * x) / sum(w)
  sqrt(sum(w * (x - wm)^2) / (sum(w) - 1))
}

compute_estimates <- function(svy, person_df, intake_var, grp_vars) {
  # Filter persons to non-NA intake (EA variables are NA when energy is
  # missing/implausible). Rebuild a subset survey design from the filtered
  # data rather than `svy %>% filter()`: the latter can throw
  # "subscript out of bounds" in srvyr's `unweighted(n())` when subsequent
  # group_by() operations encounter groups whose cached split indices are
  # stale after the filter.
  pers_f <- person_df %>% filter(!is.na(.data[[intake_var]]))
  pers_e <- person_df %>% filter(!is.na(mean_energy_kcal))
  # If the intake variable is entirely NA (e.g. an EA outcome where no
  # persons were modellable AND no persons are non-consumers), there is
  # nothing to estimate -- return an empty tibble with the right schema.
  if (nrow(pers_f) == 0) {
    cols <- list()
    for (g in grp_vars) cols[[g]] <- integer(0)
    cols$n <- integer(0); cols$mean_g <- numeric(0)
    cols$sd_g <- numeric(0); cols$se_g <- numeric(0)
    cols$pct_non_consumers <- numeric(0)
    cols$mean_kcal <- numeric(0); cols$sd_kcal <- numeric(0)
    return(tibble::as_tibble(cols))
  }
  rep_cols <- grep("^WPM01[0-9]{2}$", names(pers_f), value = TRUE)
  build_subset_design <- function(df) {
    # Defensive: drop any NA/zero weights and coerce labelled vectors to
    # plain numeric. `combined.weights = TRUE` is passed explicitly because
    # NNPAS WPM01xx replicates already incorporate the base weight, AND
    # because survey::svrepdesign()'s auto-detection (mean(repweights[,1])
    # > 1) can return NA on filtered subsets and crash with
    # "missing value where TRUE/FALSE needed".
    df <- df %>% filter(!is.na(NPAFINWT), NPAFINWT > 0) %>%
      mutate(NPAFINWT = as.numeric(NPAFINWT),
             across(all_of(rep_cols), as.numeric))
    df %>%
      as_survey_rep(weights = NPAFINWT, repweights = all_of(rep_cols),
                    type = "JKn", scale = 59/60, rscales = rep(1, 60),
                    combined.weights = TRUE)
  }
  svy_f <- build_subset_design(pers_f)
  svy_e <- build_subset_design(pers_e)

  if (length(grp_vars) == 0) {
    mean_est <- svy_f %>%
      summarise(
        n                 = unweighted(n()),
        mean_g            = survey_mean(.data[[intake_var]], na.rm = TRUE,
                                        vartype = "se"),
        pct_non_consumers = survey_mean(.data[[intake_var]] == 0,
                                        na.rm = TRUE, vartype = NULL)
      )
    eng_est <- svy_e %>%
      summarise(mean_kcal = survey_mean(mean_energy_kcal, na.rm = TRUE,
                                        vartype = NULL))
    sd_est  <- tibble(
      sd_g    = calc_weighted_sd(pers_f[[intake_var]], pers_f$NPAFINWT),
      sd_kcal = calc_weighted_sd(pers_e$mean_energy_kcal, pers_e$NPAFINWT)
    )
    result <- bind_cols(mean_est, eng_est, sd_est)
  } else {
    mean_est <- svy_f %>%
      group_by(across(all_of(grp_vars))) %>%
      summarise(
        n                 = unweighted(n()),
        mean_g            = survey_mean(.data[[intake_var]], na.rm = TRUE,
                                        vartype = "se"),
        pct_non_consumers = survey_mean(.data[[intake_var]] == 0,
                                        na.rm = TRUE, vartype = NULL),
        .groups = "drop"
      )
    eng_est <- svy_e %>%
      group_by(across(all_of(grp_vars))) %>%
      summarise(mean_kcal = survey_mean(mean_energy_kcal, na.rm = TRUE,
                                        vartype = NULL),
                .groups = "drop")
    sd_est <- pers_f %>%
      group_by(across(all_of(grp_vars))) %>%
      group_modify(~ tibble(
        sd_g = calc_weighted_sd(.x[[intake_var]], .x$NPAFINWT)
      )) %>% ungroup()
    sd_e_est <- pers_e %>%
      group_by(across(all_of(grp_vars))) %>%
      group_modify(~ tibble(
        sd_kcal = calc_weighted_sd(.x$mean_energy_kcal, .x$NPAFINWT)
      )) %>% ungroup()
    result <- mean_est %>%
      left_join(eng_est,  by = grp_vars) %>%
      left_join(sd_est,   by = grp_vars) %>%
      left_join(sd_e_est, by = grp_vars)
  }

  result %>%
    rename(se_g = mean_g_se) %>%
    mutate(pct_non_consumers = pct_non_consumers * 100) %>%
    select(any_of(grp_vars), n, mean_g, sd_g, se_g,
           pct_non_consumers, mean_kcal, sd_kcal)
}

bread_subtype_map <- list(
  ba_total_g  = list(bread_def = 1L, bread_subtype = 0L, var_name = "ba_total_g"),
  ba_wg_g     = list(bread_def = 1L, bread_subtype = 1L, var_name = "ba_wg_g"),
  ba_ref_g    = list(bread_def = 1L, bread_subtype = 2L, var_name = "ba_ref_g"),
  bas_total_g = list(bread_def = 2L, bread_subtype = 0L, var_name = "bas_total_g"),
  bas_wg_g    = list(bread_def = 2L, bread_subtype = 1L, var_name = "bas_wg_g"),
  bas_ref_g   = list(bread_def = 2L, bread_subtype = 2L, var_name = "bas_ref_g")
)
ea_subtype_map <- map(bread_subtype_map, function(info) {
  modifyList(info, list(var_name = paste0(info$var_name, "_ea")))
})

grp_full <- c("sex", "residence", "edu_level", "age_grp")

build_all_strata <- function(var_name, energy_adj_code, info) {
  full <- compute_estimates(svy_design, persons, var_name, grp_full) %>%
    mutate(bread_def = info$bread_def, bread_subtype = info$bread_subtype,
           energy_adj = energy_adj_code)
  agesex_m_f <- compute_estimates(svy_design, persons, var_name,
                                  c("sex", "age_grp")) %>%
    mutate(residence = NA_integer_, edu_level = NA_integer_,
           bread_def = info$bread_def, bread_subtype = info$bread_subtype,
           energy_adj = energy_adj_code)
  agesex_comb <- compute_estimates(svy_design, persons, var_name, "age_grp") %>%
    mutate(sex = 0L, residence = NA_integer_, edu_level = NA_integer_,
           bread_def = info$bread_def, bread_subtype = info$bread_subtype,
           energy_adj = energy_adj_code)
  total_by_sex <- compute_estimates(svy_design, persons, var_name, "sex") %>%
    mutate(age_grp = 0L, residence = NA_integer_, edu_level = NA_integer_,
           bread_def = info$bread_def, bread_subtype = info$bread_subtype,
           energy_adj = energy_adj_code)
  total_overall <- compute_estimates(svy_design, persons, var_name,
                                     character(0)) %>%
    mutate(sex = 0L, age_grp = 0L, residence = NA_integer_,
           edu_level = NA_integer_,
           bread_def = info$bread_def, bread_subtype = info$bread_subtype,
           energy_adj = energy_adj_code)
  bind_rows(full, agesex_m_f, agesex_comb, total_by_sex, total_overall)
}

cat("\n--- Computing unadjusted stratum estimates ---\n")
results_unadj <- map_dfr(bread_subtype_map, function(info) {
  cat("  ", info$var_name, "\n")
  build_all_strata(info$var_name, 1L, info)
})

cat("\n--- Computing energy-adjusted stratum estimates ---\n")
results_ea <- map_dfr(ea_subtype_map, function(info) {
  cat("  ", info$var_name, "\n")
  build_all_strata(info$var_name, 2L, info)
})

# =============================================================================
# SECTION 9: Assemble final output
# =============================================================================
ba_notes <- paste(
  "Bread alone = explicit AUSNUT 2023 code list:",
  "12201001-12305002, 12305004-12305007, 12306001-12306004,",
  "13201001, 13201002, 13201018, 13201019, 13202001, 13204003.",
  "Bread all sources = bread-alone list plus mixed-dish codes",
  "(13503001-13504001, 13504004-13507004, 13507008-13507012,",
  "13507016-13507019). Bread g per food record summed across",
  "WGBRGM+WGSVGM+WGMFGM (wholegrain) and RFBRGM+RFSVGM+RFMFGM (refined),",
  "all already in grams per the AUSNUT 2023 disaggregation.",
  "Person-level mean intake averaged across all valid recall days,",
  "with 0-intake days retained.",
  "Energy adjustment: residual method, log(bread) ~ log(energy),",
  "fitted unweighted on consumers only, standardized to 2000 kcal/day;",
  "non-consumers assigned 0 g/day adjusted intake.",
  "Energy plausibility: ABS-derived BMR + EIBMR1/EIBMR2 used directly",
  "(Goldberg cutoffs <0.9 or >2.4 applied to mean EI:BMR across recall days);",
  "children <10y exempt; pregnant women retained (ABS sets their BMR to NA).",
  "Urban = ARIA21SL 0-1; Rural = ARIA21SL 2-4."
)

final_output <- bind_rows(results_unadj, results_ea) %>%
  mutate(
    survey_name = "National Nutrition and Physical Activity Survey",
    year_start  = 2023L,
    year_end    = 2024L,
    notes       = if_else(bread_def == 1 & energy_adj == 1, ba_notes,
                          NA_character_),
    mean_g            = round(mean_g, 1),
    sd_g              = round(sd_g, 1),
    se_g              = round(se_g, 2),
    pct_non_consumers = round(pct_non_consumers, 1),
    mean_kcal         = round(mean_kcal, 1),
    sd_kcal           = round(sd_kcal, 1)
  ) %>%
  select(
    survey_name, year_start, year_end,
    bread_def, bread_subtype, energy_adj,
    sex, residence, edu_level, age_grp,
    n, mean_g, sd_g, se_g, pct_non_consumers,
    mean_kcal, sd_kcal,
    notes
  ) %>%
  arrange(energy_adj, bread_def, bread_subtype,
          desc(!is.na(residence)),
          sex, residence, edu_level, age_grp)

# =============================================================================
# SECTION 10: QC checks
# =============================================================================
cat("\n=== FINAL OUTPUT SUMMARY ===\n")
cat("Total rows:", nrow(final_output), "\n\n")

final_output %>%
  count(bread_def, energy_adj, is_fully_strat = !is.na(residence)) %>%
  print()

cat("\n--- QC: Unadjusted Total = WG + Refined (within stratum) ---\n")
qc <- final_output %>%
  filter(energy_adj == 1) %>%
  select(bread_def, energy_adj, sex, residence, edu_level, age_grp,
         bread_subtype, mean_g) %>%
  pivot_wider(names_from = bread_subtype, values_from = mean_g,
              names_prefix = "bt_") %>%
  mutate(diff = bt_0 - (bt_1 + bt_2))
cat("Max absolute difference (unadjusted):",
    max(abs(qc$diff), na.rm = TRUE), "g/day\n")
cat("(Should be <=0.2 -- rounding artefact only.\n",
    " EA estimates are not expected to be additive per GBI spec.)\n")

cat("\nMax n in any stratum:", max(final_output$n, na.rm = TRUE), "\n")
cat("Total persons in analytic sample:", nrow(persons), "\n")
if (max(final_output$n, na.rm = TRUE) > nrow(persons)) {
  warning("n exceeds sample size -- possible duplication issue!")
}

# =============================================================================
# SECTION 11: Statistical Disclosure Control (cell suppression flags)
# =============================================================================
final_output <- final_output %>%
  mutate(suppress_primary = !is.na(n) & n < MIN_CELL_N)

final_output <- final_output %>%
  group_by(bread_def, energy_adj, sex, residence, edu_level, age_grp) %>%
  mutate(
    .n_prim_st = if_else(energy_adj == 1L,
                         sum(suppress_primary & bread_subtype %in% c(1L, 2L)),
                         0L)
  ) %>%
  ungroup() %>%
  mutate(suppress_sec_subtype =
           .n_prim_st == 1L & bread_subtype %in% c(1L, 2L) & !suppress_primary) %>%
  select(-.n_prim_st)

final_output <- final_output %>%
  group_by(bread_def, bread_subtype, energy_adj, residence, edu_level, age_grp) %>%
  mutate(
    .n_prim_sx = sum(suppress_primary & sex %in% c(1L, 2L))
  ) %>%
  ungroup() %>%
  mutate(suppress_sec_sex =
           .n_prim_sx == 1L & sex %in% c(1L, 2L) & !suppress_primary) %>%
  select(-.n_prim_sx)

final_output <- final_output %>%
  group_by(bread_def, bread_subtype, energy_adj, sex, residence, edu_level) %>%
  mutate(
    .has_age_total = any(age_grp == 0L),
    .n_prim_ag     = if_else(.has_age_total,
                             sum(suppress_primary & age_grp != 0L),
                             0L)
  ) %>%
  ungroup()

age_sec_targets <- final_output %>%
  filter(.n_prim_ag == 1L, age_grp != 0L, !suppress_primary) %>%
  group_by(bread_def, bread_subtype, energy_adj, sex, residence, edu_level) %>%
  slice_min(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(bread_def, bread_subtype, energy_adj, sex, residence, edu_level, age_grp) %>%
  mutate(.sec_age = TRUE)

final_output <- final_output %>%
  left_join(age_sec_targets,
            by = c("bread_def","bread_subtype","energy_adj",
                   "sex","residence","edu_level","age_grp")) %>%
  mutate(suppress_sec_age = replace_na(.sec_age, FALSE)) %>%
  select(-.has_age_total, -.n_prim_ag, -.sec_age)

final_output <- final_output %>%
  mutate(
    suppress = case_when(
      suppress_primary                                             ~ "P",
      suppress_sec_subtype | suppress_sec_sex | suppress_sec_age  ~ "S",
      TRUE                                                         ~ ""
    )
  ) %>%
  select(-suppress_primary, -suppress_sec_subtype,
         -suppress_sec_sex, -suppress_sec_age)

cat("\nCell suppression summary (MIN_CELL_N =", MIN_CELL_N, "):\n")
final_output %>% count(suppress) %>% print()

# =============================================================================
# SECTION 12: Export CSV
# =============================================================================
write_csv(final_output, output_csv)
cat("\nCSV exported to:", output_csv, "\n")

# =============================================================================
# SECTION 13: Fill GBI Excel template
# =============================================================================
cat("\n--- Filling GBI Excel template ---\n")

wb <- loadWorkbook(template_path)

n_total       <- nrow(persons)
n_two_day     <- sum(persons$has_d1 & persons$has_d2)
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
              "Day 2 follow-up); person-level mean intakes averaged",
              "across all valid recall days.")),
  c(11, paste("Multi-stage area probability sample of private dwellings;",
              "stratified by state/territory and capital city/rest of state")),
  c(12, "Yes -- NPAFINWT (selected-person Day-1 weight)"),
  c(13, "Yes -- 60 jackknife replicate weights (WPM0101-WPM0160)"),
  c(14, paste("ABS, Microdata: National Nutrition and Physical Activity Survey,",
              "Australia, 2023 (DataLab/SAS unit-record files);",
              "measured height/weight imputed (hot-deck) by ABS so BMR is",
              "calculable for almost all records")),
  c(15, "AUSNUT 2023 (ABS/FSANZ)"),
  c(16, paste("Bread alone: explicit AUSNUT 2023 code list per GBI",
              "2026-05 specification clarification",
              "(12201001-12305002, 12305004-12305007, 12306001-12306004,",
              "13201001, 13201002, 13201018, 13201019, 13202001, 13204003).",
              "Wholegrain vs refined classified by description keyword",
              "(wholemeal/wholegrain/multigrain/mixed grain/rye/spelt/",
              "pumpernickel/added fibre/high fibre/chapatti/injera/roti)",
              "-> wholegrain; else AUSNUT WGBRGM vs RFBRGM majority;",
              "else FIBRE > 5 g/100 g -> wholegrain; default refined.")),
  c(17, paste("Bread all sources: bread-alone codes plus mixed-dish codes",
              "13503001-13504001, 13504004-13507004, 13507008-13507012,",
              "13507016-13507019.",
              "For each food record, bread g summed from WGBRGM+WGSVGM+WGMFGM",
              "(wholegrain) and RFBRGM+RFSVGM+RFMFGM (refined), which the",
              "AUSNUT 2023 disaggregation already provides in grams per record.")),
  c(18, paste("Residual method on the log scale (per GBI 2026-04 note):",
              "fit log(bread) ~ log(energy_kcal) once per bread outcome",
              "across all consumers (unweighted; pooled across strata);",
              "compute log(bread)_adj = log(bread) - beta*(log(energy)",
              "- log(2000)); exponentiate. Non-consumers assigned 0 g/day.",
              "Implausible energy reporters (and persons with missing",
              "energy or BMR) excluded from EA estimates only;",
              "they remain in the unadjusted estimates.")),
  c(19, paste("Energy plausibility: ABS-derived BMR (Schofield-style on the",
              "imputed measured height/weight) and EIBMR1/EIBMR2 (energy-",
              "intake-to-BMR ratio per recall day) read directly from the",
              "NNPAS 2023 SPS file. Flag mean EI:BMR (across valid recall",
              "days) < 0.9 or > 2.4 (Goldberg cutoffs).",
              "Children <10y exempt. Pregnant women are retained: ABS does",
              "not impute their measured weight/height, so BMR is NA and",
              "they fall through the 'can't screen -> retain' branch.",
              "Sex = SEXBIRTH; education from HIGHLVLD (children <15 assigned",
              "highest-educated adult in household).",
              "Urban = ARIA21SL 0-1; Rural = ARIA21SL 2-4.",
              "Day-1 energy = ENERGYF1 (food only, kJ -> kcal /4.184).",
              "Person-level mean intake = mean across all valid recall days",
              "(0-intake days retained as true zeros).",
              "SD reported = weighted empirical SD of person-level means",
              "within stratum (no ANOVA partitioning).",
              "SE estimated via jackknife replication (60 replicate weights)."))
)

ws_info <- "Survey_Info_Template"
for (item in survey_info) {
  writeData(wb, sheet = ws_info, x = item[[2]],
            startCol = 3, startRow = as.integer(item[[1]]),
            colNames = FALSE)
}
cat("  Survey_Info_Template filled\n")

codebook_availability <- rep("Yes", 17)

codebook_comments <- c(
  NA, NA, NA,
  "Both bread alone (1) and bread all sources (2) provided per GBI 2026-05 code lists.",
  paste("Wholegrain/refined classification of bread-alone items:",
        "description keyword -> WGBRGM/RFBRGM majority -> FIBRE > 5 g/100 g;",
        "default refined."),
  paste("Both unadjusted (1) and energy-adjusted (2) estimates provided.",
        "Energy adjustment is the residual method (log-log) standardised",
        "to 2,000 kcal/day, fitted unweighted on consumers only."),
  "Sex = SEXBIRTH (sex at birth) on the NNPAS 2023 selected-person file.",
  paste("Urban = ARIA21SL 0 (Major cities) + 1 (Inner regional);",
        "Rural = ARIA21SL 2 (Outer regional) + 3 (Remote) + 4 (Very remote)."),
  paste("Mapped from HIGHLVLD: Tertiary = postgraduate, bachelor, advanced",
        "diploma/diploma, certificate III/IV; Secondary = year 10-12,",
        "certificate I/II; Primary = year 9 or below, no educational",
        "attainment. Children <15: highest-educated adult in household."),
  "NNPAS age range is 2 years and over. Mapped to GBI bands; band 12 = 85+.",
  NA,
  paste("Weighted mean using NPAFINWT, of person-level mean intake",
        "(averaged across all valid recall days; 0-intake days retained)."),
  paste("Weighted empirical SD of person-level mean intakes within stratum.",
        "No ANOVA partitioning of within/between-person variance is applied."),
  "Design-based SE from jackknife replication using 60 replicate weights (WPM0101-WPM0160).",
  paste("Weighted proportion of persons whose person-level mean intake = 0 g/day",
        "(i.e., bread intake = 0 g on every valid recall day)."),
  paste("Weighted mean person-level mean total energy intake (kcal/day),",
        "across same valid recall days as bread; among persons with valid energy.",
        "An additional sd_kcal column is exported alongside in the CSV."),
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
