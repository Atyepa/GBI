# =============================================================================
# GBI Aggregate Bread Intake Estimates -- NNPAS 2011-12
# =============================================================================
# Produces population-level bread intake estimates from the ABS 2011-12
# National Nutrition and Physical Activity Survey (NNPAS) Basic CURF,
# formatted for the Global Bread Intake (GBI) Study aggregate data request.
#
# Updated 2026-05 to follow the GBI methodological clarification note
# ("GBI_Methodological_Note_Energy_Adjustment.pdf", 29 April 2026):
#   - Person-level mean intakes are computed across all valid recall days
#     (Day 1 and, where available, Day 2), with zero-intake days retained.
#   - Energy-adjusted intake (residual method, log-log, standardised to
#     2,000 kcal/day) is the preferred output, alongside unadjusted intake.
#   - Energy-plausibility screening uses Schofield BMR + Goldberg cutoffs
#     (EI:BMR < 0.9 or > 2.4). Children <10y and pregnant women are exempt
#     from this filter and are always retained.
#   - SDs are the weighted empirical SD of person-level mean intakes
#     within each stratum (no ANOVA partitioning).
#   - Both bread-alone and bread-from-all-sources use explicit AUSNUT
#     code lists per the 2026-05 GBI specification clarification.
#   - Bread grams and the wholegrain/refined split now come from the
#     bread-gram fields carried on the 2011-12 Basic CURF food file
#     (WGBRGM/WGSVGM/WGMFGM, RFBRGM/RFSVGM/RFMFGM -- grams per record),
#     This matches the 2023 NNPAS mechanism.
# =============================================================================

library(tidyverse)
library(survey)
library(srvyr)
library(openxlsx)   # template fill only; 

# --- Paths ----------------------------------------------------------------
curf_dir <- "CURF_2011-12"  # update if CURF files live elsewhere

template_path <- "GBI_Aggregate_Data_Form_unprotected.xlsx"
output_csv    <- "GBI_DataTemplate_NNPAS_2011-12.csv"
output_xlsx   <- "GBI_AggregateDataForm_NNPAS_2011-12.xlsx"

# --- Disclosure-control "rule of n" --------------------------------------
# Minimum cell size below which a stratum is primary-suppressed.
# The 2011-12 Basic CURF is already confidentialised by the ABS, so no
# further cell suppression is required here -- MIN_CELL_N = 1 leaves all
# cells releasable. (ABS DataLab egress, by contrast, requires 10; raise
# this if adapting the script to a non-confidentialised source.)
MIN_CELL_N <- 1

# --- Run report -----------------------------------------------------------
# Tee every console diagnostic below (all cat()/print() output) to a plain-
# text report while still showing it in the console. Set REPORT <- FALSE to
# disable; swap report_path for the commented timestamp form to keep every run.
REPORT      <- TRUE
report_path <- "GBI_run_report_NNPAS_2011-12.txt"
# report_path <- format(Sys.time(), "GBI_run_report_NNPAS_2011-12_%Y%m%d_%H%M%S.txt")
if (REPORT) {
  while (sink.number() > 0) sink()        # clear any stray sink from a failed run
  report_con <- file(report_path, open = "wt")
  sink(report_con, split = TRUE)
  cat(strrep("=", 78), "\n")
  cat("GBI bread-intake estimator -- run report\n")
  cat("Script     :", "GBI_bread_estimates_NNPAS_2011_12.R", "\n")
  cat("Run at     :", format(Sys.time()), "\n")
  cat("MIN_CELL_N :", MIN_CELL_N, "\n")
  cat(strrep("=", 78), "\n\n")
}

# =============================================================================
# SECTION 1: AUSNUT food-code lists
# =============================================================================
# The updated 2011-12 Basic CURF carries the AUSNUT/ADG bread-gram fields on
# the food file itself (WGBRGM/WGSVGM/WGMFGM, RFBRGM/RFSVGM/RFMFGM -- already
# in grams per food record, like the 2023 NNPAS), so the separate
# ADG_Database.xlsx lookup and its g/100g recode are no longer required.

# --- 1a. AUSNUT 2011-13 code lists ------------------------------------------
# Bread-product code ranges per the 2026-05 GBI spec clarification, then
# pruned to the GBI "bread alone" / "bread all sources" definitions
# (GBI_Aggregate_Data_Form.xlsx) after reviewing food descriptions.
bread_product_codes <- c(
  12201001:12307004,                                # Breads
  13201001, 13201002,                               # Crispbreads
  13201010, 13201012, 13203001, 13203002, 13205003  # Crispbreads
)

# Items the GBI definition excludes as NOT bread (quick/sweet breads and
# French toast) -- dropped from every measure. ABS already assigns most of
# these zero bread-grams, so "all sources" is barely affected. Focaccia and
# topped breads (olive/cheese-topped, garlic/herb) ARE bread and are kept.
not_bread_codes <- c(
  12201003:12201006,   # Breadcrumbs, croutons, damper
  12203018:12203024,   # Pizza base, pumpkin bread, damper
  12203027, 12214001,  # johnny cake, corn bread
  12305007:12307000,   # Sweet breads (garlic/herb 12307001-2 kept as bread)
  12307003, 12307004   # French toast
)

# "Bread alone" = bread products minus non-bread items.
bread_alone_codes_2011_12 <- setdiff(bread_product_codes, not_bread_codes)

# "All bread" = bread products (dropping non-bread items) plus the mixed-dish
# bread codes (sandwiches, burgers, etc.).
all_bread_codes_2011_12 <- c(
  setdiff(bread_product_codes, not_bread_codes),
  13503001:13507004, 13507014:13507036, 13508012
)

# --- 1b. Wholegrain tie-breaker for zero-content bread-alone codes ----------
# Wholegrain vs refined is decided per food record from the bread-gram columns
# on the food file (WG sum vs refined sum; see Sections 2b and 4). A handful of
# bread-alone codes carry no bread-gram content at all (pumpkin bread,
# dried-fruit bread, two English muffins) and so tie at 0 = 0; these default to
# refined, except the two that are wholemeal / mixed-grain by name:
wholegrain_tiebreak_codes <- c(
  12301004,   # Muffin, English style, from wholemeal flour
  12301005    # Muffin, English style, mixed grain
)

# =============================================================================
# SECTION 2: Load CURF data files
# =============================================================================

# --- 2a. Person file --------------------------------------------------------
# Variables loaded (DIL names; verify in your CURF release):
#   ABSHID, ABSPID  ............ identifiers
#   AGEC, SEX  .................. demographics
#   PHDCMHBC, PHDKGWBC  ........ measured height (cm), weight (kg);
#                                 imputed values overwrite 997/998 codes
#   PREGNANT  .................. pregnancy flag (1 = pregnant) -- TODO:
#                                 confirm name against the DIL distributed
#                                 with the imputed dataset
#   NPAFINWT, WPM01xx  ......... Day-1 person weight + 60 replicate weights
#   NPAD2WGT  .................. Day-2 weight (0 if no Day 2 recall)
#   NUMRECAL  .................. number of recall days (1 or 2)
#   ARIABC, HYSCHCBC, LVHNSQBC . stratification variables
#   ENERGYT1, ENERGYT2  ........ Day-1 / Day-2 total energy intake (kJ)
persons <- read_csv(file.path(curf_dir, "npa11bp.csv"),
                    show_col_types = FALSE)

persons <- persons %>%
  select(
    ABSHID, ABSPID,
    AGEC, SEX,
    any_of(c("PHDCMHBC", "PHDKGWBC", "PREGNANT")),
    NPAFINWT, NPAD2WGT, NUMRECAL,
    ARIABC, HYSCHCBC, LVHNSQBC,
    ENERGYT1, ENERGYT2,
    starts_with("WPM01")
  )

# Backstop: if the measured h/w or pregnancy columns are named differently
# in the imputed CURF release, replace these lines to populate the columns
# the script uses below (height_cm, weight_kg, pregnant_flag).
if (!"PHDCMHBC" %in% names(persons)) persons$PHDCMHBC <- NA_real_
if (!"PHDKGWBC" %in% names(persons)) persons$PHDKGWBC <- NA_real_
if (!"PREGNANT" %in% names(persons)) persons$PREGNANT <- NA_integer_

cat("\nPersons loaded:", nrow(persons), "\n")

# --- 2b. Food file ----------------------------------------------------------
# The updated CURF carries the bread-gram fields per food record (grams of
# wholegrain / refined bread per record, already AUSNUT g/100g x GRAMWGT/100):
#   WGBRGM / WGSVGM / WGMFGM  -- wholegrain bread / savoury crackers / muffins
#   RFBRGM / RFSVGM / RFMFGM  -- refined   bread / savoury crackers / muffins
foods <- read_csv(file.path(curf_dir, "npa11bf.csv"),
                  show_col_types = FALSE) %>%
  select(ABSPID, DAYNUM, FOODCODC, GRAMWGT,
         WGBRGM, WGSVGM, WGMFGM, RFBRGM, RFSVGM, RFMFGM) %>%
  mutate(
    wg_g  = coalesce(WGBRGM, 0) + coalesce(WGSVGM, 0) + coalesce(WGMFGM, 0),
    ref_g = coalesce(RFBRGM, 0) + coalesce(RFSVGM, 0) + coalesce(RFMFGM, 0)
  )

cat("Food records loaded:", nrow(foods), "\n")
cat("  Day 1 records:", sum(foods$DAYNUM == 1), "\n")
cat("  Day 2 records:", sum(foods$DAYNUM == 2), "\n")

# --- 2c. Bread-alone wholegrain/refined classification (per code) -----------
# Classify each bread-alone code by its bread-gram majority (constant per code,
# since wg_g/ref_g scale with GRAMWGT); zero-content codes default to refined
# unless wholegrain by name (see wholegrain_tiebreak_codes).
bread_alone_classify <- foods %>%
  filter(FOODCODC %in% bread_alone_codes_2011_12) %>%
  group_by(FOODCODC) %>%
  summarise(wg_sum = sum(wg_g), rf_sum = sum(ref_g), .groups = "drop") %>%
  mutate(
    bread_class = case_when(
      wg_sum > rf_sum                          ~ "wholegrain",
      rf_sum > wg_sum                          ~ "refined",
      FOODCODC %in% wholegrain_tiebreak_codes  ~ "wholegrain",
      TRUE                                     ~ "refined"
    )
  ) %>%
  select(FOODCODC, bread_class)

cat("\nBread-alone codes consumed & classified:", nrow(bread_alone_classify), "\n")
count(bread_alone_classify, bread_class) %>% print()
write_csv(bread_alone_classify, "bread_alone_classification_NNPAS_2011_12.csv")

# =============================================================================
# SECTION 3: Stratification and person-level prep
# =============================================================================

# --- 3a. Exclude any participants aged <2 years (GBI age eligibility) -------
n_under2 <- sum(persons$AGEC < 2, na.rm = TRUE)
if (n_under2 > 0) {
  cat("\nExcluding", n_under2, "participants aged <2 years (GBI eligibility).\n")
  persons <- persons %>% filter(AGEC >= 2)
}

# --- 3b. Age groups (GBI bands 1-12) ----------------------------------------
age_breaks <- c(2, 6, 11, 15, 20, 25, 35, 45, 55, 65, 75, 85, Inf)
age_labels <- 1:12   # 1 = 2-5y, 2 = 6-10y, ..., 12 = 85+

persons <- persons %>%
  mutate(
    age_grp = cut(AGEC, breaks = age_breaks, labels = age_labels,
                  right = FALSE, include.lowest = TRUE) %>% as.integer()
  )

cat("\nAge group distribution:\n")
persons %>% count(age_grp) %>% print()

# --- 3c. Education level ----------------------------------------------------
# Tertiary (3): LVHNSQBC 1-4 (postgrad, bachelor, diploma, cert III/IV)
# Secondary (2): HYSCHCBC 1-3 (Year 10-12) with no tertiary qual
# Primary  (1): HYSCHCBC 4-5 (Year 9 or below) with no tertiary qual
# Children <15: highest adult education in the same household.
persons <- persons %>%
  mutate(
    edu_level_own = case_when(
      AGEC < 15            ~ NA_integer_,
      LVHNSQBC %in% 1:4   ~ 3L,
      HYSCHCBC %in% 1:3   ~ 2L,
      HYSCHCBC %in% 4:5   ~ 1L,
      TRUE                ~ 2L
    )
  )

adult_edu <- persons %>%
  filter(AGEC >= 15, !is.na(edu_level_own)) %>%
  group_by(ABSHID) %>%
  summarise(hh_adult_edu = max(edu_level_own), .groups = "drop")

persons <- persons %>%
  left_join(adult_edu, by = "ABSHID") %>%
  mutate(edu_level = if_else(AGEC < 15, hh_adult_edu, edu_level_own))

n_edu_na <- sum(is.na(persons$edu_level))
if (n_edu_na > 0) {
  cat("\nWARNING:", n_edu_na, "persons with missing education -- defaulting to Secondary\n")
  persons <- persons %>% mutate(edu_level = replace_na(edu_level, 2L))
}

cat("\nEducation level distribution:\n")
persons %>% count(edu_level) %>% print()

# --- 3d. Residence ----------------------------------------------------------
# Urban (1) = Major cities + Inner regional (ARIABC 1-2)
# Rural (2) = Other/remote (ARIABC 3)
persons <- persons %>%
  mutate(residence = if_else(ARIABC %in% c(1, 2), 1L, 2L))

cat("\nResidence distribution:\n")
persons %>% count(residence) %>% print()

# --- 3e. Lowercase sex for output -------------------------------------------
persons <- persons %>% mutate(sex = as.integer(SEX))

# =============================================================================
# SECTION 4: Person-level bread intake (mean across all valid recall days)
# =============================================================================
# Per GBI 2026-05 spec:
#   - Each valid recall day contributes to the person-level mean.
#   - Days with zero bread intake are retained as true zero-intake days.
#   - The same set of valid days must be used for bread and for energy.
#
# We treat a recall day as "valid" for a person if any food records exist
# for that (ABSPID, DAYNUM) combination in the food file.

# --- 4a. Valid recall days per person ---------------------------------------
person_recall_days <- foods %>%
  distinct(ABSPID, DAYNUM) %>%
  group_by(ABSPID) %>%
  summarise(
    has_d1 = any(DAYNUM == 1),
    has_d2 = any(DAYNUM == 2),
    n_days = n_distinct(DAYNUM),
    .groups = "drop"
  )

persons <- persons %>%
  left_join(person_recall_days, by = "ABSPID") %>%
  mutate(
    has_d1 = coalesce(has_d1, FALSE),
    has_d2 = coalesce(has_d2, FALSE),
    n_days = coalesce(n_days, 0L)
  )

cat("\nRecall-day coverage:\n")
persons %>% count(has_d1, has_d2) %>% print()

# Universe of (person, day) cells that must average into person means.
recall_days_long <- foods %>% distinct(ABSPID, DAYNUM)

# --- 4b. Bread alone per (person, day) --------------------------------------
bread_alone_pd <- recall_days_long %>%
  left_join(
    foods %>%
      filter(FOODCODC %in% bread_alone_codes_2011_12) %>%
      inner_join(bread_alone_classify %>% select(FOODCODC, bread_class),
                 by = "FOODCODC") %>%
      group_by(ABSPID, DAYNUM) %>%
      summarise(
        ba_total_g_d = sum(GRAMWGT, na.rm = TRUE),
        ba_wg_g_d    = sum(GRAMWGT[bread_class == "wholegrain"], na.rm = TRUE),
        ba_ref_g_d   = sum(GRAMWGT[bread_class == "refined"],    na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("ABSPID", "DAYNUM")
  ) %>%
  mutate(across(c(ba_total_g_d, ba_wg_g_d, ba_ref_g_d),
                ~ replace_na(.x, 0)))

# --- 4c. Bread all sources per (person, day) --------------------------------
# Bread grams are read straight from the food-file bread-gram columns
# (wg_g / ref_g, computed in Section 2b), summed over the all-bread code list.
bread_allsrc_pd <- recall_days_long %>%
  left_join(
    foods %>%
      filter(FOODCODC %in% all_bread_codes_2011_12) %>%
      group_by(ABSPID, DAYNUM) %>%
      summarise(
        bas_wg_g_d    = sum(wg_g,  na.rm = TRUE),
        bas_ref_g_d   = sum(ref_g, na.rm = TRUE),
        bas_total_g_d = bas_wg_g_d + bas_ref_g_d,
        .groups = "drop"
      ),
    by = c("ABSPID", "DAYNUM")
  ) %>%
  mutate(across(c(bas_total_g_d, bas_wg_g_d, bas_ref_g_d),
                ~ replace_na(.x, 0)))

# --- 4d. Person-level means across valid recall days ------------------------
bread_alone_person <- bread_alone_pd %>%
  group_by(ABSPID) %>%
  summarise(
    ba_total_g = mean(ba_total_g_d),
    ba_wg_g    = mean(ba_wg_g_d),
    ba_ref_g   = mean(ba_ref_g_d),
    .groups    = "drop"
  )

bread_allsrc_person <- bread_allsrc_pd %>%
  group_by(ABSPID) %>%
  summarise(
    bas_total_g = mean(bas_total_g_d),
    bas_wg_g    = mean(bas_wg_g_d),
    bas_ref_g   = mean(bas_ref_g_d),
    .groups     = "drop"
  )

# --- 4e. Person-level mean energy (kcal/day) across the SAME recall days ----
persons <- persons %>%
  mutate(
    energy_kcal_d1 = if_else(has_d1 & !is.na(ENERGYT1),
                             ENERGYT1 / 4.184, NA_real_),
    energy_kcal_d2 = if_else(has_d2 & !is.na(ENERGYT2),
                             ENERGYT2 / 4.184, NA_real_),
    mean_energy_kcal = case_when(
      has_d1 & has_d2 & !is.na(energy_kcal_d1) & !is.na(energy_kcal_d2) ~
        (energy_kcal_d1 + energy_kcal_d2) / 2,
      has_d1 & !is.na(energy_kcal_d1) ~ energy_kcal_d1,
      has_d2 & !is.na(energy_kcal_d2) ~ energy_kcal_d2,
      TRUE                            ~ NA_real_
    )
  )

# --- 4f. Merge bread intakes onto persons; non-recall persons get NA --------
persons <- persons %>%
  left_join(bread_alone_person,   by = "ABSPID") %>%
  left_join(bread_allsrc_person,  by = "ABSPID")

# Persons with no recall days at all (n_days == 0) have NA bread intake;
# everyone else either consumed bread on >=1 day or had a recorded recall
# with no bread (true zero). Treat the latter as 0 g/day. Drop the former
# from analyses since neither bread nor energy can be computed.
n_no_recall <- sum(persons$n_days == 0L)
if (n_no_recall > 0) {
  cat("\nExcluding", n_no_recall, "persons with no recall days at all.\n")
  persons <- persons %>% filter(n_days > 0L)
}

# =============================================================================
# SECTION 5: Schofield BMR and Goldberg-style energy plausibility flag
# =============================================================================
# Schofield (1985) weight-only equations, MJ/day. Converted to kcal/day
# using 1 MJ = 239.006 kcal.
#
# Goldberg cutoffs: flag persons with EI:BMR < 0.9 or > 2.4 as implausible.
# Per agreed instruction, pregnant women and children <10 years are
# exempt from this filter and are always retained as plausible.

schofield_bmr_mj <- function(weight_kg, age, sex) {
  case_when(
    is.na(weight_kg) | is.na(age) | is.na(sex)        ~ NA_real_,
    sex == 1L & age <  3                              ~ 0.249 * weight_kg - 0.127,
    sex == 1L & age >=  3 & age < 10                  ~ 0.095 * weight_kg + 2.110,
    sex == 1L & age >= 10 & age < 18                  ~ 0.074 * weight_kg + 2.754,
    sex == 1L & age >= 18 & age < 30                  ~ 0.063 * weight_kg + 2.896,
    sex == 1L & age >= 30 & age < 60                  ~ 0.048 * weight_kg + 3.653,
    sex == 1L & age >= 60                             ~ 0.049 * weight_kg + 2.459,
    sex == 2L & age <  3                              ~ 0.244 * weight_kg - 0.130,
    sex == 2L & age >=  3 & age < 10                  ~ 0.085 * weight_kg + 2.033,
    sex == 2L & age >= 10 & age < 18                  ~ 0.056 * weight_kg + 2.898,
    sex == 2L & age >= 18 & age < 30                  ~ 0.062 * weight_kg + 2.036,
    sex == 2L & age >= 30 & age < 60                  ~ 0.034 * weight_kg + 3.538,
    sex == 2L & age >= 60                             ~ 0.038 * weight_kg + 2.755,
    TRUE                                              ~ NA_real_
  )
}

persons <- persons %>%
  mutate(
    bmr_kcal = schofield_bmr_mj(PHDKGWBC, AGEC, sex) * 239.006,
    ei_bmr_ratio = if_else(!is.na(bmr_kcal) & bmr_kcal > 0,
                           mean_energy_kcal / bmr_kcal, NA_real_),
    pregnant_flag = !is.na(PREGNANT) & PREGNANT == 1,
    energy_implausible = case_when(
      AGEC < 10                                ~ FALSE,
      pregnant_flag                           ~ FALSE,
      is.na(ei_bmr_ratio)                     ~ FALSE,  # can't screen -> retain
      ei_bmr_ratio < 0.9 | ei_bmr_ratio > 2.4 ~ TRUE,
      TRUE                                    ~ FALSE
    )
  )

n_impl     <- sum(persons$energy_implausible, na.rm = TRUE)
n_bmr_miss <- sum(is.na(persons$bmr_kcal))
cat("\nEnergy plausibility (Schofield BMR + Goldberg <0.9 or >2.4):\n")
cat("  BMR not calculable:        ", n_bmr_miss, "(retained for unadjusted)\n")
cat("  Flagged implausible:       ", n_impl,
    sprintf("(%.1f%% of those screened)\n",
            100 * n_impl / max(1, sum(!is.na(persons$energy_implausible)))))
cat("  Exempt (preg or AGEC<10):   ",
    sum((persons$AGEC < 10) | persons$pregnant_flag), "\n")

# =============================================================================
# SECTION 6: Energy adjustment (residual method, log-log, pooled per outcome)
# =============================================================================
# Per GBI 2026-05 spec:
#   - For each bread outcome, fit log(bread) ~ log(energy_kcal) on consumers
#     (mean bread > 0) who have valid mean energy AND are not flagged as
#     implausible energy reporters. Regression is unweighted (Step 20).
#   - Standardize: log(bread)_adj = log(bread) - beta * (log(energy) - log(2000))
#     then exponentiate.
#   - Non-consumers have adjusted intake set to 0 g/day.
#   - Persons with NA bread (no recall) or NA energy or implausible energy
#     have NA adjusted intake -- excluded from EA stratum estimates only.

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
rep_wt_cols <- paste0("WPM01", sprintf("%02d", 1:60))

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
# For each (bread definition, bread subtype, energy adjustment, stratum):
#   - n: unweighted persons contributing to the estimate
#   - mean_g, se_g: weighted mean and jackknife SE of person-level intake
#   - sd_g: weighted empirical SD of person-level intakes
#   - pct_non_consumers: weighted % whose person-level mean intake = 0
#   - mean_kcal, sd_kcal: weighted mean and SD of person-level mean energy
#                          (computed among persons with valid energy)
#
# For energy-adjusted (energy_adj == 2) outputs, persons whose adjusted
# intake is NA (implausible or missing energy) are filtered out before
# survey estimation, per PDF Section 4.2 step 20.

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

# Helper: build the full slate of stratum cuts for a single bread variable.
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
  "Bread alone = AUSNUT 2011-13 bread-product codes (12201001-12307004 plus",
  "crispbreads 13201001-2, 13201010/12, 13203001-2, 13205003), pruned to the",
  "GBI bread-alone definition: excludes breadcrumbs/croutons, pizza base,",
  "pumpkin bread, johnny cake, corn bread, sweet breads and French toast as",
  "not bread. Focaccia and topped breads (olive/cheese-topped, garlic/herb)",
  "are counted as bread.",
  "Bread all sources = bread products (excluding the non-bread items) plus",
  "mixed-dish codes (13503001-13507004, 13507014-13507036, 13508012).",
  "Bread g read from the food-file bread-gram columns:",
  "wholegrain = WGBRGM+WGSVGM+WGMFGM, refined = RFBRGM+RFSVGM+RFMFGM",
  "(grams per record). Bread-alone wholegrain/refined split by the",
  "per-code bread-gram majority.",
  "Person-level mean intake averaged across all valid recall days,",
  "with 0-intake days retained.",
  "Weighting uses the Day-1 final person weight (NPAFINWT) for all persons;",
  "the Day-2 subsample weight (NPAD2WGT) is not applied -- intake and energy",
  "are the within-person mean across available days but are not Day-2",
  "reweighted.",
  "Energy adjustment: residual method, log(bread) ~ log(energy),",
  "fitted unweighted on consumers only, standardized to 2000 kcal/day;",
  "non-consumers assigned 0 g/day adjusted intake.",
  "Energy plausibility: Schofield BMR + Goldberg EI:BMR <0.9 or >2.4;",
  "pregnant women and children <10y exempt.",
  "Urban = ARIABC 1-2 (Major cities + Inner regional);",
  "Rural = ARIABC 3 (Other/remote).",
  "Education for children <15 assigned from highest-educated household adult."
)

final_output <- bind_rows(results_unadj, results_ea) %>%
  mutate(
    survey_name = "National Nutrition and Physical Activity Survey",
    year_start  = 2011L,
    year_end    = 2012L,
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
# SECTION 10: QC
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
# suppress = "P": primary suppression (n < MIN_CELL_N)
# suppress = "S": secondary suppression (prevents back-calculation via total)
#
# Additive relationships guarded:
#   (1) bread_subtype: 0 = 1 + 2   - binary complement rule (unadjusted only;
#                                     EA estimates are not expected to be
#                                     additive per GBI 2026-05 spec)
#   (2) sex:           0 = 1 + 2   - binary complement rule
#   (3) age_grp:       0 = sum(1:12)

final_output <- final_output %>%
  mutate(suppress_primary = !is.na(n) & n < MIN_CELL_N)

# Bread-subtype additivity (unadjusted only)
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

# Sex additivity (0 = 1 + 2)
final_output <- final_output %>%
  group_by(bread_def, bread_subtype, energy_adj, residence, edu_level, age_grp) %>%
  mutate(
    .n_prim_sx = sum(suppress_primary & sex %in% c(1L, 2L))
  ) %>%
  ungroup() %>%
  mutate(suppress_sec_sex =
           .n_prim_sx == 1L & sex %in% c(1L, 2L) & !suppress_primary) %>%
  select(-.n_prim_sx)

# Age-group additivity (0 = sum(1:12)); only relevant where age_grp = 0 exists
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

# --- 12a. Survey_Info_Template ----------------------------------------------
n_total   <- nrow(persons)
n_two_day <- sum(persons$has_d1 & persons$has_d2)
sample_text <- sprintf("%s persons (%s with 2-day recall)",
                       format(n_total, big.mark = ","),
                       format(n_two_day, big.mark = ","))

survey_info <- list(
  c(3,  "National Nutrition and Physical Activity Survey (NNPAS)"),
  c(4,  "Australia"),
  c(5,  "2011-2012"),
  c(6,  "National (all states and territories, urban and rural)"),
  c(7,  "Persons aged 2 years and over, usual residents of private dwellings"),
  c(8,  sample_text),
  c(9,  "24-hour dietary recall (interviewer-administered, AMPM method)"),
  c(10, paste("2 days for most respondents (Day 1 face-to-face,",
              "Day 2 telephone); person-level mean intakes averaged",
              "across all valid recall days.")),
  c(11, paste("Multi-stage area probability sample of private dwellings;",
              "stratified by state/territory and capital city/rest of state")),
  c(12, paste("Yes -- NPAFINWT (Day-1 final person weight), applied to every",
              "person. Intake and energy are the within-person mean across",
              "available recall days, but the Day-2 subsample weight (NPAD2WGT)",
              "is not used and the Day-2 days are not reweighted.")),
  c(13, "Yes -- 60 jackknife replicate weights (WPM0101-WPM0160)"),
  c(14, paste("ABS Cat. No. 4324.0.55.002, Microdata: Australian Health Survey,",
              "Nutrition and Physical Activity, 2011-12, Basic CURF",
              "(with measured height/weight imputed for missing values)")),
  c(15, paste("AUSNUT 2011-13 (ABS/FSANZ). Bread-gram fields carried on the",
              "updated Basic CURF food file (no separate ADG lookup needed).")),
  c(16, paste("Bread alone: explicit AUSNUT 2011-13 code list per GBI",
              "2026-05 specification clarification (12201001-12307004 plus",
              "crispbreads 13201001-2, 13201010/12, 13203001-2, 13205003),",
              "pruned to exclude breadcrumbs/croutons, pizza base, pumpkin",
              "bread, johnny cake, corn bread, sweet breads and French toast",
              "as not bread (focaccia and topped breads kept as bread).",
              "Wholegrain vs refined classified by the food-file bread-gram",
              "majority per code (WGBRGM+WGSVGM+WGMFGM vs",
              "RFBRGM+RFSVGM+RFMFGM); the few codes with no bread-gram",
              "content default to refined, except two English muffins that",
              "are wholemeal/mixed-grain by name.")),
  c(17, paste("Bread all sources: bread-alone codes plus mixed-dish codes",
              "13503001-13507004, 13507014-13507036, 13508012.",
              "Bread g read from the food-file bread-gram columns",
              "WGBRGM+WGSVGM+WGMFGM (wholegrain) and",
              "RFBRGM+RFSVGM+RFMFGM (refined), already in grams per record.")),
  c(18, paste("Residual method on the log scale (per GBI 2026-04 note):",
              "fit log(bread) ~ log(energy_kcal) once per bread outcome",
              "across all consumers (unweighted; pooled across strata);",
              "compute log(bread)_adj = log(bread) - beta*(log(energy)",
              "- log(2000)); exponentiate. Non-consumers assigned 0 g/day.",
              "Implausible energy reporters (and persons with missing",
              "energy or BMR) excluded from EA estimates only;",
              "they remain in the unadjusted estimates.")),
  c(19, paste("Energy plausibility: Schofield (1985) weight-based BMR;",
              "flag EI:BMR < 0.9 or > 2.4 (Goldberg cutoffs).",
              "Pregnant women and children <10y exempt and always retained.",
              "Measured height and weight are imputed (hot-deck by ABS-style",
              "approach) so BMR is calculable for almost all records.",
              "Urban = ARIABC 1-2; Rural = ARIABC 3.",
              "Education for children <15 assigned from highest-educated",
              "adult in the household.",
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

# --- 12b. Codebook -----------------------------------------------------------
codebook_availability <- rep("Yes", 17)

codebook_comments <- c(
  NA, NA, NA,
  "Both bread alone (1) and bread all sources (2) provided per GBI 2026-05 code lists.",
  paste("Wholegrain/refined classified by the food-file bread-gram majority",
        "per code (WGBRGM+WGSVGM+WGMFGM vs RFBRGM+RFSVGM+RFMFGM);",
        "zero-content codes default to refined (two wholemeal/mixed-grain",
        "English muffins excepted)."),
  paste("Both unadjusted (1) and energy-adjusted (2) estimates provided.",
        "Energy adjustment is the residual method (log-log) standardised",
        "to 2,000 kcal/day, fitted unweighted on consumers only."),
  NA,
  "Urban = ARIABC 1 (Major cities) + 2 (Inner regional). Rural = ARIABC 3.",
  paste("Mapped from NNPAS: Tertiary = LVHNSQBC 1-4; Secondary = HYSCHCBC 1-3;",
        "Primary = HYSCHCBC 4-5. Children <15: highest-educated adult in household."),
  "NNPAS age range is 2-85+. Mapped to GBI bands; band 12 = 85+.",
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

# --- 12c. Data_Template ------------------------------------------------------
# The template expects the original 17 columns (no sd_kcal). Provide them,
# and keep sd_kcal in the CSV for the contributor documentation note.
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

# --- Close the run report -------------------------------------------------
if (REPORT) {
  sink(); close(report_con)
  message("Run report written to: ",
          normalizePath(report_path, mustWork = FALSE))
}
