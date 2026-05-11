# GBI Bread Intake Estimates — NNPAS 2011-12 and NNPAS 2023

R programs that produce aggregate bread intake estimates from the ABS National Nutrition and Physical Activity Survey (NNPAS), formatted for the [Global Bread Intake (GBI) Study](https://globalbreadintake.org).

Two scripts are provided, one per survey wave:

| Script | Survey | Data source |
|---|---|---|
| `GBI_bread_estimates_NNPAS_2011_12.R` | NNPAS 2011-12 | ABS Basic CURF (CSV) |
| `GBI_bread_estimates_NNPAS_2023.R` | NNPAS 2023 | ABS DataLab unit-record files (`sas7bdat`) |

Both scripts produce output matching the GBI Aggregate Data Form template (included as `GBI_Aggregate_Data_Form.xlsx` and an editable copy `GBI_Aggregate_Data_Form_unprotected.xlsx`).

## What they do

For each survey wave, generate population-level estimates of bread intake (g/day) stratified by age group, sex, education level, and area of residence, for:

- **Bread alone** (standalone bread items) and **Bread all sources** (including bread in mixed dishes)
- **Total bread**, **wholegrain bread**, and **refined bread**
- **Non-energy-adjusted** and **energy-adjusted** (residual method, standardised to 2000 kcal)

Outputs:

- A long-format CSV matching the GBI Data_Template (`GBI_DataTemplate_NNPAS_<year>.csv`)
- A filled GBI Excel template (`GBI_AggregateDataForm_NNPAS_<year>.xlsx`)
- (2023 only) `bread_alone_classification_NNPAS_2023.csv` — the standalone-bread wholegrain/refined lookup table for QC

## Data requirements

Neither dataset is included in this repository — both are restricted ABS microdata.

### NNPAS 2011-12 (Basic CURF)

Place in a `CURF_2011_12/` subdirectory:

| File | Description |
|---|---|
| `npa11bf.csv` | Food-level records (~342k rows) |
| `npa11bp.csv` | Person-level records (~12k rows) |
| `npa11ehh.csv` | Household-level records |
| `ADG_Database.xlsx` | Australian Dietary Guidelines food composition database |

### NNPAS 2023 (DataLab unit-record)

Place in a `NNPAS_2023/` subdirectory:

| File | Description |
|---|---|
| `nnpas23hhd.sas7bdat` | Household level (~9k rows) |
| `nnpas23sps.sas7bdat` | Selected person level (~14k rows; includes Day-1 nutrient totals, replicate weights) |
| `nnpas23food.sas7bdat` | Food/recall level (~313k rows; includes WGBRGM, RFBRGM at food-record level) |
| `AUSNUT_2023.xlsx` | AUSNUT 2023 food composition database |

The 2023 script must be run inside the ABS DataLab (or any other environment that holds the unit-record files) — it cannot be executed from public files.

## Usage

```r
# 2011-12
source("GBI_bread_estimates_NNPAS_2011_12.R")

# 2023
source("GBI_bread_estimates_NNPAS_2023.R")
```

## R packages

```r
install.packages(c("tidyverse", "readxl", "haven", "survey", "srvyr", "openxlsx"))
```

(`haven` is required only for the 2023 script — it reads the `sas7bdat` files.)

## Key analytical decisions (shared across both scripts)

- **Day 1 only** for point estimates (means, % non-consumers, energy); Day 2 used only for within-person variance estimation (SD correction via ANOVA on 2-day responders).
- **Age groups**: 12 GBI bands (2-5, 6-10, 11-14, 15-19, 20-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75-84, 85+).
- **Education for children <15**: assigned from the highest-educated adult in the same household.
- **SE**: design-based via jackknife replication (60 replicate weights).
- **SD**: corrected for within-person variation using the between-person variance ratio from the 2-day subsample.
- **Energy adjustment**: residual method, with optional log-transform when Shapiro-Wilk indicates non-normal positives.

## Wave-specific differences

|  | NNPAS 2011-12 | NNPAS 2023 |
|---|---|---|
| **Bread "all sources"** | Disaggregated via the ADG Database (g of WG/refined bread per 100 g of any food, multiplied by `GRAMWGT`/100). | Direct sum of food-record-level `WGBRGM` + `RFBRGM` — the ABS already disaggregates per food, so no ADG step is needed. |
| **Standalone bread codes** | AUSNUT 2011-13 codes 12201001–12307004 | AUSNUT 2023 codes 12201001–12306003 |
| **Wholegrain/refined classification** | ADG columns 1011/1021 first; description-keyword fallback for items neither column flags. | Layered: description keywords → AUSNUT 2023 `WGBRGM`/`RFBRGM` majority → `FIBRE` > 5 g/100 g → default refined. |
| **Sex** | `SEX` | `SEXBIRTH` |
| **Education** | `HYSCHCBC` + `LVHNSQBC` | `HIGHLVLD` (single combined level) |
| **Remoteness** | `ARIABC` 1-2 → urban; 3 → rural | `ARIA21SL` 0-1 → urban; 2-4 → rural |
| **Day-1 energy** | `ENERGYT1` (food + supplements) | `ENERGYF1` (food only) |
| **Recall-day count** | `NUMRECAL` | derived from `COUNTFD2 > 0` |
| **Person ID** | `ABSPID` (effectively unique in CURF) | concatenated `ABSHIDD` + `ABSPID` (`ABSPID` is only unique within household in DataLab; sas column is actually `ABSPIDD` and is renamed on read) |

## Statistical disclosure control (2023 only)

The 2023 script runs on full unit-record DataLab files and must not release cells derived from fewer than 3 respondents. Section 11 of the 2023 script adds a `suppress` column to `final_output` before export:

| Value | Meaning |
|---|---|
| `""` | No suppression — safe to release |
| `"P"` | **Primary**: cell has `n < 3` — must be redacted |
| `"S"` | **Secondary**: cell is safe on its own, but publishing it would allow back-calculation of an adjacent primary-suppressed cell by differencing from a total — should also be redacted |

Three additive relationships are checked: `bread_subtype` (total = WG + refined), `sex` (persons = males + females), and `age_grp` (all-ages total = sum of 12 bands). For the binary dimensions a complementary cell is always suppressed alongside the primary; for the 12 age bands a single additional band (smallest remaining `n`) is flagged to make the system underdetermined.

The 2011-12 script uses the Basic CURF, which is already confidentialised by ABS, so no suppression step is needed.

## Output schema

Both scripts emit identical schemas matching the GBI Data_Template:

```
survey_name, year_start, year_end,
bread_def (1=alone, 2=all sources),
bread_subtype (0=total, 1=wholegrain, 2=refined),
energy_adj (1=raw, 2=energy-adjusted),
sex (0=combined, 1=male, 2=female),
residence (1=urban, 2=rural, NA=combined),
edu_level (1=primary, 2=secondary, 3=tertiary, NA=combined),
age_grp (0=all ages, 1-12=GBI bands),
n, mean_g, sd_g, se_g, pct_non_consumers, mean_kcal, notes
```

## Citation / data access

- ABS, *Microdata: Australian Health Survey, Nutrition and Physical Activity, 2011-12*, Cat. 4324.0.55.002 (Basic CURF).
- ABS, *Microdata: National Nutrition and Physical Activity Survey, Australia, 2023* (DataLab unit-record files).
