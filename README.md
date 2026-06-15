# GBI Bread Intake Estimates — NNPAS 2011-12 and NNPAS 2023

R programs that produce aggregate bread intake estimates from the ABS National Nutrition and Physical Activity Survey (NNPAS), formatted for Global Bread Intake Study].

Two scripts are provided, one per survey wave:

| Script | Survey | Data source |
|---|---|---|
| `GBI_bread_estimates_NNPAS_2011_12.R` | NNPAS 2011-12 | ABS Basic CURF (CSV) |
| `GBI_bread_estimates_NNPAS_2023.R` | NNPAS 2023 | ABS DataLab unit-record files (`sas7bdat`) |

Each script produces output matching the data specifications provided. 

## What they do

For each survey wave, generate population-level estimates of bread intake (g/day) stratified by age group, sex, education level, and area of residence, for:

- **Bread alone** (standalone bread items) and **Bread all sources** (including bread in mixed dishes)
- **Total bread**, **wholegrain bread**, and **refined bread**
- **Non-energy-adjusted** and **energy-adjusted** (residual method, standardised to 2000 kcal)

Outputs:

- A long-format CSV matching the GBI Data_Template (`GBI_DataTemplate_NNPAS_<year>.csv`)
- A filled GBI Excel template (`GBI_AggregateDataForm_NNPAS_<year>.xlsx`)
- `bread_alone_classification_NNPAS_<year>.csv` — the standalone-bread wholegrain/refined lookup table for QC

## Data requirements

Neither dataset is included in this repository — both are restricted ABS microdata.

### NNPAS 2011-12 (Basic CURF)

Place in a `CURF_2011_12/` subdirectory:

| File | Description |
|---|---|
| `npa11bf.csv` | Food-level records (~342k rows); must be the updated CURF carrying the `WGBRGM`/`WGSVGM`/`WGMFGM` + `RFBRGM`/`RFSVGM`/`RFMFGM` bread-gram columns and the imputed `PHDCMHBC`/`PHDKGWBC` |
| `npa11bp.csv` | Person-level records (~12k rows) |

(The separate `ADG_Database.xlsx` is no longer required — the updated CURF carries the bread-gram fields on the food file.)

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

Updated 2026-05 to follow the GBI methodological clarification note (29 April 2026).

- **Person-level means across all valid recall days**: bread intake and total energy are averaged across every valid recall/record day per person (Day 1, and Day 2 where available), with zero-intake days retained as true zeros. Point estimates (means, % non-consumers, energy) use these person-level means — *not* Day 1 alone. Every person is weighted by the **Day-1 final person weight `NPAFINWT`** (selected-person weight in 2023); the Day-2 subsample weight (`NPAD2WGT` in 2011-12) is *not* applied and the Day-2 days are not reweighted.
- **Participants aged <2 years** are excluded (GBI eligibility).
- **Age groups**: 12 GBI bands (2-5, 6-10, 11-14, 15-19, 20-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75-84, 85+).
- **Education for children <15**: assigned from the highest-educated adult in the same household.
- **Energy-adjusted intake** (the preferred output, reported alongside the unadjusted intake): residual method on the log scale — `log(bread) ~ log(energy_kcal)` fitted unweighted on consumers only, pooled across strata, standardised to 2,000 kcal/day; non-consumers are assigned 0 g/day.
- **Energy-plausibility screening**: Goldberg energy-intake-to-BMR cut-offs (< 0.9 or > 2.4). The 2023 script uses the ABS-derived `BMR` and `EIBMR1`/`EIBMR2`; the 2011-12 script computes a Schofield (1985) BMR. Children < 10 y and pregnant women are exempt and always retained; flagged reporters are dropped from the energy-adjusted estimates only.
- **SD**: weighted **empirical** standard deviation of the person-level mean intakes within each stratum (no ANOVA / within-person variance partitioning). A companion `sd_kcal` (SD of person-level mean energy) is also reported.
- **SE**: design-based via jackknife replication (60 replicate weights).

## Wave-specific differences

|  | NNPAS 2011-12 | NNPAS 2023 |
|---|---|---|
| **Bread alone** (AUSNUT codes, per the 2026-05 GBI spec) | 12201001–12307004 + crispbreads 13201001/02, 13201010/12, 13203001/02, 13205003 — pruned to the GBI definition (excludes breadcrumbs/croutons, pizza base, pumpkin bread, johnny cake, corn bread, sweet breads and French toast as not bread; focaccia and topped breads — olive/cheese-topped, garlic/herb — are kept as bread). | 12201001–12306004 + crispbreads 13201001/02, 13201018/19, 13202001, 13204003 — pruned to the GBI definition (excludes breadcrumbs/croutons 12201003–08, **pizza base 12201022–24**, pumpkin bread 12201025, corn bread 12202001, brioche 12305003, sweet breads 12305008–19 and French toast 12306004 as not bread; focaccia and topped breads kept as bread). |
| **Bread all sources** | Bread alone **plus** the bread contained in mixed dishes (sandwiches, burgers, etc.). The bread component of a mixed dish comes from the recipe (ADG) disaggregation — carried on the updated CURF as per-record bread-gram fields (wholegrain `WGBRGM`+`WGSVGM`+`WGMFGM`, refined `RFBRGM`+`RFSVGM`+`RFMFGM`) — summed over the all-bread code list, i.e. the bread-alone codes **+** mixed-dish codes 13503001–13507004, 13507014–13507036, 13508012. | Same concept — bread alone **plus** the bread component of mixed dishes, taken from the ABS recipe disaggregation (the same per-record `WG*GM`/`RF*GM` fields) and summed over the all-bread code list, i.e. the bread-alone codes **+** mixed-dish codes 13503001–13504001, 13504004–13507004, 13507008–13507012, 13507016–13507019. |
| **Wholegrain/refined classification** | Per-code bread-gram majority from the food file (`WGBRGM`+`WGSVGM`+`WGMFGM` vs `RFBRGM`+`RFSVGM`+`RFMFGM`); zero-gram codes default to refined (two wholemeal/mixed-grain English muffins excepted). | Per-code bread-gram majority from AUSNUT 2023 (same six columns); ties default to refined. |
| **Sex** | `SEX` | `SEXBIRTH` |
| **Education** | `HYSCHCBC` + `LVHNSQBC` | `HIGHLVLD` (single combined level) |
| **Remoteness** | `ARIABC` 1-2 → urban; 3 → rural | `ARIA21SL` 0-1 → urban; 2-4 → rural |
| **Energy** | Person-level mean across recall days of `ENERGYT1`/`ENERGYT2` (kJ → kcal). | Person-level mean across recall days of `ENERGYF1`/`ENERGYF2` (food only, kJ → kcal). |
| **BMR for plausibility** | Schofield (1985), computed from imputed measured weight. | ABS-derived `BMR` + `EIBMR1`/`EIBMR2` read from the SPS file. |
| **Recall-day count** | `NUMRECAL` | derived from `COUNTFD2 > 0` |
| **Person ID** | `ABSPID` (effectively unique in CURF) | concatenated `ABSHIDD` + `ABSPID` (`ABSPID` is only unique within household in DataLab; sas column is actually `ABSPIDD` and is renamed on read) |

## Statistical disclosure control (2023 only)

The 2023 script runs on full unit-record DataLab files and must not release cells derived from fewer than n respondents. Section 11 of the 2023 script adds a `suppress` column to `final_output` before export:

| Value | Meaning |
|---|---|
| `""` | No suppression — safe to release |
| `"P"` | **Primary**: cell has `n < x` — must be redacted (where x is threshold minimum) |
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
n, mean_g, sd_g, se_g, pct_non_consumers, mean_kcal, sd_kcal, notes
```

## Citation / data access

- ABS, *Microdata: Australian Health Survey, Nutrition and Physical Activity, 2011-12*, Cat. 4324.0.55.002 (Basic CURF).
- ABS, *Microdata: National Nutrition and Physical Activity Survey, Australia, 2023* (DataLab unit-record files).
