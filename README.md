# GBI Bread Intake Estimates — NNPAS 2011-12

R program to produce aggregate bread intake estimates from the ABS 2011-12 National Nutrition and Physical Activity Survey (NNPAS) Basic CURF, for the [Global Bread Intake (GBI) Study](https://globalbreadintake.org).

## What it does

Generates population-level estimates of bread intake (g/day) stratified by age group, sex, education level, and area of residence, for:

- **Bread alone** (standalone bread items) and **Bread all sources** (including bread in mixed dishes)
- **Total bread**, **wholegrain bread**, and **refined bread**
- **Non-energy-adjusted** and **energy-adjusted** (residual method, standardised to 2000 kcal)

Output matches the GBI Aggregate Data Form template structure.

## Data requirements

The script expects the following files in a `CURF/` subdirectory (not included — requires ABS CURF access):

| File | Description |
|------|-------------|
| `npa11bf.csv` | Food-level records (341,897 rows) |
| `npa11bp.csv` | Person-level records (12,153 rows) |
| `npa11ehh.csv` | Household-level records (9,519 rows) |
| `ADG_Database.xlsx` | Australian Dietary Guidelines food composition database |

Also requires `GBI_Aggregate_Data_Form.xlsx` (the GBI template) in the working directory.

## Usage

```r
source("GBI_NNPAS_bread_estimates.R")
```

Outputs:
- `GBI_DataTemplate_NNPAS_2011-12.csv` — long-format estimates
- `GBI_AggregateDataForm_NNPAS_2011-12.xlsx` — filled GBI Excel template

## R packages

```r
install.packages(c("tidyverse", "readxl", "survey", "srvyr", "openxlsx"))
```

## Key analytical decisions

- **Day 1 only** for point estimates (means, % non-consumers, energy); Day 2 used only for within-person variance estimation (SD correction via ANOVA)
- **Urban** = Major cities + Inner regional (ARIABC 1-2); **Rural** = Other (ARIABC 3)
- **Education for children <15**: assigned from the highest-educated adult in the household
- **Bread alone range**: AUSNUT 2011-13 codes 12201001–12307004 (inclusive of all standalone bread products)
- **SE**: design-based via jackknife replication (60 replicate weights)
- **SD**: corrected for within-person variation using between-person variance ratio from 2-day ANOVA
