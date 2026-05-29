# =============================================================================
# GBI Bread Intake Shiny App
# =============================================================================
# Visualises the long-format aggregate estimates produced by
# GBI_bread_estimates_NNPAS_2011_12.R and GBI_bread_estimates_NNPAS_2023.R.
#
# Loads:
#   GBI_DataTemplate_NNPAS_2011-12.csv
#   GBI_DataTemplate_NNPAS_2023.csv
# Aesthetic conventions are sourced from the NNPAS2023 repo:
#   - shinytheme("darkly")
#   - custom_styles() (dark-theme DT styling)
#   - apply_font_styles() + hc_theme_economist() + abscol palette for plots
# =============================================================================

library(tidyverse)
library(highcharter)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)

setwd("C:/Users/atyeo/OneDrive/R data/GBI")

options(warn = -1)

# Pull custom styles, apply_font_styles helper, and the abscol palette
source("https://raw.githubusercontent.com/Atyepa/NNPAS2023/main/custom_styles.R")

# =============================================================================
# Data load and labelling
# =============================================================================
csv_paths <- c(
  "2011-12" = "GBI_DataTemplate_NNPAS_2011-12.csv",
  "2023"    = "GBI_DataTemplate_NNPAS_2023.csv"
)

missing <- csv_paths[!file.exists(csv_paths)]
if (length(missing) > 0) {
  stop("Missing input CSV(s): ", paste(missing, collapse = ", "),
       ". Run the GBI estimator scripts first.")
}

raw <- imap_dfr(csv_paths, ~ read_csv(.x, show_col_types = FALSE) %>%
                  mutate(wave = .y))

# Label maps
sex_lbl     <- c(`0` = "Persons", `1` = "Males", `2` = "Females")
res_lbl     <- c(`1` = "Urban",   `2` = "Rural")
edu_lbl     <- c(`1` = "Primary", `2` = "Secondary", `3` = "Tertiary")
age_lbl     <- c(`0` = "All ages", `1` = "2-5", `2` = "6-10", `3` = "11-14",
                 `4` = "15-19", `5` = "20-24", `6` = "25-34", `7` = "35-44",
                 `8` = "45-54", `9` = "55-64", `10` = "65-74",
                 `11` = "75-84", `12` = "85+")
def_lbl     <- c(`1` = "Bread alone", `2` = "Bread all sources")
subtype_lbl <- c(`0` = "Total bread", `1` = "Wholegrain", `2` = "Refined")
ea_lbl      <- c(`1` = "Non-adjusted", `2` = "Energy-adjusted")

age_order <- age_lbl  # already in display order

dat <- raw %>%
  mutate(
    wave          = factor(wave, levels = c("2011-12", "2023")),
    sex_label     = factor(sex_lbl[as.character(sex)],         levels = sex_lbl),
    res_label     = factor(res_lbl[as.character(residence)],   levels = res_lbl),
    edu_label     = factor(edu_lbl[as.character(edu_level)],   levels = edu_lbl),
    age_label     = factor(age_lbl[as.character(age_grp)],     levels = age_lbl),
    def_label     = factor(def_lbl[as.character(bread_def)],   levels = def_lbl),
    subtype_label = factor(subtype_lbl[as.character(bread_subtype)],
                           levels = subtype_lbl),
    ea_label      = factor(ea_lbl[as.character(energy_adj)],   levels = ea_lbl),
    # Single-level factor used when the user picks the "Overall" X-axis
    # dimension. All rows share the same level so the grand-total marginal
    # row (age=All, sex=Persons, res=NA, edu=NA) collapses to one bar.
    overall_label = factor("All persons"),
    lowerCI       = mean_g - 1.96 * se_g,
    upperCI       = mean_g + 1.96 * se_g
  )

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  theme = shinytheme("darkly"),
  custom_styles(),
  headerPanel("GBI Bread Intake — NNPAS 2011-12 & 2023"),

  sidebarPanel(
    width = 3,

    pickerInput("wave", "Survey wave:",
                choices  = levels(dat$wave),
                selected = levels(dat$wave),
                multiple = TRUE,
                options  = list(`actions-box` = TRUE), width = "260px"),

    radioButtons("bread_def", "Bread definition:",
                 choices  = setNames(names(def_lbl), def_lbl),
                 selected = "1", inline = FALSE),

    radioButtons("bread_subtype", "Bread subtype:",
                 choices  = setNames(names(subtype_lbl), subtype_lbl),
                 selected = "0", inline = TRUE),
    helpText(em("To compare all three subtypes, set 'Group by' to 'Bread subtype'.")),

    radioButtons("energy_adj", "Energy adjustment:",
                 choices  = setNames(names(ea_lbl), ea_lbl),
                 selected = "1", inline = TRUE),

    selectInput("xvar", "X-axis dimension:",
                choices  = c("Overall (no breakdown)" = "overall_label",
                             "Age group"  = "age_label",
                             "Sex"        = "sex_label",
                             "Residence"  = "res_label",
                             "Education"  = "edu_label"),
                selected = "age_label", width = "260px"),

    selectInput("groupvar", "Group by (series):",
                choices  = c("(none)"        = "_none",
                             "Survey wave"   = "wave",
                             "Bread subtype" = "subtype_label",
                             "Sex"           = "sex_label"),
                selected = "wave", width = "260px"),

    selectInput("metric", "Metric:",
                choices  = c("Mean (g/day)"        = "mean_g",
                             "% non-consumers"     = "pct_non_consumers",
                             "Mean energy (kcal)"  = "mean_kcal",
                             "SD (g/day)"          = "sd_g",
                             "Sample size (n)"     = "n"),
                selected = "mean_g", width = "260px"),

    checkboxInput("show_ci", "Show 95% CI (mean ± 1.96·SE)", TRUE),

    radioButtons("orient", "Chart type:",
                 choices  = c("Column" = "column", "Bar" = "bar",
                              "Line"   = "line"),
                 selected = "column", inline = TRUE),

    helpText(em("Tip: error bars are only drawn for the mean (g/day) metric. ",
                "When X-axis is Residence or Education, estimates are weighted ",
                "averages over the other demographic dimensions."))
  ),

  mainPanel(
    width = 9,
    tabsetPanel(
      tabPanel("Plot",  highchartOutput("hc",  height = "720px")),
      tabPanel("Data",  DTOutput("tbl")),
      tabPanel("About",
               br(),
               p("Visualises GBI bread-intake estimates derived from the ABS NNPAS",
                 "2011-12 and 2023 surveys. Estimates are population-weighted",
                 "(Day-1 person weight) with design-based standard errors from",
                 "60 jackknife replicate weights."),
               p("Source code and data preparation:",
                 a("github.com/Atyepa/GBI",
                   href = "https://github.com/Atyepa/GBI",
                   target = "_blank")),
               p("Aesthetic conventions sourced from",
                 a("github.com/Atyepa/NNPAS2023",
                   href = "https://github.com/Atyepa/NNPAS2023",
                   target = "_blank"), "."))
    )
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # ---- Filter to the relevant slice of the data ---------------------------
  filtered <- reactive({
    req(input$wave, input$bread_subtype)

    # If user is grouping by Bread subtype we want all 3 subtypes as series,
    # so we ignore the radio selection in that case. Otherwise we filter to
    # the single chosen subtype.
    subtypes_to_keep <- if (input$groupvar == "subtype_label") {
      c(0L, 1L, 2L)
    } else {
      as.integer(input$bread_subtype)
    }

    # Wave can only be multi-selected if Group by = Wave; otherwise the
    # display would have ambiguous duplicate rows per X cell.
    validate(
      need(length(input$wave) == 1 || input$groupvar == "wave",
           paste("Multiple survey waves selected. Either pick one,",
                 "or set 'Group by' to 'Survey wave'."))
    )

    d <- dat %>%
      filter(wave %in% input$wave,
             bread_def     == as.integer(input$bread_def),
             bread_subtype %in% subtypes_to_keep,
             energy_adj    == as.integer(input$energy_adj))

    demo_dims  <- c("age_label", "sex_label", "res_label", "edu_label")
    active_dims <- c(input$xvar,
                     if (input$groupvar != "_none") input$groupvar)
    inactive_demos <- setdiff(demo_dims, active_dims)

    # If X or Group involves residence/education, the CSV has no marginals
    # for those dims, so we must use fully-stratified rows and aggregate
    # across inactive demo dims using n as weights.
    use_fs <- any(c("res_label", "edu_label") %in% active_dims)

    if (!use_fs) {
      # Marginal rows: residence + education are NA; sex/age have specific
      # marginal levels we filter to or away from.
      d <- d %>% filter(is.na(res_label), is.na(edu_label))
      if ("age_label" %in% inactive_demos)
        d <- d %>% filter(age_label == "All ages")
      else
        d <- d %>% filter(age_label != "All ages")
      if ("sex_label" %in% inactive_demos)
        d <- d %>% filter(sex_label == "Persons")
      else
        d <- d %>% filter(sex_label != "Persons")
    } else {
      # Fully-stratified rows: residence and education non-NA, age and sex
      # are specific bands.
      d <- d %>%
        filter(!is.na(res_label), !is.na(edu_label),
               age_label != "All ages", sex_label != "Persons")

      keep_dims <- unique(c("wave", "def_label", "subtype_label", "ea_label",
                            active_dims))
      keep_dims <- intersect(keep_dims, names(d))

      d <- d %>%
        group_by(across(all_of(keep_dims))) %>%
        summarise(
          mean_g            = sum(mean_g    * n, na.rm = TRUE) / sum(n, na.rm = TRUE),
          sd_g              = sum(sd_g      * n, na.rm = TRUE) / sum(n, na.rm = TRUE),
          se_g              = sqrt(sum((se_g * n)^2, na.rm = TRUE)) / sum(n, na.rm = TRUE),
          pct_non_consumers = sum(pct_non_consumers * n, na.rm = TRUE) / sum(n, na.rm = TRUE),
          mean_kcal         = sum(mean_kcal * n, na.rm = TRUE) / sum(n, na.rm = TRUE),
          n                 = sum(n, na.rm = TRUE),
          .groups           = "drop"
        ) %>%
        mutate(
          lowerCI = mean_g - 1.96 * se_g,
          upperCI = mean_g + 1.96 * se_g
        )
    }

    # Ensure all label columns exist (fill NA where aggregated away)
    for (col in c("age_label", "sex_label", "res_label", "edu_label",
                  "def_label", "subtype_label", "ea_label")) {
      if (!col %in% names(d)) d[[col]] <- NA
    }
    d
  })

  # ---- Plot ---------------------------------------------------------------
  output$hc <- renderHighchart({
    d <- filtered()
    validate(need(nrow(d) > 0,
                  "No rows match the current filters. Loosen the selections."))

    metric_lbl <- c(mean_g            = "Mean intake (g/day)",
                    pct_non_consumers = "% non-consumers",
                    mean_kcal         = "Mean energy (kcal/day)",
                    sd_g              = "Within-stratum SD (g/day)",
                    n                 = "Sample size (n)")[[input$metric]]

    suffix <- c(mean_g = "g", pct_non_consumers = "%", mean_kcal = "kcal",
                sd_g = "g", n = "")[[input$metric]]

    show_err <- isTRUE(input$show_ci) && input$metric == "mean_g"

    grp_sym <- if (input$groupvar == "_none") NULL else sym(input$groupvar)
    x_sym   <- sym(input$xvar)

    plot_df <- d %>%
      mutate(val = .data[[input$metric]]) %>%
      select(any_of(c("wave", input$xvar, input$groupvar)), val,
             lowerCI, upperCI, n)

    # X-axis categories in display order
    cats <- levels(droplevels(d[[input$xvar]]))

    # Build base chart
    title_bits <- c(
      def_lbl[input$bread_def],
      paste(subtype_lbl[input$bread_subtype], collapse = " / "),
      ea_lbl[input$energy_adj]
    )
    title_text <- paste(title_bits, collapse = " — ")

    hc <- highchart() %>%
      hc_chart(type = input$orient) %>%
      hc_xAxis(categories = cats,
               title = list(text = c(overall_label = "",
                                     age_label = "Age group",
                                     sex_label = "Sex",
                                     res_label = "Residence",
                                     edu_label = "Education")[[input$xvar]]),
               # Highcharts truncates tick labels to "A" when there is only
               # one category, because the tick-width heuristic returns ~0
               # with no neighbouring tick to measure against. The fix layers
               # three things:
               #   (a) padding around the axis range so the tick has visual
               #       width to render into,
               #   (b) useHTML = TRUE so the label is rendered via a real
               #       HTML span rather than an SVG <text> subject to the
               #       theme's text-overflow style, and
               #   (c) overflow + textOverflow defaults disabled.
               minPadding = if (length(cats) == 1) 0.5 else 0,
               maxPadding = if (length(cats) == 1) 0.5 else 0,
               labels = list(
                 overflow = "allow",
                 useHTML  = TRUE,
                 style    = list(textOverflow = "none",
                                 whiteSpace   = "nowrap")
               )) %>%
      hc_yAxis(title = list(text = metric_lbl)) %>%
      hc_title(text = title_text) %>%
      hc_subtitle(text = "Source: ABS NNPAS (CURF 2011-12; DataLab 2023)") %>%
      hc_add_theme(hc_theme_economist()) %>%
      hc_colors(abscol) %>%
      hc_tooltip(crosshairs = TRUE,
                 valueSuffix = if (nzchar(suffix)) paste0(" ", suffix) else "",
                 shared = FALSE) %>%
      apply_font_styles()

    # Build series
    if (is.null(grp_sym)) {
      vals <- tibble(.cat = as.character(d[[input$xvar]]),
                     y    = as.numeric(d[[input$metric]]),
                     low  = as.numeric(d$lowerCI),
                     high = as.numeric(d$upperCI)) %>%
        filter(!is.na(y)) %>%
        arrange(match(.cat, cats))

      hc <- hc %>%
        hc_add_series(
          name = metric_lbl,
          data = list_parse2(tibble(name = vals$.cat, y = vals$y))
        )
      if (show_err) {
        err <- vals %>% filter(!is.na(low), !is.na(high)) %>%
          transmute(name = .cat, low = low, high = high)
        hc <- hc %>%
          hc_add_series(type = "errorbar", name = "95% CI",
                        data = list_parse2(err),
                        tooltip = list(pointFormat =
                                         "95% CI: {point.low:.1f}–{point.high:.1f}"),
                        showInLegend = FALSE)
      }
    } else {
      groups <- levels(droplevels(d[[input$groupvar]]))
      n_g    <- length(groups)
      is_bar_col <- input$orient %in% c("column", "bar")

      # For column/bar with >1 series, use manual x positioning so error
      # bars sit on each series column rather than at the category centre.
      use_manual <- is_bar_col && n_g > 1 && show_err
      if (use_manual) {
        inner_pad <- 0.2
        span      <- 1 - 2 * inner_pad
        offsets   <- seq(from = -span/2 + span/(2*n_g),
                         to   =  span/2 - span/(2*n_g),
                         length.out = n_g)
        cat_idx   <- setNames(seq_along(cats) - 1, cats)
      }

      for (i in seq_along(groups)) {
        g   <- groups[[i]]
        sub <- d %>% filter(.data[[input$groupvar]] == g) %>%
          arrange(match(.data[[input$xvar]], cats))
        if (nrow(sub) == 0) next

        if (use_manual) {
          off <- offsets[[i]]
          sub <- sub %>%
            mutate(
              .cat = as.character(.data[[input$xvar]]),
              .i   = unname(cat_idx[.cat]),
              x    = .i + off,
              y    = as.numeric(.data[[input$metric]])
            ) %>%
            filter(!is.na(y))
          if (nrow(sub) == 0) next

          ser <- sub %>%
            transmute(x = x, y = y, name = .cat) %>%
            list_parse2()

          hc <- hc %>%
            hc_add_series(
              name         = as.character(g),
              type         = input$orient,
              data         = ser,
              grouping     = FALSE,           # manual layout
              pointRange   = span / max(n_g, 1),
              pointPadding = 0
            )

          err <- sub %>%
            filter(!is.na(lowerCI), !is.na(upperCI)) %>%
            transmute(x = x, low = lowerCI, high = upperCI)
          if (nrow(err) > 0) {
            hc <- hc %>%
              hc_add_series(
                type         = "errorbar",
                linkedTo     = "previous",
                name         = paste(g, "95% CI"),
                data         = list_parse2(err),
                grouping     = FALSE,
                pointRange   = span / max(n_g, 1),
                tooltip      = list(pointFormat =
                  "95% CI: {point.low:.1f}–{point.high:.1f}"),
                showInLegend = FALSE,
                zIndex       = 6
              )
          }

        } else {
          # Single series, or line chart, or CI off: standard name-based
          # positioning is fine.
          ser <- tibble(
            name = as.character(sub[[input$xvar]]),
            y    = as.numeric(sub[[input$metric]])
          ) %>% filter(!is.na(y))
          if (nrow(ser) == 0) next

          hc <- hc %>%
            hc_add_series(name = as.character(g),
                          data = list_parse2(ser))

          if (show_err) {
            err <- sub %>% filter(!is.na(lowerCI), !is.na(upperCI)) %>%
              transmute(name = as.character(.data[[input$xvar]]),
                        low = lowerCI, high = upperCI)
            if (nrow(err) > 0) {
              hc <- hc %>%
                hc_add_series(
                  type = "errorbar", linkedTo = "previous",
                  name = paste(g, "95% CI"),
                  data = list_parse2(err),
                  tooltip = list(pointFormat =
                    "95% CI: {point.low:.1f}–{point.high:.1f}"),
                  showInLegend = FALSE)
            }
          }
        }
      }
    }

    hc
  })

  # ---- Data table ---------------------------------------------------------
  output$tbl <- renderDT({
    d <- filtered() %>%
      transmute(
        Wave         = wave,
        `Bread def`  = def_label,
        Subtype      = subtype_label,
        `Energy adj` = ea_label,
        Sex          = sex_label,
        Residence    = res_label,
        Education    = edu_label,
        `Age group`  = age_label,
        n,
        `Mean (g)`   = round(mean_g, 1),
        `SD (g)`     = round(sd_g, 1),
        `SE (g)`     = round(se_g, 2),
        `% non-cons` = round(pct_non_consumers, 1),
        `Mean kcal`  = round(mean_kcal, 1)
      )
    datatable(d, rownames = FALSE,
              extensions = "Buttons",
              options = list(pageLength = 25, scrollX = TRUE,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv", "excel")))
  })
}

shinyApp(ui, server)


shiny::runApp("app.R", launch.browser = TRUE)
