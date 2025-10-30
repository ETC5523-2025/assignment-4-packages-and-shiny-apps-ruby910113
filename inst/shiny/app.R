# inst/shiny/app.R
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(bslib)
library(scales)

requireNamespace("BHAIBYE", quietly = TRUE)

# ---- Data (loaded from package) ---------------------------------------------
data("bhai_summary",   package = "BHAIBYE")
data("bhai_rates",     package = "BHAIBYE")
data("bhai_cases_de",  package = "BHAIBYE")
data("bhai_cases_eu",  package = "BHAIBYE")

# try() avoids crashing if microdata are missing
invisible(suppressWarnings(try(utils::data("bhai_cases_de", package = "BHAIBYE"), silent = TRUE)))
invisible(suppressWarnings(try(utils::data("bhai_cases_eu", package = "BHAIBYE"), silent = TRUE)))
has_cases_de <- exists("bhai_cases_de", inherits = TRUE)
has_cases_eu <- exists("bhai_cases_eu", inherits = TRUE)

# ---- Constants ---------------------------------------------------------------
HAI_LEVELS <- c("HAP","UTI","BSI","SSI","CDI")
AGE_LEVELS <- c("0-1","2-4","5-9","10-14","15-19","20-24","25-34","35-44",
                "45-54","55-64","65-74","75-79","80-84","85+")
SAMPLE_LEVELS <- c("German PPS","ECDC PPS (EU/EEA)")
SAMPLE_COLORS <- c("German PPS"="#a3a380", "ECDC PPS (EU/EEA)"="#bb8588")
METRICS <- c("HAIs" = "cases", "Deaths" = "deaths", "DALYs" = "dalys")

# ----------------------------- UI -------------------------------------------
ui <- tagList(
  navbarPage(
    title = "HAI Burden Explorer (BHAI)",
    theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#283618"),
    
    tabPanel(
      "Explore",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            "view", "View:",
            choices = c("Bubble (per HAI)" = "bubble",
                        "Bar (per HAI)"    = "bar",
                        "Age pyramid (DALYs, by age & sex)" = "age",
                        "Geo comparison (per N)" = "compare"),
            selected = "bubble"
          ),
          conditionalPanel(
            condition = "input.view == 'bubble' || input.view == 'bar'",
            checkboxGroupInput(
              "hai", "HAI types:",
              choices = HAI_LEVELS,
              selected = HAI_LEVELS
            )
          ),
          conditionalPanel(
            condition = "input.view == 'bubble' || input.view == 'bar'",
            checkboxGroupInput(
              "samples", "Sample(s):",
              choices = SAMPLE_LEVELS,
              selected = "German PPS"
            )
          ),
          conditionalPanel(
            condition = "input.view == 'bar'",
            radioButtons("metric", "Metric:",
                         choices = names(METRICS),
                         selected = "DALYs")
          ),
          conditionalPanel(
            condition = "input.view == 'age'",
            radioButtons("pyr_geo", "Sample:",
                         choices = c("German PPS" = "DE", "ECDC PPS (EU/EEA)" = "EU"),
                         selected = "DE")
          ),
          conditionalPanel(
            condition = "input.view == 'compare'",
            radioButtons("cmp_metric", "Metric (per N):",
                         choices = c("HAIs", "Deaths", "DALYs"),
                         selected = "Deaths")
          ),
          conditionalPanel(
            condition = "input.view == 'compare'",
            sliderInput("perN", "Scale (per N population):",
                        min = 1000, max = 100000, value = 100000, step = 1000)
          )
        ),
        mainPanel(
          tags$h4("German / ECDC - HAI"),
          conditionalPanel(
            condition = "input.view == 'bubble' || input.view == 'bar' || input.view == 'compare'",
            div(
              style = "border:1px solid #cfcfcf; border-radius:8px; padding:8px; background:#fff;",
              plotlyOutput("plot", height = 520)
            )
          ),
          conditionalPanel(
            condition = "input.view == 'age'",
            div(
              style = "border:1px solid #cfcfcf; border-radius:8px; padding:8px; background:#fff;",
              plotlyOutput("pyramid", height = 520)
            )
          ),
          conditionalPanel(
            condition = "input.view == 'compare'",
            tags$hr(),
            tags$h5(tags$b("Geo comparison - text summary")),
            htmlOutput("cmp_text")
          )
        )
      )
    ),
    
    tabPanel(
      "About",
      tagList(
        tags$h4("Method workflow (BHAI)"),
        tags$ol(
          tags$li(tags$b("Prevalence in hospital"), " from PPS."),
          tags$li(
            "Convert to ", tags$b("hospital incidence"),
            " via a modified ", tags$b("Rhame-Sudderth"),
            " formula using ", tags$b("length of infection (LOI)"), "."
          ),
          tags$li(
            tags$b("Scale to population"),
            " using national ", tags$b("hospital discharges"),
            " and ", tags$b("population size"), "."
          ),
          tags$li(tags$b("Stratify by age/sex"), " using the PPS distribution."),
          tags$li(
            tags$b("Adjust for comorbidity"),
            " (McCabe classes) and estimate ",
            tags$b("YLL, YLD, and DALYs"), "."
          )
        ),
        tags$hr(),
        tags$h4("Field meanings"),
        tags$ul(
          tags$li(
            code("bhai_summary"), ": Germany totals with 95% uncertainty intervals (UI).",
            tags$ul(
              tags$li(code("geo"), " - geography (always ", code("Germany"), ")."),
              tags$li(code("sample"), " - source/sample (", code("German PPS"), ")."),
              tags$li(code("hai"), " - infection type: ", code("HAP, UTI, BSI, SSI, CDI"), "."),
              tags$li(code("cases"), ", ", code("deaths"), ", ", code("dalys"),
                      " - annual totals (counts; ", code("dalys"), " in years)."),
              tags$li(code("cases_low/high"), ", ", code("deaths_low/high"), ", ", code("dalys_low/high"),
                      " - 95% UI bounds for the totals."),
              tags$li(code("yll"), ", ", code("yld"),
                      " - components of ", code("dalys"), " (identity: ", code("DALY = YLL + YLD"), ")."),
              tags$li(code("yll_low/high"), ", ", code("yld_low/high"),
                      " - 95% UI bounds for YLL/YLD.")
            )
          ),
          tags$li(
            code("bhai_rates"), ": Rates per 100,000 population with 95% UI (Germany & EU/EEA).",
            tags$ul(
              tags$li(code("geo"), " - geography: ", code("Germany"), " or ", code("EU/EEA"), "."),
              tags$li(code("sample"), " - ", code("German PPS"), " or ", code("ECDC PPS (EU/EEA)"), "."),
              tags$li(code("hai"), " - infection type or ", code("All"), " (sum across types)."),
              tags$li(code("metric"), " - outcome: ", code("HAIs"), ", ", code("Deaths"), ", ", code("DALYs"),
                      "; for EU/EEA the dataset also includes derived ", code("YLL"), " and ", code("YLD"), "."),
              tags$li(code("per100k"), " - point estimate of the rate per 100,000 people."),
              tags$li(code("per100k_low/high"), " - 95% UI bounds for the rate."),
              tags$li("Convert rate to an approximate count for population ", code("N"), ": ",
                      code("count ≈ per100k / 1e5 * N"), ".")
            )
          ),
          tags$li(
            code("bhai_cases_de"), " / ", code("bhai_cases_eu"),
            ": simulated person-level microdata for age-sex analyses (weights supplied).",
            tags$ul(
              tags$li(code("hai"), " - infection type (", code("HAP/UTI/BSI/SSI/CDI"), ")."),
              tags$li(code("age_group"), " - ordered bands: ",
                      "0-1, 2-4, 5-9, 10-14, 15-19, 20-24, 25-34, 35-44, 45-54, ",
                      "55-64, 65-74, 75-79, 80-84, 85+."),
              tags$li(code("sex"), " - ", code("Female"), " / ", code("Male"), "."),
              tags$li(code("death"), " - indicator (", code("1"), " = died; ", code("0"), " = survived)."),
              tags$li(code("yll"), ", ", code("yld"), ", ", code("daly"),
                      " - per-case years (", code("daly = yll + yld"), ")."),
              tags$li(code("weight"), " - population weight for each simulated record. ",
                      "Summing ", code("weight"), " by ", code("hai"), " approximates total cases; ",
                      "summing ", code("weight * daly"), " recovers DALYs totals.")
            )
          )
        ),
        tags$hr(),
        tags$h4("How to interpret"),
        tags$ul(
          tags$li(tags$b("Bubble (per HAI): "),
                  "x-axis = HAIs (cases), y-axis = attributable deaths; bubble size = DALYs."),
          tags$li(tags$b("Bar (per HAI): "),
                  "grouped bars by sample for HAIs / Deaths / DALYs."),
          tags$li(tags$b("Age pyramid: "),
                  "mirrored bars of DALYs by age group; female left / male right."),
          tags$li(tags$b("Geo comparison (per N): "),
                  "Germany vs EU/EEA per N people with 95% UI.")
        ),
        tags$hr(),
        tags$h4("Data description"),
        tags$ul(
          tags$li(tags$b("The analysis"), " draws on estimates produced with the ",
                  tags$b("Burden of Healthcare-Associated Infections (BHAI)"), " workflow."),
          tags$li(tags$b("Data sources:"), " the ", tags$b("2011"), " German ",
                  tags$b("point-prevalence survey (PPS)"), " and the ", tags$b("2011-2012"),
                  " EU/EEA PPS."),
          tags$li(tags$b("Download:"), " data and supplementary materials are available from ",
                  tags$a(href="https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2019.24.46.1900135#supplementary_data",
                         target="_blank", "Eurosurveillance"), 
                  ", and the datasets plus analysis code used in the paper are bundled with the open-source ",
                  "BHAI R package on CRAN (", 
                  tags$a(href="https://CRAN.R-project.org/package=BHAI", target="_blank", "CRAN: BHAI"), ")."),
          tags$li(tags$b("HAI types included:"),
                  tags$ul(
                    tags$li(tags$b("HAP"), " - healthcare-associated pneumonia"),
                    tags$li(tags$b("BSI"), " - primary bloodstream infection"),
                    tags$li(tags$b("UTI"), " - urinary tract infection"),
                    tags$li(tags$b("SSI"), " - surgical-site infection"),
                    tags$li(tags$b("CDI"), " - Clostridioides difficile infection")
                  )
          )
        ),
        tags$hr(),
        tags$h4("Limitations"),
        tags$p("The microdata used in this app are ",
               tags$b("simulated"),
               " for teaching/demo purposes. While totals and rates follow the published patterns, ",
               "the authoritative numbers and figures are those reported in ",
               tags$a(href="https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2019.24.46.1900135#html_fulltext",
                      target="_blank", "the Eurosurveillance article"), "."),
        tags$hr(),
        tags$h4("References"),
        tags$ul(
          tags$li(tags$a(href="https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2019.24.46.1900135#html_fulltext",
                         target="_blank",
                         "Zacher et al. (2019). Burden of healthcare-associated infections in Germany and the EU/EEA. Eurosurveillance.")),
          tags$li(tags$a(href="https://CRAN.R-project.org/package=BHAI", target="_blank",
                         "BHAI: Burden of Healthcare-Associated Infections R package (CRAN)."))
        )
      )
    ),
    tags$head(tags$link(rel="stylesheet", type="text/css", href="app.css"))
  )
)

# --------------------------- Server -----------------------------------------
server <- function(input, output, session) {
  
  # Build totals after checking inputs (calls package helper via :::)
  totals_selected <- reactive({
    req(input$view %in% c("bubble", "bar"))
    validate(need(length(input$samples) > 0,
                  "Select at least one sample (German PPS and/or ECDC PPS (EU/EEA))."))
    validate(need(length(input$hai) > 0,
                  "Select at least one HAI type."))
    big_totals <- dplyr::bind_rows(lapply(input$samples, BHAIBYE:::totals_for_sample))
    big_totals %>% dplyr::filter(hai %in% input$hai)
  })
  
  # Prepare per-N compare rates
  cmp_rates <- reactive({
    req(input$view == "compare")
    chosen_metric <- req(input$cmp_metric)
    chosen_perN   <- req(input$perN)
    rate_tbl <- bhai_rates %>%
      dplyr::filter(sample %in% c("German PPS", "ECDC PPS (EU/EEA)"),
                    hai %in% HAI_LEVELS,
                    metric == chosen_metric)
    scale_factor <- chosen_perN / 100000
    rate_tbl %>%
      dplyr::mutate(
        perN = chosen_perN,
        f = scale_factor,
        perN_val = per100k * f,
        perN_low = per100k_low * f,
        perN_high = per100k_high * f,
        hai = factor(hai, levels = HAI_LEVELS),
        sample = factor(sample, levels = SAMPLE_LEVELS)
      )
  })
  
  # Main plot
  output$plot <- renderPlotly({
    # Geo comparison
    if (identical(input$view, "compare")) {
      rate_data <- cmp_rates()
      chosen_metric <- req(input$cmp_metric)
      chosen_perN   <- req(input$perN)
      validate(need(nrow(rate_data) > 0, "No data available for geo comparison."))
      pos_dodge <- position_dodge(width = 0.7)
      tooltip_txt <- paste0(
        "Sample: ", rate_data$sample,
        "<br>HAI: ", rate_data$hai,
        "<br>", chosen_metric, " per ", comma(chosen_perN), ": ", number(rate_data$perN_val, accuracy = 0.1),
        "<br>95% UI: [", number(rate_data$perN_low, accuracy = 0.1), ", ",
        number(rate_data$perN_high, accuracy = 0.1), "]"
      )
      compare_plot <- ggplot(rate_data, aes(x = hai, y = perN_val, fill = sample, text = tooltip_txt)) +
        geom_col(position = pos_dodge, width = 0.6) +
        geom_errorbar(aes(ymin = perN_low, ymax = perN_high),
                      position = pos_dodge, width = 0.2, linewidth = 0.4) +
        labs(
          x = "HAI type",
          y = paste0(chosen_metric, " per ", comma(chosen_perN)),
          title = paste0("Germany vs EU/EEA - ", chosen_metric, " per ", comma(chosen_perN))
        ) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values = SAMPLE_COLORS)
      return(ggplotly(compare_plot, tooltip = "text"))
    }
    
    # Hide main plot when age view
    if (identical(input$view, "age")) return(NULL)
    
    # Bubble chart
    if (identical(input$view, "bubble")) {
      bubble_df <- totals_selected()
      validate(need(nrow(bubble_df) > 0, "No data to plot. Check your selections."))
      max_dalys_for_size <- max(bubble_df$dalys, bubble_df$dalys_high, na.rm = TRUE)
      bubble_tooltip <- paste0(
        "Sample: ", bubble_df$sample,
        "<br>HAI: ", bubble_df$hai,
        "<br>HAIs: ", comma(bubble_df$cases),
        ifelse(is.finite(bubble_df$cases_low),
               paste0(" [", comma(bubble_df$cases_low), ", ", comma(bubble_df$cases_high), "]"), ""),
        "<br>Deaths: ", comma(bubble_df$deaths),
        ifelse(is.finite(bubble_df$deaths_low),
               paste0(" [", comma(bubble_df$deaths_low), ", ", comma(bubble_df$deaths_high), "]"), ""),
        "<br>DALYs: ", comma(bubble_df$dalys),
        ifelse(is.finite(bubble_df$dalys_low),
               paste0(" [", comma(bubble_df$dalys_low), ", ", comma(bubble_df$dalys_high), "]"), "")
      )
      bubble_plot <- ggplot(bubble_df, aes(x = cases, y = deaths)) +
        geom_point(aes(size = dalys, color = sample, shape = sample, text = bubble_tooltip),
                   alpha = 0.7) +
        geom_text(aes(label = hai, color = sample), vjust = -0.8, size = 3, show.legend = FALSE) +
        scale_color_manual(values = SAMPLE_COLORS) +
        scale_shape_manual(values = c(16, 17)) +
        scale_size_area(limits = c(0, max_dalys_for_size), max_size = 18, guide = "legend", name = "DALYs") +
        scale_x_continuous(limits = c(0, 260000)) +
        scale_y_continuous(limits = c(0, 5000)) +
        labs(x = "HAIs (annual)", y = "Attributable deaths (annual)") +
        theme_minimal() +
        theme(legend.position = "none")
      return(ggplotly(bubble_plot, tooltip = "text"))
    }
    
    # Grouped bar chart
    if (identical(input$view, "bar")) {
      bar_df <- totals_selected()
      selected_metric <- req(input$metric)
      metric_col_name <- METRICS[[selected_metric]]
      low_col_name    <- paste0(metric_col_name, "_low")
      high_col_name   <- paste0(metric_col_name, "_high")
      bar_df <- bar_df %>%
        mutate(
          hai    = factor(hai, levels = HAI_LEVELS[HAI_LEVELS %in% levels(bar_df$hai)]),
          sample = factor(sample, levels = SAMPLE_LEVELS)
        )
      y_axis_limit <- max(bar_df[[metric_col_name]], bar_df[[high_col_name]], na.rm = TRUE) * 1.10
      pos_dodge_bar <- position_dodge(width = 0.75)
      bar_tooltip <- paste0(
        "Sample: ", bar_df$sample,
        "<br>HAI: ", bar_df$hai,
        "<br>", selected_metric, ": ", comma(bar_df[[metric_col_name]]),
        ifelse(is.finite(bar_df[[low_col_name]]),
               paste0("<br>95% UI: [", comma(bar_df[[low_col_name]]), ", ",
                      comma(bar_df[[high_col_name]]), "]"), "")
      )
      bar_plot <- ggplot(bar_df, aes(x = hai, y = !!as.name(metric_col_name), fill = sample, text = bar_tooltip)) +
        geom_col(width = 0.7, position = pos_dodge_bar) +
        geom_errorbar(aes(ymin = !!as.name(low_col_name), ymax = !!as.name(high_col_name)),
                      width = 0.2, linewidth = 0.4, position = pos_dodge_bar, na.rm = TRUE) +
        scale_fill_manual(values = SAMPLE_COLORS, name = NULL) +
        scale_y_continuous(limits = c(0, y_axis_limit)) +
        coord_flip() +
        labs(x = "HAI type", y = selected_metric) +
        theme_minimal()
      return(ggplotly(bar_plot, tooltip = "text"))
    }
    NULL
  })
  
  # Age pyramid (use helper from package via :::)
  output$pyramid <- renderPlotly({
    req(input$view == "age")
    validate(need(has_cases_de || has_cases_eu,
                  "Age view needs microdata (bhai_cases_de / bhai_cases_eu)."))
    selected_geo <- req(input$pyr_geo)
    raw_df_for_age <- if (selected_geo == "DE") {
      validate(need(has_cases_de, "No microdata for German PPS."))
      bhai_cases_de
    } else {
      validate(need(has_cases_eu, "No microdata for ECDC PPS (EU/EEA)."))
      bhai_cases_eu
    }
    pyramid_ready <- BHAIBYE:::pyramid_df(raw_df_for_age) %>%
      mutate(
        value_signed = ifelse(sex == "Female", -value, value),
        age_group    = forcats::fct_rev(age_group)
      )
    age_x_limit <- max(abs(pyramid_ready$value_signed), na.rm = TRUE) * 1.05
    age_tip <- paste0(
      "Sex: ", pyramid_ready$sex,
      "<br>Age: ", pyramid_ready$age_group,
      "<br>DALYs: ", comma(abs(pyramid_ready$value_signed))
    )
    age_plot <- ggplot(pyramid_ready, aes(x = age_group, y = value_signed, fill = sex, text = age_tip)) +
      geom_col(width = 0.75) +
      coord_flip() +
      scale_y_continuous(limits = c(-age_x_limit, age_x_limit),
                         labels = function(x) comma(abs(x))) +
      scale_fill_manual(values = c("Female" = "#bb8588", "Male" = "#a3a380"), name = NULL) +
      labs(x = "Age (years)", y = "DALYs (weighted totals)",
           title = "Age-sex DALY pyramid (totals)") +
      theme_minimal()
    ggplotly(age_plot, tooltip = "text")
  })
  
  # Compare text summary
  output$cmp_text <- renderUI({
    req(input$view == "compare")
    rate_view <- cmp_rates()
    chosen_perN <- req(input$perN)
    chosen_metric <- req(input$cmp_metric)
    wide_tbl <- rate_view %>%
      select(hai, sample, perN_val) %>%
      pivot_wider(names_from = sample, values_from = perN_val) %>%
      mutate(
        de = `German PPS`,
        eu = `ECDC PPS (EU/EEA)`,
        winner = case_when(
          is.na(de) | is.na(eu) ~ NA_character_,
          de > eu ~ "German PPS",
          eu > de ~ "ECDC PPS (EU/EEA)",
          TRUE    ~ "Tie"
        ),
        diff = abs(de - eu)
      )
    bullet_items <- lapply(seq_len(nrow(wide_tbl)), function(i) {
      hai_i <- as.character(wide_tbl$hai[i])
      de_i  <- wide_tbl$de[i]
      eu_i  <- wide_tbl$eu[i]
      win_i <- wide_tbl$winner[i]
      dif_i <- wide_tbl$diff[i]
      tail_note <- if (is.na(win_i)) {
        NULL
      } else if (win_i == "Tie") {
        " (tie)."
      } else {
        list(" (", tags$b(win_i), " higher by ",
             tags$b(number(dif_i, accuracy = 0.1)), ").")
      }
      tags$li(
        tags$b(hai_i), ": German PPS ",
        tags$b(number(de_i, accuracy = 0.1)), " / ",
        "ECDC PPS ", tags$b(number(eu_i, accuracy = 0.1)),
        " per ", tags$b(comma(chosen_perN)), tail_note
      )
    })
    tags$div(
      tags$p("Metric: ", tags$b(chosen_metric), " · Scale: per ", tags$b(comma(chosen_perN))),
      tags$ul(bullet_items)
    )
  })
}

# Run app 
shinyApp(ui, server)