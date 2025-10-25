# inst/shiny/app.R
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)

# ---- Data ----
data("bhai_summary", package = utils::packageName())
data("bhai_rates",   package = utils::packageName())

# ---- Constants ----
HAI_LEVELS <- c("HAP","UTI","BSI","SSI","CDI")

# Custom bar colors mapped to HAI types
HAI_COLORS <- c(
  HAP = "#a3a380",
  UTI = "#d6ce93",
  BSI = "#efebce",
  SSI = "#d8a48f",
  CDI = "#bb8588"
)

METRICS <- c("HAIs" = "cases", "Deaths" = "deaths", "DALYs" = "dalys")

ui <- tagList(
  
  navbarPage(
    title = "HAI Burden Explorer (BHAI) - Germany (German PPS)",
    theme = bs_theme(version = 5, bootswatch = "flatly", primary   = "#283618"),
    tabPanel(
      "Explore",
      sidebarLayout(
        sidebarPanel(
          width = 3, 
          selectInput(
            "view", "View:",
            choices = c("Bubble (per HAI)" = "bubble",
                        "Bar (per HAI)"    = "bar",
                        "Geo comparison (per N)" = "compare"),
            selected = "bubble"
          ),
          conditionalPanel(
            condition = "input.view == 'bubble' || input.view == 'bar'",
            checkboxGroupInput("hai", "HAI types:",
                               choices = HAI_LEVELS, selected = HAI_LEVELS)
          ),
          conditionalPanel(
            condition = "input.view == 'bar'",
            radioButtons("metric", "Metric:", choices = names(METRICS), selected = "DALYs")
          ),
          conditionalPanel(
            condition = "input.view == 'compare'",
            radioButtons("cmp_metric", "Metric (per N):",
                         choices = c("HAIs", "Deaths", "DALYs"), selected = "Deaths")
          ),
          conditionalPanel(
            condition = "input.view == 'compare'",
            sliderInput("perN", "Scale (per N population):",
                        min = 1000, max = 100000, value = 100000, step = 1000)
          )
        ),
        mainPanel(
          tags$h4("Germany - German PPS"),
          div(
            style = "border:1px solid #cfcfcf; border-radius:8px; padding:8px; background:#fff;",
            plotlyOutput("plot", height = 420)
          ),
          tags$hr(),
          tags$h5(tags$b("How to interpret")),
          tags$ul(
            tags$li(tags$b("Bubble (per HAI): "), "x-axis = annual HAIs (cases), y-axis = attributable deaths; bubble size encodes DALYs. Labels mark HAI types. Hover to see 95% UI for cases, deaths, and DALYs."),
            tags$li(tags$b("Bar (per HAI): "), "shows Germany totals by chosen metric (HAIs, Deaths, DALYs). Error bars are 95% uncertainty intervals; bars are colour-coded by HAI type. Use this when you want clean comparisons within Germany."),
            tags$li(tags$b("Geo comparison (per N): "), "compares ", tags$i("German PPS"), " vs ", tags$i("ECDC PPS (EU/EEA)"), " for the selected metric per N people (default N = 100,000). The slider rescales rates; error bars show 95% UI. If intervals overlap strongly, apparent differences may be non-informative."),
            tags$li(tags$b("Reading uncertainty: "), "wide intervals indicate limited precision (e.g., smaller sample size or rarer events). Focus on magnitudes and overlap rather than tiny differences in bar heights."),
            tags$li(tags$b("Context clues: "), "UTI tends to have large case counts with moderate DALYs; BSI has fewer cases but higher fatality burden; HAP often contributes substantially to DALYs.")
          )
        )
      )
    ),
    tabPanel(
      "About",
      tagList(
        tags$h4("Method workflow (BHAI)"),
        tags$ol(
          tags$li("Estimate hospital prevalence from PPS."),
          tags$li("Convert prevalence to incidence (modified Rhame-Sudderth; Grenander/mean for LOI)."),
          tags$li("Extrapolate incidence per patient to population using discharges."),
          tags$li("Stratify by age/sex; adjust remaining life expectancy via McCabe categories."),
          tags$li("Apply outcome trees to compute YLL, YLD, and DALYs.")
        ),
        tags$hr(),
        tags$h4("Data description"),
        tags$p("The analysis draws on estimates produced with the Burden of Healthcare-Associated Infections (BHAI) workflow."),
        tags$p(tags$b("Data sources: "), "the 2011 German point-prevalence survey (PPS) and the 2011-2012 EU/EEA PPS."),
        tags$p(
          tags$b("Download: "),
          "Article is available from ",
          tags$a(href="https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2019.24.46.1900135#html_fulltext",
                 "Eurosurveillance", target="_blank", rel="noopener noreferrer"),
          "; datasets and supplementary materials are available there. ",
          "The datasets plus analysis code used in the paper are bundled with the open-source BHAI R package on CRAN (",
          tags$a(href="https://CRAN.R-project.org/package=BHAI",
                 "https://CRAN.R-project.org/package=BHAI",
                 target="_blank", rel="noopener noreferrer"),
          "), enabling full reproduction of the Germany and EU/EEA estimates."
        ),
        tags$p(tags$b("HAI types included:")),
        tags$ul(
          tags$li(tags$b("HAP"), " - healthcare-associated pneumonia"),
          tags$li(tags$b("BSI"), " - primary bloodstream infection"),
          tags$li(tags$b("UTI"), " - urinary tract infection"),
          tags$li(tags$b("SSI"), " - surgical-site infection"),
          tags$li(tags$b("CDI"), " - ", tags$i("Clostridioides difficile"), " infection")
        )
      )
    ),
    # load custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    ),
  )
)

server <- function(input, output, session) {
  
  # ---- Base subsets for HAI-level views  ----
  base_all_hai <- reactive({
    bhai_summary %>% filter(geo == "Germany", sample == "German PPS")
  })
  
  dat_filtered <- reactive({
    base_all_hai() %>% filter(hai %in% input$hai)
  })
  
  global_xmax <- reactive(max(base_all_hai()$cases,  na.rm = TRUE))
  global_ymax <- reactive(max(base_all_hai()$deaths, na.rm = TRUE))
  global_dalys_max <- reactive(max(base_all_hai()$dalys, na.rm = TRUE))
  
  output$plot <- renderPlotly({
    
    # ---- Geo comparison (Germany vs EU/EEA) ----
    if (input$view == "compare") {
      cmp_metric <- req(input$cmp_metric)
      perN <- req(input$perN)
      
      rates <- bhai_rates %>%
        filter(sample %in% c("German PPS", "ECDC PPS (EU/EEA)"),
               hai %in% HAI_LEVELS,
               metric == cmp_metric) %>%
        mutate(
          perN = perN,
          scale_fac = perN / 100000,
          perN_val  = per100k      * scale_fac,
          perN_low  = per100k_low  * scale_fac,
          perN_high = per100k_high * scale_fac,
          hai = factor(hai, levels = HAI_LEVELS),
          sample = factor(sample, levels = c("German PPS", "ECDC PPS (EU/EEA)"))
        )
      
      validate(need(nrow(rates) > 0, "No data available for geo comparison."))
      dodge <- position_dodge(width = 0.7)
      
      bar_compare <- ggplot(
        rates,
        aes(
          x = hai, y = perN_val, fill = sample,
          text = paste0(
            "Sample: ", sample,
            "<br>HAI: ", hai,
            "<br>", cmp_metric, " per ", scales::comma(perN), ": ",
            scales::number(perN_val, accuracy = 0.1),
            "<br>95% UI: [", scales::number(perN_low, accuracy = 0.1), ", ",
            scales::number(perN_high, accuracy = 0.1), "]"
          )
        )
      ) +
        geom_col(position = dodge, width = 0.6) +
        # 95% UI error bars 
        geom_errorbar(aes(ymin = perN_low, ymax = perN_high),
                      position = dodge, width = 0.2, linewidth = 0.4) +
        labs(
          x = "HAI type",
          y = paste0(cmp_metric, " per ", scales::comma(perN)),
          title = paste0("Germany vs EU/EEA - ", cmp_metric, " per ",
                         scales::comma(perN))
        ) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        scale_fill_manual(values = c("German PPS" = "#a3a380",
                                     "ECDC PPS (EU/EEA)" = "#bb8588"))
      
      return(ggplotly(bar_compare, tooltip = "text"))
    }
    
    # ---- Bubble ----
    df_base <- base_all_hai()
    df <- dat_filtered()
    validate(need(nrow(df) > 0, "Please select at least one HAI type in the sidebar."))
    
    if (input$view == "bubble") {
      bubble_plot <- ggplot(df, aes(x = cases, y = deaths)) +
        geom_point(
          color = "#d8a48f",
          shape = 16,
          aes(size = dalys,
              text = paste0(
                "HAI: ", hai,
                "<br>HAIs: ", scales::comma(cases),
                " [", scales::comma(cases_low), ", ", scales::comma(cases_high), "]",
                "<br>Deaths: ", scales::comma(deaths),
                " [", scales::comma(deaths_low), ", ", scales::comma(deaths_high), "]",
                "<br>DALYs: ", scales::comma(dalys),
                " [", scales::comma(dalys_low), ", ", scales::comma(dalys_high), "]"
              )),
          alpha = 0.65
        ) +
        geom_text(aes(label = hai), vjust = -0.8, size = 3) +
        scale_size_area(limits = c(0, global_dalys_max()), max_size = 18, guide = "legend") +
        scale_x_continuous(limits = c(0, global_xmax() * 1.15)) +
        scale_y_continuous(limits = c(0, global_ymax() * 1.25)) +
        labs(x = "HAIs (annual)", y = "Attributable deaths (annual)", size = "DALYs") +
        theme_minimal()
      
      return(ggplotly(bubble_plot, tooltip = "text"))
    }
    
    # ---- HAI-level Bar  ----
    metric <- req(input$metric)
    metric_col <- METRICS[[metric]]        
    low_col  <- paste0(metric_col, "_low") 
    high_col <- paste0(metric_col, "_high")
    
    order <- HAI_LEVELS[HAI_LEVELS %in% df$hai]
    df <- df %>% mutate(hai = factor(hai, levels = order))
    
    # Use the CI high bound to set a comfortable axis limit
    y_max <- max(df[[metric_col]], df[[high_col]], na.rm = TRUE) * 1.10
    
    bar_plot <- ggplot(
      df,
      aes(
        x = hai, y = .data[[metric_col]], fill = hai,
        text = paste0(
          "HAI: ", hai,
          "<br>", metric, ": ", scales::comma(.data[[metric_col]]),
          "<br>95% UI: [", scales::comma(.data[[low_col]]), ", ",
          scales::comma(.data[[high_col]]), "]"
        )
      )
    ) +
      geom_col(width = 0.7, show.legend = FALSE) +
      # 95% UI error bars
      geom_errorbar(
        aes(ymin = .data[[low_col]], ymax = .data[[high_col]]),
        width = 0.2, linewidth = 0.4
      ) +
      scale_fill_manual(values = HAI_COLORS[levels(df$hai)]) +
      scale_y_continuous(limits = c(0, y_max)) +
      coord_flip() +
      labs(x = "HAI type", y = metric) +
      theme_minimal()
    
    return(ggplotly(bar_plot, tooltip = "text") %>%
                              layout(showlegend = FALSE))
  })
}

shinyApp(ui, server)