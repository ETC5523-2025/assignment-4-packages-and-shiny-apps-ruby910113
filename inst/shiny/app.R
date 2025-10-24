# inst/shiny/app.R
library(shiny)
library(bslib)
library(plotly)
library(dplyr)

# ---- Data ----
data("bhai_summary", package = utils::packageName())
data("bhai_rates",   package = utils::packageName())

# ---- Constants ----
GEO    <- "Germany"
SAMPLE <- "German PPS"
HAI_LEVELS <- c("HAP","UTI","BSI","SSI","CDI")
METRICS    <- c("HAIs" = "cases", "Deaths" = "deaths", "DALYs" = "dalys")

ui <- navbarPage(
  title = "HAI Burden Explorer (BHAI) - Germany (German PPS)",
  theme = bs_theme(bootswatch = "flatly"),
  
  tabPanel(
    "Explore",
    sidebarLayout(
      sidebarPanel(
        helpText("Source: Zacher et al. (2019), BHAI R package. Sample: Germany - German PPS."),
        
        selectInput(
          "view", "View:",
          choices = c("Bubble (per HAI)" = "bubble",
                      "Bar (per HAI)"    = "bar",
                      "Overall rates per 100,000" = "overall"),
          selected = "bubble"
        ),
        
        conditionalPanel(
          condition = "input.view != 'overall'",
          checkboxGroupInput("hai", "HAI types:",
                             choices = HAI_LEVELS, selected = HAI_LEVELS)
        ),
        
        conditionalPanel(
          condition = "input.view == 'bar'",
          radioButtons("metric", "Metric:", choices = names(METRICS), selected = "DALYs")
        )
      ),
      mainPanel(
        tags$h4("Germany - German PPS"),
        div(
          style = "border:1px solid #cfcfcf; border-radius:8px; padding:8px; background:#fff;",
          plotlyOutput("plot", height = 420)
        ),
        tags$hr(),
        tags$h5("How to interpret"),
        tags$p("Bubble plot: x = HAIs, y = attributable deaths, bubble size = DALYs. ",
               "UTI often has many cases but comparatively lower DALYs; ",
               "BSI has fewer cases but higher deaths and DALYs; ",
               "HAP contributes substantially to DALYs."),
        tags$h5("Field definitions"),
        tags$ul(
          tags$li("HAIs: estimated annual incident infections"),
          tags$li("Attributable deaths: deaths attributable to the HAI"),
          tags$li("DALYs: YLL + YLD (years of life lost + years lived with disability)")
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
        tags$li("Convert prevalence to incidence (modified Rhame–Sudderth; Grenander/mean for LOI)."),
        tags$li("Extrapolate incidence per patient to population using discharges."),
        tags$li("Stratify by age/sex; adjust remaining life expectancy via McCabe categories."),
        tags$li("Apply outcome trees to compute YLL, YLD, and DALYs.")
      ),
      tags$p("This app is a didactic explorer; uncertainty intervals are not shown.")
    )
  )
)

server <- function(input, output, session) {
  
  base_all_hai <- reactive({
    bhai_summary |> filter(geo == GEO, sample == SAMPLE)
  })
  
  dat_filtered <- reactive({
    base_all_hai() |> filter(hai %in% input$hai)
  })
  
  global_xmax <- reactive(max(base_all_hai()$cases,  na.rm = TRUE))
  global_ymax <- reactive(max(base_all_hai()$deaths, na.rm = TRUE))
  global_dalys_max <- reactive(max(base_all_hai()$dalys, na.rm = TRUE))
  
  output$plot <- renderPlotly({
    
    if (input$view == "overall") {
      rates <- bhai_rates |> filter(geo == GEO, sample == SAMPLE)
      validate(need(nrow(rates) > 0, "No data available for overall rates."))
      
      p <- ggplot(rates, aes(x = metric, y = per100k)) +
        geom_col() +
        labs(x = "Metric", y = "Per 100,000 population",
             title = "Population-standardised rates (All HAIs)") +
        theme_minimal()
      
      return(ggplotly(p))
    }
    
    df_base <- base_all_hai()
    df <- dat_filtered()
    validate(need(nrow(df) > 0, "Please select at least one HAI type in the sidebar."))
    
    if (input$view == "bubble") {
      p <- ggplot(df, aes(x = cases, y = deaths)) +
        geom_point(
          aes(size = dalys,
              text = paste0("HAI: ", hai,
                            "<br>HAIs: ", cases,
                            "<br>Deaths: ", deaths,
                            "<br>DALYs: ", dalys)),
          alpha = 0.65
        ) +
        geom_text(aes(label = hai), vjust = -0.8, size = 3) +
        scale_size_area(limits = c(0, global_dalys_max()), max_size = 18, guide = "legend") +
        scale_x_continuous(limits = c(0, global_xmax() * 1.15)) +
        scale_y_continuous(limits = c(0, global_ymax() * 1.25)) +
        labs(x = "HAIs (annual)", y = "Attributable deaths (annual)", size = "DALYs") +
        theme_minimal()
      
      return(ggplotly(p, tooltip = "text"))
    }
    
    # Bar
    metric <- req(input$metric)
    metric_col <- METRICS[[metric]]
    order <- HAI_LEVELS[HAI_LEVELS %in% df$hai]
    df <- df |> mutate(hai = factor(hai, levels = order))
    
    p <- ggplot(df, aes(x = .data[[metric_col]], y = hai)) +
      geom_col() +
      scale_x_continuous(limits = c(0, max(df_base[[metric_col]], na.rm = TRUE) * 1.10)) +
      labs(x = metric, y = "HAI type") +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui, server)