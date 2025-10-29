#' Infer German population from totals and overall HAI rate
#'
#' Simple back-calculation: pop = total cases / (rate per person).
#'
#' @return A single numeric (implied population size).
#' @keywords internal
#' @noRd
implied_pop_de <- function() {
  de_cases_total <- bhai_summary %>%
    dplyr::filter(geo == "Germany", sample == "German PPS") %>%
    dplyr::summarise(n = sum(cases), .groups = "drop") %>%
    dplyr::pull(n)
  
  de_overall_rate <- bhai_rates %>%
    dplyr::filter(
      geo == "Germany",
      sample == "German PPS",
      hai == "All",
      metric == "HAIs"
    ) %>%
    dplyr::pull(per100k)
  
  # pop = cases / (rate per person)
  de_cases_total / (de_overall_rate / 1e5)
}