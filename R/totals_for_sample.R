#' Build totals table (with 95% UI) for a selected sample
#'
#' - If `smpl` == "German PPS": use published Germany totals.
#' - Else (EU/EEA): convert per-100k rates to totals using implied DE population.
#'
#' Kept as internal to avoid polluting the public API.
#'
#' @param smpl Character scalar, e.g. "German PPS" or "ECDC PPS (EU/EEA)".
#' @param levels_hai Character vector of HAI levels used to filter/order (defaults to BHAI 5 types).
#' @return A tibble with totals and UI for cases/deaths/DALYs, plus `sample` and factor `hai`.
#' @keywords internal
#' @noRd
totals_for_sample <- function(
    smpl,
    levels_hai = c("HAP", "UTI", "BSI", "SSI", "CDI")
) {
  if (identical(smpl, "German PPS")) {
    totals_tbl <- bhai_summary %>%
      dplyr::filter(geo == "Germany", sample == "German PPS", hai %in% levels_hai) %>%
      dplyr::select(
        hai,
        cases,  cases_low,  cases_high,
        deaths, deaths_low, deaths_high,
        dalys,  dalys_low,  dalys_high
      )
  } else {
    pop_guess <- implied_pop_de()
    
    wide_rates <- bhai_rates %>%
      dplyr::filter(sample == "ECDC PPS (EU/EEA)", hai %in% levels_hai) %>%
      dplyr::select(hai, metric, per100k, per100k_low, per100k_high) %>%
      tidyr::pivot_wider(
        names_from  = metric,
        values_from = c(per100k, per100k_low, per100k_high)
      )
    
    to_total <- function(x) (x / 1e5) * pop_guess
    
    totals_tbl <- tibble::tibble(
      hai         = wide_rates$hai,
      cases       = to_total(wide_rates$per100k_HAIs),
      cases_low   = to_total(wide_rates$per100k_low_HAIs),
      cases_high  = to_total(wide_rates$per100k_high_HAIs),
      deaths      = to_total(wide_rates$per100k_Deaths),
      deaths_low  = to_total(wide_rates$per100k_low_Deaths),
      deaths_high = to_total(wide_rates$per100k_high_Deaths),
      dalys       = to_total(wide_rates$per100k_DALYs),
      dalys_low   = to_total(wide_rates$per100k_low_DALYs),
      dalys_high  = to_total(wide_rates$per100k_high_DALYs)
    )
  }
  
  totals_tbl %>%
    dplyr::mutate(
      sample = smpl,
      hai    = factor(hai, levels = levels_hai)
    )
}