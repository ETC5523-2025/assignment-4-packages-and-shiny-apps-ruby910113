#' BHAI summary by infection type (annual totals)
#'
#' A dataset of estimated healthcare-associated infections (HAIs) by infection
#' type, used for demonstration and the Shiny app.
#'
#' @format A data frame with 8 variables:
#' \describe{
#'   \item{geo}{character. Geography (e.g., "Germany", "EU/EEA").}
#'   \item{sample}{character. Sample/data source (e.g., "German PPS",
#'   "German convenience", "ECDC PPS").}
#'   \item{hai}{character. Infection type (HAP, UTI, BSI, SSI, CDI).}
#'   \item{cases}{numeric. Estimated number of incident infections (annual).}
#'   \item{deaths}{numeric. Estimated attributable deaths (annual).}
#'   \item{dalys}{numeric. Disability-adjusted life years (YLL + YLD).}
#'   \item{yll}{numeric. Years of life lost.}
#'   \item{yld}{numeric. Years lived with disability.}
#' }
#' @source Zacher et al. (2019) BHAI methodology
#' @keywords datasets
#' @usage data(bhai_summary)
"bhai_summary"


#' BHAI aggregate rates per 100,000 population
#'
#' A small dataset of population-standardised rates across all HAIs, used for
#' demonstration and the Shiny app.
#'
#' @format A data frame with 5 variables:
#' \describe{
#'   \item{geo}{character. Geography (e.g., "Germany", "EU/EEA").}
#'   \item{sample}{character. Sample/data source (e.g., "German PPS", "ECDC PPS").}
#'   \item{hai}{character. Infection type label; usually "All".}
#'   \item{metric}{character. Measure name: "HAIs", "Deaths", or "DALYs".}
#'   \item{per100k}{numeric. Rate per 100{,}000 population.}
#' }
#' @source Zacher et al. (2019) BHAI methodology
#' @keywords datasets
#' @usage data(bhai_rates)
"bhai_rates"