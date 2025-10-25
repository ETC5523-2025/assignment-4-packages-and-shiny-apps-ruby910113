#' BHAI summary by infection type (annual totals, Germany – German PPS)
#'
#' Estimated healthcare-associated infections (HAIs) by infection type with
#' point estimates and 95% uncertainty intervals (UI).
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{geo}{Geography (e.g., "Germany").}
#'   \item{sample}{Data source (e.g., "German PPS").}
#'   \item{hai}{HAI type: HAP, UTI, BSI, SSI, CDI.}
#'   \item{cases}{Estimated incident infections (annual).}
#'   \item{cases_low}{Lower 95\% UI for cases.}
#'   \item{cases_high}{Upper 95\% UI for cases.}
#'   \item{deaths}{Attributable deaths (annual).}
#'   \item{deaths_low}{Lower 95\% UI for deaths.}
#'   \item{deaths_high}{Upper 95\% UI for deaths.}
#'   \item{dalys}{Disability-adjusted life years (YLL + YLD).}
#'   \item{dalys_low}{Lower 95\% UI for DALYs.}
#'   \item{dalys_high}{Upper 95\% UI for DALYs.}
#'   \item{yll}{Years of life lost.}
#'   \item{yld}{Years lived with disability.}
#' }
#' @source Zacher et al. (2019), Eurosurveillance.
#' @keywords datasets
#' @usage data(bhai_summary)
"bhai_summary"

#' BHAI rates per 100,000 population (Germany vs EU/EEA)
#'
#' Population-standardised rates by HAI type and metric (HAIs, Deaths, DALYs)
#' with 95\% UI for German PPS and ECDC PPS (EU/EEA).
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{geo}{Geography ("Germany", "EU/EEA").}
#'   \item{sample}{Data source ("German PPS", "ECDC PPS (EU/EEA)").}
#'   \item{hai}{HAI type: HAP, UTI, BSI, SSI, CDI.}
#'   \item{metric}{One of "HAIs", "Deaths", "DALYs".}
#'   \item{per100k}{Rate per 100{,}000 population.}
#'   \item{per100k_low}{Lower 95\% UI.}
#'   \item{per100k_high}{Upper 95\% UI.}
#' }
#' @source Zacher et al. (2019), Eurosurveillance.
#' @keywords datasets
#' @usage data(bhai_rates)
"bhai_rates"