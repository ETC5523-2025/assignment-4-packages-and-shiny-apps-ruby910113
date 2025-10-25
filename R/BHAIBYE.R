#' BHAI summary by infection type (annual totals)
#'
#' Annual estimates for five healthcare-associated infections (HAIs) used by
#' the demo Shiny app. Includes point estimates and 95% uncertainty intervals.
#'
#' @format A data frame with 18 variables:
#' \describe{
#'   \item{geo}{character. Geography (e.g., "Germany", "EU/EEA").}
#'   \item{sample}{character. Data source / sample (e.g., "German PPS", "ECDC PPS (EU/EEA)").}
#'   \item{hai}{character. Infection type: HAP, UTI, BSI, SSI, CDI.}
#'   \item{cases}{numeric. Estimated annual incident infections.}
#'   \item{cases_low}{numeric. 95\% UI lower bound for \code{cases}.}
#'   \item{cases_high}{numeric. 95\% UI upper bound for \code{cases}.}
#'   \item{deaths}{numeric. Estimated annual attributable deaths.}
#'   \item{deaths_low}{numeric. 95\% UI lower bound for \code{deaths}.}
#'   \item{deaths_high}{numeric. 95\% UI upper bound for \code{deaths}.}
#'   \item{dalys}{numeric. Disability-adjusted life years (YLL + YLD).}
#'   \item{dalys_low}{numeric. 95\% UI lower bound for \code{dalys}.}
#'   \item{dalys_high}{numeric. 95\% UI upper bound for \code{dalys}.}
#'   \item{yll}{numeric. Years of life lost.}
#'   \item{yll_low}{numeric. 95\% UI lower bound for \code{yll}.}
#'   \item{yll_high}{numeric. 95\% UI upper bound for \code{yll}.}
#'   \item{yld}{numeric. Years lived with disability.}
#'   \item{yld_low}{numeric. 95\% UI lower bound for \code{yld}.}
#'   \item{yld_high}{numeric. 95\% UI upper bound for \code{yld}.}
#' }
#' @source Zacher et al. (2019), BHAI methodology.
#' @keywords datasets
#' @examples
#' data(bhai_summary)
#' head(bhai_summary)
"bhai_summary"

#' BHAI aggregate rates per 100,000 population
#'
#' Population-standardised rates for HAIs, deaths and DALYs used by the
#' Geo comparison view in the Shiny app. Includes 95\% uncertainty intervals.
#'
#' @format A data frame with 7 variables:
#' \describe{
#'   \item{geo}{character. Geography (e.g., "Germany", "EU/EEA").}
#'   \item{sample}{character. Data source / sample (e.g., "German PPS", "ECDC PPS (EU/EEA)").}
#'   \item{hai}{character. Infection type label (HAP, UTI, BSI, SSI, CDI, or "All").}
#'   \item{metric}{character. One of "HAIs", "Deaths", "DALYs".}
#'   \item{per100k}{numeric. Rate per 100,000 population.}
#'   \item{per100k_low}{numeric. 95\% UI lower bound for \code{per100k}.}
#'   \item{per100k_high}{numeric. 95\% UI upper bound for \code{per100k}.}
#' }
#' @source Zacher et al. (2019), BHAI methodology.
#' @keywords datasets
#' @examples
#' data(bhai_rates)
#' subset(bhai_rates, geo == "Germany" & metric == "DALYs")
"bhai_rates"