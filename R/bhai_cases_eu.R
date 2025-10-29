#' Simulated microdata: EU/EEA (ECDC PPS)
#'
#' Case-level simulated microdata for age/sex analyses of HAI burden in the EU/EEA.
#' Each row represents a simulated case with a sampling \code{weight} so that
#' weighted sums reproduce the published rates/totals (within uncertainty).
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{hai}{character. HAI type (\code{HAP}, \code{UTI}, \code{BSI},
#'     \code{SSI}, \code{CDI}).}
#'   \item{age_group}{character. Age band: \code{"0-1"}, \code{"2-4"}, \dots, \code{"85+"}.}
#'   \item{sex}{character. \code{"Female"} or \code{"Male"}.}
#'   \item{death}{integer. 1 if death occurred, 0 otherwise.}
#'   \item{yll}{numeric. Years of life lost for this case (0 if \code{death == 0}).}
#'   \item{yld}{numeric. Years lived with disability for this case.}
#'   \item{daly}{numeric. \code{yll + yld}.}
#'   \item{weight}{numeric. Case weight to scale up to population totals.}
#' }
#'
#' @details
#' These microdata are \strong{simulated for teaching/demo} from published
#' EU/EEA rates; they are not real patient-level records. For authoritative
#' numbers and figures, consult the Eurosurveillance article.
#'
#' @source Zacher et al. (2019) Eurosurveillance.
#' \doi{10.2807/1560-7917.ES.2019.24.46.1900135}
#'
#' @examples
#' data("bhai_cases_eu", package = "BHAIBYE")
#' # Weighted deaths by HAI type (illustration)
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   dplyr::summarise(
#'     dplyr::group_by(bhai_cases_eu, hai),
#'     deaths_weighted = sum(death * weight)
#'   )
#' }
"bhai_cases_eu"