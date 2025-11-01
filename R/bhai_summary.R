#' Germany HAI burden totals (with 95% uncertainty)
#'
#' Annual totals for Germany (German PPS) by HAI type:
#' HAP, UTI, BSI, SSI, CDI. Includes point estimates and 95\% uncertainty
#' intervals (UI) for cases, deaths, DALYs, and the DALY components
#' (YLL, YLD).
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with 5 rows (one per HAI type) and 18 columns:
#' \describe{
#'   \item{geo}{character. Geography label (always \code{"Germany"}).}
#'   \item{sample}{character. Data source/sample (always \code{"German PPS"}).}
#'   \item{hai}{character. HAI type: \code{HAP}, \code{UTI}, \code{BSI}, \code{SSI}, \code{CDI}.}
#'   \item{cases}{numeric. Estimated number of HAIs.}
#'   \item{cases_low}{numeric. Lower bound of 95\% UI for \code{cases}.}
#'   \item{cases_high}{numeric. Upper bound of 95\% UI for \code{cases}.}
#'   \item{deaths}{numeric. Estimated attributable deaths.}
#'   \item{deaths_low}{numeric. Lower bound of 95\% UI for \code{deaths}.}
#'   \item{deaths_high}{numeric. Upper bound of 95\% UI for \code{deaths}.}
#'   \item{dalys}{numeric. Disability-adjusted life years (DALYs).}
#'   \item{dalys_low}{numeric. Lower bound of 95\% UI for \code{dalys}.}
#'   \item{dalys_high}{numeric. Upper bound of 95\% UI for \code{dalys}.}
#'   \item{yll}{numeric. Years of life lost.}
#'   \item{yll_low}{numeric. Lower bound of 95\% UI for \code{yll}.}
#'   \item{yll_high}{numeric. Upper bound of 95\% UI for \code{yll}.}
#'   \item{yld}{numeric. Years lived with disability.}
#'   \item{yld_low}{numeric. Lower bound of 95\% UI for \code{yld}.}
#'   \item{yld_high}{numeric. Upper bound of 95\% UI for \code{yld}.}
#' }
#'
#' @details
#' Values mirror the patterns reported in Zacher et al. (2019) and are
#' intended for teaching/demo. See references below for the original study.
#'
#' @source Zacher et al. (2019) Eurosurveillance.
#' \doi{10.2807/1560-7917.ES.2019.24.46.1900135}
#'
#' @references
#' Zacher B., et al. (2019). \emph{Burden of healthcare-associated infections
#' in Germany and the EU/EEA}. Eurosurveillance, 24(46).
#'
#' @examples
#' head(bhai_summary)
"bhai_summary"