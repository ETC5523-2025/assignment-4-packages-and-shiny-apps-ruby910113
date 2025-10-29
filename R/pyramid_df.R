#' Aggregate DALYs for age pyramid (totals only)
#'
#' Summarises simulated microdata into age-by-sex totals for DALYs.
#'
#' @param df_input A data frame with columns: age_group, sex, weight, daly.
#' @param age_levels Factor levels for age groups (youngest to oldest).
#' @param sex_levels Factor levels for sex (defaults: Female, Male).
#' @return A tibble with columns: age_group (factor), sex (factor), value (DALYs).
#' @keywords internal
#' @noRd
pyramid_df <- function(
    df_input,
    age_levels = c(
      "0-1","2-4","5-9","10-14","15-19","20-24","25-34","35-44",
      "45-54","55-64","65-74","75-79","80-84","85+"
    ),
    sex_levels = c("Female", "Male")
) {
  df_input %>%
    dplyr::mutate(
      age_group = factor(age_group, levels = age_levels),
      sex       = factor(sex, levels = sex_levels)
    ) %>%
    dplyr::group_by(age_group, sex) %>%
    dplyr::summarise(
      dalys = sum(weight * daly, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::transmute(age_group, sex, value = dalys)
}