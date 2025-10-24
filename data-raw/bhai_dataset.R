## code to prepare `bhai_dataset` dataset goes here

library(dplyr)
library(tibble)

# Annual totals for Germany (German PPS)
bhai_summary <- tribble(
  ~geo,      ~sample,        ~hai, ~cases, ~deaths, ~dalys, ~yll,  ~yld,
  "Germany", "German PPS",   "HAP", 106586,   3968,   69508, 41306, 27539,
  "Germany", "German PPS",   "UTI", 214150,   3664,   66701, 44871, 20243,
  "Germany", "German PPS",   "BSI",  26976,   3095,   58350, 49578,  8787,
  "Germany", "German PPS",   "SSI",  93222,   2328,   28842, 28376,   452,
  "Germany", "German PPS",   "CDI",  36002,   1947,   20820, 19937,   977
)

# Rates per 100,000 population
bhai_rates <- tribble(
  ~geo,      ~sample,      ~hai, ~metric,  ~per100k,
  "Germany", "German PPS", "All","HAIs",     592.1,
  "Germany", "German PPS", "All","Deaths",    20.1,
  "Germany", "German PPS", "All","DALYs",    308.2,
  "EU/EEA",  "ECDC PPS",   "All","HAIs",     467.9,
  "EU/EEA",  "ECDC PPS",   "All","Deaths",    15.3,
  "EU/EEA",  "ECDC PPS",   "All","DALYs",    290.0
)

# Save
usethis::use_data(bhai_summary, overwrite = TRUE)
usethis::use_data(bhai_rates,   overwrite = TRUE)