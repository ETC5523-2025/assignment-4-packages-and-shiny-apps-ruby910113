## code to prepare `bhai_dataset` dataset goes here

library(dplyr)
library(tibble)

# Annual totals for Germany (German PPS) with 95% UI
bhai_summary <- tibble::tribble(
  ~geo,      ~sample,       ~hai,
  ~cases, ~cases_low, ~cases_high,
  ~deaths, ~deaths_low, ~deaths_high,
  ~dalys, ~dalys_low, ~dalys_high,
  ~yll,   ~yll_low,   ~yll_high,
  ~yld,   ~yld_low,   ~yld_high,
  
  "Germany","German PPS","HAP",
  106586,     83618,      137476,
  3968,      1107,         8164,
  69508,     34042,       117232,
  41306,     11475,        84483,
  27539,     16528,        42824,
  
  "Germany","German PPS","UTI",
  214150,    175086,      253524,
  3664,      1462,         7533,
  66701,     27890,       128543,
  44871,     18043,        92915,
  20243,      8095,        40522,
  
  "Germany","German PPS","BSI",
  26976,     16520,       42252,
  3905,      2004,         6987,
  58350,     30940,       104227,
  49578,     25499,        90816,
  8787,      4463,        16609,
  
  "Germany","German PPS","SSI",
  93222,     75369,      114241,
  2328,      1888,         2882,
  28842,     23313,        35303,
  28376,     22983,        34714,
  452,       352,          580,
  
  "Germany","German PPS","CDI",
  36002,     25108,       49934,
  1917,       112,         4547,
  20890,      2023,        49443,
  19937,      1166,        47973,
  977,       172,         2125
)

# Rates per 100,000 population (with 95% UI by HAI type)
bhai_rates <- tibble::tribble(
  ~geo,      ~sample,                ~hai, ~metric,  ~per100k, ~per100k_low, ~per100k_high,
  
  # -------- Germany — German PPS --------
  "Germany", "German PPS",           "HAP", "HAIs",    132.0,      103.5,        170.2,
  "Germany", "German PPS",           "UTI", "HAIs",    265.1,      216.8,        313.9,
  "Germany", "German PPS",           "BSI", "HAIs",     33.4,       20.5,         52.3,
  "Germany", "German PPS",           "SSI", "HAIs",    115.4,       93.3,        141.4,
  "Germany", "German PPS",           "CDI", "HAIs",     44.6,       31.1,         61.8,
  "Germany", "German PPS",           "All", "HAIs",    592.1,      521.7,        665.8,
  
  "Germany", "German PPS",           "HAP", "Deaths",    4.9,        4.1,         10.1,
  "Germany", "German PPS",           "UTI", "Deaths",    4.5,        1.8,          9.3,
  "Germany", "German PPS",           "BSI", "Deaths",    4.8,        2.5,          8.7,
  "Germany", "German PPS",           "SSI", "Deaths",    2.9,        2.3,          3.6,
  "Germany", "German PPS",           "CDI", "Deaths",    2.4,        0.1,          5.6,
  "Germany", "German PPS",           "All", "Deaths",   20.1,       13.4,         28.2,
  
  "Germany", "German PPS",           "HAP", "DALYs",   86.1,       42.1,        145.1,
  "Germany", "German PPS",           "UTI", "DALYs",   82.6,       34.5,        159.2,
  "Germany", "German PPS",           "BSI", "DALYs",   72.2,       38.3,        132.9,
  "Germany", "German PPS",           "SSI", "DALYs",   35.7,       28.9,         43.7,
  "Germany", "German PPS",           "CDI", "DALYs",   25.9,        2.5,         61.2,
  "Germany", "German PPS",           "All", "DALYs",  308.2,      221.2,        416.3,
  
  # -------- EU/EEA — ECDC PPS --------
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "HAP", "HAIs",    143.7,      136.9,        150.8,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "UTI", "HAIs",    174.7,      166.3,        182.4,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "BSI", "HAIs",     22.2,       20.0,         25.1,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "SSI", "HAIs",    111.3,      105.4,        116.6,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "CDI", "HAIs",     16.0,       14.2,         18.3,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "All", "HAIs",    467.9,      456.2,        480.2,
  
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "HAP", "Deaths",    5.3,        1.3,         10.2,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "UTI", "Deaths",    3.0,        1.2,          5.9,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "BSI", "Deaths",    3.3,        2.1,          4.6,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "SSI", "Deaths",    2.6,        2.4,          2.7,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "CDI", "Deaths",    0.9,        0.0,          1.8,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "All", "Deaths",   15.3,       10.2,         21.2,
  
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "HAP", "DALYs",  109.8,       55.3,        170.5,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "UTI", "DALYs",   57.1,       24.3,        102.9,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "BSI", "DALYs",   76.2,       52.6,        104.8,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "SSI", "DALYs",   35.1,       33.3,         36.8,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "CDI", "DALYs",   10.0,        0.9,         19.2,
  "EU/EEA",  "ECDC PPS (EU/EEA)",    "All", "DALYs",  290.0,      214.9,        376.9
)

# Save
usethis::use_data(bhai_summary, overwrite = TRUE)
usethis::use_data(bhai_rates,   overwrite = TRUE)