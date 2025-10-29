## code to prepare `bhai_dataset` dataset goes here

library(dplyr)
library(tibble)
library(tidyr)
library(scales)

# Annual totals for Germany (German PPS) with 95% UI
bhai_summary <- tribble(
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
bhai_rates <- tribble(
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

## ===========================================================================
## 1) Helpers (incl. SAFE lognormal sampler for low<=0)
## ===========================================================================
lnorm_params_from_ci <- function(low, high) {
  stopifnot(all(is.finite(low)), all(is.finite(high)), all(high > low))
  z <- qnorm(0.975)
  sigma <- (log(high) - log(low)) / (2 * z)
  mu    <- (log(high) + log(low)) / 2
  list(mu = mu, sigma = sigma)
}

r_from_ci_lognorm <- function(low, high) {
  p <- lnorm_params_from_ci(low, high)
  rlnorm(1, meanlog = p$mu, sdlog = p$sigma)
}

# SAFE: when low<=0, use (point, high); else tiny positive fallback
r_from_ci_lognorm_safe <- function(low, high, point = NULL) {
  z <- qnorm(0.975)
  if (is.finite(low) && is.finite(high) && low > 0 && high > low) {
    p <- lnorm_params_from_ci(low, high)
    return(rlnorm(1, meanlog = p$mu, sdlog = p$sigma))
  }
  if (!is.null(point) && is.finite(point) && point > 0 && is.finite(high) && high > point) {
    sigma <- max((log(high) - log(point)) / z, 1e-6)
    mu    <- log(point) - 0.5 * sigma^2
    return(rlnorm(1, meanlog = mu, sdlog = sigma))
  }
  if (is.finite(high) && high > 0) return(runif(1, min = high * 1e-6, max = high * 1e-3))
  1e-6
}

count_from_rate <- function(per100k, pop) (per100k / 1e5) * pop
softmax <- function(x) exp(x) / sum(exp(x))
rgamma_mean_cv <- function(n, mean, cv) {
  mean <- pmax(mean, 0); cv <- pmax(cv, 1e-6)
  shape <- 1/(cv^2); scale <- mean * (cv^2)
  stats::rgamma(n, shape = shape, scale = scale)
}

## ===========================================================================
## 2) Germany: draw true totals from bhai_summary (lognormal on UI)
## ===========================================================================
de_totals <- bhai_summary %>%
  transmute(
    infection_type = hai,
    cases_point  = cases,  cases_low  = cases_low,  cases_high  = cases_high,
    deaths_point = deaths, deaths_low = deaths_low, deaths_high = deaths_high,
    dalys_point  = dalys,  dalys_low  = dalys_low,  dalys_high  = dalys_high,
    yll_point    = yll,    yll_low    = yll_low,    yll_high    = yll_high,
    yld_point    = yld,    yld_low    = yld_low,    yld_high    = yld_high
  )

# Implied DE population from "All HAIs" rate
all_rate_de <- bhai_rates %>%
  filter(geo == "Germany", sample == "German PPS", hai == "All", metric == "HAIs") %>%
  pull(per100k)

de_total_cases_point <- sum(de_totals$cases_point)
pop_de_implied <- de_total_cases_point / (all_rate_de / 1e5)

set.seed(5523)

de_draw <- de_totals %>%
  rowwise() %>%
  mutate(
    cases_true  = r_from_ci_lognorm(cases_low,  cases_high),
    deaths_true = r_from_ci_lognorm(deaths_low, deaths_high),
    dalys_true  = r_from_ci_lognorm(dalys_low,  dalys_high),
    prop_yll    = if_else(dalys_point > 0, yll_point / dalys_point, 0),
    yll_true    = dalys_true * prop_yll,
    yld_true    = pmax(dalys_true - yll_true, 0)
  ) %>%
  ungroup() %>%
  mutate(
    p_death_true     = if_else(cases_true > 0,  deaths_true / cases_true, 0),
    yll_per_death_m  = if_else(deaths_true > 0, yll_true   / deaths_true, 0),
    yld_per_case_m   = if_else(cases_true  > 0, yld_true   / cases_true,  0)
  ) %>%
  select(infection_type, cases_true, deaths_true, dalys_true,
         yll_true, yld_true, p_death_true, yll_per_death_m, yld_per_case_m)

## ===========================================================================
## 3) EU/EEA rates (input) → simulate counts; split DALYs into YLL/YLD
## ===========================================================================
eu_long <- bhai_rates %>%
  filter(geo == "EU/EEA", sample == "ECDC PPS (EU/EEA)", hai != "All") %>%
  transmute(
    infection_type = trimws(hai),
    metric         = trimws(metric),
    per100k, per100k_low, per100k_high
  ) %>%
  distinct()

eu_hai <- eu_long %>%
  filter(metric == "HAIs") %>%
  group_by(infection_type) %>% slice_head(n = 1) %>% ungroup() %>%
  rename(per100k_HAIs = per100k, per100k_low_HAIs = per100k_low, per100k_high_HAIs = per100k_high) %>%
  select(-metric)

eu_death <- eu_long %>%
  filter(metric == "Deaths") %>%
  group_by(infection_type) %>% slice_head(n = 1) %>% ungroup() %>%
  rename(per100k_Deaths = per100k, per100k_low_Deaths = per100k_low, per100k_high_Deaths = per100k_high) %>%
  select(-metric)

eu_daly <- eu_long %>%
  filter(metric == "DALYs") %>%
  group_by(infection_type) %>% slice_head(n = 1) %>% ungroup() %>%
  rename(per100k_DALYs = per100k, per100k_low_DALYs = per100k_low, per100k_high_DALYs = per100k_high) %>%
  select(-metric)

eu_rates <- eu_hai %>% left_join(eu_death, by = "infection_type") %>% left_join(eu_daly, by = "infection_type")

# Use Germany's YLL share by HAI to split EU DALYs into YLL/YLD
de_yll_prop <- bhai_summary %>%
  transmute(infection_type = hai, prop_yll = if_else(dalys > 0, yll / dalys, NA_real_))

eu_yll_yld_rates <- eu_rates %>%
  left_join(de_yll_prop, by = "infection_type") %>%
  mutate(prop_yll = tidyr::replace_na(prop_yll, 0.7)) %>%  # fallback if any missing
  transmute(
    geo    = "EU/EEA",
    sample = "ECDC PPS (EU/EEA)",
    hai    = infection_type,
    
    # Point/UI for YLL (scale DALYs UI by prop_yll)
    metric = "YLL",
    per100k      = per100k_DALYs     * prop_yll,
    per100k_low  = per100k_low_DALYs * prop_yll,
    per100k_high = per100k_high_DALYs* prop_yll
  ) %>%
  bind_rows(
    eu_rates %>%
      left_join(de_yll_prop, by = "infection_type") %>%
      mutate(prop_yll = tidyr::replace_na(prop_yll, 0.7)) %>%
      transmute(
        geo    = "EU/EEA",
        sample = "ECDC PPS (EU/EEA)",
        hai    = infection_type,
        metric = "YLD",
        per100k      = per100k_DALYs     * (1 - prop_yll),
        per100k_low  = per100k_low_DALYs * (1 - prop_yll),
        per100k_high = per100k_high_DALYs* (1 - prop_yll)
      )
  )

# Add "All" rows for EU/EEA YLL & YLD
eu_all_yll_yld <- eu_yll_yld_rates %>%
  group_by(metric) %>%
  summarise(
    per100k = sum(per100k),
    per100k_low = sum(per100k_low),
    per100k_high = sum(per100k_high),
    .groups = "drop"
  ) %>%
  mutate(geo = "EU/EEA", sample = "ECDC PPS (EU/EEA)", hai = "All") %>%
  select(geo, sample, hai, metric, per100k, per100k_low, per100k_high)

# Bind into bhai_rates (keeps your original rows; just adds YLL/YLD for EU/EEA)
bhai_rates <- bhai_rates %>%
  bind_rows(eu_yll_yld_rates, eu_all_yll_yld) %>%
  arrange(geo, sample, factor(hai, levels = c("HAP","UTI","BSI","SSI","CDI","All")), metric)

## ===========================================================================
## 4) Build EU/EEA draw to simulate microdata (counts come from rates)
## ===========================================================================
pop_eu_ref <- pop_de_implied   # scale to Germany’s implied pop to keep units coherent

eu_draw <- eu_rates %>%
  rowwise() %>%
  mutate(
    rate_hai   = r_from_ci_lognorm_safe(per100k_low_HAIs,   per100k_high_HAIs,   point = per100k_HAIs),
    rate_death = r_from_ci_lognorm_safe(per100k_low_Deaths, per100k_high_Deaths, point = per100k_Deaths),
    rate_daly  = r_from_ci_lognorm_safe(per100k_low_DALYs,  per100k_high_DALYs,  point = per100k_DALYs),
    
    cases_true  = count_from_rate(rate_hai,   pop_eu_ref),
    deaths_true = count_from_rate(rate_death, pop_eu_ref),
    dalys_true  = count_from_rate(rate_daly,  pop_eu_ref)
  ) %>%
  ungroup() %>%
  left_join(de_yll_prop, by = "infection_type") %>%
  mutate(
    prop_yll         = tidyr::replace_na(prop_yll, 0.7),
    yll_true         = dalys_true * prop_yll,
    yld_true         = pmax(dalys_true - yll_true, 0),
    p_death_true     = if_else(cases_true > 0,  deaths_true / cases_true, 0),
    yll_per_death_m  = if_else(deaths_true > 0, yll_true   / deaths_true, 0),
    yld_per_case_m   = if_else(cases_true  > 0, yld_true   / cases_true,  0)
  ) %>%
  select(infection_type, cases_true, deaths_true, dalys_true,
         yll_true, yld_true, p_death_true, yll_per_death_m, yld_per_case_m)

## ===========================================================================
## 5) Demographics (softmax age; Dirichlet-like gender)
## ===========================================================================
age_groups_ref <- c("0-1","2-4","5-9","10-14","15-19","20-24","25-34","35-44",
                    "45-54","55-64","65-74","75-79","80-84","85+")
age_logits <- scales::rescale(seq_along(age_groups_ref), to = c(-1, 2))
age_probs  <- softmax(age_logits)

gender_levels <- c("Female","Male")
g_raw <- stats::rgamma(2, shape = 20, rate = 1)
gender_probs <- g_raw / sum(g_raw)

## ===========================================================================
## 6) Simulator (Gamma per-case YLL/YLD) + weights
## ===========================================================================
simulate_micro_from_draw <- function(draw_tbl,
                                     n_cases = 5000,
                                     seed = 1,
                                     yll_cv = 0.35,
                                     yld_cv = 0.50) {
  set.seed(seed)
  
  type_probs <- draw_tbl$cases_true / sum(draw_tbl$cases_true)
  
  infection_draw <- sample(draw_tbl$infection_type, n_cases, TRUE, type_probs)
  age_draw      <- sample(age_groups_ref, n_cases, TRUE, age_probs)
  gender_draw   <- sample(gender_levels,  n_cases, TRUE, gender_probs)
  
  df <- tibble(
    infection_type = infection_draw,
    age_group = age_draw,
    gender = gender_draw
  ) %>%
    left_join(draw_tbl, by = "infection_type") %>%
    rowwise() %>%
    mutate(
      died = stats::rbinom(1, 1, p = p_death_true),
      yll  = if (died == 1) rgamma_mean_cv(1, mean = yll_per_death_m, cv = yll_cv) else 0,
      yld  = rgamma_mean_cv(1, mean = yld_per_case_m,  cv = yld_cv),
      daly = yll + yld
    ) %>%
    ungroup()
  
  n_by_type <- df %>% count(infection_type, name = "n_sim")
  
  df %>%
    left_join(n_by_type, by = "infection_type") %>%
    mutate(weight_pop = cases_true / n_sim) %>%
    transmute(
      hai       = infection_type,
      age_group = age_group,
      sex       = gender,
      death     = died,
      yll       = yll,
      yld       = yld,
      daly      = daly,
      weight    = weight_pop
    )
}

## ===========================================================================
## 7) Build microdata (~5k each) and SAVE
## ===========================================================================
bhai_cases_de <- simulate_micro_from_draw(de_draw, n_cases = 5000, seed = 20020113)
bhai_cases_eu <- simulate_micro_from_draw(eu_draw, n_cases = 5000, seed = 20020113)

# Save
usethis::use_data(bhai_summary, overwrite = TRUE)
usethis::use_data(bhai_rates, overwrite = TRUE)
usethis::use_data(bhai_cases_de, overwrite = TRUE)
usethis::use_data(bhai_cases_eu, overwrite = TRUE)