
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BHAIBYE

<!-- badges: start -->

<!-- badges: end -->

## Overview

**BHAIBYE** is a teaching package that ships a small, tidy dataset and a
Shiny explorer for the burden of healthcare-associated infections (HAIs)
in Germany and the EU/EEA. The numbers are based on the published BHAI
workflow (Burden of Healthcare-Associated Infections) and let you:

- explore per-HAI totals with bubble and bar charts,

- compare Germany vs EU/EEA rates per N people,

- view 95% uncertainty intervals (UI) for cases, deaths and DALYs.

The package is intentionally lightweight: it’s meant for coursework,
demos, and quick exploration.

## Installation

You can install the development version of BHAIBYE from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ETC5523-2025/assignment-4-packages-and-shiny-apps-ruby910113")
```

## Dataset Description

**Datasets**

This package includes two small data frames:

- `bhai_summary` - annual totals for Germany (German PPS) by HAI type,
  with point estimates and 95% UI.

- `bhai_rates` - rates per 100,000 population for Germany (German PPS)
  and EU/EEA (ECDC PPS), by HAI type and metric (HAIs, Deaths, DALYs),
  with 95% UI.

Both datasets contain the HAI types: `HAP`, `UTI`, `BSI`, `SSI`, `CDI`.

## Shiny app

The package bundles a Shiny app (under inst/shiny/) to interactively
explore the data. It provides:

- Bubble plot (per HAI): cases vs deaths, bubble size = DALYs (with 95%
  UI in tooltips)

- Bar plot (per HAI): choose metric (HAIs / Deaths / DALYs), with 95% UI
  error bars

- Geo comparison: Germany vs EU/EEA per-N rates (slider to change N),
  with 95% UI

Launch it with:

``` r
BHAIBYE::launch_app()
```

## Documentation

For more details, see the full documentation website:

- **Site home:**
  <https://ETC5523-2025.github.io/assignment-4-packages-and-shiny-apps-ruby910113>

## Sources & citation

- Article available from Eurosurveillance:
  <https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2019.24.46.1900135#html_fulltext>

### License

This package is released under the MIT License. See the included LICENSE
/ LICENSE.md files for the full terms.

### Authors

Ting-Ting, Wu (author & maintainer)

## Contributing

Issues and pull requests are welcome.
