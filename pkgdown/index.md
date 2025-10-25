
# BHAIBYE

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

## Dataset Description and Shiny app

Check out the **Examples** → [Getting started with
BHAIBYE](https://ETC5523-2025.github.io/assignment-4-packages-and-shiny-apps-ruby910113/articles/BHAIBYE.html)
to learn how to use it.

And you can launch it with:

``` r
BHAIBYE::launch_app()
```

## Sources & citation

- Article available from Eurosurveillance:
  <https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2019.24.46.1900135#html_fulltext>

## Contributing

Issues and pull requests are welcome.
