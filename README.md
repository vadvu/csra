
<!-- README.md is generated from README.Rmd. Please edit that file -->

# csra

<!-- badges: start -->
<!-- badges: end -->

csra is a package with functions used by CSRA staff analyzing political
events

## Installation

You can install the development version of csra from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("vadvu/csra")
```

## Example

This is a basic example which shows you how the main function
“equalparts” works:

``` r
library(csra)
#> Warning: заменяю предыдущий импорт 'dplyr::filter' на 'stats::filter' во время
#> загрузки 'csra'
#> Warning: заменяю предыдущий импорт 'dplyr::lag' на 'stats::lag' во время
#> загрузки 'csra'
data <- data("dataex") #it's a data for example that is added to pack

#Now let's try to analyze how democracy index affects revolutionary situations
equalparts(
  data = datex,
  independent = 'VDEM_v2x_polyarchy',
  lag_independent = T,
  lag_code = "iso3",
  lead = T,
  dependent = 'NVC_1.3_NONVIOL',
  n = 6,
  bar_or_scatter = 'scatter',
  regline = TRUE,
  return_data = FALSE,
  conf_bars = TRUE,
  range_bars = FALSE,
  save_plot = FALSE
)
```

<img src="man/figures/README-example 1-1.png" width="100%" />

Plot that is returned by default can be easily changed by ggplot2
syntax. Just save function output and add to it ggplot2 blocks:

``` r
library(csra)
library(ggplot2)
data <- data("dataex")

plot <- equalparts(
  data = datex,
  independent = 'VDEM_v2x_polyarchy',
  lag_independent = T,
  lag_code = "iso3",
  lead = T,
  dependent = 'NVC_1.3_NONVIOL',
  n = 6,
  bar_or_scatter = 'scatter',
  regline = TRUE,
  return_data = FALSE,
  conf_bars = TRUE,
  range_bars = FALSE,
  save_plot = FALSE
)

# for ex., changing axis names and theme
plot + xlab("x var name") + ylab("y var name") + theme_grey()
```

<img src="man/figures/README-example2-1.png" width="100%" />
