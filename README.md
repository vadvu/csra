
<!-- README.md is generated from README.Rmd. Please edit that file -->

# csra

<!-- badges: start -->
<!-- badges: end -->

csra is a package with functions (now just 1) used by
[CSRA](https://social.hse.ru/mr/) staff analyzing political events

## Installation

You can install the development version of csra from
[GitHub](https://github.com/) with:

``` r

# install.packages("devtools")
devtools::install_github("vadvu/csra")
```

## Example

This is a basic example which shows you how the main (and the only one
now) function “equalparts” works:

``` r
library(csra)
#> Warning: заменяю предыдущий импорт 'dplyr::filter' на 'stats::filter' во время
#> загрузки 'csra'
#> Warning: заменяю предыдущий импорт 'dplyr::lag' на 'stats::lag' во время
#> загрузки 'csra'
data <- data("dataex") #it's an example panel data that you can use

#Now let's try to analyze how VDEM democracy index affects revolutionary situations
equalparts(
  data = datex, #our data
  independent = 'VDEM_v2x_polyarchy', #independent var
  lag_independent = T, #Should it be lagged? Yea because political regime can change gramatically during revolutionary year
  lag_code = "iso3", #by what unit lag is realized
  lead = T, #due to specific of data (in the top we have earlier data - 2019, 2018, 2017, ...) lead should be used. Otherwise, False is needed
  dependent = 'NVC_1.3_NONVIOL', #dependent var
  n = 6, #number of equal parts. If n = 10, it is decile analysis, 4 - quartile analysis and etc
  bar_or_scatter = 'scatter', #plot type, scatter is more powerful
  regline = TRUE, #linear regression line
  return_data = FALSE, #we want to see plot, so we do not need data
  conf_bars = TRUE, #95%CI
  range_bars = FALSE, #range (max-min) of independent var in each unit (for ex., decile)
  save_plot = FALSE #we do not want to save plot, so its False
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

# for ex., change axis names and theme
plot + xlab("x var name") + ylab("y var name") + theme_grey()
```

<img src="man/figures/README-example2-1.png" width="100%" />

Also table with results can be returned, just set return_data = TRUE
