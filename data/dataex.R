#' Data of revolutionary campaigns and democracy index, 1901-2019
#'
#' A data from NANVCO 1.3 and VDEM
#'
#' @format ## `who` A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{iso3}{Country code iso3c}
#'   \item{year}{Year}
#'   \item{NVC_1.3_NONVIOL}{Year}
#'   \item{NVC_1.3_VIOL}{Year}
#'   \item{VDEM_v2x_polyarchy}{Electoral democracy index [0,1]}
#'   \item{VDEM_v2x_polyarchy_lag}{{Electoral democracy index [0,1] at time t-1 for country i}
#'   \item{UN_Total_Population_log}{{Natural logarithm of population in thousands from UN}
#'   \item{UN_Median_Age}{{Median age from UN}
#' }
#' @source NAVCO 1.3, VDEM, UN
"dataex"
