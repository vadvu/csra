#' Data of revolutionary campaigns and democracy index, 1901-2019
#'
#'
#' @format ## `datex` A data frame with 9,836 rows and 6 columns:
#' \describe{
#'   \item{iso3}{Country code in "iso3c" [format](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)}
#'   \item{year}{Year}
#'   \item{NVC_1.3_NONVIOL}{Year}
#'   \item{NVC_1.3_VIOL}{Year}
#'   \item{VDEM_v2x_polyarchy}{Electoral democracy index (0,1)}
#'   \item{VDEM_v2x_polyarchy_lag}{{Electoral democracy index (0,1) at time t-1 for country i}
#' }
#' @source (NAVCO 1.3)[https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ON9XND], (VDEM)[https://v-dem.net/data/the-v-dem-dataset/]
"datex"
