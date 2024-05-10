#' Data of revolutionary campaigns and democracy index, 1901-2019
#'
#'
#' @format ## `datex` A data frame with 9,836 rows and 6 columns:
#' \describe{
#'   \item{iso3}{Country code in "iso3c" [format](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)}
#'   \item{year}{Year}
#'   \item{NVC_1.3_NONVIOL}{Binary indicator. Do protestors primary use nonviolent tactic? 1 = Yes}
#'   \item{NVC_1.3_VIOL}{Binary indicator. Do protestors primary use violent tactic? 1 = Yes}
#'   \item{VDEM_v2x_polyarchy}{Electoral democracy index (0,1) from V-Dem project}
#'   \item{VDEM_v2x_polyarchy_lag}{Electoral democracy index (0,1) at time t-1 for country i}
#' }
#' @source [NAVCO 1.3](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ON9XND), [VDEM](https://v-dem.net/data/the-v-dem-dataset/)
"datex"


#' Revolutions dataset, 2000-2022 (Version 9)
#'
#'The database on revolutionary events of the 21st century was prepared within the framework of the project “Quantitative Analysis and Forecasting of the Risks of Socio-Political Destabilization in the Countries of the Afrasian Macrozone of Instability” supported by the Russian Science Foundation (project No. 18-18-00254-P).
#'The database describes revolutionary events of the 21st century by various characteristics: chronological, geographical, type of protestors' tactics, purpose and degree of success.
#'
#' @format ## `revolutions` A data frame with event as unit of analysis.
#' \describe{
#'   \item{n}{unique episode number}
#'   \item{start_year}{event start year}
#'   \item{end_year}{event end year}
#'   \item{country}{country name}
#'   \item{iso3c}{three-letter ISO 3166-1 alpha-3 ([iso3c](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)) country code}
#'   \item{region}{the region to which the country where the event took place belongs (UN Subregion classification)}
#'   \item{successful}{complete success (1 = yes, 0 = no)}
#'   \item{limited}{limited success (1 = yes, 0 = no)}
#'   \item{failed}{fail (1 = yes, 0 = no)}
#'   \item{ongoing}{is episode still not ended? (1 = yes, 0 = no). Note, episode can be ongoing and has "success" coding that is preliminary estimate.}
#'   \item{armed}{armed tactic: fabric weapons (1 = yes, 0 = no)}
#'   \item{unarmed}{unarmed tactic: nonviolent resistance or improvised weapons - sticks, stones (1 = yes, 0 = no)}
#'   \item{regime_change}{the goal was a regime change (1 = yes, 0 = no)}
#'   \item{islamistic}{the goal was connected to Islam (1 = yes, 0 = no). Note, this feature usually goes with regime change goal}
#'   \item{democratic}{the goal was to establish/improve democratic institutions (1 = yes, 0 = no). Note, this feature usually goes with regime change goal}
#'   \item{social}{the goal was primary social, firstly connected with life-conditions (1 = yes, 0 = no)}
#'   \item{separatism}{the goal was to gain independence/autonomy (1 = yes, 0 = no)}
#'   \item{other}{other goal (1 = yes, 0 = no). Can be overlapped by mentioned goal types}
#'   \item{coupvolution}{Was an event ended with "End game Coup"? (1 = yes, 0 = no)}
#'   \item{revolutions}{Is event a pure revolution (mass mobilization, aim to overthrough regime)? (1 = yes, 0 = no)}
#'   \item{revolutions_plus_question}{If event is not a pure revolution, can it be still considered as revolutionary movement? (1 = yes, 0 = no)}
#'   \item{all}{All types of event including quasi-revolutionary events (one unique value = 1)}
#'   \item{ethnic}{Was an episode connected to ethnic cleaveges? (1 = yes, 0 = no)}
#' }
#' @source [Center for Stability and Risk Analysis](https://social.hse.ru/mr/rev_bd)
"revolutions"
