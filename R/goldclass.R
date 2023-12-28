#' Goldstone claasification of regime types based on Polity-V variables
#'
#' @param exrec array with EXREC variable from Polity-V project
#' @param parcomp array with PARCOMP variable from Polity-V project
#'
#' @return array with Goldstone classification:
#' 1 = full autocracy
#' 2 = partial autocracy
#' 3 = partial democracy
#' 4 = partial democracy with factionalism
#' 5 = full democracy
#' @references Goldstone, J.A., Bates, R.H., Epstein, D.L., Gurr, T.R., Lustik, M.B., Marshall, M.G., Ulfelder, J., Woodward, M., 2010. A Global Model for Forecasting Political Instability. American Journal of Political Science 54, 190–208.
#' @export
#' @examples
#' polity5data$goldstone_regime <- goldclass(exrec = polity5data$exrec, parcomp = polity5data$parcomp)
#'
goldclass <- function(exrec, parcomp){
  if (length(parcomp) != length(exrec)){
    stop("arrays have unequal lengths")
  }
  matrix_Goldstone <- matrix(nrow = 8, ncol = 6)
  matrix_Goldstone[1:5,1:2] <- 1 #full autocracy
  matrix_Goldstone[6:8,1:2] <- 2 #partial autocracy
  matrix_Goldstone[1:5,3:6] <- 2 #partial autocracy
  matrix_Goldstone[6:8,3:6] <- 3 #partial democracy
  matrix_Goldstone[6:8,4] <- 4 #partial democracy with factionalism
  matrix_Goldstone[8,6] <- 5 #full democracy
  gold = c()
  for(i in 1:length(exrec)){
    if (is.na(exrec[i]) | is.na(parcomp[i])){
      gold = c(gold, NA)
    }else{
      gold = c(gold,
               matrix_Goldstone[exrec[i], parcomp[i]+1])
    }
  }
  return(gold)
}
