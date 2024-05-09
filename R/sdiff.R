#' Nonparametric test for comparing means in 2 groups
#'
#' @param x Numeric array with observations from the first group.
#' @param y Numeric array with observations from the second group.
#' @param n Number of simulations for constriction Null hypothesis area. Default `n=1000`
#' @details
#' This is nonparametric test (alternative to t-test) based on Monte Carlo Simulations. The idea is straightforward. First, the initial difference between the groups is counted. Null hypothesis - this difference is equal to zero, alternative hypothesis - the difference is significantly different from zero. To test this hypothesis, an iterative procedure is created where values from both groups are randomly mixed (i.e. each observation can be randomly assigned to one of the groups). Due to randomness, the difference in the mean of the mixed groups should be zero. After a thousand such procedures, we construct a distribution of the resulting differences and then simply compare it with the observed difference.
#'
#' @return ggplot
#' @export
#' @import ggplot2
#' @examples
#' x = rnorm(1000, 10, 10)
#' y = rnorm(1000, 11, 2)
#' sdiff(x, y, n = 1000)
#'
sdiff <- function(x,y, n = 1000){
  initial = mean(x, na.rm = T) - mean(y, na.rm = T)
  diff <- NULL
  for(i in 1:n){
    xn = sample(c(x,y), size = (length(x)+length(y))/2, replace = F)
    yn = sample(c(x,y), size = (length(x)+length(y))/2, replace = F)
    diff[i] = mean(xn, na.rm = T) - mean(yn, na.rm = T)
  }
  gg <- ggplot(data.frame(diff = diff), aes(x = diff))+
    geom_histogram(aes(y=..density..), position="identity", alpha = 0.5)+
    geom_density(alpha=0.3, fill = "grey")+
    theme_classic()+
    geom_vline(xintercept = quantile(diff, c(0.025,0.975)), color = "grey", linetype = "dashed", linewidth = 1)+
    geom_vline(xintercept = initial, color = "red")+
    labs(x = "E(X) - E(Y)", y = "Density",
         caption = paste0(paste0("Observed diff: ", round(initial,3)),"\n",
                          paste0("Simulated Null Hypotheses 95% interval: ",
                                 round(quantile(diff, 0.025), 3), " - ", round(quantile(diff, 0.975), 3)
                          ), "\n",
                          paste0("p-value: ",
                                 ifelse(mean(abs(diff) >= abs(initial))==0,"<<0.001",mean(abs(diff) >= abs(initial)))
                                 )
                          )
         )+
    theme(plot.caption=element_text(hjust = 0))
  return(gg)
}
