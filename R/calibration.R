#' In-sample calibration graph for binary outcome models
#'
#' @param model model
#' @param ns numver of bins
#'
#' @return `ggplot2` graph
#' @import ggplot2 dplyr
#' @export
calibration <- function(model, ns = 10){
  dats <- model$model
  colnames(dats)[1] <- "y"
  dats$pred <- predict(model, type = "response", se.fit = T)$fit
  dats$se <- predict(model, type = "response", se.fit = T)$se.fit
  dats <- dats %>% arrange(pred)
  dats$bin <- ntile(dats$pred, ns)
  dats$i = 1
  dats2 <- dats %>%
    group_by(bin) %>%
    mutate(mean.pred = mean(pred),
           mean.real = mean(y == 1),
           n = sum(i),
           rev.n = sum(y == 1)) %>%
    as.data.frame() %>%
    distinct(bin, mean.pred, mean.real, n, rev.n) %>%
    mutate(pred.n = round(mean.pred*n))
  plotina <- dats2 %>%
    ggplot(aes(mean.real, mean.pred))+
    geom_line()+
    theme_linedraw()+
    geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed")+
    labs(x = "Fraction y = 1",
         y = "Mean Pr(y = 1)",
         caption = paste0("Note: R^2 = ",round(cor(dats2$rev.n, dats2$pred.n)^2,2),
                          ", n = ", nrow(dats2))
    )
  return(plotina)
}
