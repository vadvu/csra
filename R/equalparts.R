#' Analysis of a binary data by dividing independent into equal parts
#'
#' @param data Dataframe
#' @param independent Character. Independent variable.
#' @param lag_independent Logical. Should be independent variable lagged for 1
#'   time-unit?
#' @param lag_code Character. Passed if lag_independent is False. The name of a
#'   variable in the dataframe that is a subject in the data. For ex., country
#'   name, iso3 and etc.
#' @param lead Logical. Passed if lag_independent is False. Should lead be used
#'   instead of lag? Lead is used in the case, when in the data's top are
#'   earlier time units (for ex., 2022, 2021, 2020, ...). Lag is used when
#'   data's top starts with later time units (1900, 1901, ...).
#' @param dependent Character. Dependent variable.
#' @param n Int. Number of equal parts on which independent variable should be
#'   divided.
#' @param nominal_or_percent Logical. Should be the output on y-axis (intensity
#'   of dependent variable) be in percents or in nominal values? If you want
#'   scatter plot it is automatically changed on nominal.
#' @param bar_or_scatter Character with 2 possible values: "bar", "scatter".
#'   Type of figure that should be plotted.
#' @param regline Logical. Should linear regression line be plotted?
#' @param range_bars Logical. Should range bars be shown? Range depicts the min
#'   and max value of independent variable in each divided group.
#' @param conf_bars Logical. Should confidence bars be shown? They depict 95%CI
#'   for a binary dependent variable.
#' @param return_data Logical. Should be returned data (table)? If False, plot
#'   is returned.
#' @param save_plot Logical. Should plot be saved in a working directory?
#' @param name_save_plot Character. Passed if save_plot is False. Name of a plot
#'   for saving.
#'
#' @return Plot (bar or scatter) or dataframe. In a dataframe there are following columns:
#' parts: number of equal part. In the first are the lowest values, in the last are the highest values
#' Freq_0: number of 0 cases in dependent variable
#' Freq_1: number of 1 cases in dependent variable
#' means: mean of an independent variable in specific equal part
#' min: min value of an independent variable in specific equal part
#' max: max value of an independent variable in specific equal part
#' prc5: 5% percentile of an independent variable in specific equal part
#' prc95: 95% percentile of an independent variable in specific equal part
#' low95CI: lower border of 95% CI of Freq_1
#' high95CI: higher border of 95% CI of Freq_1
#' @import dplyr ggplot2 stats confintr
#' @examples
#' data("datex")
#' equalparts(data = datex,
#' independent = 'VDEM_v2x_polyarchy_lag',
#' lag_independent = FALSE,
#' dependent = 'NVC_1.3_NONVIOL',
#' n = 6,
#' bar_or_scatter = 'scatter',
#' regline= TRUE,
#' return_data = FALSE,
#' conf_bars = TRUE,
#' range_bars = FALSE,
#' save_plot = FALSE)
#'
#' equalparts(data = datex,
#' independent = 'VDEM_v2x_polyarchy',
#' lag_independent = TRUE,
#' lag_code = "iso3",
#' lead = TRUE,
#' dependent = 'NVC_1.3_NONVIOL',
#' n = 10,
#' bar_or_scatter = 'bar',
#' nominal_or_percent = "percent",
#' return_data = FALSE,
#' conf_bars = TRUE,
#' save_plot = TRUE,
#' name_save_plot = "figure1")
#'
#' @export
equalparts <-
  function(data,
           independent,
           lag_independent = FALSE,
           lag_code,
           lead = TRUE,
           dependent,
           n = 6,
           nominal_or_percent = "nominal",
           bar_or_scatter = "bar",
           regline = TRUE,
           range_bars = FALSE,
           conf_bars = TRUE,
           return_data = FALSE,
           save_plot = FALSE,
           name_save_plot = "plot") {

    `%notin%` = Negate(`%in%`)

    if(independent %notin% colnames(data) | dependent %notin% colnames(data)){
      stop("Independent or Dependent variables are not found in the data")
    }

    dep_n <- which(colnames(data) == dependent)
    ind_n <- which(colnames(data) == independent)
    if (lag_independent == T) {
      if(lag_code %notin% colnames(data)){stop("lag_code is not provided (or not found in the data), while lag_independent = T")}
      if (lead) {
        data <- data %>%
          group_by(get(lag_code)) %>%
          dplyr::mutate(lag = dplyr::lead(get(independent), n = 1, default = NA)) %>%
          as.data.frame()
        ind_n <- which(colnames(data) == "lag")
      } else {
        data <- data %>%
          group_by(get(lag_code)) %>%
          dplyr::mutate(lag = dplyr::lag(get(independent), n = 1, default = NA)) %>%
          as.data.frame()
        ind_n <- which(colnames(data) == "lag")
      }
    }

    data1 <- as.data.frame(stats::na.omit(data[, c(ind_n, dep_n)]))
    if(n<2){stop("n should be >=2")}
    data1$parts <- dplyr::ntile(data1[, 1], n)
    s <- as.data.frame(sjmisc::flat_table(data1, parts, get(dependent)))
    s["means"] <- NA
    s["min"] <- NA
    s["max"] <- NA
    s["prc5"] <- NA
    s["prc95"] <- NA
    data1[, 1] <- as.numeric(data1[, 1])
    for (i in 1:n) {
      s[s$parts == i, "means"] <-
        mean(data1[data1$parts == i, 1], na.rm = T)
      s[s$parts == i, "min"] <-
        min(data1[data1$parts == i, 1], na.rm = T)
      s[s$parts == i, "max"] <-
        max(data1[data1$parts == i, 1], na.rm = T)
      s[s$parts == i, "prc5"] <-
        quantile(data1[data1$parts == i, 1], probs = c(0.05), na.rm = T)
      s[s$parts == i, "prc95"] <-
        quantile(data1[data1$parts == i, 1], probs = c(0.95), na.rm = T)
    }

    s_return <- s[-c(1:n), ]
    s_return$Freq_0 <- s[c(1:n), 3]
    s_return <- s_return[, c(1, ncol(s_return), 3:8)]
    colnames(s_return)[3] <- "Freq_1"

    #Wald CI
    s_return["low95CI"] <-
      s_return$Freq_1 / (s_return$Freq_0 + s_return$Freq_1) -
      1.96 * (((
        s_return$Freq_1 / (s_return$Freq_0 + s_return$Freq_1)
      ) * (1 - (
        s_return$Freq_1 / (s_return$Freq_0 + s_return$Freq_1)
      ))) /
        (s_return$Freq_0 + s_return$Freq_1)) ^ 0.5
    s_return["high95CI"] <-
      s_return$Freq_1 / (s_return$Freq_0 + s_return$Freq_1) +
      1.96 * (((
        s_return$Freq_1 / (s_return$Freq_0 + s_return$Freq_1)
      ) * (1 - (
        s_return$Freq_1 / (s_return$Freq_0 + s_return$Freq_1)
      ))) /
        (s_return$Freq_0 + s_return$Freq_1)) ^ 0.5


    if (return_data == T) {
      return(s_return)
    } else {
      chi <- stats::chisq.test(s_return[c(2, 3)])
      chi_sq <- round(chi$statistic, 2)
      chi_sq_p <- chi$p.value

      if (nominal_or_percent == "nominal" &
          bar_or_scatter == "bar") {
        pl <- ggplot(data = s_return, aes(x = parts, y = Freq_1)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = Freq_1),
                    vjust = -0.3,
                    size = 3.5) +
          xlab(paste0(independent, " divided into ", n, " parts")) + ylab(paste0(dependent, " (", nominal_or_percent, ")")) +
          theme_classic() +
          labs(caption = paste0("Chi-sq = ", chi_sq, ifelse(
            chi_sq_p < 0.01,
            "***",
            ifelse(chi_sq_p <
                     0.05, "**",
                   ifelse(chi_sq_p <
                            0.1, "*", ""))
          )))

        if (conf_bars) {
          pl <-
            pl + geom_errorbar(aes(
              ymax = high95CI * (Freq_1 + Freq_0),
              ymin = low95CI * (Freq_1 + Freq_0)
            ),
            size = 0.1,
            width = 0.1)
        }


      }
      if (nominal_or_percent == "percent" &
          bar_or_scatter == "bar") {
        s["percent"] = NA
        for (i in (n + 1):nrow(s)) {
          s[i, ncol(s)] <- s[i, 3] / s[i - n, 3]
        }
        pl <-
          ggplot(data = s[-c(1:n), ], aes(x = parts, y = percent)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = round(percent, 4)),
                    vjust = -0.3,
                    size = 3.5) +
          xlab(paste0(independent, " divided into ", n, " parts")) + ylab(paste0(dependent, " (", nominal_or_percent, ")")) +
          theme_classic() +
          labs(caption = paste0("Chi-sq = ", chi_sq, ifelse(
            chi_sq_p < 0.01,
            "***",
            ifelse(chi_sq_p <
                     0.05, "**",
                   ifelse(chi_sq_p <
                            0.1, "*", ""))
          )))

      }
      if (bar_or_scatter == "scatter") {
        rr <-
          confintr::ci_cor(x = s_return$means,
                           y = s_return$Freq_1,
                           method = "pearson")
        r <- round((rr$estimate), 2)
        t <- r * ((nrow(s_return) - 2) / (1 - r ^ 2)) ^ 0.5
        t <- round(t, 2)
        p <- pt(t, nrow(s_return) - 2, lower.tail = F)
        p <- round(p, 3)
        cil = as.numeric( round(rr$interval[1], 3) )
        cih = as.numeric( round(rr$interval[2], 3) )
        if (p == 0) {
          p = "<0.001"
        }

        pl <- ggplot(data = s_return, aes(x = means, y = Freq_1)) +
          geom_point(size = 3.5) + geom_line() +
          xlab(paste0(independent, " divided into ", n, " parts (means)")) +
          ylab(paste0(dependent, " (nominal)")) +
          theme_classic() +
          labs(
            caption = paste0(
              "Pearson's r = ",
              r,
              " [",
              cil,
              " - ",
              ifelse(cih > 0, cih, paste0("(", cih, ")")),
              "]",
              ", p = ",
              p,
              " (t = ",
              t,
              ")",
              ifelse(p < 0.01,
                     "***, ",
                     ifelse(p < 0.05,
                            "**, ",
                            ifelse(p <
                                     0.1, "*, ", ", "))),
              "Chi-sq = ",
              chi_sq,
              ifelse(chi_sq_p < 0.01,
                     "***",
                     ifelse(
                       chi_sq_p <
                         0.05, "**",
                       ifelse(chi_sq_p <
                                0.1, "*", "")
                     ))
            )
          )
        if (regline) {
          pl <-
            pl + geom_smooth(
              method = "lm",
              se = F,
              linetype = "dashed",
              color = "black",
              formula = 'y~x'
            )
        }
        if (range_bars) {
          pl <-
            pl + geom_errorbarh(aes(xmax = prc95, xmin = prc5),
                                size = 0.1,
                                width = 0.1)
        }
        if (conf_bars) {
          pl <-
            pl + geom_pointrange(aes(
              ymax = high95CI * (Freq_1 + Freq_0),
              ymin = low95CI * (Freq_1 + Freq_0)
            ))
        }

      }
      if (save_plot == T) {
        ggsave(
          filename = paste0(name_save_plot, ".png"),
          plot = pl,
          dpi = 600,
          device = "png",
          units = "cm",
          width = 15,
          height = 10
        )
      }
      return(pl)
    }
  }
