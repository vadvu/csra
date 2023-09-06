#'U-shape test for rare events logistic regression
#'
#'@param data Dataframe
#'@param dep_var Character. Dependent variable.
#'@param ind_var Character. Independent variable.
#'@param control_vars Vector of characters with names of control variables.
#'@param vcov_type Character. Type from lmtest::coeftest.
#'@param boot Logical. Should middle point CI be calculated via bootstrap?
#'@param n Int. Number of bootstraps.
#'@param save_plot Logical. Should plot be saved.
#'@param save_table Logical. Should regression table be saved.
#'@return Plot and regression table with U-shape test summary.
#'@import mgcv brglm2 lmtest tidyr sandwich stargazer doParallel foreach
#'  ggeffects
#' @examples
#' data("datex")
#' U_shape_test(data = datex,
#' dep_var = "NVC_1.3_NONVIOL",
#' ind_var = "VDEM_v2x_polyarchy_lag",
#' control_vars = c("UN_Total_Population_log", "UN_Median_Age"),
#' boot = FALSE,
#' vcov_type = "HC",
#' n = 1000,
#' save_plot = FALSE, save_table = FALSE)
#'
#'@export
U_shape_test <-
  function(data,
           dep_var,
           ind_var,
           control_vars,
           vcov_type = "HC",
           boot = FALSE,
           n = 1000,
           save_plot = FALSE,
           save_table = FALSE) {
    library(brglm2)
    coef <- function(x, vcov_type = "HC") {
      if (vcov_type == "HAC") {
        co <-
          round(lmtest::coeftest(x, vcov = sandwich::vcovHAC(x))[2, 1], 2)
        p <-
          round(lmtest::coeftest(x, vcov = sandwich::vcovHAC(x))[2, 4], 2)
      } else{
        co <-
          round(lmtest::coeftest(x, vcov = sandwich::vcovHC(x, type = "HC0"))[2, 1], 2)
        p <-
          round(lmtest::coeftest(x, vcov = sandwich::vcovHC(x, type = "HC0"))[2, 4], 2)
      }
      cop <-
        paste0(co, ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))))
      return(cop)
    }
    gam_edf <- function(x, dep_var, ind_var, control_vars, data) {
      f <- as.formula(paste0(
        paste0(dep_var, " ~ "),
        paste0("s(", ind_var, ", bs = 'cr') + "),
        paste0(control_vars, collapse = " + ")
      ))

      mod <-
        summary(mgcv::gam(f, data = data, family = binomial(link = "logit")))
      edf_p <- paste0(round(mod$edf, 2),
                      ifelse(
                        mod$s.pv < 0.001,
                        "***",
                        ifelse(mod$s.pv < 0.01, "**", ifelse(mod$s.pv < 0.05, "*", ""))
                      ))
      return(edf_p)
    }
    comp <- function(x, dep_var, ind_var, control_vars, data) {
      f3 <- as.formula(paste0(
        paste0(dep_var, " ~ "),
        paste0("s(", ind_var, ", bs = 'cr') + "),
        paste0(control_vars, collapse = " + ")
      ))

      mod <-
        mgcv::gam(f3, data = data, family = binomial(link = "logit"))
      an_p <- anova(x, mod, test = "Chisq")$`Pr(>Chi)`[2]
      return(round(an_p, 3))
    }

    df = data %>% dplyr::select(c(dep_var, ind_var, control_vars)) %>% tidyr::drop_na()
    df <- as.data.frame(df)
    if (nrow(df) < 5) {
      stop("data is too small after NA dropping (<5 n), the function is stopped")
    } else {
      print("data looks ok")
    }

    f <- as.formula(paste0(
      paste0(dep_var, " ~ "),
      paste0(ind_var, " + ", "I(", ind_var, "^2)", " + "),
      paste0(control_vars, collapse = " + ")
    ))

    model <- glm(
      f,
      data = df,
      family = binomial(link = "logit"),
      method = "brglm_fit",
      type = "MPL_Jeffreys"
    )

    if (vcov_type == "HAC") {
      coef_model <-
        lmtest::coeftest(model, vcov = sandwich::vcovHAC(model))
    } else{
      coef_model <-
        lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC0"))
    }

    f2 <- as.formula(paste0(
      paste0(dep_var, " ~ "),
      paste0(ind_var, " + "),
      paste0(control_vars, collapse = " + ")
    ))
    point <-
      round(-1 * model$coefficients[2] / (2 * model$coefficients[3]), 3)

    mod_l <-
      glm(
        f2,
        data = df %>% dplyr::filter(get(ind_var) <= point),
        family = binomial(link = "logit"),
        method = "brglm_fit",
        type = "MPL_Jeffreys"
      )
    mod_h <-
      glm(
        f2,
        data = df %>% dplyr::filter(get(ind_var) >= point),
        family = binomial(link = "logit"),
        method = "brglm_fit",
        type = "MPL_Jeffreys"
      )
    anova_p = comp(model, dep_var, ind_var, control_vars, df)
    anova_p = ifelse(is.na(anova_p), 1, anova_p)
    print(paste0("GAM vs polynomial term p-value: ", anova_p))

    for (i in control_vars) {
      p = FALSE
      if (is.factor(df[, i])) {
        p = TRUE
        break
      } else{
        next
      }
    }
    if (p == TRUE) {
      tryCatch(
        expr = {
          pred_gen <-
            as.data.frame(
              ggeffects::ggemmeans(
                rg.limit = 1e10,
                model = model,
                terms = paste0(ind_var, " [all]"),
                vcov.fun = "vcovHC",
                vcov.type = "HC0",
                ci.lvl = 0.95,
                back.transform = FALSE
              )
            )
          if (anova_p < 0.1) {
            f3 <- as.formula(paste0(
              paste0(dep_var, " ~ "),
              paste0("s(", ind_var, ", bs = 'cr') + "),
              paste0(control_vars, collapse = " + ")
            ))
            mod <-
              mgcv::gam(f3, data = df, family = binomial(link = "logit"))
            pred_add <-
              as.data.frame(
                ggeffects::ggemmeans(
                  rg.limit = 1e10,
                  model = mod,
                  terms = paste0(ind_var, " [all]"),
                  vcov.fun = "vcovHC",
                  vcov.type = "HC0",
                  ci.lvl = 0.95,
                  back.transform = FALSE
                )
              )
          }
          type_pred = "ggemmeans"
        },
        error = function(e) {
          type_pred = "ggpredict"
        }
      )
    } else {
      type_pred = "ggpredict"
    }
    print(type_pred)

    if (anova_p < 0.1) {
      f3 <- as.formula(paste0(
        paste0(dep_var, " ~ "),
        paste0("s(", ind_var, ", bs = 'cr') + "),
        paste0(control_vars, collapse = " + ")
      ))
      mod <-
        mgcv::gam(f3, data = df, family = binomial(link = "logit"))
    }

    if (type_pred == "ggpredict") {
      pred_gen <-
        as.data.frame(
          ggeffects::ggpredict(
            rg.limit = 1e10,
            model = model,
            terms = paste0(ind_var, " [all]"),
            vcov.fun = "vcovHC",
            vcov.type = "HC0",
            ci.lvl = 0.95,
            back.transform = FALSE
          )
        )
      if (anova_p < 0.1) {
        pred_add <-
          as.data.frame(
            ggeffects::ggpredict(
              rg.limit = 1e10,
              model = mod,
              terms = paste0(ind_var, " [all]"),
              vcov.fun = "vcovHC",
              vcov.type = "HC0",
              ci.lvl = 0.95,
              back.transform = FALSE
            )
          )
        pred_add$model = "GAM"
        pred_gen$model = "GLM"
        pred <- rbind(pred_add, pred_gen)
      } else {
        pred <- pred_gen
      }
      pred_low <-
        as.data.frame(
          ggeffects::ggpredict(
            rg.limit = 1e10,
            model = mod_l,
            terms = paste0(ind_var, " [all]"),
            vcov.fun = "vcovHC",
            vcov.type = "HC0",
            ci.lvl = 0.95,
            back.transform = FALSE
          )
        )
      pred_high <-
        as.data.frame(
          ggeffects::ggpredict(
            rg.limit = 1e10,
            model = mod_h,
            terms = paste0(ind_var, " [all]"),
            vcov.fun = "vcovHC",
            vcov.type = "HC0",
            ci.lvl = 0.95,
            back.transform = FALSE
          )
        )
    } else{
      pred_gen <-
        as.data.frame(
          ggeffects::ggemmeans(
            rg.limit = 1e10,
            model = model,
            terms = paste0(ind_var, " [all]"),
            vcov.fun = "vcovHC",
            vcov.type = "HC0",
            ci.lvl = 0.95,
            back.transform = FALSE
          )
        )
      if (anova_p < 0.1) {
        pred_add <-
          as.data.frame(
            ggeffects::ggemmeans(
              rg.limit = 1e10,
              model = mod,
              terms = paste0(ind_var, " [all]"),
              vcov.fun = "vcovHC",
              vcov.type = "HC0",
              ci.lvl = 0.95,
              back.transform = FALSE
            )
          )
        pred_add$model = "GAM"
        pred_gen$model = "GLM"
        pred <- rbind(pred_add, pred_gen)

      } else {
        pred <- pred_gen
      }
      pred_low <-
        as.data.frame(
          ggeffects::ggemmeans(
            rg.limit = 1e10,
            model = mod_l,
            terms = paste0(ind_var, " [all]"),
            vcov.fun = "vcovHC",
            vcov.type = "HC0",
            ci.lvl = 0.95,
            back.transform = FALSE
          )
        )
      pred_high <-
        as.data.frame(
          ggeffects::ggemmeans(
            rg.limit = 1e10,
            model = mod_h,
            terms = paste0(ind_var, " [all]"),
            vcov.fun = "vcovHC",
            vcov.type = "HC0",
            ci.lvl = 0.95,
            back.transform = FALSE
          )
        )
    }
    if (anova_p < 0.1) {
      pl <-
        ggplot(pred, aes(x = x, y = predicted, color = model)) + geom_line(size = 1.5) +
        theme_minimal() + geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                                      alpha = 0.25,
                                      linetype = "dashed") +
        xlab(ind_var) + ylab("Pr(y=1|X)") +
        geom_line(
          data = pred_low,
          aes(x = x, y = predicted, color = "low"),
          linetype = "dashed",
          size = 1.2
        ) +
        geom_line(
          data = pred_high,
          aes(x = x, y = predicted, color = "high"),
          linetype = "dashed",
          size = 1.2
        ) +
        theme(text = element_text(size = 13), axis.text = element_text(size = 11))

    } else {
      pl <-
        ggplot(pred, aes(x = x, y = predicted)) + geom_line(size = 1.5) +
        theme_minimal() + geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                                      alpha = 0.25,
                                      linetype = "dashed") +
        xlab(ind_var) + ylab("Pr(y=1|X)") +
        geom_line(
          data = pred_low,
          aes(x = x, y = predicted),
          linetype = "dashed",
          size = 1.2
        ) +
        geom_line(
          data = pred_high,
          aes(x = x, y = predicted),
          linetype = "dashed",
          size = 1.2
        ) +
        theme(text = element_text(size = 13), axis.text = element_text(size = 11))
    }

    if (boot == TRUE) {
      myCluster <-
        parallel::makeCluster(parallel::detectCores(), # number of cores to use
                              type = "PSOCK")
      doParallel::registerDoParallel(myCluster)
      btr <-
        foreach::foreach(
          i = 1:n,
          .combine = 'c',
          .packages = c("brglm2")
        ) %dopar% {
          suppressWarnings({
            g = glm(
              data = df[sample(nrow(df),
                               replace = TRUE,
                               size = nrow(df) * 0.6),],
              f,
              family = binomial(),
              method = "brglm_fit",
              type = "MPL_Jeffreys"
            )
          })
          - 1 * g$coefficients[2] / (2 * g$coefficients[3])

        }
      parallel::stopCluster(myCluster)
      meanb = mean(btr)
      q = quantile(btr, probs = c(0.025, 0.975))
    }

    if (save_plot) {
      ggsave(
        "U-shape plot",
        pl,
        device = "png",
        dpi = 600,
        units = "cm",
        width = 20,
        height = 10
      )
    }
    if (save_table) {
      stargazer::stargazer(
        coef_model,
        type = "html",
        out = "U-shape table.html",
        no.space = TRUE,
        star.cutoffs = c(0.05, 0.01, 0.001),
        add.lines	= list(
          c("U-shape test:", ""),
          c("....Extreme point", ifelse(
            boot == FALSE,
            point,
            paste(point,
                  "[",
                  round(meanb - q[1], 3),
                  "-",
                  round(meanb + q[2], 3),
                  "]")
          )),
          c("....Slope at Xlower", coef(mod_l, vcov_type = "HC")),
          c("....Slope at Xhigher", coef(mod_h, vcov_type = "HC")),
          c("GAM test:", ""),
          c(
            "....GAM edf",
            gam_edf(model, dep_var, ind_var, control_vars, df)
          ),
          c("....ANOVA p-value", anova_p),
          c("Nobs", nrow(model$model)),
          c("AIC", round(model$aic, 2))
        )
      )
    }

    return(list(
      stargazer::stargazer(
        coef_model,
        type = "text",
        no.space = TRUE,
        star.cutoffs = c(0.05, 0.01, 0.001),
        add.lines	= list(
          c("U-shape test:", ""),
          c("....Extreme point", ifelse(
            boot == FALSE, point,
            paste(point,
                  "[",
                  round(meanb - q[1], 3),
                  "-",
                  round(meanb + q[2], 3),
                  "]")
          )),
          c("....Slope at Xlower", coef(mod_l, vcov_type = "HC")),
          c("....Slope at Xhigher", coef(mod_h, vcov_type = "HC")),
          c("GAM test:", ""),
          c(
            "....GAM edf",
            gam_edf(model, dep_var, ind_var, control_vars, df)
          ),
          c("....ANOVA p-value", anova_p),
          c("Nobs", nrow(model$model)),
          c("AIC", round(model$aic, 2))
        )
      ),
      pl
    ))
  }
