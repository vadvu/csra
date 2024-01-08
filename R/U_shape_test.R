#' U-shape test for rare events logistic regression
#'
#' @param data Dataframe
#' @param dep Character. Dependent variable.
#' @param ind Character. Independent variable.
#' @param cnt Vector of characters with names of control variables.
#' @param mod Character. Type of model (now just `brglm` is supported)
#' @param boot Character. Should middle point and "lines" be calculated via bootstrap? If no - "none", if yes - one can choose bootstrap type
#' @param factor Character. Only if `boot = "factor"`.
#' @param n Number of bootstrap draws
#' @param HC Logical. Should heteroscedasticity-consistent SE be used?
#' @param const Logical. Should constant be included in the models?
#' @param table_remove Vector of variables names that should be excluded from the regression table.
#' @param plot Logical. Should plot be depicted?
#' @param tab_save Logical. Should table be saved? If yes - table is saved in working directory.
#'
#' @references
#'
#' Lind, J. T., & Mehlum, H. (2010). With or without U? The appropriate test for a U‐shaped relationship. *Oxford bulletin of economics and statistics*, *72(1)*, 109-118.
#'
#' Simonsohn, U. (2018). Two lines: A valid alternative to the invalid testing of U-shaped relationships with quadratic regressions. *Advances in Methods and Practices in Psychological Science*, *1(4)*, 538-555.
#'
#' @return Plot and regression table with u-shape test summary.
#' @import mgcv brglm lmtest tidyr dplyr sandwich stargazer doParallel foreach ggeffects parallel ggplot2
#' @export
#'
U_shape_test <- function(data, dep, ind, cnt, mod = "brglm",
                         boot = c("none", "nonparam", "factor"),
                         factor = NULL, n = 1000, HC = T, const = T,
                         table_remove = NULL, plot = F, tab_save = F){
  #checks
  if(F %in% (c(dep, ind, cnt) %in% colnames(data))){
    stop("Variables are not in data")
  }

  #formulas
  f2 = as.formula(
    paste0(dep, "~",
           ind, "+I(", ind, "^2)+",
           paste0(cnt, collapse = "+"),
           ifelse(const,"+1","-1"))
  )

  f1 = as.formula(
    paste0(dep, "~",
           ind, "+",
           paste0(cnt, collapse = "+"),
           ifelse(const,"+1","-1"))
  )

  fg = as.formula(
    paste0(dep, "~",
           "s(", ind,", bs = 'cr')", "+",
           paste0(cnt, collapse = "+"),
           ifelse(const,"+1","-1"))
  )

  #main model
  suppressWarnings({
    main.mod = brglm::brglm(data = data, f2, family = binomial(), method = "brglm.fit", pl = T)
  })

  #HC main
  if(HC){
    main.mod1 <- lmtest::coeftest(main.mod, vcov = sandwich::vcovHC(main.mod, type = "HC1"))
  }else{
    main.mod1 <- lmtest::coeftest(main.mod)
  }

  #ex boot
  if (boot == "none"){
    ext = main.mod$coefficients[ind]/(-2*main.mod$coefficients[paste0("I(",ind,"^2)")])

    suppressWarnings({
      low = brglm(data = dplyr::filter(data, get(ind) <= ext), f1, family = binomial(), method = "brglm.fit", pl = T)
      high = brglm(data = dplyr::filter(data, get(ind) >= ext), f1, family = binomial(), method = "brglm.fit", pl = T)
    })

    if(HC){
      low = lmtest::coeftest(low, vcov = sandwich::vcovHC(low, type = "HC1"))
      low = paste0(round(low[ind,1],2)," (p = ", round(low[ind,4],4) , ")")
      high = lmtest::coeftest(high, vcov = sandwich::vcovHC(high, type = "HC1"))
      high = paste0(round(high[ind,1],2)," (p = ", round(high[ind,4],4) , ")")
    }else{
      low = summary(low)$coefficients
      low = paste0(round(low[ind,1],2)," (p = ", round(low[ind,4],4) , ")")
      high = summary(high)$coefficients
      high = paste0(round(high[ind,1],2)," (p = ", round(high[ind,4],4) , ")")
    }

    ext = round(ext,2)

  }else if (boot == "nonparam"){

    myCluster <- parallel::makeCluster(parallel::detectCores(), type = "PSOCK")
    doParallel::registerDoParallel(myCluster)

    btr <- foreach::foreach(i = 1:n,
                            .combine = 'rbind',
                            .packages = c("brglm", "tidyr", "dplyr"),
                            .errorhandling = "remove") %dopar% {
                              dd <- data %>% tidyr::drop_na(c(ind, dep, cnt))
                              dd = dd[sample(1:nrow(dd), size=nrow(dd), replace=T),]

                              iglm = brglm::brglm(data = dd, f2,
                                           family = binomial(link = "logit"),  method = "brglm.fit", pl = T)

                              ex <- iglm$coefficients[ind]/(-2*iglm$coefficients[paste0("I(",ind,"^2)")])

                              iglm = brglm::brglm(data = dplyr::filter(dd, get(ind) <= ex), f1,
                                           family = binomial(link = "logit"),  method = "brglm.fit", pl = T)
                              low = iglm$coefficients[ind]

                              iglm = brglm::brglm(data = dplyr::filter(dd, get(ind) >= ex), f1,
                                           family = binomial(link = "logit"),  method = "brglm.fit", pl = T)
                              high = iglm$coefficients[ind]

                              matrix(c(ex,low,high), nrow = 1)
                            }

    ext = round(quantile(btr[,1], c(0.025, 0.5, 0.925), na.rm = T), 2)
    ext = paste0(ext[2], " [",ext[1],"-",ext[3], "]")

    low = round(quantile(btr[,2], c(0.025, 0.5, 0.925), na.rm = T), 2)
    low = paste0(low[2], " [",low[1],"-",ifelse(low[3]<0, paste0("(", low[3], ")"), low[3]), "]")

    high = round(quantile(btr[,3], c(0.025, 0.5, 0.925), na.rm = T), 2)
    high = paste0(high[2], " [",high[1],"-", ifelse(high[3]<0, paste0("(", high[3], ")"), high[3]), "]")
  } else if (boot == "factor"){

    myCluster <- parallel::makeCluster(parallel::detectCores(), type = "PSOCK")
    doParallel::registerDoParallel(myCluster)

    btr <- foreach::foreach(i = unique(data[[factor]]),
                            .combine = 'rbind',
                            .packages = c("brglm", "tidyr", "dplyr"),
                            .errorhandling = "remove") %dopar% {
                              dd <- data %>% tidyr::drop_na(c(ind, dep, cnt)) %>% dplyr::filter(get(factor) != i)

                              iglm = brglm::brglm(data = dd, f2,
                                           family = binomial(link = "logit"),  method = "brglm.fit", pl = T)

                              ex <- iglm$coefficients[ind]/(-2*iglm$coefficients[paste0("I(",ind,"^2)")])

                              iglm = brglm::brglm(data = dplyr::filter(dd, get(ind) <= ex), f1,
                                           family = binomial(link = "logit"),  method = "brglm.fit", pl = T)
                              low = iglm$coefficients[ind]

                              iglm = brglm::brglm(data = dplyr::filter(dd, get(ind) >= ex), f1,
                                           family = binomial(link = "logit"),  method = "brglm.fit", pl = T)
                              high = iglm$coefficients[ind]

                              matrix(c(ex,low,high), nrow = 1)
                            }

    parallel::stopCluster(myCluster)

    ext = round(quantile(btr[,1], c(0.025, 0.5, 0.925), na.rm = T), 2)
    ext = paste0(ext[2], " [",ext[1],"-",ext[3], "]")

    low = round(quantile(btr[,2], c(0.025, 0.5, 0.925), na.rm = T), 2)
    low = paste0(low[2], " [",low[1],"-",ifelse(low[3]<0, paste0("(", low[3], ")"), low[3]), "]")

    high = round(quantile(btr[,3], c(0.025, 0.5, 0.925), na.rm = T), 2)
    high = paste0(high[2], " [",high[1],"-", ifelse(high[3]<0, paste0("(", high[3], ")"), high[3]), "]")

  }

  #gam
  suppressWarnings({
    gam.mod <- mgcv::gam(data = data, fg, family = binomial())
  })
  edf = summary(gam.mod)$s.table[,c(1,4)]
  edf = paste0(round(edf[1],2), " (p = ", round(edf[2],4), ")")

  #mod comparison
  diff_aic = AIC(main.mod)-AIC(gam.mod)
  #anova = anova(main.mod, gam.mod)

  #table
  stargazer::stargazer(type = "text",
                       main.mod1, no.space = T,
                       add.lines = list(
                         c("N", nobs(main.mod1)),
                         c("AIC", round(AIC(main.mod1),2)),
                         c("U-shape test:",""),
                         c("Extremum", ext),
                         c("Xlower", low),
                         c("Xhigher", high),
                         c("GAM edf", edf),
                         c("AIC glm - AIC gam", round(diff_aic,2))
                       ),
                       omit = table_remove)

  if(tab_save == T){
    checkname = T
    i=0
    while(checkname){
      name = paste0("out_", Sys.Date(),"_",i+1, ".html")
      checkname = name %in% list.files()
      i = i+1
    }

    stargazer::stargazer(type = "html",
                         main.mod1, no.space = T,
                         add.lines = list(
                           c("N", nobs(main.mod1)),
                           c("AIC", round(AIC(main.mod1),2)),
                           c("U-shape test:",""),
                           c("Extremum", ext),
                           c("Xlower", low),
                           c("Xhigher", high),
                           c("GAM edf", edf),
                           c("AIC glm - AIC gam", round(diff_aic,2))
                         ),
                         omit = table_remove, out = name)
  }

  #plots
  if (plot){
    plot.df = as.data.frame(ggeffects::ggemmeans(main.mod, terms = ind,
                                                 vcov_fun = "vcovHC", vcov_type = "HC1",
                                                 rg.limit = 10e6))
    plot.df$group = "glm with polynomial term"

    plot.df.gam = as.data.frame(ggeffects::ggemmeans(gam.mod, terms = ind,
                                                     vcov_fun = "vcovHC", vcov_type = "HC1",
                                                     rg.limit = 10e6))
    plot.df.gam$group = "gam with smoothed term"

    plot.df <- rbind(plot.df, plot.df.gam)

    ggplot(plot.df, aes(x, predicted, color = group))+
      geom_line(size = 1.3)+
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, size = 0.1)+
      theme_classic()+
      labs(y = dep, x = ind, color = "Model:")
  }
}
