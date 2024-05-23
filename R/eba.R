
#' Extreme boundary analysis for binary outcome
#'
#' @param y Character. Dependent variable.
#' @param x Character. Independent variable.
#' @param const Array with characters. Set of constant controls.
#' @param control Array with characters. Set of controls.
#' @param data Dataframe.
#' @param nvar Number. Number of control variables (not including constant controls) to make combinations.
#' @param model Model type. Now only "logit" for logistic regression is supported.
#' @param cl Number. Number of clusters to use for computations. Default is `NULL` for 1 cluster.
#'
#' @return Dataframe with results
#' @import dplyr tidyr foreach doParallel stats lmtest sandwich
#' @export
eba <- function(y, x, const, control, data, nvar, model = "logit", cl = NULL){

  #### checks
  `%notin%` <- Negate(`%in%`)

  if(y %notin% colnames(data) | x %notin% colnames(data)){
    stop("Dependent (Y) or Independent (X) variables are not found in the provided data")
  }
  if(any(const %notin% colnames(data)) | any(control %notin% colnames(data))){
    stop("Some of constant ot control variables are not found in the provided data")
  }
  if(nvar <=0){
    stop("nvar should be >0")
  }
  if(length(control)<nvar){
    stop("nvar should be <= number of controls")
  }

  #### functions
  gmeans <- function(x){
    df <- data.frame(tres = seq(0.01, 0.1, 0.005), gmean = NA)
    for(tres in df$tres){
      predicted_values<-ifelse(predict(x,type="response")>tres,1,0)
      actual_values<-x$model[,1]
      conf_matrix<-table(predicted_values,actual_values)
      conf_matrix[is.na(conf_matrix)] = 0
      sens <- conf_matrix[2,2]/(conf_matrix[1,2]+conf_matrix[2,2])
      spec <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])
      df[df$tres==tres,2] <- (sens*spec)^0.5
    }
    return(df[which(df$gmean == max(df$gmean)),])
  }

  comb.many <- function(x,n){
    for(i in n){
      if(i == n[1]){
        comb <- list()
      }
      comb[[paste0(i)]] <- t(combn(x, i))
    }
    return(comb)
  }

  #### main
  combinations <- comb.many(control, nvar)
  print(paste0("Total numbers of combinations to estimate: ",nrow(combinations[[1]])))

  if(model == "logit"){
    null.model = logLik(glm(data = data, family = "binomial",
                            formula = as.formula(paste0(y,"~",paste0(const, collapse = "+"))))
                        )
    if(is.null(cl)){
      cl <- 1
    }
    cl <- parallel::makeCluster(cl)
    doParallel::registerDoParallel(cl)

    `%dopar%` <- foreach::`%dopar%`

    fin <- foreach::foreach(i = 1:nrow(combinations[[1]]),
                   .combine = rbind,
                   .packages = c("sandwich",
                                 "lmtest",
                                 "dplyr"),
                   .export = c("data",
                               "x",
                               "y",
                               "const",
                               "control",
                               "combinations",
                               "null.model")
                   ) %dopar% {
                     form = as.formula(
                       paste0(
                         y, "~",
                         x, "+",
                         paste0(const, collapse = "+"),
                         "+",
                         paste0(combinations[[1]][i,], collapse = "+")
                         )
                       )

                     mi <- glm(data = data,
                               family = binomial(link = 'logit'),
                               formula = form)
                     full.model = logLik(mi)

                     mi <- lmtest::coeftest(mi)
                     eba.out <- data.frame(var = x,
                                           coef = mi[2,1],
                                           se = mi[2,2],
                                           stat = 1-(full.model/null.model))


                   }
    doParallel::stopImplicitCluster()

    fin <- fin %>%
      tidyr::drop_na() %>%
      dplyr::group_by(var) %>%
      dplyr::mutate(w = stat/sum(stat),
             b.mean = sum(coef*w),
             se.mean = sum( (se^2)*w )^0.5,
             p.mean =2*pnorm(-abs(b.mean/se.mean))) %>%
      as.data.frame()
  }

  return(fin)

}

