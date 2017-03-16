#' Get best predictions
#'
#' Returns the CV predictions associated with the best performing tuning parameters. If there are multiple CV repeats, these are separated.
#'
#' @param trainobj The \code{train} object from \code{caret}, fit using mixOmics spls.
#' @param rep If trainobj was fit using repeated cross-validation, choose a repeat (e.g. "Rep1") or leave as NA for all repeats (default).
#' @param ncomp Manually select CV predictions with this parameter, instead of the best one from \code{train}.
#' @param keepX Manually select CV predictions with this parameter, instead of the best one from \code{train}.
#' @param keepY Manually select CV predictions with this parameter, instead of the best one from \code{train}.
#' @return A \code{data.frame} with the variables
#' \itemize{
#'   \item \code{pred} - the predicted values
#'   \item \code{obs} - the observed values
#'   \item \code{ncomp} - Tuning variable
#'   \item \code{keepX} - Tuning variable
#'   \item \code{keepY} - Tuning variable
#'   \item \code{fold} - Cross-validation fold
#'   \item \code{rep} - Repeat number (for repeated CV)
#' }
#' @examples
#' library(caret)
#' x <- data.frame(matrix(rnorm(1000),nrow = 100))
#' y <- rnorm(100)
#' PLS <- train(x = x, y = y, method = get_mixOmics_spls())
#' getBestPredictions(PLS)
#' @export
get_best_predictions <- function(trainobj,
                                 rep = NA,
                                 ncomp = NA,
                                 keepX = NA,
                                 keepY = NA){
  if(trainobj$modelInfo$label != "sparse PLS (mixOmics)")
    warning("Please supply train object that uses mixOmics spls. Output may be unreliable.")
  bt <- trainobj$bestTune
  if(!is.na(ncomp))
    bt$ncomp <- ncomp
  if(!is.na(keepX))
    bt$keepX <- keepX
  if(!is.na(keepY))
    bt$keepY <- keepY
  filter_terms <- paste0(names(bt), " == ", bt, collapse = " & ")

  out <- trainobj$pred %>%
    filter_(filter_terms) %>%
    separate(Resample, c("Fold", "Rep")) %>%
    arrange(Rep, rowIndex)
  if(!is.na(rep))
    out <- out %>% filter(Rep == rep)
  if(nrow(out) == 0)
    stop("Invalid parameter specification. Please check and try again, or refit train object with these parameters.")
  out
}

#' Get loadings
#'
#' Returns the loadings of a mixOmics spls train object.
#' Either from the final model fitted on the full training data, or the median and sd across CV folds from the best performing tuning parameters. If there are multiple CV repeats, these are separated or can be chosen.
#'
#' @param trainobj The \code{train} object from \code{caret}, fit using mixOmics spls.
#' @param what Which model to return loadings from. Either \code{"finalModel"} or \code{"CV"}.
#' @param xykeep Return loadings from x or y? Either \code{"x"}, \code{"y"}, or \code{"both"}.
#' @param rep If \code{what = "CV"}, and trainobj was fit using repeated cross-validation, choose a repeat (e.g. "Rep1") or leave as NA for all repeats (default).
#' @param remove_empty Remove loadings with a value of zero from output.
#' @param ncomp Manually select CV predictions with this parameter.
#' @param keepX Manually select CV predictions with this parameter.
#' @param keepY Manually select CV predictions with this parameter.
#' @return A \code{data.frame} with the variables
#' \itemize{
#'   \item \code{var} - the variable
#'   \item \code{comp} - the component
#'   \item \code{loading} - the loading value (median if across CV folds / reps)
#'   \item \code{sd} - the standard deviation of the loading (if across CV folds / reps, NA otherwise)
#'   \item \code{xy} - is it a loading on x or y?
#' }
#' @examples
#' library(caret)
#' x <- data.frame(matrix(rnorm(1000),nrow = 100))
#' y <- rnorm(100)
#' PLS <- train(x = x, y = y, method = get_mixOmics_spls())
#' get_loadings(PLS)
#'
#' get_loadings(PLS, "CV") %>%
#'   ggplot(aes(var, loading, ymin = loading - sd, ymax = loading + sd)) +
#'     facet_wrap(~ comp, scales = "free_x") +
#'     geom_errorbar() +
#'     geom_bar(stat = "identity")
#' @export
get_loadings <- function(trainobj,
                        what = c("finalModel", "CV"),
                        xykeep = c("x", "y", "both"),
                        rep = NA,
                        remove_empty = TRUE,
                        ncomp = NA,
                        keepX = NA,
                        keepY = NA) {
  if(what[1] == "finalModel"){
    loadingsx <- trainobj$finalModel$loadings$X %>%
      data.frame %>%
      mutate(var = rownames(.), sd = NA) %>%
      gather(comp, loading, -var, -sd)
    loadingsy <- trainobj$finalModel$loadings$Y %>%
      data.frame %>%
      mutate(var = rownames(.), sd = NA) %>%
      gather(comp, loading, -var, -sd)
  }

  if(what[1] == "CV"){
    bt <- trainobj$bestTune
    if(!is.na(ncomp))
      bt$ncomp <- ncomp
    if(!is.na(keepX))
      bt$keepX <- keepX
    if(!is.na(keepY))
      bt$keepY <- keepY
    control <- trainobj$control
    keep <- rep(TRUE, length(control$index))
    if(!is.na(rep))  # Select one rep.
      keep <- grepl(rep, names(control$index))
    index <- control$index[keep]
    indexOut <- control$indexOut[keep]
    seeds <- control$seeds[keep]
    refits <- lapply(seq(along = index), function(i){
      trainobj$modelInfo$fit(trainobj$finalModel$X[index[[i]],],
                             trainobj$finalModel$Y[index[[i]],],
                             param = bt,
                             trainobj$dots)
    })
    loadingsx <- lapply(seq_along(refits), function(i){
      refits[[i]]$loadings$X %>%
        data.frame %>%
        mutate(var = rownames(.)) %>%
        gather(comp, loading, -var)
    }) %>% bind_rows %>%
      group_by(comp, var) %>%
      summarize(sd = sd(loading), loading = median(loading)) %>%
      ungroup
    loadingsy <- lapply(seq_along(refits), function(i){
    refits[[i]]$loadings$Y %>%
        data.frame %>%
        mutate(var = rownames(.)) %>%
        gather(comp, loading, -var)
    }) %>% bind_rows %>%
      group_by(comp, var) %>%
      summarize(sd = sd(loading), loading = median(loading)) %>%
      ungroup
  }

  if(!what[1] %in% c("finalModel", "CV")){
    stop("Please choose \"finalModel\" or \"CV\".")
  }

  out <- bind_rows(x = loadingsx, y = loadingsy, .id = "xy") %>%
    select(var, comp, loading, sd, xy)

  if(remove_empty)
    out <- out %>% filter(loading != 0)

  if(xykeep[1] %in% c("x", "y"))
    return(out %>% filter(xy == xykeep[1]))

  out
}

# Utility for permutation_spls
.getrsq <- function(x, y, ...){
  if(is.matrix(x) & ncol(x) == 1){
    return(summary(lm(y ~ x))$r.squared)
  } else {
    m <- matrix(0, ncol = ncol(x), nrow = ncol(x))
    for(i in 1:ncol(x))
      m[i,i] <- summary(lm(y[,i] ~ x[,i]))$r.squared
    m
  }
}
# Utility for permutation_spls
.getp <- function(x, y, ...){
  if(is.matrix(x) & ncol(x) == 1){
    return(coef(summary(lm(y ~ x)))[2,"Pr(>|t|)"])
  } else {
    m <- matrix(0, ncol = ncol(x), nrow = ncol(x))
    for(i in 1:ncol(x))
      m[i,i] <- coef(summary(lm(y[,i] ~ x[,i])))[2,"Pr(>|t|)"]
    m
  }
}
# Utility for permutation_spls
.getrmse <- function(x, y, ...){
  if(is.matrix(x) & ncol(x) == 1){
    return(sqrt(mean( resid(lm(y ~ x))^2 )))
  } else {
    m <- matrix(0, ncol = ncol(x), nrow = ncol(x))
    for(i in 1:ncol(x))
      m[i,i] <- sqrt(mean( resid(lm(y[,i] ~ x[,i]))^2))
    m
  }
}

#' Perform permutation test
#'
#' Performs a permutation test of an spls fit or a train object. Uses hyperparameters from the fit (or optimal  \code{caret::train}-fit), which may be anti-conservative (overfit to data) so beware.
#'
#'
#' @param fit A model from \code{mixOmics::spls} or a \code{train} object from \code{caret}, fit using mixOmics spls.
#' @param permutations The number of permutations to perform.
#' @param tester A function used to compare the X and Y components, such as \code{cor}, or a character variable with one of the built-in methods ("cor", "rsq", "p", or "rmse"). Custom functions must return symmetric matrix (like \code{cor})
#' @param ... Additional arguments to \code{tester}, such as \code{method = "spearman"}.
#' @param comp The component(s) that we wish to compute p values for as a numeric vector (e.g. 2:4), or "all" (default: "all").
#' @param seed Random seed.
#' @param strata Value to constrain permutations.
#' @param fitfunc The function used to compute the permuted fits, currently only works with \code{mixOmics::spls}.
#' @return A \code{list} with the elements
#' \itemize{
#'   \item \code{p.values} - the computed p values per component
#'   \item \code{true} - the true values (from \code{tester}) that will be compared to a null distribution
#'   \item \code{track} - the permutation-distribution of values (from \code{tester}) that the true value was compared against.
#'   \item \code{seed} - the random seed used, for reproducibility.
#' }
#' @examples
#' library(caret)
#' x <- data.frame(matrix(rnorm(1000),nrow = 100))
#' y <- rnorm(100)
#' PLS <- train(x = x, y = y, method = get_mixOmics_spls())
#' perms <- permutation_spls(PLS)
#' @export
permutation_spls <- function(fit, permutations = 1000, tester = cor, ..., comp = "all", seed = as.integer(Sys.time()), strata = NULL, fitfunc = mixOmics::spls) {
  if("train" %in% class(fit)){
    fitfunc <- fit$modelInfo$fit
    fit <- fit$finalModel
    fitfunc <- mixOmics::spls
    # Need to figure out if there is a need to flexibly fit different kinds of models
  }
  flip_p <- FALSE
  if(is.character(tester))
    tester <- tolower(tester)
  if(identical(tester, "cor"))
    tester <- cor
  if(identical(tester, "rsq"))
    tester <- .getrsq
  if(identical(tester, "p")){
    tester <- .getp
    flip_p <- TRUE
  }
  if(identical(tester, "rmse")){
    tester <- .getrmse
    flip_p <- TRUE
  }

  if(comp == "all")
    comp <- 1:fit$ncomp # need to initialize after extracting from train

  require(vegan)
  require(doMC)
  set.seed(seed)
  message("Random seed = ", seed)
  message("Number of permutations = ", permutations)

  if(length(comp) == 1) {
    true <- tester(fit$variates$X[,comp], fit$variates$Y[,comp], ...)
    message("true = ", signif(true, 3))
    track <- numeric(permutations)
    permat <- vegan:::getPermuteMatrix(permutations, nrow(fit$variates$X), strata)

    if(getDoParWorkers() > 1){
      message("Running in parallel mode with ", getDoParWorkers(), " threads.")
      track <- foreach(i=1:permutations, .combine = 'c') %dopar% {
        permpls <- fitfunc(fit$X[permat[i,],],
                           fit$Y,
                           ncomp = fit$ncomp,
                           keepX = fit$keepX,
                           keepY = fit$keepY)
        tester(permpls$variates$X[,comp], permpls$variates$Y[,comp], ...)
      }
    } else {
      message("Running in sequential mode. Change with registerDoMC([number of cores]).")
      for(i in 1:permutations){
        if(i %in% round(quantile(1:permutations, c(0.02, 0.05, 0.1, 0.2, 0.4, 0.75)))){
          p.values <- mean(true < track, na.rm = T)
          if(flip_p)
            p.values <- 1 - p.values
          message(i, "(", round(i/permutations,2)*100, "%) preliminary p = ",
                  signif(p.values, 4))
        }
        permpls <- fitfunc(fit$X[permat[i,],],
                           fit$Y,
                           ncomp = fit$ncomp,
                           keepX = fit$keepX,
                           keepY = fit$keepY)
        track[i] <- tester(permpls$variates$X[,comp], permpls$variates$Y[,comp], ...)
      }
    }
    p.values <- sapply(comp, function(comp) mean(true[comp] < track[comp], na.rm = T))
    if(flip_p)
      p.values <- 1 - p.values
    message("Final p = ", signif(p.values, 4))
    return(list(p.values = p.values, true = true, track = track, seed = seed))
  }
  if(length(comp) > 1) {
    true <- tester(fit$variates$X[,comp], fit$variates$Y[,comp], ...) %>% diag
    message("true = ", paste0(signif(true, 3), sep = "\t"))
    track <- matrix(NA, nrow = permutations, ncol = length(comp))
    permat <- vegan:::getPermuteMatrix(permutations, nrow(fit$variates$X), strata)

    if(getDoParWorkers() > 1){
      message("Running in parallel mode with ", getDoParWorkers(), " threads.")
      track <- foreach(i=1:permutations, .combine = 'rbind') %dopar% {
        permpls <- fitfunc(fit$X[permat[i,],],
                           fit$Y,
                           ncomp = fit$ncomp,
                           keepX = fit$keepX,
                           keepY = fit$keepY)
        tester(permpls$variates$X[,comp], permpls$variates$Y[,comp], ...) %>% diag
      }
    } else {
      message("Running in sequential mode. Change with registerDoMC([number of cores]).")
      for(i in 1:permutations){
        if(i %in% round(quantile(1:permutations, c(0.02, 0.05, 0.1, 0.2, 0.4, 0.75)))){
          p.values <- sapply(comp,function(comp) mean(true[comp] < track[,comp], na.rm = T))
          if(flip_p)
            p.values <- 1 - p.values
          message(i, "(", round(i/permutations,2)*100, "%) preliminary p = ",
                  paste0(signif(p.values, 4), sep = "\t"))
        }
        permpls <- fitfunc(fit$X[permat[i,],],
                           fit$Y,
                           ncomp = fit$ncomp,
                           keepX = fit$keepX,
                           keepY = fit$keepY)
        track[i,] <- tester(permpls$variates$X[,comp], permpls$variates$Y[,comp], ...) %>% diag
      }
    }
    p.values <- sapply(comp, function(comp) mean(true[comp] < track[,comp], na.rm = T))
    if(flip_p)
      p.values <- 1 - p.values
    message("Final p = ", paste0(signif(p.values, 4), sep = "\t"))
    return(list(p.values = p.values, true = true, track = track, seed = seed))
  }
}
