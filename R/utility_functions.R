#' Get best predictions
#'
#' Returns the CV predictions associated with the best performing tuning parameters. If there are multiple CV repeats, these are separated.
#'
#' @param trainobj The \code{train} object from \code{caret}, fit using mixOmics spls.
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
get_best_predictions <- function(trainobj){
  if(trainobj$modelInfo$label != "sparse PLS (mixOmics)")
    simpleError("Please supple train object that uses mixOmics spls,.")
  bt <- trainobj$bestTune
  trainobj$pred %>%
    filter(ncomp == bt$ncomp, keepX == bt$keepX, keepY == bt$keepY) %>%
    separate(Resample, c("Fold", "Rep"))
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
                        remove_empty = TRUE) {
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
                             param = trainobj$bestTune,
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
