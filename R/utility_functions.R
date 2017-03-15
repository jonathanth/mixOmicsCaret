#' Get best predictions
#'
#' Returns the CV predictions associated with the best performing tuning parameters. If there are multiple CV repeats, these are separated.
#'
#' @param trainobj The \code{train} object from \code{caret}, fit using mixOmics spls.
#' @param rep If \code{what = "CV"}, and trainobj was fit using repeated cross-validation, choose a repeat (e.g. "Rep1") or leave as NA for all repeats (default).
#' @param ncomp Manually select CV predictions with this parameter.
#' @param keepX Manually select CV predictions with this parameter.
#' @param keepY Manually select CV predictions with this parameter.
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
