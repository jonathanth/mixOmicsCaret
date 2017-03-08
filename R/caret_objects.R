#' Returns a list object which caret uses to interface with mixOmics.
#'
#' @param ... Ignored.
#'
#' @return A \code{list} object that can be used with \code{caret::train()}.
#' @examples
#' get_mixOmics_sPLS()
#' @export
#'
get_mixOmics_sPLS <- function(...) { list(
  library = c("mixOmics", "tidyverse"),
  type = c("Regression"),
  parameters = data.frame(parameter = c("ncomp", "keepX", "keepY"),
                          class = rep("numeric", 3),
                          label = c("ncomp", "keepX", "keepY"),
                          stringsAsFactors = F),
  grid = function(x, y, len = 5, search = "grid") {
    ncomp <- 1:min(length(unique(y))-1, 5)
    keepX <- ncol(x)
    keepY <- 1
    if(!is.null(ncol(y)))
      keepY <- ncol(y)
    ## To use grid search:
    if(search == "grid") {
      keepX <- unique(round(exp(seq(log(1),log(ncol(x)), length.out = len))))
      keepY <- 1
      if(!is.null(ncol(y)))
        keepY <- unique(round(exp(seq(log(1),log(ncol(y)), length.out = len))))

    }
    out <- setNames(expand.grid(ncomp, keepX, keepY), c("ncomp", "keepX", "keepY"))
    out
  },
  fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    dots <- substitute(list(...))[-1]
    extra_args <- sapply(dots, deparse)
    fixX <- as.numeric(eval(parse(text = extra_args["fixX"])))
    fixY <- as.numeric(eval(parse(text = extra_args["fixY"])))
    keepX <- rep(param$keepX, param$ncomp)
    if(!is.null(fixX)){
      if(length(fixX) > 0){
        if(!is.na(fixX)){
          replaceX <- min(length(fixX), param$ncomp)
          keepX[1:replaceX] <- fixX[1:replaceX]
        }
      }
    }
    keepY <- rep(param$keepY, param$ncomp)
    if(!is.null(fixY)){
      if(length(fixY) > 0){
        if(!is.na(fixY)){
          replaceY <- min(length(fixY), param$ncomp)
          keepY[1:replaceY] <- fixY[1:replaceY]
        }
      }
    }
    fitfit <- mixOmics::spls(x, y,
                             ncomp = param$ncomp,
                             keepX = keepX,
                             keepY = keepY)
    fitfit
  },
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL){
    predict(modelFit, newdata)$predict[,1,modelFit$ncomp]
  },
  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL){
    predict(modelFit, newdata)$predict[,,modelFit$ncomp]
  },
  sort = function(x) x[order(x$keepX, x$keepY, x$ncomp),],
  levels = function(x) levels(x$Y),
  loop = NULL, # Can be used to get predictions for successive ncomp values with no refitting
  label = "sparse PLS (mixOmics)",
  predictors = function(model){
    model$loadings$X %>%
      data.frame %>%
      mutate(var = rownames(.)) %>%
      gather(comp, value)	%>%
      filter(value != 0) %>%
      .$var %>%
      unique
  }
  # tags,
  # oob,
  # notes,
  # check,
  # varImp
)
}
