#' Returns a list object which caret uses to interface with mixOmics.
#'
#' @param ... Ignored.
#' @return A \code{list} object that can be used with \code{caret::train()}.
#' @examples
#' library(caret)
#' x <- data.frame(matrix(rnorm(1000),nrow = 100))
#' y <- rnorm(100)
#' train(x = x, y = y, method = get_mixOmics_sPLS())
#' @export
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
			message(str(out))
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
   			out <- predict(modelFit, newdata)$predict[,1,modelFit$ncomp]
   			if(!is.null(submodels)) {
   				tmp <- out
   				out <- vector(mode = "list", length = nrow(submodels) + 1)
   				out[[1]] <- tmp
   				for(j in seq(along = submodels$ncomp)) {
                    out[[j+1]] <- predict(modelFit, newdata)$predict[,1,submodels$ncomp[j]]
                }
   			}
   			out
		},
	prob = function(modelFit, newdata, preProc = NULL, submodels = NULL){
   			predict(modelFit, newdata)$predict[,,modelFit$ncomp]
		},
	sort = function(x) x[order(x$keepX, x$keepY, x$ncomp),],
	levels = function(x) levels(x$Y),
	loop = function(grid) {
		loop <- grid %>% filter(ncomp == max(ncomp))
        submodels <- list()
        for(i in 1:nrow(loop))
        	submodels[[i]] <- data.frame(ncomp = 1:(max(grid$ncomp)-1))
		list(loop = loop, submodels = submodels)
		}, # Can be used to get predictions for successive ncomp values with no refitting
	label = "sparse PLS (mixOmics)",
	predictors = function(model){
		model$loadings$X %>%
			data.frame %>%
			mutate(var = rownames(.)) %>%
			gather(comp, value)	%>%
			filter(value != 0) %>%
			.$var %>%
			unique
		},
	VarImp = function(model, estimate = NULL, ...) {
		loadingsX <- model$loadings$X %>% data.frame %>%
			mutate(var = rownames(.)) %>%
			gather(comp, loading, -var)
		loadingsY <- model$loadings$Y %>% data.frame %>%
			mutate(var = rownames(.)) %>%
			gather(comp, loading, -var)
		loadings <- bind_rows(X = loadingsX, Y = loadingsY, .id = "XY")

		model$loadings$X %>% abs %>%
			rowSums %>%
			data.frame(Overall = .)
		# modelCoef <- coef(model, intercept = FALSE, comps = 1:model$ncomp)
		# perf <- MSEP(model)$val

		# nms <- dimnames(perf)
		# if(length(nms$estimate) > 1) {
		#   pIndex <- if(is.null(estimate)) 1 else which(nms$estimate == estimate)
		#   perf <- perf[pIndex,,,drop = FALSE]
		# }
		# numResp <- dim(modelCoef)[2]

		# if(numResp <= 2) {
		#   modelCoef <- modelCoef[,1,,drop = FALSE]
		#   perf <- perf[,1,]
		#   delta <- -diff(perf)
		#   delta <- delta/sum(delta)
		#   out <- data.frame(Overall = apply(abs(modelCoef), 1,
		#                                     weighted.mean, w = delta))
		# } else {
		#   perf <- -t(apply(perf[1,,], 1, diff))
		#   perf <- t(apply(perf, 1, function(u) u/sum(u)))
		#   out <- matrix(NA, ncol = numResp, nrow = dim(modelCoef)[1])

		#   for(i in 1:numResp) {
		#     tmp <- abs(modelCoef[,i,, drop = FALSE])
		#     out[,i] <- apply(tmp, 1,  weighted.mean, w = perf[i,])
		#   }
		#   colnames(out) <- dimnames(modelCoef)[[2]]
		#   rownames(out) <- dimnames(modelCoef)[[1]]
		# }
		# as.data.frame(out)
	}
	# tags,
	# oob,
	# notes,
	# check,
)
}
