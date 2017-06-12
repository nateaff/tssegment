#' Classify segmented time series using a weighted random forest
#'
#' @param  x A formula
#' @param ... Additional arguments for randomForest
#' @return  A random forest model of class segmentRF
#' @export
segmentRF <- function(x, ...){
  suppressWarnings(
    model <- randomForest::randomForest(x,...) 
  ) # end suppress
 	class(model) <- c("segmentRF", class(model))
	model
}


#' Predict a class useing a weighted combination of inputs
#'
#' @param object A segmentRF model
#' @param newdata The data to predict
#' @param weights A vector or data fram of weights
#' @param thresholds A vector of thresholds for each factor
#' @param ... Additional arguments for predict.randomForest
#' @return 
#'
#' A \code{list} of :
#' \tabular{lll}{
#' \code{pred} \tab The predicted factor,\cr
#' \code{prob} \tab A dataframe of probabilities associated with 
#									  each factor, \cr
#' \code{weights} \tab The weights associated with each segment \cr
#' }
predict.segmentRF <- function(object, 
                              newdata,
                              weights, 
                              thresholds = NULL, ...){
    #assuming newdata is not a vector
    if(length(weights) != dim(newdata)[1]){
    	stop("Number of weights do not equal dimension of newdata.")
    	} 
    # hack 
    class(object) <- "randomForest"
    res <- predict(object, newdata, type = "prob", ...)
    ncol = dim(res)[2]
    prob <- lapply(1:ncol, function(c) sum(res[,c]*weights / sum(weights)))
    # For binary classification
    prob <- data.frame(prob)
    names(prob) <- colnames(res)
    if(!is.null(thresholds)){
    	factor(names(prob)[prob > thresholds])
    }
    pred <- factor(names(prob)[which.max(prob)])
    structure(list(pred = pred, 
    							 prob = prob, 
    							 weights = weights),
    							 class = "segmentRF")
}
