#' Trains a model possibly based on space-time folds
#'
#' This function is basically a wrapper around functionality from the CAST and
#' caret packages. It expects as input a data.frame object with the predictors
#' and outcome variables present and well as possible variables indicating the
#' spatial and temporal sub-groups of the training data. Either one of the
#' spatial or temporal variables can be used to create individual space or time
#' folds
#'
#' @param traindata A tibble, data.frame or sf object.
#' @param predictors A character vector with the column names used as predictors.
#' @param response A character vector with the name of the response variable.
#' @param spacevar An optional character with a variable indicating spatial
#'   groups to account for during training.
#' @param timevar An optional character with a variable indicating temporal
#'   groups to account for during training.
#' @param k An integer value indicating the number of folds. Defaults to 10.
#' @param ffs A logical indicating if a Forward-Feature-Selection should be
#'   conducted. Note that this could significantly increase training time.
#' @param method A charachter vector indicating a model to be used by caret.
#'   Defaults to \code{"rf"}.
#' @param metric An accuracy metric for model optimization. Defaults to Accuracy
#'   for classification and RMSE for regression. Must be supported by caret.
#' @param maximize A logical indicating if the selected metric should be
#'   maximized. Defaults to TRUE for Accuracy and FALSE for RMSE.
#' @param trControl A train control object based on \code{\link[caret]{trainControl}}.
#' @param seed An integer value used to ensure reproducibility.
#' @param verbose A logical indicating the level of verbosity.
#' @param ... Additional parameters used by \code{\link[CAST]{ffs}} and \code{\link[caret]{train}}.
#'
#' @return A list with the model, performance metrics or confusion matrix (for
#'   regression or classification), a vector of observed values and a vector
#'   of predicted values.
#' @export train_model
#' @importFrom caret train confusionMatrix postResample
#' @importFrom CAST ffs CreateSpacetimeFolds
#' @importFrom stats predict
#' @importFrom stringr str_detect
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
train_model <- function(
  traindata,
  predictors,
  response,
  spacevar = NULL,
  timevar = NULL,
  k = 10,
  ffs = TRUE,
  method = "rf",
  metric = ifelse(is.factor(response)|is.character(response), "Accuracy", "RMSE"),
  maximize = ifelse(metric == "RMSE", FALSE, TRUE),
  trControl = caret::trainControl(),
  seed = 42,
  verbose = TRUE,
  ...
){

  if(!is.null(spacevar) | !is.null(timevar)){
    set.seed(seed)
    if(is.null(spacevar)){
      indices = CreateSpacetimeFolds(traindata,
                                     timevar = timevar,
                                     k = k,
                                     class = response)
    } else if(is.null(timevar)) {
      indices = CreateSpacetimeFolds(traindata,
                                     spacevar = spacevar,
                                     k = k,
                                     class = response)
    } else {
      indices = CreateSpacetimeFolds(traindata,
                                     spacevar = spacevar,
                                     timevar = timevar,
                                     k = k,
                                     class = response)
    }
    if(verbose) message("Spatiotemporal CV was selected. Setting trControl to CV with space-time index for folds.")
    trControl$method = "cv"
    trControl$index = indices$index
    trControl$indexOut = indices$indexOut
  }

  if(any(str_detect(predictors, "-"))){
    stop('Detected a hypen "-" in predictor names. This causes issues during prediction. Please replace, e.g. with underscore "_" or a dot "."')
  }

  if(ffs){
    if(verbose) message("FFS was selected. Starting process...")
    model = ffs(
      predictors = traindata[ ,predictors],
      response = traindata[ ,response],
      method = method,
      metric = metric,
      maximize = maximize,
      trControl = trControl,
      seed = seed,
      verbose = verbose,
      ...)

  } else {

    if(verbose) message("FFS was not selected. Training model with all predictors...")
    model = train(
      x = traindata[ ,predictors],
      y = traindata[ ,response],
      method = method,
      metric = metric,
      maximize = maximize,
      trControl = trControl,
      ...)
  }


  if(model$modelType == "Classification"){

    if(verbose) message("Model type classification. Returning confusion matrix.")
    # prepare confusion matrix
    pred = predict(model, traindata[ ,predictors])
    # coerce character vector to factor
    if(is.character(traindata[ ,response])){
      obs = as.factor(traindata[ ,response])
    } else{
      obs = traindata[ ,response]
    }

    cnf = confusionMatrix(
      data = pred,
      reference = obs,
      mode = "everything")

    return(list(model = model,
                cnf = cnf,
                obs = obs,
                pred = pred))
  } else {

    if(verbose) message("Model type regression Returning performance metrics RMSE, R2, and MAE.")
    pred = predict(model, traindata[ ,predictors])
    obs = traindata[ ,response]
    acc = postResample(pred = pred, obs = obs)

    return(list(model = model,
                acc = acc,
                obs = obs,
                pred = pred))
  }

}

