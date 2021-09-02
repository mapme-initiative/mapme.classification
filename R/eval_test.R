#' Evaluate a testing set
#'
#' Based on a model returned by \code{link{mapme.forest}[train_model]} and a
#' test set a confusion matrix is returned.
#'
#' @param model A model object.
#' @param testset A data.frame containing the test data to predict
#' @param response A character vector indicating the column name of the response
#'   variable.
#'
#' @return A list with a confusion matrix and the test set amended by a column called pred
#'   containing the predicted values.
#' @export eval_test
#' @importFrom stats predict
#' @importFrom caret confusionMatrix
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
eval_test <- function(
  model,
  testset,
  response){

  pred = predict(model, testset)
  testset$pred = pred
  cnf = confusionMatrix(
    data = pred,
    reference = testset[[response]],
    mode = "everything")
  list(cnf = cnf, data = testset)
}


#' Spatial prediction of a trained model
#'
#' This function is used to spatially predict a trained model on a SpatRaster
#' object with the predictors. Additionally users can opt in to calculate the
#' Area of Applicability based on \code{link{CAST}[aoa]}.
#'
#' @param model A model object.
#' @param predictors A spatRaster with the predictor variables.
#' @param AOA A logical indicating if the Area of Applicability shall be calculated.
#'   Defaults to FALSE. Can be quite a costly calculation. Check the documentation of
#'   \code{link{CAST}[aoa]} for possible speed ups via the ... arguments, i.e.
#'   through parallel computation.
#' @param mode A character vector indicating the behavior of \code{link{terra}[predict]}.
#'   If \code{type = "raw"} one raster layer with the predicted class is returned.
#'   If the model is a regression model the type cannot be set to "proba".
#'   If \code{type = "proba"} for each outcome class a single layer is returned
#'   indicating the probability of a pixel to be classified as that respective class.
#' @param ncores An integer specifying the number of cores to be used for parallel computations.
#' @param mask A spatRaster object with at the same resoultion as the predictor stack.
#'   All raster cells unqual to NA will be masked out of the predictor stack to
#'   omit predictions at these locations
#' @param ... Additional arguments used for \code{link{terra}[predict]} and \code{link{CAST}[aoa]}.
#'
#' @return A SpatRaster object with at least one layer which is the classification
#'   result. Depending if and how the AOA is calculated two additional layers are
#'   possible representing the AOA itself and the DI. Check \code{link{CAST}[aoa]}
#'   for more information.
#' @export spatial_predict
#' @importFrom CAST aoa
#' @importFrom terra levels predict
#' @importFrom parallel makeCluster stopCluster
#' @importFrom caret predictors
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
spatial_predict <- function(
  model,
  predictors,
  AOA = TRUE,
  mode = "raw",
  ncores = 1,
  mask = NULL,
  ...
){

  iscv = "finalModel" %in% names(model)

  if(iscv){
    variables = predictors(model)
    modelType = model$modelType
  } else {
    variables = model$xNames
    modelType = predictors(model)
  }

  if(!mode %in% c("response", "raw", "proba")){
    stop('type must be one of "response", or "proba".')
  }

  if(mode == "proba" &  modelType %in% c("classification", "Classification")){
    warning('type = "proba" was selected for a Regression model.\n
            Setting to "response".')
    mode = "response"
  }

  if(mode == "response" & iscv){
     mode = "raw"
   }

  if(any(!names(predictors) %in% variables)){
    warning("Some variables are present in predictors which are not found in the model.\n
          Trying to reduce the predictor stack to relevant predictors only")
    predictors = predictors[[which(names(predictors) %in% variables)]]
  }

  if(!is.null(mask)){
    test = c(predictors[[1]], mask)
    predictors[!is.na(mask)] = NA
  }

  pred = terra::predict(
    object = predictors,
    model = model,
    na.rm = T,
    type = mode,
    cores = ncores,
    ...)

  if(modelType %in% c("Classification", "classification") & mode == "raw"){
    pred = pred - 1
    levels(pred) = model$finalModel$classes
    names(pred) = "class"
  }

  if(AOA){
    if(ncores > 1){
      cl <- makeCluster(ncores)
      AOA = CAST::aoa(
      newdata = predictors,
      model = model,
      cl = cl,
      ...)
      stopCluster(cl)

    } else {
      AOA = CAST::aoa(
        newdata = predictors,
        model = model,
        ...)
    }
    pred = c(pred, AOA)
  }
  pred

}


#' Post-classification by focal operation
#'
#' Uses terra::focal to apply a post-classification based on a spatial moving
#' window.
#'
#' @param pred A SpatRaster object corresponding to the prediction
#' @param fun function that takes multiple numbers, and returns a single number.
#'   For example mean, modal, min or max. It should also accept a na.rm argument,
#'   either as actual argument or through use of ... See \code{link{terra}[focal]}.
#' @param w The window can be defined as one (for a square) or two numbers (row, col);
#'   or with an odd-sized weights matrix. See \code{link{terra}[focal]}.
#' @param ... Additional parameters used by See \code{link{terra}[focal]}.
#'
#' @return A SpatRaster object.
#' @export postclass
#' @importFrom terra focal
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
postclass <- function(
  pred,
  w,
  fun = "modal",
  ...
){
  pred_w = focal(pred, w = w, fun = fun, ...)
  levels(pred_w) = levels(pred)
  pred_w
}


#' Area estimation for projected rasters
#'
#' Area is estimated based on the pixel resolution. Projected CRS should be
#' used of meaningful area estimation because non-projected CRS, e.g. geographic
#' coordinates do not have regular cell size.
#'
#' @param pred A categorical raster
#' @param cnf A confusion matrix from \code{link[caret]{confusionMatrix}}.
#' @param conf A confidence interval
#'
#' @return A table with columns cell with original and adujsted area estiamates
#'   per class as well Producer's, Users's and Overall accuracies with confidence
#'   interval.
#' @references Olofson P., Foody G.M., Herold M., Stehman S.V., Woodcock C.E., Wulder M.A. 2014.
#'  Good practices for estimating area and assessing accuracy of land change
#'  \url{https://doi.org/10.1016/j.rse.2014.02.015}
#' @export area_estimation
#' @importFrom terra nlyr res values
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
area_estimation <- function(
  pred,
  cnf,
  conf = 1.95){

  lyrname = names(pred)
  lyrnum = nlyr(pred)
  x_res = res(pred)[1]
  y_res = res(pred)[2]

  est = lapply(1:lyrnum, function(i) {

    # prep area values
    vals = values(pred[[i]])
    cellnum = table(vals)
    cellarea = as.numeric(cellnum * x_res * y_res)

    # calculate parameters
    A = sum(cellarea)
    W_i = cellarea / A
    n_i = colSums(cnf$table)
    p = W_i * t(cnf$table) / n_i
    p[is.na(p)] = 0

    # area estimation
    p_area = colSums(p) * A
    p_area_CI <- conf * A * sqrt(colSums((W_i * p - p ^ 2) / (n_i - 1)))

    # accuracies
    OA = sum(diag(p))
    PA = diag(p) / colSums(p)
    UA = diag(p) / rowSums(p)
    # CIs
    OA_CI <- conf * sqrt(sum(W_i ^ 2 * UA * (1 - UA) / (n_i - 1)))
    UA_CI <- conf * sqrt(UA * (1 - UA) / (n_i - 1))
    nclass = nrow(cnf$table)
    N_j <- sapply(1:nclass, function(x) sum(cellarea / n_i * t(cnf$table)[ , x]) )
    tmp <- sapply(1:nclass, function(x) sum(cellarea[-x] ^ 2 * t(cnf$table)[-x, x] / n_i[-x] * ( 1 - t(cnf$table)[-x, x] / n_i[-x]) / (n_i[-x] - 1)) )
    PA_CI <- conf * sqrt(1 / N_j ^ 2 * (cellarea ^ 2 * ( 1 - PA ) ^ 2 * UA * (1 - UA) / (n_i - 1) + PA ^ 2 * tmp))

    # gather results
    result <- matrix(c(rownames(cnf$table), cellarea, p_area, p_area_CI, PA, PA_CI, UA, UA_CI, c(OA, rep(NA, nrow(cnf$table)-1)), c(OA_CI, rep(NA, nrow(cnf$table)-1))), nrow = nrow(cnf$table))
    #result <- round(result, digits = 2)
    colnames(result) <- c("class", "area_org", "area_adj", "area_adj.sd", "PA", "PA.sd", "UA", "UA.sd", "OA", "OA.sd")
    result = as.data.frame(result)
    result
  })
  names(est) = lyrname
  if(length(est) == 1) est = est[[1]]
}
