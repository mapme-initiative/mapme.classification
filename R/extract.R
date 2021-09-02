#' Extract pixel values from a predictor set per polygon
#'
#' This function is used to extract pixel values of a predictor set of type
#' SpatRaster on a polygon basis of a sf object. The CRS of the two objects
#' should be the same, however, the function will try to transform the aoi
#' object if that is not the case. See \code{\link[terra]{extract}} for fine
#' controll of the extraction process.
#'
#' @param aoi A sf object with polygons for which pixel values are extracted.
#' @param predictors A SpatRaster object for each layer the pixel values are
#'   extracted. The column names of the resulting object will be based on the
#'   layer names.
#' @param verbose A logical indicating the level of verbosity.
#' @param ... Additional parameters used by \code{\link[terra]{extract}}.
#'
#' @return matrix, list, or data.frame
#' @export extract_traindata
#' @importFrom terra vect crs project extract
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr left_join
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
extract_traindata <- function(aoi,
                              predictors,
                              verbose = TRUE,
                              ...){


  if(!inherits(predictors, "SpatRaster")){
    stop("Argument predictors is not a SpatRaster object.")
  }

  if(!inherits(aoi, c("sf"))){
    stop("Argument aoi must be an sf object.")
  }

  if(verbose) message("Coercing sf object to SpatVect.")
  aoi_spat = vect(aoi)

  if(verbose) message("Checking projections.")
  crs_predictors = crs(predictors)
  crs_aoi = crs(aoi_spat)

  if(crs_aoi != crs_predictors){
    if(verbose) message("Projections are not equal. Trying to project aoi.")
    tryCatch({
      aoi_spat = project(aoi_spat, predictors)},
      error = function(e){
        message(e)
        stop("Projection of aoi failed. Make sure aoi and predictors have the same CRS.")
      }
    )
  }

  if(verbose) message("Starting extraction. This might take a while...")
  data = extract(predictors, aoi_spat, ...)

  if(verbose) message("Joining aoi metadata with extracted values.")
  aoi_meta = st_drop_geometry(aoi)
  aoi_meta$.mapmeid = 1:nrow(aoi)
  data = left_join(aoi_meta, data, by = c(".mapmeid" = "ID"))
  data$.mapmeid = NULL
  data
}
