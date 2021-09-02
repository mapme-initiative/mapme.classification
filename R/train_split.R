#' Splitting polygons into training and test
#'
#' This function is used to split a data set of polygons into two data sets
#' used for training and testing. Generally the splitting is done based on the
#' response variable to achieve similar class distributions between the training
#' and testing set. Additionally, a second grouping variable can be used to
#' achieve stratification based on that group. The function works on categorical
#' and numerical data. For numeric variables the value range will be categorized
#' with \code{\link[base]{cut}} function and users are required to specify the
#' value breaks.
#'
#' @param aoi A sf object with polygons which are to be split into training and
#'   test. The object should contain one column which uniquely identfies the
#'   polygons, a column with a response variable and potentially a additional
#'   grouping variable used for stratification.
#' @param idcol A character vector indicating the name of the column that uniquely
#'   identifies the polygons.
#' @param response A character vector indicating the name of the column that
#'   contains the response variable which can be either numerical or charachter/factor.
#'   If it is a numerical variable it is mandatory to specify the behavior of
#'   \code{\link[base]{cut}} to discretize the value range into categories.
#' @param group A optional character identifying the name of a column used for
#'   stratification of the training-test split. The values of that column need to
#'   be charachters or factors since grouping by numeric variables currently is not
#'   supported.
#' @param p A numeric between 0 and 1 indicating the fraction of the polygons which
#'   shall enter the training set.
#' @param seed A numeric value used to ensure reproducibility of the split.
#' @param verbose A logical indicating the level of verbosity.
#' @param ... Additional parameters used by \code{\link[base]{cut}} to discretize
#'   the value range of a numeric response variable into categories.
#'
#' @return The original sf object amended by a column named \code{"split"} which
#'   indicates if a polygon belongs to the training or testing data set.
#' @export train_split
#' @importFrom dplyr sym group_by sample_frac ungroup pull if_else
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
train_split <- function(
  aoi,
  idcol,
  response,
  group = NULL,
  p,
  seed = round(runif(1,1L,1000L)),
  verbose = TRUE,
  ...
){

  . = NULL
  if(!inherits(aoi, "sf")){
    stop("aoi must be an sf object.")
  }

  if(any(is.na(aoi[[idcol]]))){
    stop("id variable contains NAs.")
  }

  if(length(unique(aoi[[idcol]])) != nrow(aoi)){
    stop("idcol does not uniquely identify polygons.")
  }

  if(any(is.na(aoi[[response]]))){
    stop("resposne variable contains NAs.")
  }

  as_group = !is.null(group)

  if(as_group){
    if(!any(c(is.character(aoi[[group]]), is.factor(aoi[[group]])))){
      stop("Variable group must be either charachter or factor.")
    }
    if(any(is.na(aoi[[group]]))){
      stop("group variable contains NAs.")
    }
  }




  # prepare symbols for use in dplyr
  id_var = sym(idcol)
  if(as_group) group_var = sym(group)
  response_var = sym(response)

  as_numeric = is.numeric(aoi[[response]])

  if(as_numeric){ # cut values
    .mapmeresp = NULL
    aoi$.mapmeresp = cut(aoi[[response]], ...)

    if(any(is.na(aoi[[".mapmeresp"]]))){
      stop("Arguments applied to cut function produced NAs. Please consuld ?base::cut.")
    }

  } else { # copy response
    aoi$.mapmeresp = aoi[[response]]
  }

  # apply split
  set.seed(seed)
  aoi %>% {
    if(as_group)
      group_by(., !!group_var, .mapmeresp)
    else
      group_by(., .mapmeresp)
  } %>%
    sample_frac(size = p) %>%
    ungroup() %>%
    pull(id_var) -> ids

  index = which(aoi[[idcol]] %in% ids)

  if("split" %in% names(aoi)){
    warning("Column split exists in aoi. Will be overwritten.")
  }

  aoi$split = NA
  aoi$split[index] = "train"
  aoi$split[-index] = "test"

  if(verbose){
    if(as_group) {
      info = table(aoi[[".mapmeresp"]], aoi[[group]], aoi[["split"]])
      train = info[, , 2]
      test = info[, , 1]
    } else {
      info = table(aoi[[".mapmeresp"]], aoi[["split"]])
      train = info[, 2]
      test = info[, 1]
    }
    message("Distribution of train set:\n")
    message(print(train))
    message("Distribution of test set:\n")
    message(print(test))
  }

  if(as_numeric){
    if("cut-label" %in% names(aoi)){
      warning("Column cut-label exists. Will be overwritte.")
    }
    aoi["cut-label"] = aoi$.mapmeresp
  }
  # delete copied response
  aoi$.mapmeresp = NULL
  # return
  aoi
}
