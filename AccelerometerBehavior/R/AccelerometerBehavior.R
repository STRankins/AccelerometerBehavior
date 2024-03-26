#' Classifying behavior
#' Classifying behavior
#' 
#' Classify 5-minute segments of X, Y, Z, accelerometer data obtained from GPS 
#' collars from moose, bighorn sheep and mule deer into three behavioral states.
#' @param species The species ("moose", "bighorn sheep", or "mule deer").
#' @param x The column name of the X acceleration data.
#' @param y The column name of the Y acceleration data.
#' @param z The column name of the Z acceleration data.
#' @param data The name of the dataframe object with your data.
#' @return Your dataframe object with an added column named "behavior" with a the classified behavior for each 5-minute segment.
#' @examples
#' # Create toy dataset
#' df <- data.frame(ActivityX = round(runif(10, 0, 255), 0),
#'                  ActivityY = round(runif(10, 0, 255), 0),
#'                  ActivityZ = round(runif(10, 0, 255), 0))
#'                  
#' # Classify the data
#' AccelerometerBehavior(species = "moose",
#'                       x = "ActivityX",
#'                       y = "ActivityY",
#'                       z = "ActivityZ",
#'                       data = df)
#' @import randomForest
#' @import stats
#' @import utils
#' @export
AccelerometerBehavior <- function(species, x, y, z, data) {
  # Load randomForest package ----
  suppressMessages(require(randomForest))
  # Some data checks ----
  if(!x %in% names(data)) {
    stop("x is not a valid column name")
  }
  if(!y %in% names(data)) {
    stop("y is not a valid column name")
  }
  if(!x %in% names(data)) {
    stop("z is not a valid column name")
  }
  # Make sure predict knows what columns to use ----
  if(x != "ActivityX") {
    names(data)[which(names(data) == x)] <- "ActivityX"
    Change_X <- TRUE
  } else {
    Change_X <- FALSE
  }
  if(y != "ActivityY") {
    names(data)[which(names(data) == y)] <- "ActivityY"
    Change_Y <- TRUE
  } else {
    Change_Y <- FALSE
  }
  if(z != "ActivityZ") {
    names(data)[which(names(data) == z)] <- "ActivityZ"
    Change_Z <- TRUE
  } else {
    Change_Z <- FALSE
  }
  # More data checks ----
  if(!species %in% c("mule deer", "bighorn sheep", "moose")) {
    stop("Please provide a valid species.")
  }
  if(!is.data.frame(data)) {
    stop("Data is not a dataframe.")
  }
  if(!is.numeric(data$ActivityX)) {
    stop("Column x is not numeric.")
  }
  if(!is.numeric(data$ActivityY)) {
    stop("Column y is not numeric.")
  }
  if(!is.numeric(data$ActivityZ)) {
    stop("Column z is not numeric.")
  }
  if(any(is.na(data$ActivityX))) {
    stop("Column x contains NA's.")
  }
  if(any(is.na(data$ActivityY))) {
    stop("Column y contains NA's.")
  }
  if(any(is.na(data$ActivityZ))) {
    stop("Column z contains NA's.")
  }
  # Start of classification code ----
  if(species == "mule deer") {
    
    data$behavior <- stats::predict(AccelerometerBehavior:::mods$class_deer,
                                    data)
  
    }
  if(species == "bighorn sheep") {
    
    data$behavior <- stats::predict(AccelerometerBehavior:::mods$class_bighorn,
                                    data)
    
    }
  if(species == "moose") {
   
    data$behavior <- stats::predict(AccelerometerBehavior:::mods$class_moose,
                                    data)
     
  }
  # Change column names back to original if needed ----
  if(Change_X) {
    names(data)[which(names(data) == "ActivityX")] <- x
  }
  if(Change_Y) {
    names(data)[which(names(data) == "ActivityY")] <- y
  }
  if(Change_Z) {
    names(data)[which(names(data) == "ActivityZ")] <- z
  }
  # return the results ----
  return(data)
}
