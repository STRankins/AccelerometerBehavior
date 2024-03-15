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
#'                       y = "ActivityX",
#'                       z = "ActivityX",
#'                       data = df)
#' @export
AccelerometerBehavior <- function(species, x, y, z, data) {
  # ---- Some data checks
  if(!x %in% names(data)) {
    stop("x is not a valid column name")
  }
  if(!y %in% names(data)) {
    stop("y is not a valid column name")
  }
  if(!x %in% names(data)) {
    stop("z is not a valid column name")
  }
  # Get the columns we need ----
  x_name <- names(data)[which(names(data) == x)]
  y_name <- names(data)[which(names(data) == y)]
  z_name <- names(data)[which(names(data) == z)]
  # More data checks ----
  if(!species %in% c("mule deer", "bighorn sheep", "moose")) {
    stop("Please provide a valid species.")
  }
  if(!is.data.frame(data)) {
    stop("Data is not a dataframe.")
  }
  if(!is.numeric(data[[x_name]])) {
    stop("Column x is not numeric.")
  }
  if(!is.numeric(data[[y_name]])) {
    stop("Column y is not numeric.")
  }
  if(!is.numeric(data[[z_name]])) {
    stop("Column z is not numeric.")
  }
  if(any(is.na(data[[x_name]]))) {
    stop("Column x contains NA's.")
  }
  if(any(is.na(data[[y_name]]))) {
    stop("Column y contains NA's.")
  }
  if(any(is.na(data[[z_name]]))) {
    stop("Column z contains NA's.")
  }
  # Start of classification code ----
  if(species == "mule deer") {
    
    data$behavior <- ifelse(data[[x_name]] < 8,
                            "Stationary",
                            ifelse(data[[z_name]] < 42,
                                   "Foraging",
                                   "Travelling"))
  
    }
  if(species == "bighorn sheep") {
    
    data$behavior <- ifelse(data[[y_name]] < 10.5,
                            ifelse(data[[y_name]] < 2.5,
                                   "Stationary",
                                   ifelse(data[[y_name]] < 9.5,
                                          ifelse(data[[z_name]] < 6,
                                                 ifelse(data[[x_name]] < 1.5,
                                                        "Travelling",
                                                        "Stationary"),
                                                 "Travelling"),
                                          "Stationary")), 
                            ifelse(data[[x_name]] < 59.5,
                                   ifelse(data[[x_name]] < 26.5,
                                          ifelse(data[[x_name]] < 20.5,
                                                 "Stationary",
                                                 ifelse(data[[x_name]] < 25.5,
                                                        "Foraging",
                                                        ifelse(data[[z_name]] < 8,
                                                               "Foraging",
                                                               "Travelling"))),
                                          "Foraging"),
                                   "Travelling"))
    
    }
  if(species == "moose") {
   
    data$behavior <- ifelse(data[[y_name]] < 3.5,
                            "Stationary",
                            ifelse(data[[x_name]] < 35.5,
                                   ifelse(data[[y_name]] < 9.5,
                                          ifelse(data[[y_name]] < 5.5,
                                                 ifelse(data[[x_name]] < 17.5,
                                                        "Stationary",
                                                        "Foraging"),
                                                 ifelse(data[[x_name]] < 17.5,
                                                        "Travelling",
                                                        ifelse(data[[z_name]] < 10.5,
                                                               ifelse(data[[x_name]] < 26,
                                                                      "Foraging",
                                                                      "Stationary"),
                                                               "Foraging"))),
                                          "Foraging"),
                                   ifelse(data[[x_name]] < 54,
                                          ifelse(data[[x_name]] < 39,
                                                 ifelse(data[[y_name]] < 30,
                                                        "Foraging",
                                                        "Travelling"),
                                                 ifelse(data[[y_name]] < 31.5,
                                                        "Foraging",
                                                        "Travelling")),
                                          ifelse(data[[x_name]] < 73.5,
                                                 "Foraging",
                                                 "Travelling"))
                                   ))
     
    }
  return(data)
}
