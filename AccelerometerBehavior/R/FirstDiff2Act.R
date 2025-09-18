#' Convert first difference of acceleration in to Vectronics activity data
#' 
#' Convert the mean of absolute values of the first difference of acceleration 
#' in to  Vectronics activity data for use with the AccelerometerBehavior() 
#' function.
#' @param x A number or vector of numbers representing a 5-minute average of the first difference of acceleration from a GPS collar
#' @param unit What units the input data is in. Either "g" or "m/s^2".
#' @examples
#' # Create toy dataset
#' x <- runif(10, 0, 1)
#'                  
#' # Convert the data to activity data
#' FirstDiff2Act(x, unit = "g")
#' 
#' @import stats
#' @export
FirstDiff2Act <- function(x, unit) {
  
  # Data checks ----
  if(!is.numeric(x)) {
    stop("Data is not numeric.")
  }
  if(any(x < 0)) {
    stop("Data contains negative values")
  }
  if(any(is.na(x))) {
    warning("Data contains NA values.")
  }
  if(unit != "g" & unit != "m/s^2") {
   stop("Specify a correct unit.") 
  }
  # Convert the data ----
  if(unit == "g") {
    vals <- (x/0.016)*10
  }
  if(unit == "m/s^2") {
    g <- x/9.80665 
    vals <- (g/0.016)*10
  }
  vals <- ifelse(vals > 255, 255, round(vals))
  # return the results ----
  return(vals)
}

