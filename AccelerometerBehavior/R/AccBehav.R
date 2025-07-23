#' Classifying behavior from activity count data
#' 
#' Classify 5-minute segments of activity count data from Telonics GPS collars  
#' worn by mule deer into three behavioral states (Foraging, stationary, and 
#' Travelling). Refer to Riginos et al. (2013) Mule Deer (Odocoileus hemionus) 
#' Movements and Habitat Use Patterns in Relation to Roadways in Northwest 
#' Wyoming for details of data collection. Prediction accuracy of the function 
#' AccBehav() was 74 percent (95 percent C.I. 66-81%) and the area under the receiver 
#' operating curve (AUC) was 0.92 (95 percent C.I. 0.88-0.95)
#' @param ActivityCount A number or vector of numbers representing the 5-minute activity count data from a Telonics GPS collar.
#' @return The most likely behavior for the activity count value.
#' @examples
#' # Create toy dataset
#' x <- round(rlnorm(50, 3, 1))
#'                  
#' # Classify the data
#' AccBehav(x)
#' 
#' @import stats
#' @export
AccBehav <- function(ActivityCount) {

  # Data checks ----
  if(!is.numeric(ActivityCount)) {
    stop("Activity count data is not numeric.")
  }
  if(any(is.na(ActivityCount))) {
    warning("Activity count data contains NA values.")
  }
  # Classification code ----
  pred <- ifelse(ActivityCount < 19, "Stationary",
                 ifelse(ActivityCount < 88, "Foraging", "Travelling"))
  # return the results ----
  return(pred)
}
