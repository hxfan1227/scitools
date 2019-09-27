NULL
#' Count the number of the missing values in a vector
#' @param x Vector. A vector containing missing values
#' @return the number of the missing values in the target vector
#' @export
#' @examples
#' \dontrun{
#' x <- c(1, 2, 3, NA, 4, 5, NA, 6, NA, 7)
#' count_na(x)
#' library(data.table)
#' dt <- data.table(
#' x = c(1, 2, 3, NA, 4, 5, NA, 6, NA, 7),
#' y = c(1, NA, 3, NA, 4, 5, NA, 6, NA, 7)
#' )
#' dt[, lapply(.SD, count_na)]
#' }
count_na <- function(x){
  sum(is.na(x))
}
