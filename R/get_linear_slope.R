#'@importFrom purrr possibly
NULL
#' Get the linear slope when passing the significance test
#' @export
#' @param obj A \code{lm} object.
#' @param sig Numeric. The significance level for testing. If not asigned, pass the test.
#' @param fill Numeric. The value used when not passing the significance test
#' @return If the model is significant return the linear slope, else \code{NA}.
#' @examples
#' \dontrun{
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm_obj <- lm(weight ~ group)
#' lm_obj2 <- lm(weight ~ group - 1)
#' get_linear_slope(lm_obj, 0.05)
#' get_linear_slope(lm_obj2, 0.05)
#' }
get_linear_slope <- function(obj, sig, fill = NA){
  if (class(obj) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(obj)$fstatistic
  p <- purrr::possibly(pf, otherwise = 1)(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  if(missing(sig)){
    return(coefficients(obj)[2])
  }
  if (p <= sig){
    return(coefficients(obj)[2])
  } else {
    return(as.numeric(fill))
  }
}

