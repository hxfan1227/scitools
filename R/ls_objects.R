NULL
#' Improved list of objects
#' @export
#' @param pos an alternative argument to name for specifying the environment as a position in the search list.
#' Mostly there for back compatibility.
#' @param pattern an optional regular expression. Only names matching pattern are returned.
#' glob2rx can be used to convert wildcard patterns to regular expressions.
#' @param order.by the column to sort by
#' @param decreasing logical. Should the sort order be increasing or decreasing?
#' @param head logical. Should return first n parts of the results?
#' @param n a single integer. If positive or zero, size for the resulting object: number of elements for a vector (including lists),
#' rows for a matrix or data frame or lines for a function. If negative, all but the n last/first number of elements of x.
#' @return a data.frame
ls_objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  names <- ls(pos = pos, pattern = pattern)
  if (length(names) == 0) {
    return(NA)
  }
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}
NULL
#' shorthand version of ls_objects
#' @export
#' @rdname ls_objects
lsos <- function(..., n=10) {
  ls_objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
