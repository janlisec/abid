#' @title GetGroupFactor.
#' @description \code{GetGroupFactor} will group a numeric vector according to a specified gap.
#' @param x Numeric vector.
#' @param gap Numeric gap to breaks x into groups.
#' @details tbd.
#' @return A factor vector that can be used to split x into groups.
#' @keywords internal
#' @noRd
GetGroupFactor <- function(x, gap) {
  stopifnot(is.numeric(x))
  idx <- rank(x)
  x <- x[order(x)]
  x <- c(T, diff(x) > gap)
  x <- factor(rep(1:sum(x), times = diff(c(which(x), length(x) + 1))))
  return(x[idx])
}
