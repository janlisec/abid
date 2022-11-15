#' @title get_overlap.
#' @description \code{get_overlap} will identify overlapping masses (numeric values) between two vectors within specified limits.
#' @param x Mass spectrum in two columns or object of class MALDIquant "MassSpectrum".
#' @param y Mass spectrum in two columns or object of class MALDIquant "MassSpectrum".
#' @param type Comparison type.
#' @param dmz Delta mz in milli Dalton.
#' @param ppm Parts per million parameter.
#' @param digits Rounding precision.
#' @details Not yet.
#' @return The output depends in the selected comparison type. See examples for details.
#' @examples 
#' # set up two pseudo spectra
#' x <- data.frame("m"=c(1,2.1,2.09,3.001), "i"=rep(1,4))
#' y <- data.frame("m"=1:3, "i"=rep(1,3))
#' 
#' # the allowed deviation `dmz` will define the number of peaks in x having a match in y
#' get_overlap(x=x, y=y, type="sum")
#' get_overlap(x=x, y=y, type="sum", dmz=0.1)
#' get_overlap(x=x, y=y, type="sum", dmz=0.01)
#' get_overlap(x=x, y=y, type="sum", dmz=0)
#' 
#' # alternative outputs will provide the relative intensity or
#' # mass values of matching peaks from the spectrum
#' get_overlap(x=x, y=y, type="rel", dmz=0)
#' get_overlap(x=x, y=y, type="masses", dmz=0.01)
#' get_overlap(x=x, y=y, type="intweight", dmz=0.01)
#' @importFrom MALDIquant mass intensity
#' @keywords internal
#' @noRd
get_overlap <- function(
  x = NULL, 
  y = NULL, 
  type = c("sum", "rel", "masses", "intweight"), 
  dmz = 2, 
  ppm = 2, 
  digits = 2
) {
  ppm <- ppm / 10^6
  if (inherits(x, c("MassSpectrum", "MassPeaks"))) {
    xm <- mass(x)
    xi <- intensity(x)
  } else {
    xm <- x[, 1]
    xi <- x[, 2]
  }
  if (inherits(x, c("MassSpectrum", "MassPeaks"))) {
    ym <- mass(y)
    yi <- intensity(y)
  } else {
    ym <- y[, 1]
    yi <- y[, 2]
  }
  if (length(xm) == 0 | length(ym) == 0) {
    # avoid errors for comparison with an empty dataset (no peak obtained)
    check_presence <- NA
  } else {
    check_presence <- sapply(xm, function(z) {
      ifelse(any(abs(ym - z) < max(dmz, z * ppm)), which.min(abs(ym - z)), NA)
    })
  }
  # if (is.list(check_presence)) browser()
  out <- switch(type[1],
    "sum" = sum(is.finite(check_presence)),
    "rel" = sum(is.finite(check_presence)) / length(xm),
    "masses" = xm[is.finite(check_presence)],
    "intweight" = sum(xi[is.finite(check_presence)]) / sum(xi),
    "all" = data.frame(
      "matching.peptides" = sum(is.finite(check_presence)),
      "rel" = round(sum(is.finite(check_presence)) / length(check_presence), digits = digits),
      "intweight" = round(sum(xi[is.finite(check_presence)]) / sum(xi), digits = digits),
      "masses" = paste(xm[is.finite(check_presence)], sep = " ", collapse = " "),
      stringsAsFactors = FALSE
    )
  )
  return(out)
}
