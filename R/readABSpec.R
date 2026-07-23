#' @title readABSpec.
#' @description \code{readABSpec} will read a MALDI file.
#' @param file Path to a text file containing a MALDI spectrum.
#' @param meta_data If provided will be attached to output. Will be guessed from file header if NULL.
#' @param ... Further parameters to read.table function.
#' @details \code{readABSpec} will read a MALDI file and convert it to a MALDIquant object with metadata..
#' @return A MALDIquant object with metadata.
#' @keywords internal
#' @noRd
readABSpec <- function(file = NULL, meta_data = NULL, limit_mz = NULL, ...) {
  # guess skip parameter
  tmp <- readLines(con = file, n = 20)
  n_skip <- length(grep("[[:alpha:]]", tmp))
  if ((1 + n_skip) <= length(tmp)) {
    sep <- names(which.max(table(gsub("[.[:digit:]-]", "", tmp[(1 + n_skip):length(tmp)]))))
    if (!(sep %in% c("\t", " "))) warning("could not determine sep parameter automatically")
  }
  spec_data <- utils::read.table(
    file = file,
    header = FALSE,
    skip = n_skip,
    sep = sep,
    as.is = TRUE,
    ...
  )
  colnames(spec_data) <- c("mz", "int")
  if (is.null(meta_data)) {
    # try to guess meta data from measurement file
    if (n_skip > 0) {
      tmp <- readLines(con = file, n = n_skip)
      sep <- NULL
      # are metadata separated by colon?
      if (all(grep(":", tmp) %in% 1:length(tmp))) {
        sep <- ":"
        tmp <- gsub("\t", "", tmp)
      }
      # are metadata separated by tab?
      if (is.null(sep) && all(grep("\t", tmp) %in% 1:length(tmp))) {
        sep <- "\t"
        tmp <- sapply(tmp, function(x) {
          x <- gsub("[\t]+", "\t", x)
          ifelse(length(gregexpr("\t", x)) >= 1, gsub("\t$", "", x), x)
        }, USE.NAMES = FALSE)
      }
      # use space as fallback option
      if (is.null(sep) && all(grep(" ", tmp) %in% 1:length(tmp))) {
        sep <- " "
      }
      meta_data <- ldply_base(tmp, function(x) {
        x <- strsplit(x, sep)[[1]]
        c(x[1], ifelse(length(x) >= 2, paste(x[-1]), ""))
      })
      colnames(meta_data) <- c("Type", "Value")
    } else {
      meta_data <- data.frame("Type" = "Name", "Value" = "Unknown", stringsAsFactors = FALSE)
    }
    meta_data <- rbind(c("File", file), meta_data)
  }
  if (!is.null(limit_mz)) spec_data <- spec_data[spec_data[, "mz"] <= limit_mz, , drop = FALSE]
  attr(spec_data, "meta_data") <- meta_data
  return(
    suppressWarnings(
      MALDIquant::createMassSpectrum(
        mass = spec_data[, 1],
        intensity = spec_data[, 2],
        metaData = as.list(sapply(meta_data[, 1], function(x) {
          meta_data[meta_data[, 1] == x, 2]
        }))
      )
    )
  )
}
