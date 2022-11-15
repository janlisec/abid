#' @title Setup_ABID_Lib.
#'
#' @description
#' \code{Setup_ABID_Lib} will Setup_ABID_Lib.
#'
#' @param folder Path to a text file containing a MALDI spectrum.
#' @param limit_n limit_n.
#' @param limit_mz limit_mz.
#' @param depr depr.
#' @param lib_name lib_name.
#'
#' @details
#' {Setup_ABID_Lib} will convert a number of MALDI files from a folder into a library file for ABID using Information stored in Excel as metadata.
#'
#' @examples
#' \dontrun{
#' Setup_ABID_Lib(folder = "C:/Users/jlisec/Documents/Projects/ABID/Libraries/ABIDApp", limit_mz = 7000)
#' }
#'
#' @return
#' A rda file with MALDI spectra and meta data.
#'
#' @importFrom openxlsx read.xlsx
#' @keywords internal
#' @noRd
Setup_ABID_Lib <- function(folder = "C:/Users/jlisec/Documents/Projects/ABID/Libraries/ABIDApp",
                           limit_n = NULL,
                           limit_mz = NULL,
                           depr = 0,
                           lib_name = "abid") {
  # source('~/Projects/ABID/ABID_APP/ABID/R/readABSpec.R')
  # open meta data file
  lib <- openxlsx::read.xlsx(xlsxFile = paste(folder, "ABIDLib.xlsx", sep = "/"), rowNames = TRUE, check.names = FALSE)

  # remove deprecated entries (column 'deprecated')
  lib <- lib[lib[, "N_Deprecated_at_Libversion"] >= depr, ]

  # strip columns not to be transfered to Online App
  lib <- lib[, -grep("^N_", colnames(lib))]

  # check if data files exist in specified folder
  fls <- dir(path = folder, pattern = ".dat$", recursive = TRUE, full.names = TRUE)
  flt <- sapply(lib$file, function(x) {
    x %in% basename(fls)
  })
  if (!all(flt)) warning("Nicht alle Messdaten wurden in den Unterordnern gefunden.")
  lib <- lib[flt, ]

  # remove some Files because of RAM limit
  if (!is.null(limit_n)) lib <- lib[sample(x = 1:nrow(lib), size = limit_n, replace = FALSE), ]

  # importiere Daten und annotiere mit Meta-Daten aus dem Excel
  fls <- sapply(lib$file, function(x) {
    fls[basename(fls) == x]
  })
  abid_lib <- vector("list", length = length(fls))
  for (i in 1:length(fls)) {
    abid_lib[[i]] <- readABSpec(file = fls[i], meta_data = data.frame("Type" = colnames(lib), "Value" = unlist(lib[i, ])), "limit_mz" = limit_mz)
  }
  # save(abid_lib, file = paste(folder,paste0(lib_name, "_lib.RData"), sep="/"))
  save(abid_lib, file = paste(folder, paste0(lib_name, "_lib.rda"), sep = "/"), compress = "bzip2")
}
