#' @title ldply_base
#' @description A base R implementation of plyr::ldply
#' @param .data A list or vector.
#' @param .fun Function to apply to each item.
#' @param .progress Show progress bar if 'text'.
#' @param .id Name of the index column (used if .data is a named list). Pass NULL to avoid creation of the index column. For compatibility, omit this argument or pass NA to avoid converting the index column to a factor; in this case, ".id" is used as colum name.
#' @param ... Arguments to .fun.
#' @examples
#' x <- list(a = data.frame(x = 1:2, y = 5:6), b = data.frame(x = 3:4, y = 7:8))
#' ldply_base(x)
#' ldply_base(x, .id = NULL)
#' ldply_base(unname(x))
#' ldply_base(x, .id = "test")
#' # compare against standard plyr::ldply
#' #plyr::ldply(x, .id="test")
#' str(ldply_base(x))
#' #str(plyr::ldply(x))
#' x <- c("01.01.2025","02.01.2025")
#' ldply_base(x, as.Date.character, tryFormats = "%d.%m.%Y")
#' #plyr::ldply(x, as.Date.character, tryFormats = "%d.%m.%Y")
#' @keywords internal
#' @noRd
ldply_base <- function(.data, .fun = identity, .progress = "none", .id = NA, ...) {
  n <- length(.data)
  
  if (is.character(.fun)) { .fun <- match.fun(.fun) }
  
  if (.progress == "text") { pb <- utils::txtProgressBar(min = 0, max = n, style = 3) }
  
  result <- vector("list", n)
  for (i in seq_along(.data)) {
    if (.progress == "text") utils::setTxtProgressBar(pb, i)
    result[[i]] <- .fun(.data[[i]], ...)
  }
  
  if (.progress == "text") close(pb)
  
  # are all elements atomic and no matrix
  if (all(vapply(result, is.atomic, logical(1))) && !any(vapply(result, is.matrix, logical(1)))) {
    # do all elements share the same class
    classes <- vapply(result, function(x) paste(class(x), collapse = ","), character(1))
    if (length(unique(classes)) == 1) {
      # combine and re-assign class
      combined <- unlist(result, use.names = FALSE)
      class(combined) <- strsplit(unique(classes), ",")[[1]]
      df <- data.frame("V1" = combined, row.names = NULL, check.names = FALSE)
    } else {
      # fall back case for different classes
      df <- data.frame(lapply(do.call(rbind, lapply(result, as.list)), I), row.names = NULL, check.names = FALSE)
    }
  } else {
    # complex elements
    df <- do.call(rbind, lapply(result, function(x) {
      if (is.atomic(x) && !is.matrix(x)) {
        as.data.frame(as.list(x), check.names = FALSE)
      } else {
        as.data.frame(x, check.names = FALSE)
      }
    }))
    rownames(df) <- NULL
    
  }
  
  # add .id column if .id is NA and list is named; omit id column for .id=NULL in any case
  if (!is.null(.id)) {
    if (!is.na(.id) | is.na(.id) && !is.null(names(.data))) {
      if (!is.null(names(.data))) {
        ids <- rep(names(.data), times=sapply(result,nrow))
        if (!is.na(.id)) ids <- factor(ids)
      } else {
        ids <- rep(seq_len(n), times = sapply(result, nrow))
      }
      df <- cbind(stats::setNames(data.frame(ids), ifelse(is.na(.id), ".id", .id)), df)
    }
  }
  
  return(df)
}

#' Switch panel
#'
#' Creates a panel with a title, a switch and additional UI elements.
#'
#' @param ... UI elements to place after the switch.
#' @param label Panel title.
#' @param switch_id Shiny input id.
#' @param value Switch state: TRUE or FALSE.
#' @param width Optional CSS width.
#'
#' @return A shiny tag object.
#' @keywords internal
#' @noRd
#'
switch_panel <- function(..., label, switch_id, value = TRUE, width = 180) {
  shiny::div(
    #class = "border rounded p-2 h-100 d-flex flex-column",
    class = "d-flex flex-column",
    style = if (!is.null(width)) {
      paste0("width:", width, "px;")
    },
    shiny::div(
      class = "fw-semibold mb-2",
      label
    ),
    shiny::div(
      style = "display:grid; grid-template-columns:auto 1fr; column-gap:0.5rem; align-items:start;",
      bslib::input_switch(id = switch_id, width = "40px", value = value, label = NULL),
      shiny::div(
        class = "flex-grow-1",
        style = "min-width:0;",
        ...
      )
    )
  )
}