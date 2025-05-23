#' @export
#' @keywords internal

pull_rset_attributes <- function(x) {
  excl_att <- c("names", "row.names")
  att <- attributes(x)
  att_nms <- names(att)
  att_nms <- setdiff(att_nms, excl_att)
  att$class <- setdiff(class(x), class(tibble::new_tibble(list())))
  att$class <- att$class[att$class != "rset"]

  lab <- try(pretty(x), silent = TRUE)
  if (inherits(lab, "try-error")) {
    lab <- NA_character_
  }
  list(att = att[att_nms], label = lab)
}

# ------------------------------------------------------------------------------
